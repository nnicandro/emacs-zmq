#include "core.h"
#include <assert.h>

/**
   Linked list of global references to be freed eventually.
*/
typedef struct ezmq_globref_t {
    struct ezmq_globref_t *next;
    emacs_value val;
} ezmq_globref_t;

static ezmq_globref_t *globrefs = NULL;

#define ERR_CASE(err)                                              \
    case err: {                                                    \
        const char *msg = zmq_strerror(err);                       \
        ptrdiff_t len = (ptrdiff_t)strlen(msg);                    \
        ezmq_signal(INTERN("zmq-"#err), 1, STRING(msg, len));      \
    }                                                              \
    break;

void
ezmq_signal_error()
{
    int en = zmq_errno();
    switch(en) {
    ERR_CASE(EINVAL);
    ERR_CASE(EPROTONOSUPPORT);
    ERR_CASE(ENOCOMPATPROTO);
    ERR_CASE(EADDRINUSE);
    ERR_CASE(EADDRNOTAVAIL);
    ERR_CASE(ENODEV);
    ERR_CASE(ETERM);
    ERR_CASE(ENOTSOCK);
    ERR_CASE(EMTHREAD);
    ERR_CASE(EFAULT);
    ERR_CASE(EINTR);
    ERR_CASE(ENOTSUP);
    ERR_CASE(ENOENT);
    ERR_CASE(ENOMEM);
    ERR_CASE(EAGAIN);
    ERR_CASE(EFSM);
    ERR_CASE(EHOSTUNREACH);
    ERR_CASE(EMFILE);
    default: {
        const char *msg = zmq_strerror(zmq_errno());
        ptrdiff_t len = (ptrdiff_t)strlen(msg);
        ezmq_signal(Qzmq_error, 1, STRING(msg, len));
    }
    }
}

#undef ERR_CASE

void *
ezmq_malloc(size_t nbytes)
{
    void *buf = NULL;
    if(!NONLOCAL_EXIT()) {
        buf = (void *)malloc(nbytes);
        if(!buf) ezmq_signal_error();
    }
    return buf;
}

static char const *
ezmq_type_string(enum ezmq_obj_t type)
{
    switch(type) {
    case EZMQ_CONTEXT:
        return "zmq-context";
    case EZMQ_SOCKET:
        return "zmq-socket";
    case EZMQ_MESSAGE:
        return "zmq-message";
    case EZMQ_POLLER:
        return "zmq-poller";
    default:
        return NULL;
    }
}

static emacs_value
ezmq_type_symbol(enum ezmq_obj_t type)
{
    char const *str = ezmq_type_string(type);
    return str ? INTERN(str) : Qnil;
}

void
ezmq_signal(emacs_value err, int nargs, ...)
{
    va_list args;
    emacs_value data[nargs];
    va_start(args, nargs);
    int i;
    for(i = 0; i < nargs; i++) {
        data[i] = va_arg(args, emacs_value);
    }
    SIGNAL(err, FUNCALL(Qlist, nargs, data));
}

static void
ezmq_wrong_object_type(ezmq_obj_t *obj, enum ezmq_obj_t expected)
{
    ezmq_wrong_type_argument(ezmq_type_symbol(obj->type),
                             1, ezmq_type_symbol(expected));
}

void
ezmq_wrong_type_argument(emacs_value val, int nvalid, ...)
{
    va_list args;
    va_start(args, nvalid);
    if(nvalid == 1) {
        SIGNAL(Qwrong_type_argument, LIST(2, val, va_arg(args, emacs_value)));
    } else if(nvalid > 1) {
        emacs_value options[nvalid + 1];
        options[0] = INTERN("or");
        int i;
        for(i = 1; i < nvalid + 1; i++) {
            options[i] = va_arg(args, emacs_value);
        }
        emacs_value options_list = FUNCALL(Qlist, nvalid + 1, options);
        SIGNAL(Qwrong_type_argument, LIST(2, val, options_list));
    } else {
        SIGNAL(Qwrong_type_argument, LIST(1, val));
    }
}

void
ezmq_args_out_of_range(emacs_value val, emacs_value range)
{
    SIGNAL(Qargs_out_of_range, LIST(2, val, range));
}

int
ezmq_obj_of_type(emacs_value val, enum ezmq_obj_t type)
{
    ezmq_obj_t *obj = USER_PTR(val);
    if(!NONLOCAL_EXIT() &&
       USER_FINALIZER(val) == &ezmq_obj_finalizer &&
       obj->type == type) {
        return 1;
    } else {
        CLEAR_NONLOCAL_EXIT();
        return 0;
    }
}

ezmq_obj_t *
ezmq_extract_obj(enum ezmq_obj_t type, emacs_value val)
{
    ezmq_obj_t *obj = USER_PTR(val);
    if(!NONLOCAL_EXIT()) {
        if(USER_FINALIZER(val) == &ezmq_obj_finalizer) {
            if(obj->type != type) {
                ezmq_wrong_object_type(obj, type);
            } else if(type == EZMQ_SOCKET && obj->obj == NULL) {
                const char *msg = "Socket closed";
                ezmq_signal(INTERN("zmq-ENOTSOCK"), 1, STRING(msg, strlen(msg)));
            }
        } else {
            ezmq_wrong_type_argument(val, 1, ezmq_type_symbol(type));
        }
    }
    return obj;
}

ezmq_obj_t *
ezmq_new_obj(enum ezmq_obj_t type, void *obj)
{
    ezmq_obj_t *eobj = ezmq_malloc(sizeof(*eobj));
    if(!NONLOCAL_EXIT()) {
        switch(type) {
        case EZMQ_MESSAGE:
            if(!obj) {
                obj = ezmq_malloc(sizeof(zmq_msg_t));
                if(!obj) {
                    free(eobj);
                    return NULL;
                }
            }
            break;
        default: break;
        }
        eobj->obj = obj;
        eobj->type = type;
        eobj->val = NULL;
    }
    return eobj;
}

// TODO: Handle the case of multiple context objects, we
// will need to join multiple threads at the end.
void
ezmq_obj_finalizer(void *ptr)
{
    ezmq_obj_t *obj = (ezmq_obj_t *)ptr;

    switch(obj->type) {
    case EZMQ_MESSAGE:
        zmq_msg_close(obj->obj);
        break;
    case EZMQ_SOCKET: {
        // If obj == NULL, zmq_close has already been called. See ezmq_close.
        if(obj->obj) {
            // http://zguide.zeromq.org/page:all#Making-a-Clean-Exit
            int opt = 0;
            zmq_setsockopt(obj->obj, ZMQ_LINGER, &opt, sizeof(opt));
            zmq_close(obj->obj);
        }
        break;
    }
    case EZMQ_CONTEXT: {
        int max_tries = 3;
        int i = 0;
        // Since a socket holds a reference to the context that was used to
        // create it (see ezmq_socket), by the time a context is finalized, all
        // sockets of the context have already been closed and their LINGER
        // periods set to 0 so this context termination call should not fail
        // due to open sockets. The loop is here as a sanity check.
        while((zmq_ctx_term(obj->obj) == -1) && (zmq_errno() == EINTR) && i < max_tries)
            i += 1;
        break;
    }
    case EZMQ_POLLER:
        zmq_poller_destroy(&(obj->obj));
        break;
    }
    ezmq_free_obj(obj);
}

emacs_value
ezmq_new_obj_ptr(ezmq_obj_t *obj)
{
    return env->make_user_ptr(env, &ezmq_obj_finalizer, obj);
}

void
ezmq_free_obj(ezmq_obj_t *obj)
{
    if(obj) {
        ezmq_debug("ezmq_free_obj(%s)\n", ezmq_type_string(obj->type));
        if(obj->type == EZMQ_MESSAGE) {
            free(obj->obj);
        }
        ezmq_push_globref(obj);
        free(obj);
    }
}

void
ezmq_obj_set_val(ezmq_obj_t *obj, emacs_value val)
{
    if(!NONLOCAL_EXIT()) {
        ezmq_push_globref(obj);
        obj->val = val ? GLOBREF(val) : NULL;
    }
}

emacs_value
ezmq_obj_get_val(ezmq_obj_t *obj)
{
    return obj->val ? obj->val : Qnil;
}

void
ezmq_push_globref(ezmq_obj_t *obj)
{
    if(obj->val) {
        ezmq_debug("push globref\n");
        ezmq_globref_t *el = malloc(sizeof(*el));
        assert(el != NULL);
        el->next = globrefs;
        el->val = obj->val;
        globrefs = el;
    }
}

emacs_value
ezmq_pop_globref()
{
    if(globrefs) {
        ezmq_debug("pop globref\n");
        ezmq_globref_t *el = globrefs;
        emacs_value val = globrefs->val;
        globrefs = globrefs->next;
        free(el);
        return val;
    } else
        return NULL;
}

char *
ezmq_copy_string(emacs_value str, ptrdiff_t *size)
{
    *size = 0;
    ptrdiff_t sz = 1;
    if(!env->copy_string_contents(env, str, NULL, &sz)) return NULL;
    char *buf = ezmq_malloc(sz);
    env->copy_string_contents(env, str, buf, &sz);
    // The size returned by copy_string_contents contains the terminanting NULL
    // byte.
    if(size != NULL) *size = sz - 1;
    return buf;
}
