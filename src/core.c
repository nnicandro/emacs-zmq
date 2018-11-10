#include "core.h"
#include <pthread.h>
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
    // TODO: Define error symbols for common errors and use Qzmq_error as a
    // catch all. Look at the zmq documentation for all of the errors that can
    // occur.
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
    ezmq_obj_t *obj = NILP(val) ? NULL : USER_PTR(val);
    if(obj != NULL && !NONLOCAL_EXIT() && obj->type != type)
        ezmq_wrong_object_type(obj, type);
    return obj;
}

static void *
ezmq_wait_for_context_destruction(void *obj)
{
    ezmq_obj_t *ctx = (ezmq_obj_t *)obj;
    if((zmq_ctx_term(ctx->obj) == -1) && (zmq_errno() == EINTR)) {
        pthread_t thread;
        // If we are in here, the context is no longer
        // accessible from Emacs so we can't really do much
        // except to try to destroy it again.
        pthread_create(&thread,
                       NULL,
                       &ezmq_wait_for_context_destruction,
                       obj);
    } else
        ezmq_free_obj(ctx);
    return NULL;
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
        // http://zguide.zeromq.org/page:all#Making-a-Clean-Exit
        int opt = 0;
        zmq_setsockopt(obj->obj, ZMQ_LINGER, &opt, sizeof(opt));
        zmq_close(obj->obj);
        break;
    }
    case EZMQ_CONTEXT: {
        pthread_t thread;
        // Avoid blocking Emacs when waiting for all sockets to close.
        pthread_create(&thread,
                       NULL,
                       &ezmq_wait_for_context_destruction,
                       obj);
        // Don't free obj
        return;
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
    ezmq_push_globref(obj);
    obj->val = val ? GLOBREF(val) : NULL;
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
