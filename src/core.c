#include "core.h"
#include <pthread.h>

#define ERR_CASE(err)                                              \
    case err: {                                                    \
        const char *msg = zmq_strerror(err);                       \
        ptrdiff_t len = (ptrdiff_t)strlen(msg);                    \
        ezmq_signal(env, INTERN("zmq-"#err), 1, STRING(msg, len)); \
    }                                                              \
    break;

void
ezmq_signal_error(emacs_env *env)
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
        ezmq_signal(env, Qzmq_error, 1, STRING(msg, len));
    }
    }
}

char *
ezmq_malloc(emacs_env *env, size_t nbytes)
{
    char *buf = NULL;
    if(!NONLOCAL_EXIT()) {
        buf = malloc(nbytes);
        if(!buf) ezmq_signal_error(env);
    }
    return buf;
}

static emacs_value
ezmq_type_symbol(emacs_env *env, enum ezmq_obj_t type)
{
    switch(type) {
    case EZMQ_CONTEXT:
        return INTERN("zmq-context");
    case EZMQ_SOCKET:
        return INTERN("zmq-socket");
    case EZMQ_MESSAGE:
        return INTERN("zmq-message");
    case EZMQ_POLLER:
        return INTERN("zmq-poller");
    default:
        return Qnil;
    }
}

void
ezmq_signal(emacs_env *env, emacs_value err, int nargs, ...)
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
ezmq_wrong_object_type(emacs_env *env, ezmq_obj_t *obj, enum ezmq_obj_t expected)
{
    SIGNAL(Qwrong_type_argument,
           LIST(2,
                ezmq_type_symbol(expected),
                ezmq_type_symbol(obj->type)));
}

void
ezmq_wrong_type_argument(emacs_env *env, emacs_value val, int nvalid, ...)
{
    va_list args;
    emacs_value options[nvalid + 1];
    options[0] = INTERN("or");
    va_start(args, nvalid);
    int i;
    for(i = 0; i < nvalid; i++) {
        options[i] = va_arg(args, emacs_value);
    }
    emacs_value options_list = FUNCALL(Qlist, nvalid + 1, options);
    SIGNAL(Qwrong_type_argument, LIST(2, val, options_list));
}

ezmq_obj_t *
ezmq_extract_obj(emacs_env *env, enum ezmq_obj_t type, emacs_value val)
{
    ezmq_obj_t *obj = env->get_user_ptr(env, val);
    if(obj != NULL && !NONLOCAL_EXIT() && obj->type != type)
        ezmq_wrong_object_type(env, obj, type);
    return obj;
}

static void *
ezmq_wait_for_context_destruction(void *obj)
{
    ezmq_obj_t *ctx = (ezmq_obj_t *)obj;
    zmq_ctx_term(ctx->obj);
    ezmq_free_obj(ctx);
    return NULL;
}

ezmq_obj_t *
ezmq_new_obj(emacs_env *env, enum ezmq_obj_t type, void *obj)
{
    ezmq_obj_t *eobj = (ezmq_obj_t *)ezmq_malloc(env, sizeof(*eobj));
    if(!NONLOCAL_EXIT()) {
        switch(type) {
        case EZMQ_MESSAGE:
            if(!obj) {
                obj = (zmq_msg_t *)ezmq_malloc(env, sizeof(zmq_msg_t));
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
        eobj->refcount = 0;
    }
    return eobj;
}

// TODO: Properly handle EINTR for the context
//
// TODO: Handle the case of multiple context objects, we will need to join
// multiple threads at the end.
void
ezmq_obj_finalizer(void *ptr)
{
    ezmq_obj_t *obj = (ezmq_obj_t *)ptr;

    if((obj->refcount--) <= 0) {
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
}

emacs_value
ezmq_new_obj_ptr(emacs_env *env, ezmq_obj_t *obj)
{
    if(!NONLOCAL_EXIT())
        obj->refcount++;
    return env->make_user_ptr(env, &ezmq_obj_finalizer, obj);
}

void
ezmq_free_obj(ezmq_obj_t *obj)
{
    if(obj) {
        if(obj->type == EZMQ_MESSAGE) {
            free(obj->obj);
        }
        free(obj);
    }
}

char *
ezmq_copy_string(emacs_env *env, emacs_value str, ptrdiff_t *size)
{
    *size = 0;
    ptrdiff_t sz = 1;
    if(!env->copy_string_contents(env, str, NULL, &sz)) return NULL;
    char *buf = ezmq_malloc(env, sz);
    env->copy_string_contents(env, str, buf, &sz);
    // The size returned by copy_string_contents contains the terminanting NULL
    // byte.
    if(size != NULL) *size = sz - 1;
    return buf;
}
