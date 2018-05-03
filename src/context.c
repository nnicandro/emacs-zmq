#include "context.h"

emacs_value
Fzmq_context(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    void *ctx = zmq_ctx_new();
    if(!ctx) {
        ezmq_signal_error(env);
        return NULL;
    }
    return ezmq_new_obj_ptr(env, EZMQ_CONTEXT, ctx);
}

emacs_value
Fzmq_ctx_set(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(env, context, EZMQ_CONTEXT, args[0]);
    EZMQ_EXTRACT_INT(env, option, args[1]);
    EZMQ_EXTRACT_INT(env, value, args[2]);
    EZMQ_CHECK_ERROR(env, zmq_ctx_set(context->obj, option, value));
    return Qnil;
}

emacs_value
Fzmq_ctx_get(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(env, context, EZMQ_CONTEXT, args[0]);
    EZMQ_EXTRACT_INT(env, option, args[1]);
    int retval = zmq_ctx_get(context->obj, option);
    EZMQ_CHECK_ERROR(env, retval);
    return EZMQ_NONLOCAL_EXIT(env) ? Qnil : env->make_integer(env, retval);
}

emacs_value
Fzmq_ctx_term(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(env, context, EZMQ_CONTEXT, args[0]);
    EZMQ_CHECK_ERROR(env, zmq_ctx_term(context->obj));
    if(!EZMQ_NONLOCAL_EXIT(env)) ezmq_free_obj(context);
    return Qnil;
}

emacs_value
Fzmq_ctx_shutdown(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(env, context, EZMQ_CONTEXT, args[0]);
    EZMQ_CHECK_ERROR(env, zmq_ctx_shutdown(context->obj));
    return Qnil;
}
