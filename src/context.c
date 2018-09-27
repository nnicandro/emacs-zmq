#include "context.h"

EZMQ_DOC(ezmq_context, "", "Create a new context.");
emacs_value
ezmq_context(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    void *ctx = zmq_ctx_new();
    EZMQ_CHECK_NULL_ERROR(ctx);
    return ezmq_new_obj_ptr(env, ezmq_new_obj(env, EZMQ_CONTEXT, ctx));
}

EZMQ_DOC(ezmq_ctx_set,  "CONTEXT OPTION VALUE", "Set a CONTEXT OPTION to VALUE.");
emacs_value
ezmq_ctx_set(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, args[0]);
    EZMQ_EXTRACT_INT(option, args[1]);
    if(option == ZMQ_BLOCKY || option == ZMQ_IPV6)
        EZMQ_CHECK_ERROR(zmq_ctx_set(context->obj, option, (int)!NILP(args[2])));
    else {
        EZMQ_EXTRACT_INT(value, args[2]);
        EZMQ_CHECK_ERROR(zmq_ctx_set(context->obj, option, value));
    }
    return Qnil;
}

EZMQ_DOC(ezmq_ctx_get, "CONTEXT OPTION", "Get a CONTEXT OPTION.");
emacs_value
ezmq_ctx_get(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    emacs_value retval = Qnil;
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, args[0]);
    EZMQ_EXTRACT_INT(option, args[1]);

    int val = zmq_ctx_get(context->obj, option);
    EZMQ_CHECK_ERROR(val);
    if(option == ZMQ_BLOCKY || option == ZMQ_IPV6) {
        if(val) retval = Qt;
    }
    else
        retval = INT(val);

    return retval;
}

EZMQ_DOC(ezmq_ctx_shutdown, "CONTEXT", "Shutdown CONTEXT.");
emacs_value
ezmq_ctx_shutdown(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, args[0]);
    EZMQ_CHECK_ERROR(zmq_ctx_shutdown(context->obj));
    return Qnil;
}

EZMQ_DOC(ezmq_ctx_term, "CONTEXT", "Terminate CONTEXT.");
emacs_value
ezmq_ctx_term(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, args[0]);
    EZMQ_CHECK_ERROR(zmq_ctx_term(context->obj));
    return Qnil;
}
