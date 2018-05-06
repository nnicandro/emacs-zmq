#include "context.h"

EZMQ_DOC(zmq_context, "Create a new context.", "");
emacs_value
Fzmq_context(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    void *ctx = zmq_ctx_new();
    EZMQ_CHECK_NULL_ERROR(ctx);
    return ezmq_new_obj_ptr(env, ezmq_new_obj(env, EZMQ_CONTEXT, ctx));
}

EZMQ_DOC(zmq_ctx_set, "Set a CONTEXT OPTION to VALUE.", "CONTEXT OPTION VALUE");
emacs_value
Fzmq_ctx_set(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
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

EZMQ_DOC(zmq_ctx_get, "Get a CONTEXT OPTION.", "CONTEXT OPTION");
emacs_value
Fzmq_ctx_get(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
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

EZMQ_DOC(zmq_ctx_shutdown, "Shutdown CONTEXT.", "CONTEXT");
emacs_value
Fzmq_ctx_shutdown(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, args[0]);
    EZMQ_CHECK_ERROR(zmq_ctx_shutdown(context->obj));
    return Qnil;
}
