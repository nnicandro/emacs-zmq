#include "context.h"

EZMQ_DOC(ezmq_context, "", "Create a new context.");
emacs_value
ezmq_context(void)
{
    void *ctx = zmq_ctx_new();
    EZMQ_CHECK_NULL_ERROR(ctx);
    ezmq_debug("ezmq_context()\n");
    return ezmq_new_obj_ptr(ezmq_new_obj(EZMQ_CONTEXT, ctx));
}

EZMQ_DOC(ezmq_ctx_set,  "CONTEXT OPTION VALUE", "Set a CONTEXT OPTION to VALUE.");
emacs_value
ezmq_ctx_set(emacs_value econtext, emacs_value eoption, emacs_value evalue)
{
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, econtext);
    EZMQ_EXTRACT_INT(option, eoption);
    if(option == ZMQ_BLOCKY || option == ZMQ_IPV6)
        EZMQ_CHECK_ERROR(zmq_ctx_set(context->obj, option, (int)!NILP(evalue)));
    else {
        EZMQ_EXTRACT_INT(value, evalue);
        EZMQ_CHECK_ERROR(zmq_ctx_set(context->obj, option, value));
    }
    return Qnil;
}

EZMQ_DOC(ezmq_ctx_get, "CONTEXT OPTION", "Get a CONTEXT OPTION.");
emacs_value
ezmq_ctx_get(emacs_value econtext, emacs_value eoption)
{
    emacs_value retval = Qnil;
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, econtext);
    EZMQ_EXTRACT_INT(option, eoption);

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
ezmq_ctx_shutdown(emacs_value econtext)
{
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, econtext);
    EZMQ_CHECK_ERROR(zmq_ctx_shutdown(context->obj));
    return Qnil;
}

EZMQ_DOC(ezmq_ctx_term, "CONTEXT", "Terminate CONTEXT.");
emacs_value
ezmq_ctx_term(emacs_value econtext)
{
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, econtext);
    EZMQ_CHECK_ERROR(zmq_ctx_term(context->obj));
    return Qnil;
}
