#include "msg.h"

static void
ezmq_free_message(void *data, void *hint)
{
    free(data);
}

static char *
ezmq_extract_message_data(emacs_env *env, emacs_value val, ptrdiff_t *size)
{
    emacs_value type = TYPE(val);
    *size = 0;

    if(EQ(type, Qstring)) {
        EZMQ_EXTRACT_STRING(content, clen, val);
        // clen includes the terminating NULL byte, but ZMQ only works with
        // binary data
        *size = clen;
        return content;
    } else if(EQ(type, Qvector)) {
        ptrdiff_t clen = env->vec_size(env, val);
        char *content = ezmq_malloc(env, clen);

        if(!EZMQ_NONLOCAL_EXIT()) {
            ptrdiff_t i;
            for(i = 0; i < clen; i++) {
                // TODO: Give a more informative error, i.e. that a vector
                // doesn't contnain an integer. What we can do is do validation
                // on the whole vector up front.
                if(EZMQ_NONLOCAL_EXIT()) break;
                EZMQ_EXTRACT_INT(byte, AREF(val, i));
                if(!(0 <= byte && byte <= 255)) {
                    ezmq_signal(env, Qargs_out_of_range, 2, byte,
                                LIST(3, INTERN("<="), INT(0), INTERN("X"), INT(255)));
                    break;
                }
                content[i] = byte;
            }
            if(!EZMQ_NONLOCAL_EXIT()) {
                *size = clen;
                return content;
            }
        }
    } else
        ezmq_wrong_type_argument(env, val, 2, Qstring, Qvector);

    return NULL;
}

// TODO: From the documentation of zmq_msg_init: "never initialize the same
// message twice", I think I do this somewhere in zmq-ffi.el or in jupyter.el,
// find where.
EZMQ_DOC(ezmq_message,
         "&optional DATA",
         "Initialize a ZMQ message.\n"
         "If DATA is non-nil, initialize the message using DATA.\n"
         "DATA can be either a string or a vector of byte integers.\n"
         "If DATA is nil, initialize an empty message.");
emacs_value
ezmq_message(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ezmq_obj_t *msg = ezmq_new_obj(env, EZMQ_MESSAGE, NULL);
    if(EZMQ_NONLOCAL_EXIT()) return NULL;

    if(nargs != 0) {
        ptrdiff_t size = 0;
        char *data = ezmq_extract_message_data(env, args[0], &size);

        if(!EZMQ_NONLOCAL_EXIT()) {
            EZMQ_CHECK_ERROR(zmq_msg_init_data(msg->obj,
                                               data,
                                               size,
                                               &ezmq_free_message,
                                               NULL));
            if(EZMQ_NONLOCAL_EXIT()) free(data);
        }
    } else
        EZMQ_CHECK_ERROR(zmq_msg_init(msg->obj));

    return ezmq_new_obj_ptr(env, msg);
}

EZMQ_DOC(ezmq_message_size, "MESSAGE", "Get the size of MESSAGE.");
emacs_value
ezmq_message_size(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    return INT(zmq_msg_size(msg->obj));
}

EZMQ_DOC(ezmq_message_data, "MESSAGE", "Get the data of MESSAGE.");
emacs_value
ezmq_message_data(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    emacs_value retval = Qnil;
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    const char *content = zmq_msg_data(msg->obj);

    if(content) {
        size_t size = zmq_msg_size(msg->obj);
        char *buf = malloc(size + 1);
        buf[size] = 0;
        memcpy(buf, zmq_msg_data(msg->obj), size);
        retval = STRING(buf, size);
        free(buf);
    }

    return retval;
}

EZMQ_DOC(ezmq_message_more, "MESSAGE", "Does MESSAGE have more parts?");
emacs_value
ezmq_message_more(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    int retval = zmq_msg_more(msg->obj);
    EZMQ_CHECK_ERROR(retval);
    return retval ? Qt : Qnil;
}

EZMQ_DOC(ezmq_message_copy, "MESSAGE", "Copy MESSAGE.");
emacs_value
ezmq_message_copy(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ezmq_obj_t *dest = ezmq_new_obj(env, EZMQ_MESSAGE, NULL);
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    zmq_msg_init(dest->obj);
    EZMQ_CHECK_ERROR(zmq_msg_copy(dest->obj, msg->obj));
    return ezmq_new_obj_ptr(env, dest);
}

EZMQ_DOC(ezmq_message_move, "SRC DEST", "Move a message from SRC to DEST.");
emacs_value
ezmq_message_move(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_OBJ(dest, EZMQ_MESSAGE, args[1]);
    EZMQ_CHECK_ERROR(zmq_msg_move(dest->obj, msg->obj));
    return Qnil;
}

EZMQ_DOC(ezmq_message_close, "MESSAGE", "Close a message");
emacs_value
ezmq_message_close(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_CHECK_ERROR(zmq_msg_close(msg->obj));
    return Qnil;
}

EZMQ_DOC(ezmq_message_set, "MESSAGE PROPERTY VALUE", "Set a MESSAGE's PROPERTY to VALUE.");
emacs_value
ezmq_message_set(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_INT(property, args[1]);

    switch(property) {
    case ZMQ_MORE:
        EZMQ_CHECK_ERROR(zmq_msg_set(msg->obj,
                                     property,
                                     !NILP(args[2])));
        break;
    default: {
        EZMQ_EXTRACT_INT(value, args[2]);
        EZMQ_CHECK_ERROR(zmq_msg_set(msg->obj, property, value));
    }
    }
    return Qnil;
}

EZMQ_DOC(ezmq_message_get, "MESSAGE PROPERTY", "Get a MESSAGE PROPERTY.");
emacs_value
ezmq_message_get(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_INT(property, args[1]);
    int retval = zmq_msg_get(msg->obj, property);
    EZMQ_CHECK_ERROR(retval);
    if(property == ZMQ_MORE)
        return retval ? Qt : Qnil;
    else
        return INT(retval);
}

EZMQ_DOC(ezmq_message_recv,  "MESSAGE SOCK &optional FLAGS",
         "Receive a MESSAGE from SOCK with additional FLAGS.\n"
         "MESSAGE should be an initialized message.");
emacs_value
ezmq_message_recv(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[1]);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, nargs == 3 ? args[2] : Qnil);
    EZMQ_CHECK_ERROR(zmq_msg_recv(msg->obj, sock->obj, flags));
    return args[0];
}

EZMQ_DOC(ezmq_message_send, "MESSAGE SOCK &optional FLAGS",
         "Send a MESSAGE on SOCK with additional FLAGS.");
emacs_value
ezmq_message_send(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[1]);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, nargs == 3 ? args[2] : Qnil);
    EZMQ_CHECK_ERROR(zmq_msg_send(msg->obj, sock->obj, flags));
    return Qnil;
}

EZMQ_DOC(ezmq_message_gets, "MESSAGE PROPERTY",
         "Get a MESSAGE's metadata PROPERTY.\n"
         "PROPERTY is a keyword and can only be one of those in\n"
         "`zmq-message-properties'.");
emacs_value
ezmq_message_gets(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_STRING(property, plen, args[1]);
    const char *retval = zmq_msg_gets(msg->obj, property);
    free(property);
    EZMQ_CHECK_NULL_ERROR(retval);
    return STRING(retval, strlen(retval));
}

EZMQ_DOC(ezmq_message_routing_id, "MESSAGE", "Get the routing ID of MESSAGE.");
emacs_value
ezmq_message_routing_id(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    return INT(zmq_msg_routing_id(msg->obj));
}

EZMQ_DOC(ezmq_message_set_routing_id, "MESSAGE ID", "Set MESSAGE's routing ID.");
emacs_value
ezmq_message_set_routing_id(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_INT(id, args[1]);
    EZMQ_CHECK_ERROR(zmq_msg_set_routing_id(msg->obj, id));
    return Qnil;
}
