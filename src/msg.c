#include "msg.h"

static void
ezmq_free_message(void *data, void *hint)
{
    free(data);
}

/**
   Return a copy of the data contained in VAL. VAL is either a string or vector
   of bytes. SIZE will be updated with the number of bytes contained in VAL. In
   the case that VAL is a vector, an `args-out-ouf-range` error is raised if
   the integers are not within range of a byte.
*/
static char *
_ezmq_extract_message_data(emacs_value val, ptrdiff_t *size)
{
    emacs_value type = TYPE(val);
    *size = 0;

    if(EQ(type, Qstring)) {
        EZMQ_EXTRACT_STRING(content, clen, val);
        *size = clen;
        return content;
    } else if(EQ(type, Qvector)) {
        ptrdiff_t clen = VEC_LENGTH(val);
        char *content = ezmq_malloc(clen);
        ptrdiff_t i;

        for(i = 0; !NONLOCAL_EXIT() && i < clen; i++) {
            EZMQ_EXTRACT_INT(byte, AREF(val, i));

            if(0 <= byte && byte <= 255) {
                content[i] = (char)byte;
            } else {
                ezmq_args_out_of_range(INT(byte), CONS(INT(0), INT(255)));
            }
        }

        *size = clen;
        return content;
    } else
        ezmq_wrong_type_argument(val, 2, Qstring, Qvector);

    return NULL;
}

EZMQ_DOC(ezmq_message,
         "&optional DATA",
         "Initialize a ZMQ message.\n"
         "If DATA is non-nil, initialize the message using DATA.\n"
         "DATA can be either a string or a vector of byte integers.\n"
         "If DATA is nil, initialize an empty message.");
emacs_value
ezmq_message(emacs_value edata)
{
    ezmq_obj_t *msg = ezmq_new_obj(EZMQ_MESSAGE, NULL);
    if(!NILP(edata)) {
        ptrdiff_t size = 0;
        char *data = _ezmq_extract_message_data(edata, &size);
        EZMQ_CHECK_ERROR(zmq_msg_init_data(msg->obj,
                                           data,
                                           size,
                                           &ezmq_free_message,
                                           NULL));
        if(NONLOCAL_EXIT()) free(data);
    } else {
        EZMQ_CHECK_ERROR(zmq_msg_init(msg->obj));
    }
    if(NONLOCAL_EXIT()) ezmq_free_obj(msg);
    return ezmq_new_obj_ptr(msg);
}

EZMQ_DOC(ezmq_message_size, "MESSAGE", "Get the size of MESSAGE.");
emacs_value
ezmq_message_size(emacs_value emessage)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    return INT(zmq_msg_size(msg->obj));
}

EZMQ_DOC(ezmq_message_data, "MESSAGE", "Get the data of MESSAGE.");
emacs_value
ezmq_message_data(emacs_value emessage)
{
    emacs_value retval = Qnil;
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    const char *content = zmq_msg_data(msg->obj);

    if(content) {
        size_t size = zmq_msg_size(msg->obj);
        retval = STRING(content, size);
    }

    return retval;
}

EZMQ_DOC(ezmq_message_more, "MESSAGE", "Does MESSAGE have more parts?");
emacs_value
ezmq_message_more(emacs_value emessage)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    int retval = zmq_msg_more(msg->obj);
    EZMQ_CHECK_ERROR(retval);
    return retval ? Qt : Qnil;
}

EZMQ_DOC(ezmq_message_copy, "MESSAGE", "Copy MESSAGE.");
emacs_value
ezmq_message_copy(emacs_value emessage)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    ezmq_obj_t *dest = ezmq_new_obj(EZMQ_MESSAGE, NULL);
    zmq_msg_init(dest->obj);
    EZMQ_CHECK_ERROR(zmq_msg_copy(dest->obj, msg->obj));
    return ezmq_new_obj_ptr(dest);
}

EZMQ_DOC(ezmq_message_move, "SRC DEST", "Move a message from SRC to DEST.");
emacs_value
ezmq_message_move(emacs_value esrc, emacs_value edest)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, esrc);
    EZMQ_EXTRACT_OBJ(dest, EZMQ_MESSAGE, edest);
    EZMQ_CHECK_ERROR(zmq_msg_move(dest->obj, msg->obj));
    return Qnil;
}

EZMQ_DOC(ezmq_message_close, "MESSAGE", "Close a message");
emacs_value
ezmq_message_close(emacs_value emessage)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    EZMQ_CHECK_ERROR(zmq_msg_close(msg->obj));
    return Qnil;
}

EZMQ_DOC(ezmq_message_set, "MESSAGE PROPERTY VALUE", "Set a MESSAGE's PROPERTY to VALUE.");
emacs_value
ezmq_message_set(emacs_value emessage, emacs_value eproperty, emacs_value evalue)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    EZMQ_EXTRACT_INT(property, eproperty);

    switch(property) {
    case ZMQ_MORE:
        EZMQ_CHECK_ERROR(zmq_msg_set(msg->obj,
                                     property,
                                     !NILP(evalue)));
        break;
    default: {
        EZMQ_EXTRACT_INT(value, evalue);
        EZMQ_CHECK_ERROR(zmq_msg_set(msg->obj, property, value));
    }
    }
    return Qnil;
}

EZMQ_DOC(ezmq_message_get, "MESSAGE PROPERTY", "Get a MESSAGE PROPERTY.");
emacs_value
ezmq_message_get(emacs_value emessage, emacs_value eproperty)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    EZMQ_EXTRACT_INT(property, eproperty);
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
ezmq_message_recv(emacs_value emessage, emacs_value esock, emacs_value eflags)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, eflags);
    EZMQ_CHECK_ERROR(zmq_msg_recv(msg->obj, sock->obj, flags));
    return emessage;
}

EZMQ_DOC(ezmq_message_send, "MESSAGE SOCK &optional FLAGS",
         "Send a MESSAGE on SOCK with additional FLAGS.");
emacs_value
ezmq_message_send(emacs_value emessage, emacs_value esock, emacs_value eflags)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, eflags);
    EZMQ_CHECK_ERROR(zmq_msg_send(msg->obj, sock->obj, flags));
    return Qnil;
}

EZMQ_DOC(ezmq_message_gets, "MESSAGE PROPERTY",
         "Get a MESSAGE's metadata PROPERTY.\n"
         "PROPERTY is a keyword and can only be one of those in\n"
         "`zmq-message-properties'.");
emacs_value
ezmq_message_gets(emacs_value emessage, emacs_value eproperty)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    EZMQ_EXTRACT_STRING(property, plen, eproperty);
    const char *retval = zmq_msg_gets(msg->obj, property);
    free(property);
    EZMQ_CHECK_NULL_ERROR(retval);
    return STRING(retval, strlen(retval));
}

EZMQ_DOC(ezmq_message_routing_id, "MESSAGE", "Get the routing ID of MESSAGE.");
emacs_value
ezmq_message_routing_id(emacs_value emessage)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    return INT(zmq_msg_routing_id(msg->obj));
}

EZMQ_DOC(ezmq_message_set_routing_id, "MESSAGE ID", "Set MESSAGE's routing ID.");
emacs_value
ezmq_message_set_routing_id(emacs_value emessage, emacs_value eid)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    EZMQ_EXTRACT_INT(id, eid);
    EZMQ_CHECK_ERROR(zmq_msg_set_routing_id(msg->obj, id));
    return Qnil;
}

EZMQ_DOC(ezmq_message_group, "MESSAGE", "Get MESSAGE's GROUP.");
emacs_value
ezmq_message_group(emacs_value emessage)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    const char *group = zmq_msg_group(msg->obj);
    EZMQ_CHECK_NULL_ERROR(group);
    return NONLOCAL_EXIT() ? Qnil : STRING(group, strlen(group));
}

EZMQ_DOC(ezmq_message_set_group, "MESSAGE GROUP", "Set MESSAGE's GROUP.");
emacs_value
ezmq_message_set_group(emacs_value emessage, emacs_value egroup)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emessage);
    EZMQ_EXTRACT_STRING(group, elen, egroup);
    EZMQ_CHECK_ERROR(zmq_msg_set_group(msg->obj, group));
    return Qnil;
}
