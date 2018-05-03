#include "msg.h"

static void
zmq_free_message(void *data, void *hint)
{
    free(data);
}

// TODO: From the documentation of zmq_msg_init: "never initialize the same
// message twice", I think I do this somewhere in zmq-ffi.el or in jupyter.el,
// find where.
emacs_value
Fzmq_message(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    if(nargs == 0) {
        // Initialize an empty message
        emacs_value retval = ezmq_new_obj_ptr(env, EZMQ_MESSAGE, NULL);
        if(!retval) return NULL;
        EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, retval);
        EZMQ_CHECK_ERROR(zmq_msg_init(msg->obj));
        if(EZMQ_NONLOCAL_EXIT()) {
            ezmq_free_obj(msg);
            return NULL;
        } else
            return retval;
    } else {
        ptrdiff_t size;
        char *content = NULL;

        if(ezmq_value_of_type(env, args[0], Qstring)) {
            content = ezmq_copy_string(env, args[0], &size);
            if(!content) return NULL;
            // size includes the terminating NULL byte
            size -= 1;
        } else if(ezmq_value_of_type(env, args[0], Qvector)) {
            size = env->vec_size(env, args[0]);
            content = ezmq_malloc(env, size);
            if(!content) return NULL;
            for(ptrdiff_t i = 0; i < size; i++) {
                // TODO: Check for a valid number between 0-255. Don't leak
                // memory when vec[i] is a non-integer, i.e. free content when
                // this is the case.
                EZMQ_EXTRACT_INT(value, env->vec_get(env, args[0], i));
                content[i] = value;
            }
        } else {
            emacs_value data = LIST(2, args[0], LIST(3, INTERN("or"), Qstring, Qvector));
            env->non_local_exit_signal(env, Qwrong_type_argument, data);
            return NULL;
        }

        ezmq_obj_t *msg = ezmq_new_obj(env, EZMQ_MESSAGE, NULL);
        if(!msg) {
            free(content);
            return NULL;
        }
        EZMQ_CHECK_ERROR(zmq_msg_init_data(msg->obj,
                                           content,
                                           size,
                                           &zmq_free_message,
                                           NULL));
        if(EZMQ_NONLOCAL_EXIT()) {
            ezmq_free_obj(msg);
            free(content);
            return NULL;
        } else
            return env->make_user_ptr(env, NULL, msg);
    }
}

emacs_value
Fzmq_message_size(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);

    return env->make_integer(env, zmq_msg_size(msg->obj));
}

emacs_value
Fzmq_message_data(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);

    const char *content = zmq_msg_data(msg->obj);
    if(content) {
        size_t size = zmq_msg_size(msg->obj);
        char *buf = malloc(size + 1);
        emacs_value retval;

        buf[size] = 0;
        memcpy(buf, zmq_msg_data(msg->obj), size);
        retval = ezmq_make_string(env, buf, size);
        free(buf);
        return retval;
    } else {
        return Qnil;
    }
}

emacs_value
Fzmq_message_more(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);

    int retval = zmq_msg_more(msg->obj);
    EZMQ_CHECK_ERROR(retval);
    if(EZMQ_NONLOCAL_EXIT()) return NULL;
    return retval ? Qt : Qnil;
}

emacs_value
Fzmq_message_copy(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);

    ezmq_obj_t *dest = ezmq_new_obj(env, EZMQ_MESSAGE, NULL);
    if(!dest) return NULL;
    EZMQ_CHECK_ERROR(zmq_msg_copy(dest->obj, msg->obj));
    if(EZMQ_NONLOCAL_EXIT()) {
        ezmq_free_obj(dest);
        return NULL;
    } else
        return env->make_user_ptr(env, NULL, dest);
}

emacs_value
Fzmq_message_move(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);

    ezmq_obj_t *dest = ezmq_new_obj(env, EZMQ_MESSAGE, NULL);
    if(!dest) {
        free(msg);
        return NULL;
    }
    EZMQ_CHECK_ERROR(zmq_msg_move(dest->obj, msg->obj));
    if(EZMQ_NONLOCAL_EXIT()) {
        ezmq_free_obj(dest);
        return NULL;
    } else
        return env->make_user_ptr(env, NULL, dest);
}

emacs_value
Fzmq_message_close(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);

    EZMQ_CHECK_ERROR(zmq_msg_close(msg->obj));
    if(!EZMQ_NONLOCAL_EXIT()) ezmq_free_obj(msg);
    return Qnil;
}

emacs_value
Fzmq_message_set(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_INT(property, args[1]);

    switch(property) {
    case ZMQ_MORE:
        EZMQ_CHECK_ERROR(zmq_msg_set(msg->obj,
                                     property,
                                     env->is_not_nil(env, args[2])));
        break;
    default: {
        EZMQ_EXTRACT_INT(value, args[2]);
        EZMQ_CHECK_ERROR(zmq_msg_set(msg->obj, property, value));
    }
    }
    return Qnil;
}

emacs_value
Fzmq_message_get(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_INT(property, args[1]);

    int retval = zmq_msg_get(msg->obj, property);
    EZMQ_CHECK_ERROR(retval);
    if(EZMQ_NONLOCAL_EXIT()) return NULL;
    if(property == ZMQ_MORE)
        return retval ? Qt : Qnil;
    else
        return env->make_integer(env, retval);
}

emacs_value
Fzmq_message_recv(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[1]);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, nargs == 3 ? args[2] : Qnil);

    EZMQ_CHECK_ERROR(zmq_msg_recv(msg->obj, sock->obj, flags));
    return args[0];
}

emacs_value
Fzmq_message_send(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[1]);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, nargs == 3 ? args[2] : Qnil);

    EZMQ_CHECK_ERROR(zmq_msg_send(msg->obj, sock->obj, flags));
    return Qnil;
}

emacs_value
Fzmq_message_gets(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    char *property = ezmq_copy_string(env, args[1], NULL);
    if(!property) return NULL;

    const char *retval = zmq_msg_gets(msg->obj, property);
    if(retval == NULL) {
        ezmq_signal_error(env);
        return NULL;
    }
    free(property);
    return ezmq_make_string(env, retval, strlen(retval));
}

emacs_value
Fzmq_message_routing_id(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    return env->make_integer(env, zmq_msg_routing_id(msg->obj));
}

emacs_value
Fzmq_message_set_routing_id(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[0]);
    EZMQ_EXTRACT_INT(id, args[1]);

    EZMQ_CHECK_ERROR(zmq_msg_set_routing_id(msg->obj, id));
    return Qnil;
}
