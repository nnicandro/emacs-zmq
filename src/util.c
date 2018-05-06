#include "util.h"

EZMQ_DOC(zmq_has, "Return non-nil if ZMQ has CAPABILITY.", "CAPABILITY");
emacs_value
Fzmq_has(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_STRING(capability, clen, args[0]);
    emacs_value retval = zmq_has(capability) ? Qt : Qnil;
    free(capability);
    return retval;
}

EZMQ_DOC(zmq_version, "Return the currently installed version of ZMQ.", "");
emacs_value
Fzmq_version(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    int major, minor, patch;
    char buf[16];
    zmq_version(&major, &minor, &patch);
    sprintf(buf, "%d.%d.%d", major, minor, patch);
    return STRING(buf, strlen(buf));
}

EZMQ_DOC(zmq_z85_decode, "Decode a z85 encoded KEY.", "KEY");
emacs_value
Fzmq_z85_decode(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    emacs_value retval = Qnil;

    EZMQ_EXTRACT_STRING(key, klen, args[0]);

    // klen includes the terminating NULL byte, hence we use klen - 1 to get
    // the length of the string
    if(klen % 5 == 0) {
        ptrdiff_t dlen = (ptrdiff_t)(0.8*klen);
        char *decoded = ezmq_malloc(env, dlen + 1);

        if(!EZMQ_NONLOCAL_EXIT()) {
            decoded[dlen] = 0;
            if(zmq_z85_decode((uint8_t *)decoded, key))
                retval = STRING(decoded, dlen);
        }

        free(decoded);
    } else {
        char *msg =  "Length not a multiple of 5";
        ezmq_signal(env, Qargs_out_of_range, 2, INT(klen), STRING(msg, strlen(msg)));
    }

    free(key);
    return retval;
}

EZMQ_DOC(zmq_z85_encode, "Encode DATA using the z85 encoding.", "DATA");
emacs_value
Fzmq_z85_encode(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    emacs_value retval = Qnil;

    EZMQ_EXTRACT_STRING(content, clen, args[0]);

    if(clen % 4 == 0) {
        ptrdiff_t elen = (ptrdiff_t)(1.25*(float)clen);
        char *encoded = ezmq_malloc(env, elen + 1);

        if(!EZMQ_NONLOCAL_EXIT()) {
            if(zmq_z85_encode(encoded, (uint8_t *)content, clen))
                retval = STRING(encoded, elen);
        }

        free(encoded);
    } else {
        const char *msg = "Length not a multiple of 4";
        ezmq_signal(env, Qargs_out_of_range, 2, INT(clen), STRING(msg, strlen(msg)));
    }

    free(content);

    return retval;
}

EZMQ_DOC(zmq_curve_keypair, "Return a (PUBLIC-KEY . SECRET-KEY) pair.", "");
emacs_value
Fzmq_curve_keypair(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    emacs_value retval = Qnil;

    if(zmq_has("curve")) {
        char *public = ezmq_malloc(env, 41);
        char *private = ezmq_malloc(env, 41);

        if(!EZMQ_NONLOCAL_EXIT()) {
            EZMQ_CHECK_ERROR(zmq_curve_keypair(public, private));
            if(!EZMQ_NONLOCAL_EXIT())
                retval = CONS(STRING(public, 40), STRING(private, 40));
        }

        free(public);
        free(private);
    } else
        ezmq_error(env, Qzmq_error, "ZMQ not built with CURVE security");

    return retval;
}

EZMQ_DOC(zmq_curve_public, "Return the corresponding public key of SECRET-KEY.", "SECRET-KEY");
emacs_value
Fzmq_curve_public(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    emacs_value retval = Qnil;

    if(zmq_has("curve")) {
        EZMQ_EXTRACT_STRING(private, plen, args[0]);
        char *public = ezmq_malloc(env, 41);

        if(!EZMQ_NONLOCAL_EXIT()) {
            EZMQ_CHECK_ERROR(zmq_curve_public(public, private));
            if(!EZMQ_NONLOCAL_EXIT())
                retval = STRING(public, 40);
        }

        free(public);
        free(private);
    } else
        ezmq_error(env, Qzmq_error, "ZMQ not built with CURVE security");

    return retval;
}

EZMQ_DOC(zmq_equal, "Same as `equal' but properly handles ZMQ objects.", "O1 O2");
emacs_value
Fzmq_equal(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    if(!EQUAL(args[0], args[1])) {
        ezmq_obj_t *a = env->get_user_ptr(env, args[0]);
        ezmq_obj_t *b = env->get_user_ptr(env, args[1]);

        if(!EZMQ_NONLOCAL_EXIT() &&
           // If both objects are user-ptrs, ensure the objects indeed point to
           // ZMQ objects
           env->get_user_finalizer(env, args[0]) == &ezmq_obj_finalizer &&
           env->get_user_finalizer(env, args[1]) == &ezmq_obj_finalizer &&
           a == b) {
            return Qt;
        } else {
            env->non_local_exit_clear(env);
            return Qnil;
        }
    } else
        return Qt;
}

static emacs_value
ezmq_obj_of_type(emacs_env *env, emacs_value val, enum ezmq_obj_t type)
{
    ezmq_obj_t *obj = env->get_user_ptr(env, val);
    if(!EZMQ_NONLOCAL_EXIT() &&
       env->get_user_finalizer(env, val) == &ezmq_obj_finalizer &&
       obj->type == type) {
        return Qt;
    } else {
        env->non_local_exit_clear(env);
        return Qnil;
    }
}

EZMQ_DOC(zmq_message_p, "Is OBJ a `zmq-message'?", "OBJ");
emacs_value
Fzmq_message_p(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    return ezmq_obj_of_type(env, args[0], EZMQ_MESSAGE);
}

EZMQ_DOC(zmq_socket_p, "Is OBJ a `zmq-socket'?", "OBJ");
emacs_value
Fzmq_socket_p(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    return ezmq_obj_of_type(env, args[0], EZMQ_SOCKET);
}

EZMQ_DOC(zmq_context_p, "Is OBJ a `zmq-context'?", "OBJ");
emacs_value
Fzmq_context_p(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    return ezmq_obj_of_type(env, args[0], EZMQ_CONTEXT);
}

EZMQ_DOC(zmq_poller_p, "Is OBJ a `zmq-poller'?", "OBJ");
emacs_value
Fzmq_poller_p(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    return ezmq_obj_of_type(env, args[0], EZMQ_POLLER);
}
