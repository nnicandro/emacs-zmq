#include "util.h"

EZMQ_DOC(ezmq_has, "CAPABILITY", "Return non-nil if ZMQ has CAPABILITY.");
emacs_value
ezmq_has(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_STRING(capability, clen, args[0]);
    emacs_value retval = zmq_has(capability) ? Qt : Qnil;
    free(capability);
    return retval;
}

EZMQ_DOC(ezmq_version, "", "Return the currently installed version of ZMQ.");
emacs_value
ezmq_version(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    int major, minor, patch;
    char buf[16];
    zmq_version(&major, &minor, &patch);
    sprintf(buf, "%d.%d.%d", major, minor, patch);
    return STRING(buf, strlen(buf));
}

EZMQ_DOC(ezmq_z85_decode, "KEY", "Decode a z85 encoded KEY.");
emacs_value
ezmq_z85_decode(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
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

EZMQ_DOC(ezmq_z85_encode, "DATA", "Encode DATA using the z85 encoding.");
emacs_value
ezmq_z85_encode(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
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

EZMQ_DOC(ezmq_curve_keypair, "", "Return a (PUBLIC-KEY . SECRET-KEY) pair.");
emacs_value
ezmq_curve_keypair(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
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
    } else {
        const char *msg =  "ZMQ not built with CURVE security";
        ezmq_signal(env, Qzmq_error, 1, STRING(msg, strlen(msg)));
    }

    return retval;
}

EZMQ_DOC(ezmq_curve_public, "SECRET-KEY", "Return the corresponding public key of SECRET-KEY.");
emacs_value
ezmq_curve_public(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
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
    } else {
        const char *msg =  "ZMQ not built with CURVE security";
        ezmq_signal(env, Qzmq_error, 1, STRING(msg, strlen(msg)));
    }

    return retval;
}

EZMQ_DOC(ezmq_equal,  "O1 O2", "Same as `equal' but properly handles ZMQ objects.");
emacs_value
ezmq_equal(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
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

EZMQ_DOC(ezmq_message_p, "OBJ", "Is OBJ a `zmq-message'?");
emacs_value
ezmq_message_p(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    return ezmq_obj_of_type(env, args[0], EZMQ_MESSAGE);
}

EZMQ_DOC(ezmq_socket_p, "OBJ", "Is OBJ a `zmq-socket'?");
emacs_value
ezmq_socket_p(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    return ezmq_obj_of_type(env, args[0], EZMQ_SOCKET);
}

EZMQ_DOC(ezmq_context_p, "OBJ", "Is OBJ a `zmq-context'?");
emacs_value
ezmq_context_p(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    return ezmq_obj_of_type(env, args[0], EZMQ_CONTEXT);
}

EZMQ_DOC(ezmq_poller_p, "OBJ", "Is OBJ a `zmq-poller'?");
emacs_value
ezmq_poller_p(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    return ezmq_obj_of_type(env, args[0], EZMQ_POLLER);
}
