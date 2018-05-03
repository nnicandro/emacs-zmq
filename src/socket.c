#include "socket.h"

emacs_value
Fzmq_socket(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, args[0]);
    EZMQ_EXTRACT_INT(type, args[1]);

    void *sock = zmq_socket(context->obj, type);
    if(sock == NULL) {
        ezmq_signal_error(env);
        return NULL;
    }
    return ezmq_new_obj_ptr(env, EZMQ_SOCKET, sock);
}

emacs_value
Fzmq_send(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, nargs == 3 ? args[2] : Qnil);

    if(ezmq_value_of_type(env, args[1], Qstring)) {
        ptrdiff_t size;
        char *msg = ezmq_copy_string(env, args[1], &size);
        if(!msg) return NULL;
        EZMQ_CHECK_ERROR(zmq_send(sock->obj, msg, size - 1, flags));
        free(msg);
    } else {
        EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[1]);
        EZMQ_CHECK_ERROR(zmq_msg_send(msg->obj, sock->obj, flags));
    }
    return Qnil;
}

emacs_value
Fzmq_recv(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_INT(nbytes, args[1]);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, args[2]);

    char *buf = ezmq_malloc(env, nbytes + 1);
    if(!buf) return NULL;
    buf[nbytes] = 0;
    EZMQ_CHECK_ERROR(zmq_recv(sock->obj, buf, nbytes, flags));
    emacs_value retval = Qnil;
    if(!EZMQ_NONLOCAL_EXIT())
        retval = ezmq_make_string(env, buf, nbytes);
    free(buf);
    return retval;
}

emacs_value
Fzmq_bind(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);

    char *endpoint = ezmq_copy_string(env, args[1], NULL);
    if(!endpoint) return NULL;
    EZMQ_CHECK_ERROR(zmq_bind(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

emacs_value
Fzmq_connect(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);

    char *endpoint = ezmq_copy_string(env, args[1], NULL);
    if(!endpoint) return NULL;
    EZMQ_CHECK_ERROR(zmq_connect(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

emacs_value
Fzmq_unbind(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);

    char *endpoint = ezmq_copy_string(env, args[1], NULL);
    if(!endpoint) return NULL;
    EZMQ_CHECK_ERROR(zmq_unbind(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

emacs_value
Fzmq_disconnect(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);

    char *endpoint = ezmq_copy_string(env, args[1], NULL);
    if(!endpoint) return NULL;
    EZMQ_CHECK_ERROR(zmq_disconnect(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

emacs_value
Fzmq_close(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);

    EZMQ_CHECK_ERROR(zmq_close(sock->obj));
    if(!EZMQ_NONLOCAL_EXIT()) ezmq_free_obj(sock);
    return Qnil;
}

emacs_value
Fzmq_setsockopt(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    size_t size;
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_INT(option, args[1]);

    switch(option) {
        // INT
    case ZMQ_BACKLOG: case ZMQ_RATE: case ZMQ_RECOVERY_IVL: case ZMQ_SNDHWM:
    case ZMQ_SNDBUF: case ZMQ_SNDTIMEO: case ZMQ_RCVHWM: case ZMQ_RCVBUF:
    case ZMQ_RCVTIMEO: case ZMQ_LINGER: case ZMQ_RECONNECT_IVL:
    case ZMQ_RECONNECT_IVL_MAX: case ZMQ_MULTICAST_HOPS:
    case ZMQ_MULTICAST_MAXTPDU: case ZMQ_CONNECT_TIMEOUT:
    case ZMQ_HANDSHAKE_IVL: case ZMQ_HEARTBEAT_IVL:
    case ZMQ_HEARTBEAT_TIMEOUT: case ZMQ_HEARTBEAT_TTL:
    case ZMQ_USE_FD: case ZMQ_TCP_KEEPALIVE:
    case ZMQ_TCP_KEEPALIVE_CNT: case ZMQ_TCP_KEEPALIVE_IDLE:
    case ZMQ_TCP_KEEPALIVE_INTVL: case ZMQ_TCP_MAXRT: case ZMQ_TOS:
    case ZMQ_VMCI_CONNECT_TIMEOUT: {
        EZMQ_EXTRACT_INT(value, args[2]);
        zmq_setsockopt(sock, option, &value, sizeof(int));
        break;
    }
        // UINT64
        // TODO: Figure out the best way to handle UINT64 since Emacs integers
        // are all signed.
    case ZMQ_AFFINITY: case ZMQ_VMCI_BUFFER_SIZE:
    case ZMQ_VMCI_BUFFER_MAX_SIZE: case ZMQ_VMCI_BUFFER_MIN_SIZE:
        // INT64
    case ZMQ_MAXMSGSIZE: {
        EZMQ_EXTRACT_INT(value, args[2]);
        zmq_setsockopt(sock, option, &value, sizeof(int64_t));
        break;
    }
        // INT with BOOL values
    case ZMQ_CONFLATE: case ZMQ_CURVE_SERVER: case ZMQ_GSSAPI_PLAINTEXT:
    case ZMQ_GSSAPI_SERVER: case ZMQ_IMMEDIATE: case ZMQ_INVERT_MATCHING:
    case ZMQ_IPV6: case ZMQ_PLAIN_SERVER: case ZMQ_PROBE_ROUTER:
    case ZMQ_REQ_CORRELATE: case ZMQ_REQ_RELAXED: case ZMQ_ROUTER_HANDOVER:
    case ZMQ_ROUTER_MANDATORY: case ZMQ_ROUTER_RAW: case ZMQ_STREAM_NOTIFY:
    case ZMQ_XPUB_VERBOSE: case ZMQ_XPUB_VERBOSER: case ZMQ_XPUB_MANUAL:
    case ZMQ_XPUB_NODROP: {
        int value = env->is_not_nil(env, args[2]);
        zmq_setsockopt(sock, option, &value, sizeof(int));
        break;
    }
        // STRING
        // TODO: Error when multibyte characters are found
    case ZMQ_GSSAPI_PRINCIPAL: case ZMQ_GSSAPI_SERVICE_PRINCIPAL:
    case ZMQ_PLAIN_PASSWORD: case ZMQ_PLAIN_USERNAME:
    case ZMQ_SOCKS_PROXY: case ZMQ_ZAP_DOMAIN: {
        char *str = ezmq_copy_string(env, args[2], (ptrdiff_t *)&size);
        zmq_setsockopt(sock, option, str, size);
        free(str);
        break;
    }
        // BINARY
    case ZMQ_CONNECT_ROUTING_ID:
    case ZMQ_ROUTING_ID: case ZMQ_SUBSCRIBE: case ZMQ_UNSUBSCRIBE:
    case ZMQ_XPUB_WELCOME_MSG: {
        char *bin = ezmq_copy_string(env, args[2], (ptrdiff_t *)&size);
        zmq_setsockopt(sock, option, bin, size - 1);
        free(bin);
        break;
    }
        // CURVE
    case ZMQ_CURVE_PUBLICKEY: case ZMQ_CURVE_SECRETKEY:
    case ZMQ_CURVE_SERVERKEY: {
        char *str = ezmq_copy_string(env, args[2], (ptrdiff_t *)&size);
        // size includes the terminating NULL
        // binary representation is 32 bytes
        // string representation is 40 bytes
        if(size != 33 || size != 41) {
            ezmq_error(env, Qzmq_error, "CURVE key not of valid length.");
            free(str);
            return NULL;
        }
        if(size == 33) size -= 1;
        zmq_setsockopt(sock, option, str, size);
        free(str);
    }
    default:
        ezmq_error(env, Qzmq_error, "Socket option not handled yet.");
        return NULL;
    }
    return Qnil;
}

emacs_value
Fzmq_getsockopt(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    char buf[256];
    size_t size = 256;
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_INT(option, args[1]);

    EZMQ_CHECK_ERROR(zmq_getsockopt(sock, option, buf, &size));
    if(EZMQ_NONLOCAL_EXIT()) return NULL;

    switch(option) {
        // INT
    case ZMQ_MECHANISM: case ZMQ_BACKLOG: case ZMQ_RATE: case ZMQ_RECOVERY_IVL:
    case ZMQ_SNDHWM: case ZMQ_SNDBUF: case ZMQ_SNDTIMEO: case ZMQ_RCVHWM:
    case ZMQ_RCVBUF: case ZMQ_RCVTIMEO: case ZMQ_LINGER: case ZMQ_RECONNECT_IVL:
    case ZMQ_RECONNECT_IVL_MAX: case ZMQ_MULTICAST_HOPS:
    case ZMQ_MULTICAST_MAXTPDU: case ZMQ_CONNECT_TIMEOUT:
    case ZMQ_HANDSHAKE_IVL: case ZMQ_HEARTBEAT_IVL:
    case ZMQ_HEARTBEAT_TIMEOUT: case ZMQ_HEARTBEAT_TTL: case ZMQ_USE_FD:
    case ZMQ_EVENTS: case ZMQ_TCP_KEEPALIVE: case ZMQ_TCP_KEEPALIVE_CNT:
    case ZMQ_TCP_KEEPALIVE_IDLE: case ZMQ_TCP_KEEPALIVE_INTVL:
    case ZMQ_TCP_MAXRT: case ZMQ_TOS: case ZMQ_VMCI_CONNECT_TIMEOUT:
        // UINT64
    case ZMQ_AFFINITY: case ZMQ_VMCI_BUFFER_SIZE:
    case ZMQ_VMCI_BUFFER_MAX_SIZE: case ZMQ_VMCI_BUFFER_MIN_SIZE:
        // INT64
    case ZMQ_MAXMSGSIZE: {
        // TODO: What values can the UINT64 options take? Can they all be
        // represented by intmax_t?
        intmax_t val;
        memcpy(&val, buf, size);
        return env->make_integer(env, val);
    }
        // INT with BOOL valus
    case ZMQ_IMMEDIATE: case ZMQ_INVERT_MATCHING: case ZMQ_CONFLATE:
    case ZMQ_IPV6: case ZMQ_PLAIN_SERVER: case ZMQ_GSSAPI_PLAINTEXT:
    case ZMQ_GSSAPI_SERVER: case ZMQ_RCVMORE: {
        int val;
        memcpy(&val, buf, size);
        return val ? Qt : Qnil;
    }
        // BOOL
    case ZMQ_THREAD_SAFE: {
        bool val;
        memcpy(&val, buf, size);
        return val ? Qt : Qnil;
    }
        // STRING
    case ZMQ_GSSAPI_PRINCIPAL: case ZMQ_GSSAPI_SERVICE_PRINCIPAL:
    case ZMQ_LAST_ENDPOINT: case ZMQ_PLAIN_PASSWORD: case ZMQ_PLAIN_USERNAME:
    case ZMQ_SOCKS_PROXY: case ZMQ_ZAP_DOMAIN: {
        return ezmq_make_string(env, buf, size);
    }
        // BINARY
    case ZMQ_ROUTING_ID:
        buf[size] = 0;
        return ezmq_make_string(env, buf, size);
        // CURVE
        // Note that these always returns the string representation
    case ZMQ_CURVE_PUBLICKEY: case ZMQ_CURVE_SECRETKEY: case ZMQ_CURVE_SERVERKEY:
        return ezmq_make_string(env, buf, size);
    default:
        ezmq_error(env, Qzmq_error, "Socket option not handled yet.");
        return NULL;
    }
}
