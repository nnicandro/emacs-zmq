#include "socket.h"

EZMQ_DOC(ezmq_socket,  "CONTEXT TYPE",
         "Return a new socket of TYPE in CONTEXT.");
emacs_value
ezmq_socket(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, args[0]);
    EZMQ_EXTRACT_INT(type, args[1]);

    void *sock = zmq_socket(context->obj, type);
    EZMQ_CHECK_NULL_ERROR(sock);
    return ezmq_new_obj_ptr(env, ezmq_new_obj(env, EZMQ_SOCKET, sock));
}

EZMQ_DOC(ezmq_send,
         "SOCK MESSAGE FLAGS",
         "Send a single message on SOCK.\n"
         "MESSAGE can either be a `zmq-message' or a string containing only\n"
         "unibyte characters. FLAGS is a bitmask of flag options. See the\n"
         "documentation of zmq_send in the C API for the values FLAGS can take.");
emacs_value
ezmq_send(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, nargs == 3 ? args[2] : Qnil);

    if(EQ(TYPE(args[1]), Qstring)) {
        EZMQ_EXTRACT_STRING(msg, size, args[1]);
        EZMQ_CHECK_ERROR(zmq_send(sock->obj, msg, size, flags));
        free(msg);
    } else {
        EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, args[1]);
        EZMQ_CHECK_ERROR(zmq_msg_send(msg->obj, sock->obj, flags));
    }
    return Qnil;
}

EZMQ_DOC(ezmq_recv,
         "SOCK FLAGS COPY",
         "Receive a message on SOCK.\n"
         "If COPY is non-nil, return a copy of the message data received,\n"
         "otherwise return the `zmq-message' object storing the message data.\n"
         "FLAGS is a bitmask of flag options. See the documentation of\n"
         "zmq_recv in the C API for the values FLAGS can take.");
emacs_value
ezmq_recv(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, nargs >= 2 ? args[1] : Qnil);
    bool copy = true;
    if(nargs == 3)
        copy = !NILP(args[1]);

    zmq_msg_t msg;
    EZMQ_CHECK_ERROR(zmq_msg_init(&msg));
    if(EZMQ_NONLOCAL_EXIT()) return NULL;

    EZMQ_CHECK_ERROR(zmq_msg_recv(&msg, sock->obj, flags));
    if(!EZMQ_NONLOCAL_EXIT()) {
        if(copy) {
            size_t size = zmq_msg_size(&msg);
            char *buf = ezmq_malloc(env, size + 1);
            if(!EZMQ_NONLOCAL_EXIT()) {
                buf[size] = 0;
                memcpy(buf, zmq_msg_data(&msg), size);
                return STRING(buf, size);
            }
        } else {
            ezmq_obj_t *obj = ezmq_new_obj(env, EZMQ_MESSAGE, NULL);
            if(!EZMQ_NONLOCAL_EXIT())
                memcpy(obj->obj, &msg, sizeof(zmq_msg_t));
            return ezmq_new_obj_ptr(env, obj);
        }
    }
    return Qnil;
}

EZMQ_DOC(ezmq_bind,  "SOCK ENDPOINT", "Bind SOCK to ENDPOINT.");
emacs_value
ezmq_bind(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_STRING(endpoint, elen, args[1]);
    EZMQ_CHECK_ERROR(zmq_bind(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

EZMQ_DOC(ezmq_connect, "SOCK ENDPOINT", "Connect SOCK to ENDPOINT.");
emacs_value
ezmq_connect(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_STRING(endpoint, elen, args[1]);
    EZMQ_CHECK_ERROR(zmq_connect(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

EZMQ_DOC(ezmq_unbind, "SOCK ENDPOINT", "Unbind SOCK from ENDPOINT.");
emacs_value
ezmq_unbind(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_STRING(endpoint, elen, args[1]);
    EZMQ_CHECK_ERROR(zmq_unbind(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

EZMQ_DOC(ezmq_disconnect,  "SOCK ENDPOINT", "Disconnect SOCK from ENDPOINT.");
emacs_value
ezmq_disconnect(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_STRING(endpoint, elen, args[1]);
    EZMQ_CHECK_ERROR(zmq_disconnect(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

EZMQ_DOC(ezmq_close, "Close SOCK.", "SOCK");
emacs_value
ezmq_close(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_CHECK_ERROR(zmq_close(sock->obj));
    return Qnil;
}

EZMQ_DOC(ezmq_setsockopt,  "SOCK OPTION VALUE", "Set SOCK OPTION to VALUE.");
emacs_value
ezmq_setsockopt(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
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
        zmq_setsockopt(sock->obj, option, &value, sizeof(int));
        break;
    }
        // UINT64
        // TODO: Figure out the best way to handle U?INT64 since Emacs integers
        // are all signed and only 30 bits wide.
    case ZMQ_AFFINITY: case ZMQ_VMCI_BUFFER_SIZE:
    case ZMQ_VMCI_BUFFER_MAX_SIZE: case ZMQ_VMCI_BUFFER_MIN_SIZE:
        // INT64
    case ZMQ_MAXMSGSIZE: {
        EZMQ_EXTRACT_INT(value, args[2]);
        zmq_setsockopt(sock->obj, option, &value, sizeof(int64_t));
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
        int value = !NILP(args[2]);
        zmq_setsockopt(sock->obj, option, &value, sizeof(int));
        break;
    }
        // STRING
        // TODO: Error when multibyte characters are found
    case ZMQ_GSSAPI_PRINCIPAL: case ZMQ_GSSAPI_SERVICE_PRINCIPAL:
    case ZMQ_PLAIN_PASSWORD: case ZMQ_PLAIN_USERNAME:
    case ZMQ_SOCKS_PROXY: case ZMQ_ZAP_DOMAIN: {
        EZMQ_EXTRACT_STRING(str, len, args[2]);
        zmq_setsockopt(sock->obj, option, str, len);
        free(str);
        break;
    }
        // BINARY
    case ZMQ_CONNECT_ROUTING_ID:
    case ZMQ_ROUTING_ID: case ZMQ_SUBSCRIBE: case ZMQ_UNSUBSCRIBE:
    case ZMQ_XPUB_WELCOME_MSG: {
        EZMQ_EXTRACT_STRING(bin, size, args[2]);
        zmq_setsockopt(sock->obj, option, bin, size);
        free(bin);
        break;
    }
        // CURVE
    case ZMQ_CURVE_PUBLICKEY: case ZMQ_CURVE_SECRETKEY:
    case ZMQ_CURVE_SERVERKEY: {
        EZMQ_EXTRACT_STRING(str, len, args[2]);
        // len includes the terminating NULL
        // binary representation is 32 bytes
        // string representation is 40 bytes
        if(len == 32 || len == 40) {
            if(len == 40) len += 1;
            zmq_setsockopt(sock->obj, option, str, len);
        } else {
            const char *msg = "CURVE key not of valid length.";
            ezmq_signal(env, Qzmq_error, 1, STRING(msg, strlen(msg)));
        }
        free(str);
        break;
    }
    default: {
        const char *msg = "Socket option not handled yet.";
        ezmq_signal(env, Qzmq_error, 1, STRING(msg, strlen(msg)));
    }
        break;
    }
    return Qnil;
}

EZMQ_DOC(ezmq_getsockopt,  "SOCK OPTION", "Get SOCK OPTION.");
emacs_value
ezmq_getsockopt(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    char buf[256];
    size_t size;
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, args[0]);
    EZMQ_EXTRACT_INT(option, args[1]);

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
    case ZMQ_TCP_MAXRT: case ZMQ_TOS: case ZMQ_VMCI_CONNECT_TIMEOUT: {
        int val;
        size = sizeof(int);
        EZMQ_CHECK_ERROR(zmq_getsockopt(sock->obj, option, buf, &size));
        if(!EZMQ_NONLOCAL_EXIT()) {
            memcpy(&val, buf, size);
            return INT(val);
        }
        break;
    }
        // UINT64
    case ZMQ_AFFINITY: case ZMQ_VMCI_BUFFER_SIZE:
    case ZMQ_VMCI_BUFFER_MAX_SIZE: case ZMQ_VMCI_BUFFER_MIN_SIZE:
        // INT64
    case ZMQ_MAXMSGSIZE: {
        // NOTE: Possible truncation here
        uint64_t val;
        size = sizeof(uint64_t);
        EZMQ_CHECK_ERROR(zmq_getsockopt(sock->obj, option, buf, &size));
        if(!EZMQ_NONLOCAL_EXIT()) {
            memcpy(&val, buf, size);
            return INT(val);
        }
        break;
    }
        // INT with BOOL valus
    case ZMQ_IMMEDIATE: case ZMQ_INVERT_MATCHING: case ZMQ_CONFLATE:
    case ZMQ_IPV6: case ZMQ_PLAIN_SERVER: case ZMQ_GSSAPI_PLAINTEXT:
    case ZMQ_GSSAPI_SERVER: case ZMQ_RCVMORE: {
        int val;
        size = sizeof(int);
        EZMQ_CHECK_ERROR(zmq_getsockopt(sock->obj, option, buf, &size));
        if(!EZMQ_NONLOCAL_EXIT()) {
            memcpy(&val, buf, size);
            return val ? Qt : Qnil;
        }
        break;
    }
        // BOOL
    case ZMQ_THREAD_SAFE: {
        bool val;
        size = sizeof(bool);
        EZMQ_CHECK_ERROR(zmq_getsockopt(sock->obj, option, buf, &size));
        if(!EZMQ_NONLOCAL_EXIT()) {
            memcpy(&val, buf, size);
            return val ? Qt : Qnil;
        }
        break;
    }
        // STRING
    case ZMQ_GSSAPI_PRINCIPAL: case ZMQ_GSSAPI_SERVICE_PRINCIPAL:
    case ZMQ_LAST_ENDPOINT: case ZMQ_PLAIN_PASSWORD: case ZMQ_PLAIN_USERNAME:
    case ZMQ_SOCKS_PROXY: case ZMQ_ZAP_DOMAIN:
        size = 256;
        EZMQ_CHECK_ERROR(zmq_getsockopt(sock->obj, option, buf, &size));
        if(!EZMQ_NONLOCAL_EXIT())
            return STRING(buf, size - 1);
        break;
        // BINARY
    case ZMQ_ROUTING_ID:
        size = 256;
        EZMQ_CHECK_ERROR(zmq_getsockopt(sock->obj, option, buf, &size));
        if(!EZMQ_NONLOCAL_EXIT()) {
            buf[size] = 0;
            return STRING(buf, size);
        }
        break;
        // CURVE
        // NOTE: These always returns the string representation
    case ZMQ_CURVE_PUBLICKEY: case ZMQ_CURVE_SECRETKEY: case ZMQ_CURVE_SERVERKEY:
        size = 41;
        EZMQ_CHECK_ERROR(zmq_getsockopt(sock->obj, option, buf, &size));
        if(!EZMQ_NONLOCAL_EXIT())
            return STRING(buf, size - 1);
        break;
    default: {
        const char *msg = "Socket option not handled yet.";
        ezmq_signal(env, Qzmq_error, 1, STRING(msg, strlen(msg)));
    }
        break;
    }
    return NULL;
}
