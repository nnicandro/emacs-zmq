#include "socket.h"
#include <assert.h>

// TODO: Use the actual documentation of the ZMQ API for all functions.

EZMQ_DOC(ezmq_socket,  "CONTEXT TYPE",
         "Return a new socket of TYPE in CONTEXT.");
emacs_value
ezmq_socket(emacs_value econtext, emacs_value etype)
{
    EZMQ_EXTRACT_OBJ(context, EZMQ_CONTEXT, econtext);
    EZMQ_EXTRACT_INT(type, etype);

    void *sock = zmq_socket(context->obj, type);
    EZMQ_CHECK_NULL_ERROR(sock);
    ezmq_obj_t *obj = ezmq_new_obj(EZMQ_SOCKET, sock);
    // Ensure the context does not get garbage collected while the socket is in
    // use
    ezmq_obj_set_val(obj, econtext);
    ezmq_debug("ezmq_socket()\n");
    return ezmq_new_obj_ptr(obj);
}

EZMQ_DOC(ezmq_send,
         "SOCK MESSAGE &optional FLAGS",
         "Send a single message on SOCK.\n"
         "MESSAGE can either be a `zmq-message' or a string containing only\n"
         "unibyte characters. FLAGS is a bitmask of flag options. See the\n"
         "documentation of zmq_send in the C API for the values FLAGS can take.");
emacs_value
ezmq_send(emacs_value esock, emacs_value emsg, emacs_value eflags)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, eflags);

    if(EQ(TYPE(emsg), Qstring)) {
        EZMQ_EXTRACT_STRING(msg, size, emsg);
        EZMQ_CHECK_ERROR(zmq_send(sock->obj, msg, size, flags));
        free(msg);
    } else {
        EZMQ_EXTRACT_OBJ(msg, EZMQ_MESSAGE, emsg);
        EZMQ_CHECK_ERROR(zmq_msg_send(msg->obj, sock->obj, flags));
    }
    return Qnil;
}

EZMQ_DOC(ezmq_recv,
         "SOCK &optional FLAGS NOCOPY",
         "Receive a message on SOCK.\n"
         "If NOCOPY is nil, return a copy of the message data received,\n"
         "otherwise when NOCOPY is non-nil, return the `zmq-message' object\n"
         "storing the message data. FLAGS is a bitmask of flag options. See\n"
         "the documentation of zmq_recv in the C API for the values FLAGS can take.");
emacs_value
ezmq_recv(emacs_value esock, emacs_value eflags, emacs_value enocopy)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_OPTIONAL_INT(flags, eflags);

    zmq_msg_t *msg = ezmq_malloc(sizeof(*msg));
    EZMQ_CHECK_ERROR(zmq_msg_init(msg));
    EZMQ_CHECK_ERROR(zmq_msg_recv(msg, sock->obj, flags));

    emacs_value retval = Qnil;
    if(!NONLOCAL_EXIT()) {
        if(NILP(enocopy)) {
            // After examining emacs/src/alloc.c it looks like the null
            // termination of STRING, i.e. env->make_string, is already taken
            // care of when allocating in allocate_string_data so there is no
            // need to do a double copy just to insert a null terminator. It
            // looks like this has been the case since Emacs 25 when modules
            // were introduced.
            retval = STRING(zmq_msg_data(msg), zmq_msg_size(msg));
            zmq_msg_close(msg);
            free(msg);
        } else {
            retval = ezmq_new_obj_ptr(ezmq_new_obj(EZMQ_MESSAGE, msg));
        }
    } else {
        free(msg);
    }


    return retval;
}


EZMQ_DOC(ezmq_bind, "SOCK ENDPOINT", "Bind SOCK to ENDPOINT.");
emacs_value
ezmq_bind(emacs_value esock, emacs_value eendpoint)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_STRING(endpoint, elen, eendpoint);
    EZMQ_CHECK_ERROR(zmq_bind(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

EZMQ_DOC(ezmq_connect, "SOCK ENDPOINT", "Connect SOCK to ENDPOINT.");
emacs_value
ezmq_connect(emacs_value esock, emacs_value eendpoint)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_STRING(endpoint, elen, eendpoint);
    EZMQ_CHECK_ERROR(zmq_connect(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

EZMQ_DOC(ezmq_unbind, "SOCK ENDPOINT", "Unbind SOCK from ENDPOINT.");
emacs_value
ezmq_unbind(emacs_value esock, emacs_value eendpoint)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_STRING(endpoint, elen, eendpoint);
    EZMQ_CHECK_ERROR(zmq_unbind(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

EZMQ_DOC(ezmq_disconnect,  "SOCK ENDPOINT", "Disconnect SOCK from ENDPOINT.");
emacs_value
ezmq_disconnect(emacs_value esock, emacs_value eendpoint)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_STRING(endpoint, elen, eendpoint);
    EZMQ_CHECK_ERROR(zmq_disconnect(sock->obj, endpoint));
    free(endpoint);
    return Qnil;
}

EZMQ_DOC(ezmq_join, "SOCK GROUP", "On SOCK, join GROUP.");
emacs_value
ezmq_join(emacs_value esock, emacs_value egroup)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_STRING(group, elen, egroup);
    EZMQ_CHECK_ERROR(zmq_join(sock->obj, group));
    free(group);
    return Qnil;
}

EZMQ_DOC(ezmq_leave, "SOCK GROUP", "On SOCK, leave GROUP.");
emacs_value
ezmq_leave(emacs_value esock, emacs_value egroup)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_STRING(group, elen, egroup);
    EZMQ_CHECK_ERROR(zmq_leave(sock->obj, group));
    free(group);
    return Qnil;
}

EZMQ_DOC(ezmq_close, "SOCK", "Close SOCK.");
emacs_value
ezmq_close(emacs_value esock)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_CHECK_ERROR(zmq_close(sock->obj));
    // From the zmq API a socket should only be closed once. This indicates to
    // the finalizer function that the socket has already been closed.
    if(!NONLOCAL_EXIT()) sock->obj = NULL;
    return Qnil;
}

EZMQ_DOC(ezmq_proxy,
         "FRONTEND BACKEND &optional CAPTURE",
         "Start the builtin ZMQ proxy.\n"
         "Connect the FRONTEND socket to the BACKEND socket. Depending on\n"
         "the socket types, replies may flow in the opposite direction. The\n"
         "direction is conceptual only; the proxy is fully symmetric and there\n"
         "is no technical difference between frontend and backend.\n\n"
         "If the CAPTURE socket is non-nil, the proxy shall send all\n"
         "messages, received on both FRONTEND and BACKEND, to the CAPTURE\n"
         "socket. The CAPTURE socket should be a `zmq-PUB', `zmq-DEALER',\n"
         "`zmq-PUSH', or `zmq-PAIR' socket.");
emacs_value
ezmq_proxy(emacs_value efrontend, emacs_value ebackend, emacs_value ecapture)
{
    EZMQ_EXTRACT_OBJ(frontend, EZMQ_SOCKET, efrontend);
    EZMQ_EXTRACT_OBJ(backend, EZMQ_SOCKET, ebackend);
    EZMQ_EXTRACT_OPTIONAL_OBJ(capture, EZMQ_SOCKET, ecapture);
    EZMQ_CHECK_ERROR(zmq_proxy(frontend->obj,
                               backend->obj,
                               capture ? capture->obj : NULL));
    return Qnil;
}

EZMQ_DOC(ezmq_proxy_steerable,
         "FRONTEND BACKEND &optional CAPTURE CONTROL",
         "Start the builtin ZMQ proxy with control flow.\n"
         "Starts the builtin ZMQ proxy with additional control flow\n"
         "(see `zmq-proxy') provided by the CONTROL socket if it is non-nil.\n"
         "If PAUSE is received on this socket, the proxy suspends its\n"
         "activities. If RESUME is received, it goes on. If TERMINATE is\n"
         "received, it terminates smoothly. At start, the proxy runs\n"
         "normally as if `zmq-proxy' was used.");
emacs_value
ezmq_proxy_steerable(emacs_value efrontend, emacs_value ebackend, emacs_value ecapture, emacs_value econtrol)
{
    EZMQ_EXTRACT_OBJ(frontend, EZMQ_SOCKET, efrontend);
    EZMQ_EXTRACT_OBJ(backend, EZMQ_SOCKET, ebackend);
    EZMQ_EXTRACT_OPTIONAL_OBJ(capture, EZMQ_SOCKET, ecapture);
    EZMQ_EXTRACT_OPTIONAL_OBJ(control, EZMQ_SOCKET, econtrol);
    EZMQ_CHECK_ERROR(zmq_proxy_steerable(frontend->obj,
                                         backend->obj,
                                         capture ? capture->obj : NULL,
                                         control ? control->obj : NULL));
    return Qnil;
}

EZMQ_DOC(ezmq_socket_monitor,
         "SOCK ENDPOINT EVENTS",
         "The `zmq-socket-monitor' method lets an application thread track\n"
         "socket events (like connects) on a ZeroMQ SOCK. Each call to\n"
         "this method creates a `zmq-PAIR' socket and binds that to the\n"
         "specified inproc:// ENDPOINT. To collect the socket events, you\n"
         "must create your own `zmq-PAIR' socket, and connect that to the\n"
         "ENDPOINT.\n\n"
         "The EVENTS argument is a list of the socket events you wish to\n"
         "monitor, see the C API for the events. To monitor all events, use\n"
         "the event value `zmq-EVENT-ALL'. NOTE: as new events are added, the\n"
         "catch-all value will start returning them. An application that\n"
         "relies on a strict and fixed sequence of events must not use\n"
         "`zmq-EVENT-ALL' in order to guarantee compatibility with future\n"
         "versions.\n\n"
         "Each event is sent as two frames. The first frame contains an\n"
         "event number (16 bits), and an event value (32 bits) that\n"
         "provides additional data according to the event number. The\n"
         "second frame contains a string that specifies the affected TCP or\n"
         "IPC endpoint.");
emacs_value
ezmq_socket_monitor(emacs_value esock, emacs_value eendpoint, emacs_value eevents)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_STRING(endpoint, elen, eendpoint);
    intmax_t events = 0;
    emacs_value list = eevents;
    if(EQ(TYPE(list), Qcons)) {
        while(!NILP(list)) {
            // TODO: Verify event, raise an error if invalid
            EZMQ_EXTRACT_INT(event, CAR(list));
            events |= event;
            list = CDR(list);
        }
    } else
        ezmq_wrong_type_argument(list, 1, Qlist);

    EZMQ_CHECK_ERROR(zmq_socket_monitor(sock, endpoint, events));
    return Qnil;
}

EZMQ_DOC(ezmq_setsockopt,  "SOCK OPTION VALUE", "Set SOCK OPTION to VALUE.");
emacs_value
ezmq_setsockopt(emacs_value esock, emacs_value eoption, emacs_value evalue)
{
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_INT(option, eoption);

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
        EZMQ_EXTRACT_INT(value, evalue);
        zmq_setsockopt(sock->obj, option, &value, sizeof(int));
        break;
    }
        // UINT64
        // TODO: Figure out the best way to handle U?INT64 since Emacs integers
        // are all signed and only 30 bits wide. This won't
        // be the case with bignum support in future Emacsen.
    case ZMQ_AFFINITY: case ZMQ_VMCI_BUFFER_SIZE:
    case ZMQ_VMCI_BUFFER_MAX_SIZE: case ZMQ_VMCI_BUFFER_MIN_SIZE:
        // INT64
    case ZMQ_MAXMSGSIZE: {
        EZMQ_EXTRACT_INT(_value, evalue);
        int64_t value = (int64_t)_value;
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
        int value = !NILP(evalue);
        zmq_setsockopt(sock->obj, option, &value, sizeof(int));
        break;
    }
        // STRING
    case ZMQ_GSSAPI_PRINCIPAL: case ZMQ_GSSAPI_SERVICE_PRINCIPAL:
    case ZMQ_PLAIN_PASSWORD: case ZMQ_PLAIN_USERNAME:
    case ZMQ_SOCKS_PROXY: case ZMQ_ZAP_DOMAIN: {
        EZMQ_EXTRACT_STRING(str, len, evalue);
        // From http://api.zeromq.org/master:zmq-setsockopt
        //
        // For options taking a value of type "character string", the provided
        // byte data should either contain no zero bytes, or end in a single
        // zero byte
        int i;
        for(i = 0; i < len; i++)
            if(!str[i]) break;
        if(i != len) {
            char const *msg = "Value cannot contain NUL characters";
            ezmq_signal(Qzmq_error, 1, STRING(msg, strlen(msg)));
        } else
            zmq_setsockopt(sock->obj, option, str, len);
        free(str);
        break;
    }
        // BINARY
    case ZMQ_CONNECT_ROUTING_ID:
    case ZMQ_ROUTING_ID: case ZMQ_SUBSCRIBE: case ZMQ_UNSUBSCRIBE:
    case ZMQ_XPUB_WELCOME_MSG: {
        EZMQ_EXTRACT_STRING(bin, size, evalue);
        zmq_setsockopt(sock->obj, option, bin, size);
        free(bin);
        break;
    }
        // CURVE
    case ZMQ_CURVE_PUBLICKEY: case ZMQ_CURVE_SECRETKEY:
    case ZMQ_CURVE_SERVERKEY: {
        EZMQ_EXTRACT_STRING(str, len, evalue);
        // len includes the terminating NULL
        // binary representation is 32 bytes
        // string representation is 40 bytes
        if(len == 32 || len == 40) {
            if(len == 40) len += 1;
            zmq_setsockopt(sock->obj, option, str, len);
        } else {
            const char *msg = "CURVE key not of valid length.";
            ezmq_signal(Qzmq_error, 1, STRING(msg, strlen(msg)));
        }
        free(str);
        break;
    }
    default: {
        const char *msg = "Socket option not handled yet.";
        ezmq_signal(Qzmq_error, 1, STRING(msg, strlen(msg)));
    }
        break;
    }
    return Qnil;
}

// This assertion is required since Emacs does not support 64 bit wide
// integers, but this will change with bignum support.
void
ezmq_assert_eint(uint64_t val)
{
    assert((int64_t)val >= INTMAX_MIN && val <= INTMAX_MAX);
}

EZMQ_DOC(ezmq_getsockopt,  "SOCK OPTION", "Get SOCK OPTION.");
emacs_value
ezmq_getsockopt(emacs_value esock, emacs_value eoption)
{
    char buf[256];
    size_t size;
    EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
    EZMQ_EXTRACT_INT(option, eoption);

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
        if(!NONLOCAL_EXIT()) {
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
        uint64_t val;
        size = sizeof(uint64_t);
        EZMQ_CHECK_ERROR(zmq_getsockopt(sock->obj, option, buf, &size));
        if(!NONLOCAL_EXIT()) {
            memcpy(&val, buf, size);
            ezmq_assert_eint(val);
            return INT((intmax_t)val);
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
        if(!NONLOCAL_EXIT()) {
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
        if(!NONLOCAL_EXIT()) {
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
        if(!NONLOCAL_EXIT())
            return STRING(buf, size - 1);
        break;
        // BINARY
    case ZMQ_ROUTING_ID:
        size = 256;
        EZMQ_CHECK_ERROR(zmq_getsockopt(sock->obj, option, buf, &size));
        if(!NONLOCAL_EXIT()) {
            buf[size] = 0;
            return STRING(buf, size);
        }
        break;
        // CURVE
        // NOTE: These always returns the string representation
    case ZMQ_CURVE_PUBLICKEY: case ZMQ_CURVE_SECRETKEY: case ZMQ_CURVE_SERVERKEY:
        size = 41;
        EZMQ_CHECK_ERROR(zmq_getsockopt(sock->obj, option, buf, &size));
        if(!NONLOCAL_EXIT())
            return STRING(buf, size - 1);
        break;
    default: {
        const char *msg = "Socket option not handled yet.";
        ezmq_signal(Qzmq_error, 1, STRING(msg, strlen(msg)));
    }
        break;
    }
    return NULL;
}
