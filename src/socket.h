
#ifndef __SOCKET_H__
#define __SOCKET_H__

#include "ezmq.h"

extern emacs_value
Fzmq_socket(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

EZMQ_DOC(zmq_send,
         "Send a single message on SOCK."
         "MESSAGE can either be a `zmq-message' or a string containing only"
         "unibyte characters. FLAGS is a bitmask of flag options. See the"
         "documentation of zmq_send in the C API for the values FLAGS can take.",
         "SOCK MESSAGE FLAGS");
extern emacs_value
Fzmq_send(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

EZMQ_DOC(zmq_recv,
         "On SOCK, receive NBYTES with optional FLAGS.\n"
         "FLAGS is a bitmask of flag options. See the documentation of\n"
         "zmq_recv in the C API for the values FLAGS can take.",
         "SOCK NBYTES FLAGS");
extern emacs_value
Fzmq_recv(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

EZMQ_DOC(zmq_bind, "Bind SOCK to ENDPOINT.", "SOCK ENDPOINT");
extern emacs_value
Fzmq_bind(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

EZMQ_DOC(zmq_connect, "Connect SOCK to ENDPOINT.", "SOCK ENDPOINT");
extern emacs_value
Fzmq_connect(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

EZMQ_DOC(zmq_unbind, "Unbind SOCK from ENDPOINT.", "SOCK ENDPOINT");
extern emacs_value
Fzmq_unbind(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

EZMQ_DOC(zmq_disconnect, "Disconnect SOCK from ENDPOINT.", "SOCK ENDPOINT");
extern emacs_value
Fzmq_disconnect(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

EZMQ_DOC(zmq_close, "Close SOCK.", "SOCK");
extern emacs_value
Fzmq_close(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

#endif /* __SOCKET_H__ */
