#ifndef __MSG_H__
#define __MSG_H__

#include "ezmq.h"

// TODO: From the documentation of zmq_msg_init: "never initialize the same
// message twice", I think I do this somewhere in zmq-ffi.el or in jupyter.el,
// find where.
EZMQ_DOC(zmq_message,
         "Initialize a ZMQ message.\n"
         "If DATA is non-nil, initialize the message using DATA.\n"
         "DATA can be either a string or a vector of byte integers.\n"
         "If DATA is nil, initialize an empty message.",
         "&optional DATA");
extern emacs_value
Fzmq_message(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_size(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_data(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_more(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_copy(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_move(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_close(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_set(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_get(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_recv(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_send(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_gets(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_routing_id(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_message_set_routing_id(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

#endif /* __MSG_H__ */
