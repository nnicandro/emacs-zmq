#ifndef __MSG_H__
#define __MSG_H__

#include "core.h"

EZMQ_FUN(ezmq_message, emacs_value);
EZMQ_FUN(ezmq_message_size, emacs_value);
EZMQ_FUN(ezmq_message_data, emacs_value);
EZMQ_FUN(ezmq_message_more, emacs_value);
EZMQ_FUN(ezmq_message_copy, emacs_value);
EZMQ_FUN(ezmq_message_move, emacs_value, emacs_value);
EZMQ_FUN(ezmq_message_close, emacs_value);
EZMQ_FUN(ezmq_message_set, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_message_get, emacs_value, emacs_value);
EZMQ_FUN(ezmq_message_recv, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_message_send, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_message_gets, emacs_value, emacs_value);
EZMQ_FUN(ezmq_message_routing_id, emacs_value);
EZMQ_FUN(ezmq_message_set_routing_id, emacs_value, emacs_value);
EZMQ_FUN(ezmq_message_group, emacs_value);
EZMQ_FUN(ezmq_message_set_group, emacs_value, emacs_value);

#endif /* __MSG_H__ */
