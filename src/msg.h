#ifndef __MSG_H__
#define __MSG_H__

#include "core.h"

EZMQ_DEFUN_PROTO(ezmq_message);
EZMQ_DEFUN_PROTO(ezmq_message_size);
EZMQ_DEFUN_PROTO(ezmq_message_data);
EZMQ_DEFUN_PROTO(ezmq_message_more);
EZMQ_DEFUN_PROTO(ezmq_message_copy);
EZMQ_DEFUN_PROTO(ezmq_message_move);
EZMQ_DEFUN_PROTO(ezmq_message_close);
EZMQ_DEFUN_PROTO(ezmq_message_set);
EZMQ_DEFUN_PROTO(ezmq_message_get);
EZMQ_DEFUN_PROTO(ezmq_message_recv);
EZMQ_DEFUN_PROTO(ezmq_message_send);
EZMQ_DEFUN_PROTO(ezmq_message_gets);
EZMQ_DEFUN_PROTO(ezmq_message_routing_id);
EZMQ_DEFUN_PROTO(ezmq_message_set_routing_id);

#endif /* __MSG_H__ */
