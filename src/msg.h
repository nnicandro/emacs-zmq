#ifndef __MSG_H__
#define __MSG_H__

#include "core.h"

EZMQ_DEFUN(zmq_message);
EZMQ_DEFUN(zmq_message_size);
EZMQ_DEFUN(zmq_message_data);
EZMQ_DEFUN(zmq_message_more);
EZMQ_DEFUN(zmq_message_copy);
EZMQ_DEFUN(zmq_message_move);
EZMQ_DEFUN(zmq_message_set);
EZMQ_DEFUN(zmq_message_get);
EZMQ_DEFUN(zmq_message_recv);
EZMQ_DEFUN(zmq_message_send);
EZMQ_DEFUN(zmq_message_gets);
EZMQ_DEFUN(zmq_message_routing_id);
EZMQ_DEFUN(zmq_message_set_routing_id);

#endif /* __MSG_H__ */
