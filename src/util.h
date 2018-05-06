#ifndef __UTIL_H__
#define __UTIL_H__

#include "core.h"

EZMQ_DEFUN(zmq_has);
EZMQ_DEFUN(zmq_version);
EZMQ_DEFUN(zmq_z85_decode);
EZMQ_DEFUN(zmq_z85_encode);
EZMQ_DEFUN(zmq_curve_keypair);
EZMQ_DEFUN(zmq_curve_public);
EZMQ_DEFUN(zmq_equal);
EZMQ_DEFUN(zmq_message_p);
EZMQ_DEFUN(zmq_socket_p);
EZMQ_DEFUN(zmq_context_p);
EZMQ_DEFUN(zmq_poller_p);

#endif /* __UTIL_H__ */
