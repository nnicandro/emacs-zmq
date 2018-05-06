
#ifndef __SOCKET_H__
#define __SOCKET_H__

#include "core.h"

EZMQ_DEFUN(zmq_socket);
EZMQ_DEFUN(zmq_send);
EZMQ_DEFUN(zmq_recv);
EZMQ_DEFUN(zmq_bind);
EZMQ_DEFUN(zmq_connect);
EZMQ_DEFUN(zmq_unbind);
EZMQ_DEFUN(zmq_disconnect);
EZMQ_DEFUN(zmq_setsockopt);
EZMQ_DEFUN(zmq_getsockopt);

#endif /* __SOCKET_H__ */
