
#ifndef __SOCKET_H__
#define __SOCKET_H__

#include "core.h"

EZMQ_DEFUN_PROTO(ezmq_socket);
EZMQ_DEFUN_PROTO(ezmq_send);
EZMQ_DEFUN_PROTO(ezmq_recv);
EZMQ_DEFUN_PROTO(ezmq_bind);
EZMQ_DEFUN_PROTO(ezmq_connect);
EZMQ_DEFUN_PROTO(ezmq_unbind);
EZMQ_DEFUN_PROTO(ezmq_disconnect);
EZMQ_DEFUN_PROTO(ezmq_close);
EZMQ_DEFUN_PROTO(ezmq_setsockopt);
EZMQ_DEFUN_PROTO(ezmq_getsockopt);

#endif /* __SOCKET_H__ */
