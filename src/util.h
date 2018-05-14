#ifndef __UTIL_H__
#define __UTIL_H__

#include "core.h"

EZMQ_DEFUN_PROTO(ezmq_has);
EZMQ_DEFUN_PROTO(ezmq_version);
EZMQ_DEFUN_PROTO(ezmq_z85_decode);
EZMQ_DEFUN_PROTO(ezmq_z85_encode);
EZMQ_DEFUN_PROTO(ezmq_curve_keypair);
EZMQ_DEFUN_PROTO(ezmq_curve_public);
EZMQ_DEFUN_PROTO(ezmq_equal);
EZMQ_DEFUN_PROTO(ezmq_message_p);
EZMQ_DEFUN_PROTO(ezmq_socket_p);
EZMQ_DEFUN_PROTO(ezmq_context_p);
EZMQ_DEFUN_PROTO(ezmq_poller_p);

#endif /* __UTIL_H__ */
