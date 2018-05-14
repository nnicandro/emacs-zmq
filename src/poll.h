#ifndef __POLL_H__
#define __POLL_H__

#include "core.h"

EZMQ_DEFUN_PROTO(ezmq_poll);
EZMQ_DEFUN_PROTO(ezmq_poller_new);
EZMQ_DEFUN_PROTO(ezmq_poller_add);
EZMQ_DEFUN_PROTO(ezmq_poller_modify);
EZMQ_DEFUN_PROTO(ezmq_poller_remove);
EZMQ_DEFUN_PROTO(ezmq_poller_wait);
EZMQ_DEFUN_PROTO(ezmq_poller_wait_all);
EZMQ_DEFUN_PROTO(ezmq_poller_destroy);

#endif /* __POLL_H__ */
