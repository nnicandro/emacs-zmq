#ifndef __POLL_H__
#define __POLL_H__

#include "core.h"

EZMQ_DEFUN(zmq_poll);
EZMQ_DEFUN(zmq_poller_new);
EZMQ_DEFUN(zmq_poller_add);
EZMQ_DEFUN(zmq_poller_add_fd);
EZMQ_DEFUN(zmq_poller_modify);
EZMQ_DEFUN(zmq_poller_modify_fd);
EZMQ_DEFUN(zmq_poller_remove);
EZMQ_DEFUN(zmq_poller_remove_fd);
EZMQ_DEFUN(zmq_poller_wait);
EZMQ_DEFUN(zmq_poller_wait_all);

#endif /* __POLL_H__ */
