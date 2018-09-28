#ifndef __POLL_H__
#define __POLL_H__

#include "core.h"

EZMQ_FUN(ezmq_poll, emacs_value, emacs_value);
EZMQ_FUN(ezmq_poller_new, void);
EZMQ_FUN(ezmq_poller_add, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_poller_modify, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_poller_remove, emacs_value, emacs_value);
EZMQ_FUN(ezmq_poller_wait, emacs_value, emacs_value);
EZMQ_FUN(ezmq_poller_wait_all, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_poller_destroy, emacs_value);

#endif /* __POLL_H__ */
