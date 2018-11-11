
#ifndef __SOCKET_H__
#define __SOCKET_H__

#include "core.h"

EZMQ_FUN(ezmq_socket, emacs_value, emacs_value);
EZMQ_FUN(ezmq_send, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_recv, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_bind, emacs_value, emacs_value);
EZMQ_FUN(ezmq_connect, emacs_value, emacs_value);
EZMQ_FUN(ezmq_join, emacs_value, emacs_value);
EZMQ_FUN(ezmq_unbind, emacs_value, emacs_value);
EZMQ_FUN(ezmq_disconnect, emacs_value, emacs_value);
EZMQ_FUN(ezmq_leave, emacs_value, emacs_value);
EZMQ_FUN(ezmq_close, emacs_value);
EZMQ_FUN(ezmq_proxy, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_proxy_steerable, emacs_value, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_socket_monitor, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_setsockopt, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_getsockopt, emacs_value, emacs_value);

#endif /* __SOCKET_H__ */
