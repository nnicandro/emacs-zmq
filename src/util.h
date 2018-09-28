#ifndef __UTIL_H__
#define __UTIL_H__

#include "core.h"

EZMQ_FUN(ezmq_has, emacs_value);
EZMQ_FUN(ezmq_version, void);
EZMQ_FUN(ezmq_z85_decode, emacs_value);
EZMQ_FUN(ezmq_z85_encode, emacs_value);
EZMQ_FUN(ezmq_curve_keypair, void);
EZMQ_FUN(ezmq_curve_public, emacs_value);
EZMQ_FUN(ezmq_equal, emacs_value, emacs_value);
EZMQ_FUN(ezmq_message_p, emacs_value);
EZMQ_FUN(ezmq_socket_p, emacs_value);
EZMQ_FUN(ezmq_context_p, emacs_value);
EZMQ_FUN(ezmq_poller_p, emacs_value);

#endif /* __UTIL_H__ */
