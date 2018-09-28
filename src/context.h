
#ifndef __CONTEXT_H__
#define __CONTEXT_H__

#include "core.h"

EZMQ_FUN(ezmq_context, void);
EZMQ_FUN(ezmq_ctx_set, emacs_value, emacs_value, emacs_value);
EZMQ_FUN(ezmq_ctx_get, emacs_value, emacs_value);
EZMQ_FUN(ezmq_ctx_shutdown, emacs_value);
EZMQ_FUN(ezmq_ctx_term, emacs_value);

#endif /* __CONTEXT_H__ */
