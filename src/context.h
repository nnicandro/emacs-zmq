
#ifndef __CONTEXT_H__
#define __CONTEXT_H__

#include "ezmq.h"

extern emacs_value
Fzmq_context(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_ctx_set(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_ctx_get(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_ctx_term(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

extern emacs_value
Fzmq_ctx_shutdown(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data);

#endif /* __CONTEXT_H__ */
