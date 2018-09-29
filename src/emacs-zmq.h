#ifndef __EMACS_ZMQ_H__
#define __EMACS_ZMQ_H__

#include "core.h"
#include "util.h"
#include "msg.h"
#include "socket.h"
#include "context.h"
#include "poll.h"

extern int plugin_is_GPL_compatible;

// Defined in constants.c
extern void
ezmq_expose_constants();

extern int
emacs_module_init(struct emacs_runtime *ert);

#endif /* __EMACS_ZMQ_H__ */
