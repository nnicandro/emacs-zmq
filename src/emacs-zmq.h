#ifndef __EMACS_ZMQ_H__
#define __EMACS_ZMQ_H__

#include "core.h"
#include "util.h"
#include "msg.h"
#include "socket.h"
#include "context.h"
#include "poll.h"

// https://gcc.gnu.org/wiki/Visibility
#if defined _WIN32 || defined __CYGWIN__
  #ifdef __GNUC__
    #define ZMQ_EXPORT __attribute__ ((dllexport))
  #else
    #define ZMQ_EXPORT __declspec(dllexport)
  #endif
#else
  #if __GNUC__ >= 4
    #define ZMQ_EXPORT __attribute__ ((visibility ("default")))
  #else
    #define ZMQ_EXPORT
  #endif
#endif

ZMQ_EXPORT int plugin_is_GPL_compatible;
// Defined in constants.c
extern void
ezmq_expose_constants();

ZMQ_EXPORT int
emacs_module_init(struct emacs_runtime *ert);

#endif /* __EMACS_ZMQ_H__ */
