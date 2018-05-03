#ifndef __EZMQ_H__
#define __EZMQ_H__

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <zmq.h>
#include "emacs-module.h"
#include "macros.h"

enum ezmq_obj_t {
    EZMQ_CONTEXT,
    EZMQ_MESSAGE,
    EZMQ_SOCKET,
    EZMQ_POLLER
};

typedef struct {
    enum ezmq_obj_t type;
    void *obj;
} ezmq_obj_t;

extern emacs_value Qzmq_error, Qt, Qnil, Qlist,
    Qwrong_type_argument, Qcons, Qstring, Qvector, Qdefconst, Qcar, Qcdr, Qlength, Qinteger;

extern void
ezmq_error(emacs_env *env, emacs_value err, const char *msg);

extern void
ezmq_signal(emacs_env *env, emacs_value err, int nargs, ...);

extern void
ezmq_wrong_type_argument(emacs_env *env, emacs_value val, int nvalid, ...);
/**
   Called when an error occured in ZMQ to notify Emacs to exit nonlocally.
*/
// TODO: More general error handling, many places that have a lot boiler plate
// just to send an error out.
extern void
ezmq_signal_error(emacs_env *env);

/**
   Create a new ezmq_obj with type and obj and return it. As a special case if
   type is EZMQ_MESSAGE and obj is NULL, allocate the required zmq_msg_t
   object. This is the normal way to create new ZMQ message objects. The
   environment is passed for the case when nno memory can be allocated, an
   error is signaled and this function will return NULL.
*/
extern ezmq_obj_t *
ezmq_new_obj(emacs_env *env, enum ezmq_obj_t type, void *obj);

extern emacs_value
ezmq_new_obj_ptr(emacs_env *env, enum ezmq_obj_t type, void *obj);

extern void
ezmq_free_obj(ezmq_obj_t *obj);

extern ezmq_obj_t *
ezmq_extract_obj(emacs_env *env, enum ezmq_obj_t type, emacs_value obj);

/**
   Similar to malloc, but signal an error.
*/
extern char*
ezmq_malloc(emacs_env *env, size_t nbytes);

/**
   Copy and return a pointer to an Emacs string. The caller is responsible for
   freeing the memory returned.
*/
extern char *
ezmq_copy_string(emacs_env *env, emacs_value str, ptrdiff_t *size);

extern emacs_value
ezmq_make_string(emacs_env *env, const char *str, size_t len);

extern bool
ezmq_value_of_type(emacs_env *env, emacs_value val, emacs_value sym);

extern void
ezmq_expose_constants(emacs_env *env);

#endif /* __EZMQ_H__ */
