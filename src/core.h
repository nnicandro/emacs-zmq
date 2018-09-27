#ifndef __CORE_H__
#define __CORE_H__

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <zmq.h>
#include <stdbool.h>
#include "emacs-module.h"

/**
   About what environment functions do when a non-local exit is pending, from
   http://phst.github.io/emacs-modules.html#when-a-nonlocal-exit-is-pending-module-functions-silently-do-nothing:

   environment functions exhibit saturating behavior: once a nonlocal exit
   is pending for an environment, all environment functions called for that
   environment silently ignore all their arguments (except the environment
   pointer itself) and return an unspecified value.
*/

/**
   Helper macros to aid in interacting with the Emacs environment. They all
   expect an implicit emacs_env variable named env to be defined in the calling
   environment of the macro.
*/
#define INTERN(val) env->intern(env, val)

#define FUNCALL(fun, nargs, args) env->funcall(env, fun, nargs, args)

#define CAR(list) FUNCALL(Qcar, 1, &list)

#define CDR(list) FUNCALL(Qcdr, 1, &list)

#define INT(i) env->make_integer(env, i)

#define STRING(str, len) env->make_string(env, str, len)

#define LIST(count, ...) FUNCALL(Qlist, count, ((emacs_value []){ __VA_ARGS__ }))

#define CONS(car, cdr) FUNCALL(Qcons, 2, ((emacs_value []){ car, cdr }))

#define TYPE(val) env->type_of(env, val)

#define USER_PTR(val) env->get_user_ptr(env, val)

#define USER_FINALIZER(val) env->get_user_finalizer(env, val)

#define EQ(a, b) env->eq(env, a, b)

#define NILP(a) !env->is_not_nil(env, a)

#define EQUAL(a, b) EQ(FUNCALL(Qequal, 2, ((emacs_value []){ a, b })), Qt)

#define LENGTH(list) env->extract_integer(env, FUNCALL(Qlength, 1, &list))

#define EZMQ_NONLOCAL_EXIT() (env->non_local_exit_check(env) != emacs_funcall_exit_return)
#define AREF(vec, i) env->vec_get(env, val, i)

#define SIGNAL(err, data) env->non_local_exit_signal(env, err, data)

/**
   EZMQ_EXTRACT_* macros transform the Lisp object representations to their
   corresponding C representations. If during the extraction an error is
   signaled, they immediately exit the function where the macro was expanded by
   returning NULL.
*/

#define EZMQ_EXTRACT_INT(name, val)                 \
    intmax_t name = env->extract_integer(env, val); \
    do {                                            \
        if(EZMQ_NONLOCAL_EXIT()) {                  \
            return NULL;                            \
        }                                           \
    } while(0)

#define EZMQ_EXTRACT_OPTIONAL_INT(name, val)    \
    intmax_t name = EQ(val, Qnil) ? 0 :         \
        env->extract_integer(env, val);         \
    do {                                        \
        if(EZMQ_NONLOCAL_EXIT()) {              \
            return NULL;                        \
        }                                       \
    } while(0)

 #define EZMQ_EXTRACT_OBJ(name, type, val)                \
    ezmq_obj_t *name = ezmq_extract_obj(env, type, val);  \
    if(EZMQ_NONLOCAL_EXIT()) return NULL                  \

#define EZMQ_EXTRACT_STRING(name, len, val)         \
    ptrdiff_t len = 0;                              \
    char *name = ezmq_copy_string(env, val, &len);  \
    do {                                            \
        if(EZMQ_NONLOCAL_EXIT()) {                  \
            return NULL;                            \
        }                                           \
    } while(0)

#define EZMQ_CHECK_ERROR(expr)                  \
    do {                                        \
        int __retcode = expr;                   \
        if(__retcode == -1) {                   \
            ezmq_signal_error(env);             \
        }                                       \
    } while(0)

#define EZMQ_CHECK_NULL_ERROR(expr)             \
    do {                                        \
        if((void *)(expr) == NULL) {            \
            ezmq_signal_error(env);             \
        }                                       \
    } while(0)

#define EZMQ_DOC(name, args, doc)                         \
    const char *__zmq_doc_##name = doc "\n\n(fn " args ")"

#define EZMQ_DEFUN_PROTO(name)                                          \
    extern const char *__zmq_doc_##name;                                \
    extern emacs_value                                                  \
    name(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)

enum ezmq_obj_t {
    EZMQ_CONTEXT,
    EZMQ_MESSAGE,
    EZMQ_SOCKET,
    EZMQ_POLLER
};

typedef struct {
    void *obj;
    enum ezmq_obj_t type;
    // Stores how many times a user-ptr has been created for the object, we use
    // this reference count as opposed to make_global_ref because it is easier
    // to just create new user-ptr objects to ZMQ sockets when extracting the
    // polling information.
    intmax_t refcount;
} ezmq_obj_t;

extern emacs_value Qzmq_error, Qt, Qnil, Qlist,
    Qwrong_type_argument, Qargs_out_of_range,
    Qcons, Qstring, Qvector, Qcar, Qcdr, Qlength, Qinteger, Qequal,
    Qzmq_POLLIN, Qzmq_POLLERR, Qzmq_POLLOUT,
    Izmq_POLLIN, Izmq_POLLERR, Izmq_POLLOUT;

extern void
ezmq_signal(emacs_env *env, emacs_value err, int nargs, ...);

extern void
ezmq_wrong_type_argument(emacs_env *env, emacs_value val, int nvalid, ...);

/**
   Called when an error occured in ZMQ to notify Emacs to exit nonlocally.
*/
extern void
ezmq_signal_error(emacs_env *env);

/**
   Create a new ezmq_obj with type and obj and returns a user-ptr to it. As a
   special case if type is EZMQ_MESSAGE and obj is NULL, allocate the required
   zmq_msg_t object. This is the normal way to create new ZMQ message objects.
   The environment is passed for the case when nno memory can be allocated, an
   error is signaled and this function will return NULL.
*/
extern emacs_value
ezmq_new_obj_ptr(emacs_env *env, ezmq_obj_t *obj);

/**
   Allocate and return an ezmq_obj_t with TYPE and OBJ. Signal a non-local exit
   if anything goes wrong.
*/
extern ezmq_obj_t *
ezmq_new_obj(emacs_env *env, enum ezmq_obj_t type, void *obj);

/**
   Free the memory used to wrap a ZMQ object. Note this only free's the memoery
   used by a ezmq_obj_t not the underlying object it points to. See
   ezmq_obj_finalizer for freeing up the resources used by the ZMQ objects.
*/
extern void
ezmq_free_obj(ezmq_obj_t *obj);

/**
   Extract an ezmq_obj_t from the Lisp object in OBJ. Signal a non-local exit
   if the extracted object doesn't match TYPE.
*/
extern ezmq_obj_t *
ezmq_extract_obj(emacs_env *env, enum ezmq_obj_t type, emacs_value obj);

extern void
ezmq_obj_finalizer(void *);

/**
   Similar to malloc, but signal an error if no memory could be allocated and
   do not attempt to allocate if a non-local exit is pending. In both cases,
   return NULL.
*/
extern char *
ezmq_malloc(emacs_env *env, size_t nbytes);

/**
   Copy and return a pointer to an Emacs string. The caller is responsible for
   freeing the memory returned.
*/
extern char *
ezmq_copy_string(emacs_env *env, emacs_value str, ptrdiff_t *size);

#endif /* __CORE_H__ */
