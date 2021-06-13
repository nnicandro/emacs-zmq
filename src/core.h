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

#ifndef EZMQ_DEBUG
  #define EZMQ_DEBUG 0
#endif

#define ezmq_debug(str, ...) if(EZMQ_DEBUG) fprintf(stderr, str, ##__VA_ARGS__)

/**
   Helper macros to aid in interacting with the Emacs environment. They all
   expect an implicit emacs_env variable named env to be defined in the calling
   environment of the macro.
*/
#define INTERN(val) env->intern(env, (val))

#define FUNCALL(fun, nargs, args) env->funcall(env, (fun), (nargs), (args))

#define CAR(list) FUNCALL(Qcar, 1, &(list))

#define CDR(list) FUNCALL(Qcdr, 1, &(list))

#define INT(i) env->make_integer(env, (i))

// Should only be used when i is guaranteed to be an integer, otherwise use
// EZMQ_EXTRACT_INT
#define EXTRACT_INT(i) env->extract_integer(env, (i))

#define STRING(str, len) env->make_string(env, (str), (len))

#define LIST(count, ...) FUNCALL(Qlist, (count), ((emacs_value []){ __VA_ARGS__ }))

#define CONS(car, cdr) FUNCALL(Qcons, 2, ((emacs_value []){ (car), (cdr) }))

#define TYPE(val) env->type_of(env, (val))

#define USER_PTR(val) env->get_user_ptr(env, (val))

#define USER_FINALIZER(val) env->get_user_finalizer(env, (val))

#define EQ(a, b) env->eq(env, (a), (b))

#define NILP(a) !env->is_not_nil(env, (a))

#define EQUAL(a, b) EQ(FUNCALL(Qequal, 2, ((emacs_value []){ (a), (b) })), Qt)

#define LENGTH(list) env->extract_integer(env, FUNCALL(Qlength, 1, &(list)))

#define AREF(vec, i) env->vec_get(env, (vec), (i))

#define VEC_LENGTH(vec) env->vec_size(env, (vec))

#define GLOBREF(val) env->make_global_ref(env, (val))

#define FREE_GLOBREF(val) env->free_global_ref(env, (val))

#define NONLOCAL_EXIT() (env->non_local_exit_check(env) != emacs_funcall_exit_return)

#define CLEAR_NONLOCAL_EXIT() env->non_local_exit_clear(env)

#define SIGNAL(err, data) env->non_local_exit_signal(env, (err), (data))

/**
   EZMQ_EXTRACT_* macros transform the Lisp object representations to their
   corresponding C representations. If during the extraction an error is
   signaled, they immediately exit the function where the macro was expanded by
   returning NULL.
*/

#define EZMQ_EXTRACT_INT(name, val)                     \
    intmax_t name = env->extract_integer(env, (val));   \
    do {                                                \
        if(NONLOCAL_EXIT()) {                           \
            return NULL;                                \
        }                                               \
    } while(0)

#define EZMQ_EXTRACT_OPTIONAL_INT(name, val)    \
    intmax_t name = EQ((val), Qnil) ? 0 :       \
        env->extract_integer(env, (val));       \
    do {                                        \
        if(NONLOCAL_EXIT()) {                   \
            return NULL;                        \
        }                                       \
    } while(0)

#define EZMQ_EXTRACT_OBJ(name, type, val)             \
    ezmq_obj_t *name = ezmq_extract_obj(type, (val)); \
    if(NONLOCAL_EXIT()) return NULL                   \

#define EZMQ_EXTRACT_OPTIONAL_OBJ(name, type, val)    \
    ezmq_obj_t *name = EQ((val), Qnil) ? NULL :       \
        ezmq_extract_obj(type, (val));                \
    if(NONLOCAL_EXIT()) return NULL                   \

#define EZMQ_EXTRACT_STRING(name, len, val)         \
    ptrdiff_t len = 0;                              \
    char *name = ezmq_copy_string((val), &len);     \
    do {                                            \
        if(NONLOCAL_EXIT()) {                       \
            return NULL;                            \
        }                                           \
    } while(0)

/**
   The EZMQ_CHECK_* macros check that that expression passed as their argument
   evaluates to something other than -1 (EZMQ_CHECK_ERROR) or NULL
   (EZMQ_CHECK_NULL_ERROR). If they do evaluate to -1 or NULL, they signal an
   error. Note, the expressions passed as arguments will not be evaluated if a
   non-local exit is pending already.
*/

#define EZMQ_CHECK_ERROR(expr)                  \
    do {                                        \
        if(NONLOCAL_EXIT()) break;              \
        int __retcode = (expr);                 \
        if(__retcode == -1) {                   \
            ezmq_signal_error();                \
        }                                       \
    } while(0)

#define EZMQ_CHECK_NULL_ERROR(expr)             \
    do {                                        \
        if(NONLOCAL_EXIT()) break;              \
        if((void *)(expr) == NULL) {            \
            ezmq_signal_error();                \
        }                                       \
    } while(0)

#define EZMQ_DOC(name, args, doc)                           \
    const char *__zmq_doc_##name = doc "\n\n(fn " args ")"

#define EZMQ_FUN(name, ...)                  \
    extern const char *__zmq_doc_##name;     \
    extern emacs_value name(__VA_ARGS__)

enum ezmq_obj_t {
    EZMQ_CONTEXT = 0xe178c286,
    EZMQ_MESSAGE = 0xe178c286 + 2,
    EZMQ_SOCKET = 0xe178c286 + 4,
    EZMQ_POLLER = 0xe178c286 + 8
};

typedef struct {
    enum ezmq_obj_t type;
    void *obj;
    // Extra object that can be stored, use ezmq_obj_get_val() and
    // ezmq_obj_set_val() to access it.
    emacs_value val;
} ezmq_obj_t;

// Set to the current Emacs environment, see ezmq_dispatch
extern emacs_env *env;

extern emacs_value Qzmq_error, Qt, Qnil, Qnth, Qlist,
    Qwrong_type_argument, Qargs_out_of_range,
    Qcons, Qstring, Qvector, Qcar, Qcdr, Qlength, Qinteger, Qequal,
    Qzmq_POLLIN, Qzmq_POLLPRI, Qzmq_POLLERR, Qzmq_POLLOUT,
    Izmq_POLLIN, Izmq_POLLPRI, Izmq_POLLERR, Izmq_POLLOUT;

/**
   Signal an Emacs error.
   ERR is the error symbol, the remaining arguments are used as the error data.
*/
extern void
ezmq_signal(emacs_value err, int nargs, ...);

/**
   Signal an Emacs `wrong-type-argument` error.

   VAL is the object with a wrong type, the remaining arguments should be the
   types that VAL should take.
*/
extern void
ezmq_wrong_type_argument(emacs_value val, int nvalid, ...);

/**
   Signal an Emacs `args-out-of-range` error.
   VAL is the object whose range is out of bounds. RANGE is an object
   describing the bounds that are valid.
 */
extern void
ezmq_args_out_of_range(emacs_value val, emacs_value range);

/**
   Signal an error created from the current value of zmq_errno().
*/
extern void
ezmq_signal_error();

/**
   Create a new ezmq_obj with type and obj and returns a user-ptr to it. As a
   special case if type is EZMQ_MESSAGE and obj is NULL, allocate the required
   zmq_msg_t object. This is the normal way to create new ZMQ message objects.
   The environment is passed for the case when nno memory can be allocated, an
   error is signaled and this function will return NULL.
*/
extern emacs_value
ezmq_new_obj_ptr(ezmq_obj_t *obj);

/**
   Allocate and return an ezmq_obj_t with TYPE and OBJ. Signal a non-local exit
   if anything goes wrong.
*/
extern ezmq_obj_t *
ezmq_new_obj(enum ezmq_obj_t type, void *obj);

/**
   Free the memory used to wrap a ZMQ object. Note this only free's the memoery
   used by a ezmq_obj_t not the underlying object it points to. See
   ezmq_obj_finalizer for freeing up the resources used by the ZMQ objects.
*/
extern void
ezmq_free_obj(ezmq_obj_t *obj);

/**
   Set the VAL field of OBJ.
   Un-reference any current value stored in OBJ, make a global reference to VAL
   and store in OBJ->val.
*/
extern void
ezmq_obj_set_val(ezmq_obj_t *obj, emacs_value val);

/**
   Return the VAL field of OBJ.
   If OBJ's VAL field is empty, return Qnil.
*/
emacs_value
ezmq_obj_get_val(ezmq_obj_t *obj);

/**
   Add OBJ->val to a stack of values to call FREE_GLOBREF() on next dispatch.
   Do this only if OBJ->val is not NULL. The global references are also cleaned
   up after every garbage collection in Emacs.
*/
extern void
ezmq_push_globref(ezmq_obj_t *obj);

/**
   Remove a value from the stack of values pushed to by ezmq_push_globref()
*/
extern emacs_value
ezmq_pop_globref();

/**
   Return 1 if Emacs VAL is a ZMQ object with TYPE, 0 otherwise.
*/
extern int
ezmq_obj_of_type(emacs_value val, enum ezmq_obj_t type);

/**
   Extract an ezmq_obj_t from the Lisp object in OBJ. Signal a non-local exit
   if the extracted object doesn't match TYPE.
*/
extern ezmq_obj_t *
ezmq_extract_obj(enum ezmq_obj_t type, emacs_value obj);

/**
   Cleanup resources of a ezmq_obj_t. Meant to be added as the object finalizer
   when wrapping ezmq_obj_t objects as a user_ptr.
*/
extern void
ezmq_obj_finalizer(void *);

/**
   Similar to malloc, but signal an error if no memory could be allocated and
   do not attempt to allocate if a non-local exit is pending. In both cases,
   return NULL.
*/
extern void *
ezmq_malloc(size_t nbytes);

/**
   Copy and return a pointer to an Emacs string. The caller is responsible for
   freeing the memory returned.
*/
extern char *
ezmq_copy_string(emacs_value str, ptrdiff_t *size);

#endif /* __CORE_H__ */
