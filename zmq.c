#include "emacs-module.h"
#include <string.h>
#include <stdlib.h>
#include <zmq.h>

// TODO: Expose constants to Emacs during initialization, only expose the most
// common ones for now. I want to only require compiling the module file and
// not try to extract constants from header files.

// TODO: The goal for now is to get rid of the FFI dependency, so write the
// bare minimum to remove all of the FFI stuff.

#define ZMQ_NONLOCAL_EXIT(env) (env->non_local_exit_check(env) != emacs_funcall_exit_return)

#define INTERN(env, val) (env->intern(env, (val)))

#define ZMQ_EXTRACT_INT(env, name, val)             \
    intmax_t name = env->extract_integer(env, val); \
    do {                                            \
        if(ZMQ_NONLOCAL_EXIT(env)) {                \
            return NULL;                            \
        }                                           \
    } while(0)

#define ZMQ_EXTRACT_OBJ(env, name, val)          \
    zmq_obj *name = env->get_user_ptr(env, val); \
    do {                                         \
        if(ZMQ_NONLOCAL_EXIT(env)) {             \
            return NULL;                         \
        }                                        \
    } while(0)

#define ZMQ_VALIDATE_OBJ(env, obj, type)                  \
    do {                                                  \
        if(!zmq_valid_object(obj, type)) {                \
            zmq_signal_wrong_object_type(env, obj, type); \
            return NULL;                                  \
        }                                                 \
    } while(0)

#define ZMQ_CHECK_ERROR(env, expr) \
    do {                           \
        int retcode = expr;        \
        if(retcode == -1) {        \
            zmq_signal_error(env); \
        }                          \
    } while(0)

typedef enum {
    ZMQ_CONTEXT,
    ZMQ_MESSAGE,
    ZMQ_SOCKET,
    ZMQ_POLLER
} zmq_obj_t;

typedef struct {
    zmq_obj_t type;
    void *obj;
} zmq_obj;

int plugin_is_GPL_compatible;
static emacs_value Qzmq_error, Qt, Qnil, Qlist, Qwrong_type_argument, Qcons,
    Qstring, Qvector;

static zmq_obj *
zmq_new_object(zmq_obj_t type, void *data)
{
    zmq_obj *obj = (zmq_obj *)malloc(sizeof(*obj));
    obj->type = type;
    obj->obj = data;
    return obj;
}

/*
  Return true if OBJ is of type TYPE. Otherwise signal wrong-type-argument and
  return false.
*/
static inline bool
zmq_valid_object(emacs_env *env, zmq_obj *obj, zmq_obj_t type)
{
    return obj->type == type;
}

static emacs_value
zmq_type_symbol(emacs_env *env, zmq_obj_t type)
{
    switch(type) {
    case ZMQ_CONTEXT:
        return INTERN(env, "zmq-context");
    case ZMQ_SOCKET:
        return INTERN(env, "zmq-socket");
    case ZMQ_MESSAGE:
        return INTERN(env, "zmq-message");
    case ZMQ_POLLER:
        return INTERN(env, "zmq-poller");
    default:
        return Qnil;
    }
}

static void
zmq_signal_wrong_object_type(emacs_env *env, zmq_obj *obj, zmq_obj_t expected)
{
    emacs_value args[] = { zmq_type_symbol(env, expected),
                           zmq_type_symbol(env, obj->type) };
    emacs_value data = env->funcall(env, Qlist, 2, args);
    env->non_local_exit_signal(env, Qwrong_type_argument, data);
}

static char *
zmq_copy_string(emacs_value str)
{
    ptrdiff_t size = 0;
    if(!env->copy_string_contents(env, str, NULL, &size)) {
        return NULL;
    }
    char *buf = malloc(size);
    env->copy_string_contents(env, str, buf, &size);
    return buf;
}

static inline emacs_value
zmq_make_string(emacs_env *env, const char *str)
{
    return env->make_string(env, str, strlen(str));
}

static emacs_value
Fzmq_has(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    char *capability = zmq_copy_string(args[0]);
    if(capability) {
        emacs_value retval = zmq_has(capability) ? Qt : Qnil;
        free(capability);
        return retval;
    } else {
        return NULL;
    }
}

static emacs_value
Fzmq_version(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    int major, minor, patch;
    char s[16];
    zmq_version(&major, &minor, &patch);
    sprintf(s, "%d.%d.%d", major, minor, patch);
    return zmq_make_string(env, s);
}

static void
zmq_signal_error(emacs_env *env)
{
    // TODO: Define error symbols for common errors and use Qzmq_error as a
    // catch all. Look at the zmq documentation for all of the errors that can
    // occur.
    int en = zmq_errno();
    switch(en) {
    case EINTR:
        env->non_local_exit_signal(env, INTERN(env, "zmq-EINTR"), Qnil);
        break;
    default: {
        emacs_value args[] = { zmq_make_string(env, zmq_strerror(zmq_errno())) };
        emacs_value data = env->funcall(env, Qlist, 1, args);
        env->non_local_exit_signal(env, Qzmq_error, data);
    }
    }
}

/// Contexts

static emacs_value
Fzmq_context(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    zmq_obj *ctx = zmq_new_object(ZMQ_CONTEXT, zmq_ctx_new());
    return env->make_user_ptr(env, NULL, ctx);
}

static emacs_value
Fzmq_ctx_set (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, context, args[0]);
    ZMQ_VALIDATE_OBJ(env, context, ZMQ_CONTEXT);
    ZMQ_EXTRACT_INT(env, option, args[1]);
    ZMQ_EXTRACT_INT(env, option, args[2]);
    ZMQ_CHECK_ERROR(env, zmq_ctx_set(context->obj, option, value));
    return Qnil;
}

static emacs_value
Fzmq_ctx_get(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, context, args[0]);
    ZMQ_VALIDATE_OBJ(env, context, ZMQ_CONTEXT);
    ZMQ_EXTRACT_INT(env, option, args[1]);
    ZMQ_CHECK_ERROR(env, zmq_ctx_get(context->obj, option));
    return env->make_integer(env, ret);
}

static emacs_value
Fzmq_ctx_term(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, context, args[0]);
    ZMQ_VALIDATE_OBJ(env, context, ZMQ_CONTEXT);
    ZMQ_CHECK_ERROR(env, zmq_ctx_term(context->obj));
    return Qnil;
}

static emacs_value
Fzmq_ctx_shutdown(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, context, args[0]);
    ZMQ_VALIDATE_OBJ(env, context, ZMQ_CONTEXT);
    ZMQ_CHECK_ERROR(env, zmq_ctx_shutdown(context->obj));
    return Qnil;
}

/// Encryption

static emacs_value
Fzmq_z85_decode(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    char *key = zmq_copy_string(args[0]);
    if(!key) return NULL;

    // copy_string_contents includes the null byte in size, hence we need to
    // use (size - 1) for the length of key
    if((size - 1) % 5 != 0) {
        emacs_value largs[] = { zmq_make_string(env, "Length not a multiple of 5") };
        emacs_value data = env->funcall(env, Qlist, 1, largs);
        env->non_local_exit_signal(env, INTERN(env, "args-out-of-range"), data);
        return NULL;
    }

    ptrdiff_t len = 0.8*(size - 1);
    uint8_t *decoded = malloc(len + 1);
    emacs_value retval = Qnil;

    decoded[len] = 0;
    if(decoded == zmq_z85_decode(decoded, key)) {
        retval = zmq_make_string(env, decoded);
    }

    // TODO: Signal error when fail
    free(key);
    free(decoded);
    return retval;
}

static emacs_value
Fzmq_z85_encode(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    char *content = zmq_copy_string(args[0]);
    if(!content) return NULL;

    // copy_string_contents includes the null byte in size, hence we need to
    // use (size - 1) for the length of key
    if((size - 1) % 4 != 0) {
        emacs_value args[] = { zmq_make_string(env, "Length not a multiple of 4") };
        emacs_value data = env->funcall(env, Qlist, 1, args);
        env->non_local_exit_signal(env, INTERN(env, "args-out-of-range"), data);
        return NULL;
    }

    ptrdiff_t len = 1.25*(size - 1);
    char *encoded = malloc(len + 1);
    emacs_value retval = Qnil;

    encoded[len] = 0;
    if(encoded == zmq_z85_encode(encoded, (const uint8_t *)content, len)) {
        retval = zmq_make_string(env, encoded);
    }

    free(content);
    free(encoded);
    return retval;
}

static emacs_value
Fzmq_curve_keypair(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    if(!zmq_has("curve")) {
        emacs_value args[] = { zmq_make_string(env, "ZMQ not built with CURVE security") };
        emacs_value data = env->funcall(env, Qlist, 1, args);
        env->non_local_exit_signal(env, Qzmq_error, data);
        return NULL;
    }

    char *public = malloc(41);
    char *private = malloc(41);
    zmq_curve_keypair(public, private);
    emacs_value keys[] = {
        zmq_make_string(env, public),
        zmq_make_string(env, private),
    };
    return env->funcall(env, Qcons, 2, keys);
}

static emacs_value
Fzmq_curve_public(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    if(!zmq_has("curve")) {
        emacs_value args[] = { zmq_make_string(env, "ZMQ not built with CURVE security") };
        emacs_value data = env->funcall(env, Qlist, 1, args);
        env->non_local_exit_signal(env, Qzmq_error, data);
        return NULL;
    }

    char *private = zmq_copy_string(args[0]);
    char *public = malloc(41);
    zmq_curve_public(public, private);
    return zmq_make_string(env, public);
}

/// Messages

static void
zmq_free_message(void *data, void *hint)
{
    free(data);
}

// TODO: From the documentation of zmq_msg_init: "never initialize the same
// message twice", I think I do this somewhere in zmq-ffi.el or in jupyter.el,
// find where.
static emacs_value
Fzmq_message(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    zmq_msg_t *msg = malloc(sizeof(*msg));
    emacs_value type = env->type_of(env, args[0]);

    if(env->eq(env, type, Qstring)) {

        char *content = zmq_copy_string(args[0]);
        ZMQ_CHECK_ERROR(env, zmq_msg_init_data(msg,
                                               content,
                                               strlen(content),
                                               &zmq_free_message,
                                               NULL));
    } else if(env->eq(env, type, Qvector)) {

        ptrdiff_t size = env->vec_size(env, args[0]);
        char *content = malloc(size);
        for(ptrdiff_t i = 0; i < size; i++) {
            content[i] = env->vec_get(env, args[0], i);
        }

        ZMQ_CHECK_ERROR(env, zmq_msg_init_data(msg,
                                               content,
                                               size,
                                               &zmq_free_message,
                                               NULL));
    } else if(env->eq(env, args[0], Qnil)) {

        ZMQ_CHECK_ERROR(env, zmq_msg_init(msg));

    } else {

        emacs_value args[] = { zmq_make_string(env, "Invalid message initializer"),
                               args[0] };
        emacs_value data = env->funcall(env, Qlist, 2, args);
        env->non_local_exit_signal(env, Qwrong_type_argument, data);
        return NULL;

    }

    return env->make_user_ptr(env, NULL, zmq_new_object(ZMQ_MESSAGE, msg));
}

static emacs_value
Fzmq_message_size(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    return env->make_integer(env, zmq_msg_size(msg->obj));
}

static emacs_value
Fzmq_message_data(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    const char *content = zmq_msg_data(msg->obj);
    if(content) {
        size_t size = zmq_msg_size(msg->obj);
        char *buf = malloc(size + 1);
        emacs_value retval;

        buf[size] = 0;
        memcpy(buf, zmq_msg_data(msg->obj), size);
        retval = env->make_string(env, buf, size);
        free(buf);
        return retval;
    } else {
        return Qnil;
    }
}

static emacs_value
Fzmq_message_more(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    int retval = zmq_msg_more(msg->obj);
    if(retval == -1) {
        zmq_signal_error(env);
        return NULL;
    }
    return retval ? Qt : Qnil;
}

static emacs_value
Fzmq_message_copy(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    zmq_obj *dest = zmq_new_object(ZMQ_MESSAGE, malloc(sizeof(zmq_msg_t)));
    ZMQ_CHECK_ERROR(env, zmq_msg_copy(dest->obj, msg->obj));
    return env->make_user_ptr(env, NULL, dest);
}

static emacs_value
Fzmq_message_move(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    zmq_obj *dest = zmq_new_object(ZMQ_MESSAGE, malloc(sizeof(zmq_msg_t)));
    ZMQ_CHECK_ERROR(env, zmq_msg_move(dest->obj, msg->obj));
    return env->make_user_ptr(env, NULL, dest);
}

static emacs_value
Fzmq_message_close(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    ZMQ_CHECK_ERROR(env, zmq_msg_close(msg->obj));
    free(msg->obj);
    return Qnil;
}

static emacs_value
Fzmq_message_set(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    ZMQ_EXTRACT_INT(env, property, args[1]);

    if(property == ZMQ_MORE) {
        ZMQ_CHECK_ERROR(env,
                        zmq_msg_set(msg->obj,
                                    property,
                                    env->is_not_nil(env, args[2])));
    } else {
        ZMQ_EXTRACT_INT(env, value, args[2]);
        ZMQ_CHECK_ERROR(env, zmq_msg_set(msg->obj, property, value));
    }
    return Qnil;
}

static emacs_value
Fzmq_message_get(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    ZMQ_EXTRACT_INT(env, property, args[1]);

    int retval = zmq_msg_get(msg->obj, property);
    if(retval == -1) {
        zmq_signal_error(env);
        return NULL;
    }
    if(property == ZMQ_MORE) {
        return retval ? Qt : Qnil;
    } else {
        return env->make_integer(env, retval);
    }
}

static emacs_value
Fzmq_message_gets(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    char *propery =  zmq_copy_string(env, args[1]);
    const char *retval = zmq_msg_gets(msg->obj, property);
    if(retval == NULL) {
        zmq_signal_error(env);
        return NULL;
    }
    free(property);
    return zmq_make_string(env, retval);
}

static emacs_value
Fzmq_message_routing_id(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    return env->make_integer(env, zmq_msg_routing_id(msg->obj));
}

static emacs_value
Fzmq_message_set_routing_id(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_EXTRACT_OBJ(env, msg, args[0]);
    ZMQ_VALIDATE_OBJ(env, msg, ZMQ_MESSAGE);
    ZMQ_EXTRACT_INT(env, id, args[1]);
    ZMQ_CHECK_ERROR(env, zmq_msg_set_routing_id(msg->obj, id));
    return Qnil;
}

static void
bind_function(emacs_env *env, const char *name, emacs_value Sfun)
{
    emacs_value Qfset = INTERN(env, "fset");
    emacs_value Qsym = INTERN(env, name);
    /* Prepare the arguments array */
    emacs_value args[] = {Qsym, Sfun};
    /* Make the call (2 == nb of arguments) */
    env->funcall(env, Qfset, 2, args);
}

static void
provide(emacs_env *env, const char *feature)
{
    /* call 'provide' with FEATURE converted to a symbol */
    emacs_value Qfeat = INTERN(env, feature);
    emacs_value Qprovide = INTERN(env, "provide");
    emacs_value args[] = { Qfeat };
    env->funcall (env, Qprovide, 1, args);
}

static void
zmq_make_error_symbols(emacs_env *env)
{
    emacs_value def_err = INTERN(env, "define-error");
    char buf[BUFSIZ];
    emacs_value args[3];

    // Define the root error symbol for ZMQ errors
    args[0] = Qzmq_error;
    strcpy(buf, "An error occured in ZMQ");
    args[1] = zmq_make_string(env, buf);
    args[2] = INTERN(env, "error");
    env->funcall(env, def_err, 3, args);

    // Define common errors as symbols
    // Also see zmq_signal_error
    const char *msg;
    args[2] = Qzmq_error;

    args[0] = INTERN(env, "zmq-EINTR");
    msg = zmq_strerror(EINTR);
    args[1] = zmq_make_string(env, msg);
    env->funcall(env, def_err, 3, args);
}

extern int
emacs_module_init(struct emacs_runtime *ert)
{
    // Retrieve the current emacs environment
    emacs_env *env = ert->get_environment(ert);
    emacs_value fun;

    Qt = INTERN(env, "t");
    Qnil = INTERN(env, "nil");
    Qzmq_error = env->make_global_ref(env, INTERN(env, "zmq-ERROR"));
    Qwrong_type_argument = INTERN(env, "wrong-type-argument");
    Qlist = INTERN(env, "list");
    Qstring = INTERN(env, "string");
    Qvector = INTERN(env, "vector");
    Qcons = INTERN(env, "cons");

    zmq_make_error_symbols(env);

    fun = env->make_function(env,
                             0,
                             0,
                             &Fzmq_version,
                             "Get the version of ZMQ.",
                             NULL);
    bind_function(env, "zmq-version2", fun);

    fun = env->make_function(env,
                             1,
                             1,
                             &Fzmq_has,
                             "Determine if FEAT is available.\n"
                             "\\(fn FEAT)",
                             NULL);
    bind_function(env, "zmq-has2", fun);

    provide(env, "zmq2");
    return 0;
}
