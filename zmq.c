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

#define INTERN(env, val) ((env)->intern((env), (val)))

#define ZMQ_UNWRAP_INT(name, env, val)              \
    intmax_t name = env->extract_integer(env, val); \
    if(ZMQ_NONLOCAL_EXIT(env)) {                    \
        return NULL;                                \
    }

#define ZMQ_UNWRAP_OBJ(name, env, val)              \
    zmq_obj *name = env->get_user_ptr(env, val);    \
    if(ZMQ_NONLOCAL_EXIT(env)) {                    \
        return NULL;                                \
    }

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
static emacs_value Qzmq_error, Qt, Qnil, Qlist, Qwrong_type_argument, Qcons;

static zmq_obj *
zmq_new_object(zmq_obj_t type, void *data)
{
    zmq_obj *obj = malloc(sizeof(*obj));
    obj->type = type;
    obj->obj = data;
    return obj;
}

/*
  Return true if OBJ is of type TYPE. Otherwise signal wrong-type-argument and
  return false.
 */
static bool
zmq_valid_obj(emacs_env *env, zmq_obj *obj, zmq_obj_t type)
{
    if(obj->type != type) {
        emacs_value data[1];
        switch(type) {
        case ZMQ_CONTEXT:
            data[0] = INTERN(env, "zmq-context");
            break;
        case ZMQ_SOCKET:
            data[0] = INTERN(env, "zmq-socket");
            break;
        case ZMQ_MESSAGE:
            data[0] = INTERN(env, "zmq-message");
            break;
        case ZMQ_POLLER:
            data[0] = INTERN(env, "zmq-poller");
            break;
        default:
            data[0] = Qnil;
        }
        env->non_local_exit_signal(env, Qwrong_type_argument,
                                   env->funcall(env, Qlist, 1, data));
        return false;
    }
    return true;
}


static emacs_value
Fzmq_has(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ptrdiff_t size = 0;
    // NOTE: env->make_function will never call a function with an improper
    // number of arguments so no need to check the number of arguments here.
    if(!env->copy_string_contents(env, args[0], NULL, &size)) {
        return NULL;
    }
    char *feat = malloc(size);
    env->copy_string_contents(env, args[0], feat, &size);
    return zmq_has(feat) ? Qt : Qnil;
}

static emacs_value
Fzmq_version(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    int major, minor, patch;
    char s[16];
    zmq_version(&major, &minor, &patch);
    sprintf(s, "%d.%d.%d", major, minor, patch);
    return env->make_string(env, s, strlen(s));
}

static void
zmq_signal_error(emacs_env *env)
{
    // TODO: Define error symbols for common errors and use Qzmq_error as a
    // catch all.
    int en = zmq_errno();
    emacs_value data;
    switch(en) {
    case EINTR:
        env->non_local_exit_signal(env, INTERN(env, "zmq-EINTR"), Qnil);
        break;
    default: {
        const char *_msg = zmq_strerror(zmq_errno());
        emacs_value msg = env->make_string(env, _msg, strlen(_msg));
        data = env->funcall(env, Qlist, 1, (emacs_value []){ msg });
        env->non_local_exit_signal(env, Qzmq_error, msg);
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
Fzmq_ctx_set(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_UNWRAP_OBJ(context, env, args[0])
    ZMQ_UNWRAP_INT(option, env, args[1])
    ZMQ_UNWRAP_INT(value, env, args[2])

    if(zmq_ctx_set(context->obj, option, value) == -1) {
        zmq_signal_error(env);
        return NULL;
    } else {
        return Qnil;
    }
}

static emacs_value
Fzmq_ctx_get(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_UNWRAP_OBJ(context, env, args[0])
    ZMQ_UNWRAP_INT(option, env, args[1])

    int ret = zmq_ctx_get(context->obj, option);
    if(ret == -1) {
        zmq_signal_error(env);
        return NULL;
    } else {
        return env->make_integer(env, ret);
    }
}

static emacs_value
Fzmq_ctx_term(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_UNWRAP_OBJ(context, env, args[0])

    if(zmq_ctx_term(context->obj) == -1) {
        zmq_signal_error(env);
        return NULL;
    } else {
        return Qnil;
    }
}

static emacs_value
Fzmq_ctx_shutdown(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ZMQ_UNWRAP_OBJ(context, env, args[0])

    if(zmq_ctx_shutdown(context->obj) == -1) {
        zmq_signal_error(env);
        return NULL;
    } else {
        return Qnil;
    }
}

/// Encryption

static emacs_value
Fzmq_z85_decode(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ptrdiff_t size = 0;
    if(!env->copy_string_contents(env, args[0], NULL, &size)) {
        return NULL;
    }

    char *key = malloc(size);
    env->copy_string_contents(env, args[0], key, &size);

    // copy_string_contents includes the null byte in size, hence we need to
    // use (size - 1) for the length of key
    if((size - 1) % 5 != 0) {
        const char _msg[] = "Length not a multiple of 5";
        emacs_value msg = env->make_string(env, _msg, sizeof(_msg));
        emacs_value data = env->funcall(env, Qlist, 1, (emacs_value []){ msg });
        env->non_local_exit_signal(env, INTERN(env, "args-out-of-range"), data);
        return NULL;
    }

    ptrdiff_t len = 0.8*(size - 1);
    uint8_t *decoded = malloc(len + 1);
    decoded[len] = 0;
    if(decoded == zmq_z85_decode(decoded, key)) {
        return env->make_string(env, (const char *)decoded, len);
    } else {
        // TODO: Signal an error
        return Qnil;
    }
}

static emacs_value
Fzmq_z85_encode(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ptrdiff_t size = 0;
    if(!env->copy_string_contents(env, args[0], NULL, &size)) {
        return NULL;
    }

    char *content = malloc(size);
    env->copy_string_contents(env, args[0], content, &size);

    // copy_string_contents includes the null byte in size, hence we need to
    // use (size - 1) for the length of key
    if((size - 1) % 4 != 0) {
        const char _msg[] = "Length not a multiple of 4";
        emacs_value msg = env->make_string(env, _msg, sizeof(_msg));
        emacs_value data = env->funcall(env, Qlist, 1, (emacs_value []){ msg });
        env->non_local_exit_signal(env, INTERN(env, "args-out-of-range"), data);
        return NULL;
    }

    ptrdiff_t len = 1.25*(size - 1);
    char *encoded = malloc(len + 1);
    encoded[len] = 0;
    if(encoded == zmq_z85_encode(encoded, (const uint8_t *)content, len)) {
        return env->make_string(env, (const char *)encoded, len);
    } else {
        // TODO: Signal an error
        return Qnil;
    }
}

static emacs_value
Fzmq_curve_keypair(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    if(!zmq_has("curve")) {
        const char _msg[] = "ZMQ not built with CURVE security";
        emacs_value msg = env->make_string(env, _msg, sizeof(_msg));
        emacs_value data = env->funcall(env, Qlist, 1, (emacs_value []){ msg });
        env->non_local_exit_signal(env, Qzmq_error, data);
        return NULL;
    }

    char *public = malloc(41);
    char *private = malloc(41);
    zmq_curve_keypair(public, private);

    emacs_value keys[] = {
        env->make_string(env, public, 41),
        env->make_string(env, private, 41)
    };
    return env->funcall(env, Qcons, 2, keys);
}

static emacs_value
Fzmq_curve_public(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    if(!zmq_has("curve")) {
        const char _msg[] = "ZMQ not built with CURVE security";
        emacs_value msg = env->make_string(env, _msg, sizeof(_msg));
        emacs_value data = env->funcall(env, Qlist, 1, (emacs_value []){ msg });
        env->non_local_exit_signal(env, Qzmq_error, data);
        return NULL;
    }

    ptrdiff_t size = 0;
    if(!env->copy_string_contents(env, args[0], NULL, &size)) {
        return NULL;
    }
    char *public = malloc(41);
    char *private = malloc(size);

    env->copy_string_contents(env, args[0], private, &size);
    zmq_curve_public(public, private);

    return env->make_string(env, public, 41);
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
provide (emacs_env *env, const char *feature)
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
    args[1] = env->make_string(env, buf, strlen(buf));
    args[2] = INTERN(env, "error");
    env->funcall(env, def_err, 3, args);

    // Define common errors as symbols
    // Also see zmq_signal_error
    const char *msg;
    args[2] = Qzmq_error;

    args[0] = INTERN(env, "zmq-EINTR");
    msg = zmq_strerror(EINTR);
    args[1] = env->make_string(env, msg, strlen(msg));
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
