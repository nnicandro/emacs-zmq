#include "ezmq.h"

// TODO: Expose constants to Emacs during initialization, only expose the most
// common ones for now. I want to only require compiling the module file and
// not try to extract constants from header files.

// TODO: The goal for now is to get rid of the FFI dependency, so write the
// bare minimum to remove all of the FFI stuff.

int plugin_is_GPL_compatible;

void
ezmq_signal_error(emacs_env *env)
{

    // TODO: Define error symbols for common errors and use Qzmq_error as a
    // catch all. Look at the zmq documentation for all of the errors that can
    // occur.
    int en = zmq_errno();
    switch(en) {
    case EINTR:
        ezmq_error(env, INTERN(env, "zmq-EINTR"), NULL);
        break;
    default: {
        const char *msg = zmq_strerror(zmq_errno());
        ezmq_error(env, Qzmq_error, msg);
    }
    }
}

// TODO: Make different error functions for specific purposes
void
ezmq_error(emacs_env *env, emacs_value err, const char *msg)
{
    emacs_value data = Qnil;
    if(msg) {
        emacs_value args[] = { ezmq_make_string(env, msg, strlen(msg)) };
        data = env->funcall(env, Qlist, 1, args);
    }
    env->non_local_exit_signal(env, err, data);
}

char *
ezmq_malloc(emacs_env *env, size_t nbytes)
{
    char *buf = malloc(nbytes);
    if(!buf) ezmq_signal_error(env);
    return buf;
}

static emacs_value
ezmq_type_symbol(emacs_env *env, enum ezmq_obj_t type)
{
    switch(type) {
    case EZMQ_CONTEXT:
        return INTERN(env, "zmq-context");
    case EZMQ_SOCKET:
        return INTERN(env, "zmq-socket");
    case EZMQ_MESSAGE:
        return INTERN(env, "zmq-message");
    case EZMQ_POLLER:
        return INTERN(env, "zmq-poller");
    default:
        return Qnil;
    }
}

static void
ezmq_wrong_object_type(emacs_env *env, ezmq_obj_t *obj, enum ezmq_obj_t expected)
{
    emacs_value args[] = { ezmq_type_symbol(env, expected),
                           ezmq_type_symbol(env, obj->type) };
    emacs_value data = env->funcall(env, Qlist, 2, args);
    env->non_local_exit_signal(env, Qwrong_type_argument, data);
}

void
ezmq_wrong_type_argument(emacs_env *env, emacs_value val, int nvalid, ...)
{
    va_list args;
    emacs_value options[nvalid + 1];
    options[0] = INTERN(env, "or");
    va_start(args, nvalid);
    for(int i = 0; i < nvalid; i++) {
        options[i] = va_arg(args, emacs_value);
    }
    emacs_value options_list = env->funcall(env, Qlist, nvalid + 1, options);
    emacs_value data = env->funcall(env, Qlist, 2,
                                    (emacs_value []){ val, options_list });
    env->non_local_exit_signal(env, Qwrong_type_argument, data);
}

ezmq_obj_t *
ezmq_extract_obj(emacs_env *env, enum ezmq_obj_t type, emacs_value val)
{
    ezmq_obj_t *obj = env->get_user_ptr(env, val);
    if(EZMQ_NONLOCAL_EXIT(env)) return NULL;
    if(obj->type != type)
        ezmq_wrong_object_type(env, obj, type);
    return EZMQ_NONLOCAL_EXIT(env) ? NULL : obj;
}

ezmq_obj_t *
ezmq_new_obj(emacs_env *env, enum ezmq_obj_t type, void *obj)
{
    ezmq_obj_t *eobj = (ezmq_obj_t *)ezmq_malloc(env, sizeof(*eobj));
    if(!eobj) return NULL;
    eobj->type = type;
    // Special case
    if(type == EZMQ_MESSAGE && !obj) {
        obj = (zmq_msg_t *)ezmq_malloc(env, sizeof(zmq_msg_t));
        if(!obj) {
            free(eobj);
            return NULL;
        }
    }
    eobj->obj = obj;
    return eobj;
}

emacs_value
ezmq_new_obj_ptr(emacs_env *env, enum ezmq_obj_t type, void *obj)
{
    ezmq_obj_t *eobj = ezmq_new_obj(env, type, obj);
    return eobj ? env->make_user_ptr(env, NULL, eobj) : NULL;
}

void
ezmq_free_obj(ezmq_obj_t *obj)
{
    if(obj->type == EZMQ_MESSAGE) {
        free(obj->obj);
    }
    free(obj);
}

char *
ezmq_copy_string(emacs_env *env, emacs_value str, ptrdiff_t *size)
{
    ptrdiff_t sz = 0;
    if(size != NULL) *size = sz;
    if(!env->copy_string_contents(env, str, NULL, &sz)) return NULL;
    char *buf = ezmq_malloc(env, sz);
    if(!buf) return NULL;
    env->copy_string_contents(env, str, buf, &sz);
    if(size != NULL) *size = sz;
    return buf;
}

inline emacs_value
ezmq_make_string(emacs_env *env, const char *str, size_t len)
{
    return env->make_string(env, str, len);
}

bool
ezmq_value_of_type(emacs_env *env, emacs_value val, emacs_value sym)
{
    return env->eq(env, env->type_of(env, val), sym);
}

EZMQ_DOC(zmq_has, "Return non-nil if ZMQ has CAPABILITY.", "CAPABILITY");
static emacs_value
Fzmq_has(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    char *capability = ezmq_copy_string(env, args[0], NULL);
    if(capability) {
        emacs_value retval = zmq_has(capability) ? Qt : Qnil;
        free(capability);
        return retval;
    } else {
        return NULL;
    }
}

EZMQ_DOC(zmq_version, "Return the currently installed version of ZMQ.", "");
static emacs_value
Fzmq_version(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    int major, minor, patch;
    char buf[16];
    zmq_version(&major, &minor, &patch);
    sprintf(buf, "%d.%d.%d", major, minor, patch);
    return ezmq_make_string(env, buf, strlen(buf));
}

/// Encryption

emacs_value
Fzmq_z85_decode(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ptrdiff_t klen;
    char *key = ezmq_copy_string(env, args[0], &klen);
    if(!key) return NULL;

    // klen includes the terminating NULL byte, hence we use klen - 1 to get
    // the length of the string
    if((klen - 1) % 5 != 0) {
        free(key);
        char *msg =  "Length not a multiple of 5";
        emacs_value largs[] = { ezmq_make_string(env, msg, strlen(msg)),
                                env->make_integer(env, klen - 1) };
        emacs_value data = env->funcall(env, Qlist, 2, largs);
        env->non_local_exit_signal(env, INTERN(env, "args-out-of-range"), data);
        return NULL;
    }

    ptrdiff_t dlen = (ptrdiff_t)(0.8*(klen - 1));
    char *decoded = ezmq_malloc(env, dlen + 1);
    if(!decoded) {
        free(key);
        return NULL;
    }
    decoded[dlen] = 0;

    emacs_value retval = Qnil;
    if(zmq_z85_decode((uint8_t *)decoded, key))
        retval = ezmq_make_string(env, decoded, dlen);
    free(key);
    free(decoded);
    return retval;
}

void
ezmq_signal(emacs_env *env, emacs_value err, int nargs, ...)
{
    va_list args;
    emacs_value data[nargs];
    va_start(args, nargs);
    for(int i = 0; i < nargs; i++) {
        data[i] = va_arg(args, emacs_value);
    }
    env->non_local_exit_signal(env, err, env->funcall(env, Qlist, nargs, data));
}

emacs_value
Fzmq_z85_encode(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    ptrdiff_t clen;
    char *content = ezmq_copy_string(env, args[0], &clen);
    if(!content) return NULL;

    if((clen - 1) % 4 != 0) {
        // TODO: Generalize these errors
        ezmq_error(env, INTERN(env, "args-out-of-range"),
                   "Length not a multiple of 4");
        free(content);
        return NULL;
    }

    ptrdiff_t elen = (ptrdiff_t)(1.25*(clen - 1));
    char *encoded = ezmq_malloc(env, elen + 1);
    if(!encoded) {
        free(content);
        return NULL;
    }
    encoded[elen] = 0;

    emacs_value retval = Qnil;
    if(zmq_z85_encode(encoded, (uint8_t *)content, elen))
        retval = ezmq_make_string(env, encoded, elen);
    free(content);
    free(encoded);
    return retval;
}

emacs_value
Fzmq_curve_keypair(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    if(!zmq_has("curve")) {
        ezmq_error(env, Qzmq_error, "ZMQ not built with CURVE security");
        return NULL;
    }

    char *public = ezmq_malloc(env, 41);
    if(!public) return NULL;
    char *private = ezmq_malloc(env, 41);
    if(!private) return NULL;

    zmq_curve_keypair(public, private);
    emacs_value keys[] = {
        ezmq_make_string(env, public, 40),
        ezmq_make_string(env, private, 40),
    };
    free(public);
    free(private);
    return env->funcall(env, Qcons, 2, keys);
}

emacs_value
Fzmq_curve_public(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    if(!zmq_has("curve")) {
        ezmq_error(env, Qzmq_error, "ZMQ not built with CURVE security");
        return NULL;
    }

    char *private = ezmq_copy_string(env, args[0], NULL);
    if(!private) return NULL;
    char *public = ezmq_malloc(env, 41);
    if(!public) return NULL;

    zmq_curve_public(public, private);
    emacs_value key = ezmq_make_string(env, public, 41);
    free(public);
    free(private);
    return key;
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

    Qzmq_error = INTERN(env, "zmq-ERROR");

    // Define the root error symbol for ZMQ errors
    args[0] = Qzmq_error;
    strcpy(buf, "An error occured in ZMQ");
    args[1] = ezmq_make_string(env, buf, strlen(buf));
    args[2] = INTERN(env, "error");
    env->funcall(env, def_err, 3, args);

    // Define common errors as symbols
    // Also see zmq_signal_error
    const char *msg;
    args[2] = Qzmq_error;

    args[0] = INTERN(env, "zmq-EINTR");
    msg = zmq_strerror(EINTR);
    args[1] = ezmq_make_string(env, msg, strlen(msg));
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
    Qwrong_type_argument = INTERN(env, "wrong-type-argument");
    Qlist = INTERN(env, "list");
    Qstring = INTERN(env, "string");
    Qvector = INTERN(env, "vector");
    Qcons = INTERN(env, "cons");
    Qcar = INTERN(env, "car");
    Qcdr = INTERN(env, "cdr");
    Qinteger = INTERN(env, "integer");
    Qlength = INTERN(env, "length");

    zmq_make_error_symbols(env);

    EZMQ_MAKE_FUN(env, 0, 0, zmq_version, "zmq-version");
    EZMQ_MAKE_FUN(env, 1, 1, zmq_has, "zmq-has");
    EZMQ_MAKE_FUN(env, 0, 1, zmq_message, "zmq-message");

    provide(env, "zmq2");
    return 0;
}
