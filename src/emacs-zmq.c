#include "emacs-zmq.h"

#define EZMQ_MAKE_FUN(argmin, argmax, name, ename)          \
    ezmq_bind_function(env,                                 \
                       ename,                               \
                       env->make_function(env,              \
                                          argmin, argmax,   \
                                          &F##name,         \
                                          __zmq_doc_##name, \
                                          NULL))

int plugin_is_GPL_compatible;

static void
ezmq_bind_function(emacs_env *env, const char *name, emacs_value Sfun)
{
    emacs_value Qfset = INTERN("fset");
    emacs_value Qsym = INTERN(name);
    /* Prepare the arguments array */
    emacs_value args[] = {Qsym, Sfun};
    /* Make the call (2 == nb of arguments) */
    env->funcall(env, Qfset, 2, args);
}

static void
ezmq_provide(emacs_env *env, const char *feature)
{
    /* call 'provide' with FEATURE converted to a symbol */
    emacs_value Qfeat = INTERN(feature);
    emacs_value Qprovide = INTERN("provide");
    emacs_value args[] = { Qfeat };
    env->funcall (env, Qprovide, 1, args);
}

// TODO: Why -1?
#define DEF_ERR(err)                            \
    args[0] = INTERN("zmq-"#err);               \
    args[1] = STRING(#err, sizeof(#err) - 1);   \
    env->funcall(env, Qdefine_error, 3, args)   \

static void
ezmq_make_error_symbols(emacs_env *env)
{
    emacs_value Qdefine_error = INTERN("define-error");
    const char *msg =  "An error occured in ZMQ";
    emacs_value args[3];

    // Define the root error symbol for ZMQ errors
    args[0] = Qzmq_error;
    args[1] = STRING(msg, strlen(msg));
    args[2] = INTERN("error");
    env->funcall(env, Qdefine_error, 3, args);
    args[2] = Qzmq_error;
    // Define common errors as symbols
    // Also see zmq_signal_error
    DEF_ERR(EINVAL);
    DEF_ERR(EPROTONOSUPPORT);
    DEF_ERR(ENOCOMPATPROTO);
    DEF_ERR(EADDRINUSE);
    DEF_ERR(EADDRNOTAVAIL);
    DEF_ERR(ENODEV);
    DEF_ERR(ETERM);
    DEF_ERR(ENOTSOCK);
    DEF_ERR(EMTHREAD);
    DEF_ERR(EFAULT);
    DEF_ERR(EINTR);
    DEF_ERR(ENOTSUP);
    DEF_ERR(ENOENT);
    DEF_ERR(ENOMEM);
    DEF_ERR(EAGAIN);
    DEF_ERR(EFSM);
    DEF_ERR(EHOSTUNREACH);
    DEF_ERR(EMFILE);
}

static bool initialized = false;

int
emacs_module_init(struct emacs_runtime *ert)
{
    if(initialized)
        return 0;

    // Retrieve the current emacs environment
    emacs_env *env = ert->get_environment(ert);

    Qt = INTERN("t");
    Qnil = INTERN("nil");
    Qwrong_type_argument = INTERN("wrong-type-argument");
    Qargs_out_of_range = INTERN("args-out-of-range");
    Qlist = INTERN("list");
    Qstring = INTERN("string");
    Qvector = INTERN("vector");
    Qcons = INTERN("cons");
    Qcar = INTERN("car");
    Qcdr = INTERN("cdr");
    Qequal = INTERN("equal");
    Qinteger = INTERN("integer");
    Qlength = INTERN("length");
    Qzmq_error = INTERN("zmq-ERROR");

    ezmq_make_error_symbols(env);

    ezmq_expose_constants(env);
    Qzmq_POLLIN = INTERN("zmq-POLLIN");
    Qzmq_POLLOUT = INTERN("zmq-POLLOUT");
    Qzmq_POLLERR = INTERN("zmq-POLLERR");

    emacs_value Qsval = INTERN("symbol-value");
    Izmq_POLLIN = env->funcall(env, Qsval, 1, &Qzmq_POLLIN);
    Izmq_POLLOUT = env->funcall(env, Qsval, 1, &Qzmq_POLLOUT);
    Izmq_POLLERR = env->funcall(env, Qsval, 1, &Qzmq_POLLERR);

    // Sockets
    EZMQ_MAKE_FUN(2, 2, zmq_socket, "zmq-socket");
    EZMQ_MAKE_FUN(2, 3, zmq_send, "zmq-send");
    EZMQ_MAKE_FUN(1, 3, zmq_recv, "zmq-recv");
    EZMQ_MAKE_FUN(2, 2, zmq_bind, "zmq-bind");
    EZMQ_MAKE_FUN(2, 2, zmq_connect, "zmq-connect");
    EZMQ_MAKE_FUN(2, 2, zmq_unbind, "zmq-unbind");
    EZMQ_MAKE_FUN(2, 2, zmq_disconnect, "zmq-disconnect");
    EZMQ_MAKE_FUN(3, 3, zmq_setsockopt, "zmq-socket-set");
    EZMQ_MAKE_FUN(2, 2, zmq_getsockopt, "zmq-socket-get");

    // Contexts
    EZMQ_MAKE_FUN(0, 0, zmq_context, "zmq-context");
    EZMQ_MAKE_FUN(3, 3, zmq_ctx_set, "zmq-context-set");
    EZMQ_MAKE_FUN(2, 2, zmq_ctx_get, "zmq-context-get");
    EZMQ_MAKE_FUN(3, 3, zmq_ctx_shutdown, "zmq-shutdown-context");

    // Messages
    EZMQ_MAKE_FUN(0, 1, zmq_message, "zmq-message");
    EZMQ_MAKE_FUN(1, 1, zmq_message_size, "zmq-message-size");
    EZMQ_MAKE_FUN(1, 1, zmq_message_data, "zmq-message-data");
    EZMQ_MAKE_FUN(1, 1, zmq_message_more, "zmq-message-more-p");
    EZMQ_MAKE_FUN(1, 1, zmq_message_copy, "zmq-message-copy");
    EZMQ_MAKE_FUN(2, 2, zmq_message_move, "zmq-message-move");
    EZMQ_MAKE_FUN(3, 3, zmq_message_set, "zmq-message-set");
    EZMQ_MAKE_FUN(2, 2, zmq_message_get, "zmq-message-get");
    EZMQ_MAKE_FUN(2, 3, zmq_message_recv, "zmq-message-recv");
    EZMQ_MAKE_FUN(2, 3, zmq_message_send, "zmq-message-send");
    EZMQ_MAKE_FUN(2, 2, zmq_message_gets, "zmq-message-gets");
    // These require that the draft API is available
    EZMQ_MAKE_FUN(1, 1, zmq_message_routing_id, "zmq-message-routing-id");
    EZMQ_MAKE_FUN(2, 2, zmq_message_set_routing_id, "zmq-message-set-routing-id");

    // Polling
    EZMQ_MAKE_FUN(2, 2, zmq_poll, "zmq-poll");
    EZMQ_MAKE_FUN(0, 0, zmq_poller_new, "zmq-poller");
    EZMQ_MAKE_FUN(3, 3, zmq_poller_add, "zmq-poller-add");
    EZMQ_MAKE_FUN(3, 3, zmq_poller_add_fd, "zmq-poller-add-fd");
    EZMQ_MAKE_FUN(3, 3, zmq_poller_modify, "zmq-poller-modify");
    EZMQ_MAKE_FUN(3, 3, zmq_poller_modify_fd, "zmq-poller-modify-fd");
    EZMQ_MAKE_FUN(2, 2, zmq_poller_remove, "zmq-poller-remove");
    EZMQ_MAKE_FUN(2, 2, zmq_poller_remove_fd, "zmq-poller-remove-fd");
    EZMQ_MAKE_FUN(2, 2, zmq_poller_wait, "zmq-poller-wait");
    EZMQ_MAKE_FUN(3, 3, zmq_poller_wait_all, "zmq-poller-wait-all");

    // Util
    EZMQ_MAKE_FUN(0, 0, zmq_version, "zmq-version");
    EZMQ_MAKE_FUN(1, 1, zmq_has, "zmq-has");
    EZMQ_MAKE_FUN(1, 1, zmq_z85_decode, "zmq-z85-decode");
    EZMQ_MAKE_FUN(1, 1, zmq_z85_encode, "zmq-z85-encode");
    EZMQ_MAKE_FUN(0, 0, zmq_curve_keypair, "zmq-curve-keypair");
    EZMQ_MAKE_FUN(1, 1, zmq_curve_public, "zmq-curve-public");
    EZMQ_MAKE_FUN(2, 2, zmq_equal, "zmq-equal");
    EZMQ_MAKE_FUN(1, 1, zmq_message_p, "zmq-message-p");
    EZMQ_MAKE_FUN(1, 1, zmq_socket_p, "zmq-socket-p");
    EZMQ_MAKE_FUN(1, 1, zmq_context_p, "zmq-context-p");
    EZMQ_MAKE_FUN(1, 1, zmq_poller_p, "zmq-poller-p");

    ezmq_provide(env, "zmq-core");

    initialized = true;
    return 0;
}
