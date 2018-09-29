#include "emacs-zmq.h"

int plugin_is_GPL_compatible;

typedef struct {
    char *name;
    void *fun;
    char const * doc;
    ptrdiff_t minarity;
    ptrdiff_t maxarity;
} ezmq_fun_t;

emacs_env *env = NULL;
emacs_value Qzmq_error, Qt, Qnil, Qlist,
    Qwrong_type_argument, Qargs_out_of_range,
    Qcons, Qstring, Qvector, Qcar, Qcdr, Qlength, Qinteger, Qequal,
    Qzmq_POLLIN, Qzmq_POLLERR, Qzmq_POLLOUT,
    Izmq_POLLIN, Izmq_POLLERR, Izmq_POLLOUT;

#define EZMQ_MAXARGS 5

static emacs_value _fargs[EZMQ_MAXARGS];

static emacs_value
ezmq_dispatch(emacs_env *current_env, ptrdiff_t nargs, emacs_value args[], void *info)
{
    env = current_env;

    emacs_value ret = Qnil;
    if(nargs > EZMQ_MAXARGS) {
        // Better error
        ezmq_signal(Qargs_out_of_range, 2, INT(nargs), INT(EZMQ_MAXARGS));
        return ret;
    }

    void *fun = ((ezmq_fun_t *)info)->fun;
    ptrdiff_t maxargs = ((ezmq_fun_t *)info)->maxarity;

    if(nargs < maxargs) {
        // Fill in nil values for optional arguments
        int i;
        for(i = (maxargs - 1); i >= nargs; i--) {
            _fargs[i] = Qnil;
        }
        for(i = 0; i < nargs; i++) {
            _fargs[i] = args[i];
        }
        args = _fargs;
        nargs = maxargs;
    }

    switch(nargs) {
    case 0:
        ret = ((emacs_value(*)(void))fun)();
        break;
    case 1:
        ret = ((emacs_value(*)(emacs_value))fun)(args[0]);
        break;
    case 2:
        ret = ((emacs_value(*)(emacs_value, emacs_value))fun)(args[0], args[1]);
        break;
    case 3:
        ret = ((emacs_value(*)(emacs_value, emacs_value,
                               emacs_value))fun)(args[0], args[1],
                                                 args[2]);
        break;
        // Currently unused
    case 4:
        ret = ((emacs_value(*)(emacs_value, emacs_value,
                               emacs_value, emacs_value))fun)(args[0], args[1],
                                                              args[2], args[3]);
        break;
    case EZMQ_MAXARGS:
        ret = ((emacs_value(*)(emacs_value, emacs_value,
                               emacs_value, emacs_value,
                               emacs_value))fun)(args[0], args[1],
                                                 args[2], args[3],
                                                 args[4]);
        break;
    }
    return ret;
}

static void
ezmq_bind_function(ezmq_fun_t *fun)
{
    emacs_value Sfun = env->make_function(env,
                                          fun->minarity, fun->maxarity,
                                          &ezmq_dispatch,
                                          fun->doc,
                                          fun);
    FUNCALL(INTERN("fset"), 2, ((emacs_value []){INTERN(fun->name), Sfun}));
}


static void
ezmq_provide(const char *feature)
{
    FUNCALL(INTERN("provide"), 1, ((emacs_value []){ INTERN(feature) }));
}

#define EZMQ_DEFERR(err)                        \
    args[0] = INTERN("zmq-"#err);               \
    args[1] = STRING(#err, sizeof(#err) - 1);   \
    FUNCALL(Qdefine_error, 3, args)             \

static void
ezmq_make_error_symbols()
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
    EZMQ_DEFERR(EINVAL);
    EZMQ_DEFERR(EPROTONOSUPPORT);
    EZMQ_DEFERR(ENOCOMPATPROTO);
    EZMQ_DEFERR(EADDRINUSE);
    EZMQ_DEFERR(EADDRNOTAVAIL);
    EZMQ_DEFERR(ENODEV);
    EZMQ_DEFERR(ETERM);
    EZMQ_DEFERR(ENOTSOCK);
    EZMQ_DEFERR(EMTHREAD);
    EZMQ_DEFERR(EFAULT);
    EZMQ_DEFERR(EINTR);
    EZMQ_DEFERR(ENOTSUP);
    EZMQ_DEFERR(ENOENT);
    EZMQ_DEFERR(ENOMEM);
    EZMQ_DEFERR(EAGAIN);
    EZMQ_DEFERR(EFSM);
    EZMQ_DEFERR(EHOSTUNREACH);
    EZMQ_DEFERR(EMFILE);
}

static bool initialized = false;

#define EZMQ_DEFFUN(ename, name, argmin, argmax) { ename, name, __zmq_doc_##name, argmin, argmax }

int
emacs_module_init(struct emacs_runtime *ert)
{
    if(initialized)
        return 0;

    // Retrieve the current emacs environment
    env = ert->get_environment(ert);

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

    ezmq_make_error_symbols();

    ezmq_expose_constants();
    Qzmq_POLLIN = INTERN("zmq-POLLIN");
    Qzmq_POLLOUT = INTERN("zmq-POLLOUT");
    Qzmq_POLLERR = INTERN("zmq-POLLERR");

    emacs_value Qsval = INTERN("symbol-value");
    Izmq_POLLIN = FUNCALL(Qsval, 1, &Qzmq_POLLIN);
    Izmq_POLLOUT = FUNCALL(Qsval, 1, &Qzmq_POLLOUT);
    Izmq_POLLERR = FUNCALL(Qsval, 1, &Qzmq_POLLERR);

    ezmq_fun_t functions[] =
        {
         EZMQ_DEFFUN("zmq-socket", ezmq_socket, 2, 2),
         EZMQ_DEFFUN("zmq-send", ezmq_send, 2, 3),
         EZMQ_DEFFUN("zmq-recv", ezmq_recv, 1, 3),
         EZMQ_DEFFUN("zmq-bind", ezmq_bind, 2, 2),
         EZMQ_DEFFUN("zmq-connect", ezmq_connect, 2, 2),
         EZMQ_DEFFUN("zmq-unbind", ezmq_unbind, 2, 2),
         EZMQ_DEFFUN("zmq-disconnect", ezmq_disconnect, 2, 2),
         EZMQ_DEFFUN("zmq-close", ezmq_close, 1, 1),
         EZMQ_DEFFUN("zmq-proxy", ezmq_proxy, 2, 3),
         EZMQ_DEFFUN("zmq-proxy-steerable", ezmq_proxy_steerable, 2, 4),
         EZMQ_DEFFUN("zmq-socket-monitor", ezmq_socket_monitor, 3, 3),
         EZMQ_DEFFUN("zmq-socket-set", ezmq_setsockopt, 3, 3),
         EZMQ_DEFFUN("zmq-socket-get", ezmq_getsockopt, 2, 2),
         EZMQ_DEFFUN("zmq-context", ezmq_context, 0, 0),
         EZMQ_DEFFUN("zmq-context-terminate", ezmq_ctx_term, 1, 1),
         EZMQ_DEFFUN("zmq-context-shutdown", ezmq_ctx_shutdown, 1, 1),
         EZMQ_DEFFUN("zmq-context-get", ezmq_ctx_get, 2, 2),
         EZMQ_DEFFUN("zmq-context-set", ezmq_ctx_set, 3, 3),
         EZMQ_DEFFUN("zmq-message", ezmq_message, 0, 1),
         EZMQ_DEFFUN("zmq-message-size", ezmq_message_size, 1, 1),
         EZMQ_DEFFUN("zmq-message-data", ezmq_message_data, 1, 1),
         EZMQ_DEFFUN("zmq-message-more-p", ezmq_message_more, 1, 1),
         EZMQ_DEFFUN("zmq-message-copy", ezmq_message_copy, 1, 1),
         EZMQ_DEFFUN("zmq-message-move", ezmq_message_move, 2, 2),
         EZMQ_DEFFUN("zmq-message-close", ezmq_message_close, 1, 1),
         EZMQ_DEFFUN("zmq-message-set", ezmq_message_set, 3, 3),
         EZMQ_DEFFUN("zmq-message-get", ezmq_message_get, 2, 2),
         EZMQ_DEFFUN("zmq-message-recv", ezmq_message_recv, 2, 3),
         EZMQ_DEFFUN("zmq-message-send", ezmq_message_send, 2, 3),
         EZMQ_DEFFUN("zmq-message-gets", ezmq_message_gets, 2, 2),
         EZMQ_DEFFUN("zmq-message-routing-id", ezmq_message_routing_id, 1, 1),
         EZMQ_DEFFUN("zmq-message-set-routing-id", ezmq_message_set_routing_id, 2, 2),
         EZMQ_DEFFUN("zmq-poll", ezmq_poll, 2, 2),
         EZMQ_DEFFUN("zmq-poller", ezmq_poller_new, 0, 0),
         EZMQ_DEFFUN("zmq-poller-add", ezmq_poller_add, 3, 3),
         EZMQ_DEFFUN("zmq-poller-modify", ezmq_poller_modify, 3, 3),
         EZMQ_DEFFUN("zmq-poller-remove", ezmq_poller_remove, 2, 2),
         EZMQ_DEFFUN("zmq-poller-destroy", ezmq_poller_destroy, 1, 1),
         EZMQ_DEFFUN("zmq-poller-wait", ezmq_poller_wait, 2, 2),
         EZMQ_DEFFUN("zmq-poller-wait-all", ezmq_poller_wait_all, 3, 3),
         EZMQ_DEFFUN("zmq-version", ezmq_version, 0, 0),
         EZMQ_DEFFUN("zmq-has", ezmq_has, 1, 1),
         EZMQ_DEFFUN("zmq-z85-decode", ezmq_z85_decode, 1, 1),
         EZMQ_DEFFUN("zmq-z85-encode", ezmq_z85_encode, 1, 1),
         EZMQ_DEFFUN("zmq-curve-keypair", ezmq_curve_keypair, 0, 0),
         EZMQ_DEFFUN("zmq-curve-public", ezmq_curve_public, 1, 1),
         EZMQ_DEFFUN("zmq-equal", ezmq_equal, 2, 2),
         EZMQ_DEFFUN("zmq-message-p", ezmq_message_p, 1, 1),
         EZMQ_DEFFUN("zmq-socket-p", ezmq_socket_p, 1, 1),
         EZMQ_DEFFUN("zmq-context-p", ezmq_context_p, 1, 1),
         EZMQ_DEFFUN("zmq-poller-p", ezmq_poller_p, 1, 1)
        };

    size_t i;
    for(i = 0; i < sizeof(functions)/sizeof(ezmq_fun_t); i++) {
        // FIXME: Allocate another one so that it can be
        // accessed through ezmq_dispatch. We can't just
        // statically allocate because of the documentation
        // strings are not compile time constants. There
        // should be a way to work around that though.
        ezmq_fun_t *fun = (ezmq_fun_t *)ezmq_malloc(sizeof(*fun));
        if(fun) {
            memcpy(fun, &functions[i], sizeof(*fun)) ;
            ezmq_bind_function(fun);
        }
    }

    ezmq_provide("zmq-core");

    initialized = true;
    return 0;
}
