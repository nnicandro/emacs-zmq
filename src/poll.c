#include "poll.h"

int
ezmq_contract_events(emacs_env *env, emacs_value list)
{
    if(!ezmq_value_of_type(env, list, Qcons)) return -1;
    intmax_t len = env->extract_integer(env, env->funcall(env, Qlength, 1, &list));
    if(EZMQ_NONLOCAL_EXIT(env)) return -1;

    int events = 0;
    while((len--) >= 0) {
        intmax_t j = env->extract_integer(env, EZMQ_CAR(env, list));
        if(EZMQ_NONLOCAL_EXIT(env)) return -1;
        events |= j;
        list = EZMQ_CDR(env, list);
    }
    return events;
}

emacs_value
Fzmq_poll(emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
    if(!ezmq_value_of_type(env, args[0], Qcons)) {
        ezmq_wrong_type_argument(env, args[0], 1, Qcons);
        return NULL;
    }
    EZMQ_EXTRACT_INT(env, timeout, args[1]);

    emacs_value head = args[0];
    emacs_value tail = args[0];
    intmax_t len = env->extract_integer(env, env->funcall(env, Qlength, 1, args));
    zmq_pollitem_t items[len];
    for(intmax_t i = 0; i < len; i++) {
        tail = EZMQ_CDR(env, tail);
        if(!ezmq_value_of_type(env, head, Qcons) &&
           env->extract_integer(env, env->funcall(env, Qlength, 1, &head)) != 2 &&
           !ezmq_value_of_type(env, EZMQ_CDR(env, head), Qcons)) {
            // TODO: More informative
            ezmq_wrong_type_argument(env, head, 0);
            return NULL;
        }

        emacs_value sock_or_fd = EZMQ_CAR(env, head);
        emacs_value events = EZMQ_CDR(env, head);
        if(ezmq_value_of_type(env, sock_or_fd, Qinteger)) {
            items[i].fd = env->extract_integer(env, sock_or_fd);
            items[i].socket = NULL;
        } else {
            ezmq_obj_t *sock = env->get_user_ptr(env, sock_or_fd);
            if(EZMQ_NONLOCAL_EXIT(env)) return NULL;
            items[i].socket = sock->obj;
            items[i].fd = -1;
        }
        // TODO: Properly handle errors with calls to extract integer
        intmax_t elen = env->extract_integer(env, env->funcall(env, Qlength, 1, &events));
        items[i].events = 0;
        // TODO: Validate events
        while((elen--) >= 0) {
            intmax_t j = env->extract_integer(env, EZMQ_CAR(env, events));
            if(EZMQ_NONLOCAL_EXIT(env)) return NULL;
            items[i].events |= j;
            events = EZMQ_CDR(env, events);
        }
        head = EZMQ_CAR(env, tail);
    }

    len = zmq_poll(items, len, timeout);
}
