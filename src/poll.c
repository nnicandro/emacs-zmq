#include "poll.h"

static void
ezmq_invalid_poll_event(intmax_t event)
{
    ezmq_signal(INTERN("args-out-of-range"), 2,
                INT(event), LIST(4,
                                 INTERN("or"),
                                 INTERN("zmq-POLLIN"),
                                 INTERN("zmq-POLLOUT"),
                                 INTERN("zmq-POLLERR")));
}

/**
   Extract a single poll event (ZMQ_POLLIN, ZMQ_POLLOUT, ZMQ_POLLERR) from a
   Lisp integer.


   Signal an args-out-of-bounds error if the integer is not valid.
*/
static intmax_t
ezmq_extract_poll_event(emacs_value val)
{
    intmax_t event = EXTRACT_INT(val);
    if(NONLOCAL_EXIT()) return -1;
    if(event != ZMQ_POLLIN && event != ZMQ_POLLOUT && event != ZMQ_POLLERR) {
        ezmq_invalid_poll_event(event);
        return -1;
    }
    return event;
}

/**
   Merge a list of poll events, i.e. take the bitwise or of a list of poll
   event integers.


   If the Lisp object passed to this function is not a list, signal a
   wrong-type-argument error.
*/
static intmax_t
ezmq_merge_poll_events(emacs_value list)
{
    intmax_t events = 0;
    if(EQ(TYPE(list), Qcons)) {
        while(!NILP(list)) {
            intmax_t event = ezmq_extract_poll_event(CAR(list));
            if(NONLOCAL_EXIT()) break;
            events |= event;
            list = CDR(list);
        }
    } else
        ezmq_wrong_type_argument(list, 1, Qlist);
    return events;
}

/**
   Split the bitwise or of poll events into a list of poll events. This is so
   that in Emacs, you can simply call `member` on the resulting list to find
   out if an event occurred rather than using `logand`.
*/
static emacs_value
ezmq_split_poll_events(int events)
{
    emacs_value args[3];
    int i = 0;

    if(events & ZMQ_POLLIN)
        args[i++] = Izmq_POLLIN;
    if(events & ZMQ_POLLOUT)
        args[i++] = Izmq_POLLOUT;
    if(events & ZMQ_POLLERR)
        args[i] = Izmq_POLLERR;

    if(i == 0)
        ezmq_invalid_poll_event(events);

    return FUNCALL(Qlist, i, args);
}

/**
   Given a SOCK-OR-FD and a list of poll EVENTS, construct a zmq_pollitem_t.
   Store it in ITEM. A non-local exit is pending if a poll item could not be
   constructed.
*/
static void *
ezmq_extract_pollitem(emacs_value sock_or_fd, emacs_value events, zmq_pollitem_t *item)
{
    if(EQ(TYPE(sock_or_fd), Qinteger)) {
        EZMQ_EXTRACT_INT(fd, sock_or_fd);
        item->fd = fd;
        item->socket = NULL;
    } else {
        EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, sock_or_fd);
        item->socket = sock->obj;
    }
    item->events = (short)ezmq_merge_poll_events(events);
    return NULL;
}

/**
   Return (SOCK-OR-FD . SEVENTS) where SEVENTS is a list socket events.
   See ezmq_split_poll_events() for how SEVENTS is constructed from EVENTS.
 */
static emacs_value
ezmq_make_pollitem(emacs_value sock_or_fd, int events)
{
    emacs_value sevents = ezmq_split_poll_events(events);
    emacs_value item = CONS(sock_or_fd, sevents);
    return item;
}

/**
   Return an array of zmq_pollitem_t objects constructed from LIST.
   LIST is a list of (SOCK-OR-FD . EVENTS) pairs from which to construct
   zmq_pollitem_t objects from. EVENTS can either be an integer or a list
   containing any of zmq-POLLIN, zmq-POLLOUT, or zmq-POLLERR.
   The returned array must be released by the called.
*/
static zmq_pollitem_t *
ezmq_extract_pollitem_list(emacs_value list)
{
    intmax_t len = LENGTH(list);
    zmq_pollitem_t *items = (zmq_pollitem_t *)ezmq_malloc(sizeof(zmq_pollitem_t)*len);
    intmax_t i;
    for(i = 0; i < len; i++) {
        emacs_value head = CAR(list);
        list = CDR(list);

        if(NONLOCAL_EXIT()) {
            free(items);
            break;
        }

        if(EQ(TYPE(head), Qcons) && EQ(TYPE(CDR(head)), Qcons))
            ezmq_extract_pollitem(CAR(head), CDR(head), &items[i]);
        else {
            free(items);
            // TODO: The right error convention for wrong-type-argument
            ezmq_wrong_type_argument(head, 1, INTERN("consp"));
            break;
        }
    }
    return items;
}

/**
   Return a list of (SOCK-OR-FD . EVENTS) pairs constructed from ITEMS.
   EITEMS is the original list of pairs that was given to ezmq_poll().
   NRECEIVED are the number of received events returned by zmq_poll(). NITEMS
   are the number of items in EITEMS.
*/
static emacs_value
ezmq_make_pollitem_list(intmax_t nreceived, zmq_pollitem_t *items, emacs_value eitems, intmax_t nitems)
{
    emacs_value ritems = Qnil;
    while(nreceived > 0) {
        nitems -= 1;

        short events = items[nitems].revents;
        if(events != 0) {
            nreceived -= 1;

            emacs_value eitem = FUNCALL(Qnth, 2, ((emacs_value []){INT(nitems), eitems}));
            ritems = CONS(ezmq_make_pollitem(CAR(eitem), events), ritems);
        }
    }
    return ritems;
}

EZMQ_DOC(ezmq_poll, "ITEMS TIMEOUT",
         "Poll ITEMS until TIMEOUT.\n"
         "ITEMS is a list of cons cells of the form (SOCK-OR-FD . EVENTS)\n"
         "where EVENTS is a list of valid poll events. TIMEOUT is measured in\n"
         "milliseconds.");
emacs_value
ezmq_poll(emacs_value eitems, emacs_value etimeout)
{
    EZMQ_EXTRACT_INT(timeout, etimeout);

    emacs_value retval = Qnil;
    if(EQ(TYPE(eitems), Qcons)) {
        intmax_t nitems = LENGTH(eitems);
        zmq_pollitem_t *items = ezmq_extract_pollitem_list(eitems);

        if(!NONLOCAL_EXIT()) {
            intmax_t nreceived = zmq_poll(items, nitems, timeout);
            EZMQ_CHECK_ERROR(nreceived);

            if(!NONLOCAL_EXIT())
                retval = ezmq_make_pollitem_list(nreceived, items, eitems, nitems);

            free(items);
        }
    } else
        ezmq_wrong_type_argument(eitems, 1, INTERN("consp"));
    return retval;
}

EZMQ_DOC(ezmq_poller_new,
         "",
         "Create a new `zmq-poller' object.");
emacs_value
ezmq_poller_new(void)
{
    void *poller = zmq_poller_new();
    EZMQ_CHECK_NULL_ERROR(poller);
    return ezmq_new_obj_ptr(ezmq_new_obj(EZMQ_POLLER, poller));
}

EZMQ_DOC(ezmq_poller_add,
         "POLLER SOCK-OR-FD EVENTS",
         "Listen for EVENTS on SOCK using POLLER.\n"
         "SOCK-OR-FD can either be a `zmq-socket' or a file descriptor.\n"
         "EVENTS can either be a list of events (one of `zmq-POLLIN',\n"
         "`zmq-POLLOUT', `zmq-POLLERR') or a bitwise-or of events.");
emacs_value
ezmq_poller_add(emacs_value epoller, emacs_value esock, emacs_value eevents)
{
    EZMQ_EXTRACT_OBJ(poller, EZMQ_POLLER, epoller);

    short events;
    if(EQ(TYPE(eevents), Qcons))
        events = ezmq_merge_poll_events(eevents);
    else if(EQ(TYPE(eevents), Qinteger))
        events = EXTRACT_INT(eevents);
    else
        ezmq_wrong_type_argument(eevents, 1, INTERN("consp"));

    if(!NONLOCAL_EXIT()) {
        if(EQ(TYPE(esock), Qinteger)) {
            EZMQ_EXTRACT_INT(fd, esock);
            EZMQ_CHECK_ERROR(zmq_poller_add_fd(poller->obj, fd, NULL, events));
        } else {
            EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
            EZMQ_CHECK_ERROR(zmq_poller_add(poller->obj, sock->obj, sock, events));
        }
    }

    return Qnil;
}

EZMQ_DOC(ezmq_poller_modify,
         "POLLER SOCK-OR-FD EVENTS",
         "Modify the EVENTS of SOCK-OR-FD that POLLER listens for.");
emacs_value
ezmq_poller_modify(emacs_value epoller, emacs_value esock, emacs_value eevents)
{
    EZMQ_EXTRACT_OBJ(poller, EZMQ_POLLER, epoller);

    short events;
    if(EQ(TYPE(eevents), Qcons))
        events = ezmq_merge_poll_events(eevents);
    else if(EQ(TYPE(eevents), Qinteger))
        events = EXTRACT_INT(eevents);
    else
        ezmq_wrong_type_argument(eevents, 1, INTERN("consp"));

    if(!NONLOCAL_EXIT()) {
        if(EQ(TYPE(esock), Qinteger)) {
            EZMQ_EXTRACT_INT(fd, esock);
            EZMQ_CHECK_ERROR(zmq_poller_modify_fd(poller->obj, fd, events));
        } else {
            EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
            EZMQ_CHECK_ERROR(zmq_poller_modify(poller->obj, sock->obj, events));
        }
    }

    return Qnil;
}

EZMQ_DOC(ezmq_poller_remove,
         "POLLER SOCK-OR-FD",
         "Remove SOCK-OR-FD from POLLER.");
emacs_value
ezmq_poller_remove(emacs_value epoller, emacs_value esock)
{
    EZMQ_EXTRACT_OBJ(poller, EZMQ_POLLER, epoller);
    if(EQ(TYPE(esock), Qinteger)) {
        EZMQ_EXTRACT_INT(fd, esock);
        EZMQ_CHECK_ERROR(zmq_poller_remove_fd(poller->obj, fd));
    } else {
        EZMQ_EXTRACT_OBJ(sock, EZMQ_SOCKET, esock);
        EZMQ_CHECK_ERROR(zmq_poller_remove(poller->obj, sock->obj));
    }
    return Qnil;
}

EZMQ_DOC(ezmq_poller_destroy,
         "POLLER",
         "Destroy POLLER");
emacs_value
ezmq_poller_destroy(emacs_value epoller)
{
    EZMQ_EXTRACT_OBJ(poller, EZMQ_POLLER, epoller);
    EZMQ_CHECK_ERROR(zmq_poller_destroy(&poller->obj));
    return Qnil;
}

EZMQ_DOC(ezmq_poller_wait,
         "POLLER TIMEOUT",
         "Poll for an event with POLLER until TIMEOUT ms.\n"
         "If an event occures before TIMEOUT ms, return a cons\n"
         "cell (SOCK-OR-FD . EVENTS) where EVENTS is a list of events which\n"
         "occured before TIMEOUT. Otherwise return nil. If TIMEOUT is -1,\n"
         "wait forever until an event arrives.");
emacs_value
ezmq_poller_wait(emacs_value epoller, emacs_value etimeout)
{
    EZMQ_EXTRACT_OBJ(poller, EZMQ_POLLER, epoller);
    EZMQ_EXTRACT_INT(timeout, etimeout);

    zmq_poller_event_t event;
    EZMQ_CHECK_ERROR(zmq_poller_wait(poller->obj, &event, timeout));

    if(!NONLOCAL_EXIT()) {
        emacs_value sevents = ezmq_split_poll_events(event.events);
        emacs_value trigger;
        if(event.socket)
            trigger = ezmq_new_obj_ptr((ezmq_obj_t *)event.user_data);
        else
            trigger = INT(event.fd);

        return CONS(trigger, sevents);
    }
    return Qnil;
}

EZMQ_DOC(ezmq_poller_wait_all,
         "POLLER NEVENTS TIMEOUT",
         "Wait until TIMEOUT for NEVENTS on POLLER.\n"
         "If between 1 and NEVENTS events occured within TIMEOUT (measured\n"
         "in milliseconds) return a list of cons cells, each element having\n"
         "the form (SOCK-OR-FD . EVENTS). EVENTS is a list of events which\n"
         "occured on SOCK-OR-FD during the polling period. Note that the\n"
         "length of the returned list may be less than NEVENTS if less than\n"
         "NEVENTS events occurred within TIMEOUT. If TIMEOUT is -1, wait\n"
         "forever.");
emacs_value
ezmq_poller_wait_all(emacs_value epoller, emacs_value enevents, emacs_value etimeout)
{
    EZMQ_EXTRACT_OBJ(poller, EZMQ_POLLER, epoller);
    EZMQ_EXTRACT_INT(nevents, enevents);
    EZMQ_EXTRACT_INT(timeout, etimeout);

    emacs_value retval = Qnil;
    zmq_poller_event_t revents[nevents];

    int ntriggered = zmq_poller_wait_all(poller->obj, revents, nevents, timeout);
    EZMQ_CHECK_ERROR(ntriggered);

    // TODO: Return nil on EAGAIN
    if(!NONLOCAL_EXIT()) {
        int i;
        for(i = 0; i < ntriggered; i++) {
            zmq_poller_event_t event = revents[i];
            emacs_value sevents = ezmq_split_poll_events(event.events);
            emacs_value trigger;
            if(event.socket)
                trigger = ezmq_new_obj_ptr((ezmq_obj_t *)event.user_data);
            else
                trigger = INT(event.fd);

            retval = CONS(CONS(trigger, sevents), retval);
        }
    }
    return retval;
}
