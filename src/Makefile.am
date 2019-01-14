ACLOCAL_AMFLAGS = -I m4
lib_LTLIBRARIES = emacs-zmq.la
emacs_zmq_la_LDFLAGS = -module -avoid-version $(ZMQ_LIBS)
emacs_zmq_la_SOURCES = socket.c context.c msg.c constants.c util.c core.c poll.c emacs-zmq.c
emacs_zmq_la_CFLAGS = $(ZMQ_CFLAGS) -pthread -O3
