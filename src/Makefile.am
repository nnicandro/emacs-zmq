SUBDIRS = $(MAYBE_LIBZMQ)
ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = libemacs-zmq.la

libemacs_zmq_la_CFLAGS =
libemacs_zmq_la_LDFLAGS = -avoid-version
if WINDOWS
libemacs_zmq_la_LDFLAGS += -no-undefined
endif

if ZMQ_BUILD_LOCALLY
# Its necessary to pass static libraries directly to the linker since libtool
# won't build emacs-zmq as a dynamic module on Windows otherwise.
libemacs_zmq_la_LDFLAGS += -Wl,libzmq/src/.libs/libzmq.a
libemacs_zmq_la_CFLAGS += -Ilibzmq/include
libemacs_zmq_la_CPPFLAGS = -DZMQ_BUILD_DRAFT_API=1
endif
libemacs_zmq_la_SOURCES = socket.c context.c msg.c constants.c util.c core.c poll.c emacs-zmq.c
