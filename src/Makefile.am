SUBDIRS = libzmq
ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = emacs-zmq.la

emacs_zmq_la_LDFLAGS = -module -avoid-version
if WINDOWS
emacs_zmq_la_LDFLAGS += -no-undefined
endif

# Its necessary to pass static libraries directly to the linker since libtool
# won't build emacs-zmq as a dynamic module on Windows otherwise.
emacs_zmq_la_LDFLAGS += -Wl,libzmq/src/.libs/libzmq.a
emacs_zmq_la_SOURCES = socket.c context.c msg.c constants.c util.c core.c poll.c emacs-zmq.c
emacs_zmq_la_CPPFLAGS = -DZMQ_BUILD_DRAFT_API=1
emacs_zmq_la_CFLAGS = -O3 -Ilibzmq/include
