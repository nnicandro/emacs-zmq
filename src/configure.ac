AC_PREREQ([2.69])

AC_INIT([emacs-zmq], [0.1], [nathanielnicandro@gmail.com])
AC_CONFIG_SRCDIR([core.c])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([-Wall -Wno-override foreign])

AC_PROG_CC
AM_PROG_AR

PKG_CHECK_MODULES([ZMQ], [libzmq])

dnl Ensure paths in $ZMQ_LIBS and $ZMQ_CFLAGS are searched first, this way if
dnl we are building libzmq locally and there is also a system installation, the
dnl libzmq used is the local one.
CFLAGS="$ZMQ_CFLAGS $CFLAGS"
dnl FIXME: $ZMQ_LIBS typically contains -lzmq, but LDFLAGS is meant only for
dnl search paths
LDFLAGS="$ZMQ_LIBS $LDFLAGS"
AC_MSG_NOTICE([Verifying libzmq])
AC_MSG_NOTICE([ZMQ_CFLAGS="$ZMQ_CFLAGS"])
AC_MSG_NOTICE([ZMQ_LIBS="$ZMQ_LIBS"])

AC_SEARCH_LIBS([zmq_poller_new], [zmq],
  [AC_DEFINE([ZMQ_BUILD_DRAFT_API], [1])],
  [AC_MSG_ERROR([libzmq not built with DRAFT API or libzmq is not usable.])
])

LT_INIT([shared disable-static])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
