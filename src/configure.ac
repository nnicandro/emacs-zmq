AC_PREREQ([2.69])

AC_INIT([emacs-zmq], [0.1], [nathanielnicandro@gmail.com])
AC_CONFIG_SRCDIR([core.c])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([-Wall -Wno-override foreign])

AC_PROG_CC
AM_PROG_AR

PKG_CHECK_MODULES([ZMQ], [libzmq])
case "$ZMQ_CFLAGS" in
    *-DZMQ_BUILD_DRAFT_API=1*) ;;
    *) AC_MSG_ERROR([ZMQ not built with draft API.]) ;;
esac

LT_INIT([shared])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
