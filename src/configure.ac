AC_PREREQ([2.69])

AC_INIT([emacs-zmq], [0.1], [nathanielnicandro@gmail.com])
AC_CONFIG_SRCDIR([core.c])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([-Wall -Wno-override foreign])

AC_PREFIX_DEFAULT([`pwd`])

AC_PROG_CC
AM_PROG_AR

AC_CHECK_LIB([zmq], [zmq_version], [], [
    AC_MSG_ERROR([ZMQ library not found])
])
AC_SEARCH_LIBS([zmq_poller_new], [], [
    AC_DEFINE([ZMQ_BUILD_DRAFT_API], 1)
], [
    AC_MSG_ERROR([Need poller interface. Compile ZMQ with -DZMQ_BUILD_DRAFT_API=1])
])

LT_INIT([shared])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
