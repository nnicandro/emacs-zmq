AC_PREREQ([2.69])
AC_INIT([emacs-zmq], [0.10.9], [nathanielnicandro@gmail.com])

AC_CONFIG_SRCDIR([core.c])
AC_CONFIG_MACRO_DIRS([m4])
AM_INIT_AUTOMAKE([-Wall -Wno-override foreign])
AC_PROG_CC
AM_PROG_AR
AC_CANONICAL_HOST

PKG_CHECK_MODULES([ZMQ], [libzmq], [
    AC_SEARCH_LIBS([zmq_poller_new], [zmq], [
        dnl FIXME: This isn't necessarily true in ZMQ >= 4.3
        AC_DEFINE([ZMQ_BUILD_DRAFT_API], 1)
    ], [
        AC_MSG_NOTICE([incompatible libzmq, building one locally])
        ZMQ_BUILD_LOCALLY=yes
    ])
], [
    AC_MSG_NOTICE([libzmq not found by pkg-config, building one locally])
    ZMQ_BUILD_LOCALLY=yes
])

if test "x$ZMQ_BUILD_LOCALLY" != x; then
    if test "x$ZMQ_VERSION" == x; then ZMQ_VERSION="4.3.1"; fi
    if test "x$ZMQ_GIT_REPO" == x; then ZMQ_GIT_REPO="https://github.com/zeromq/libzmq"; fi
    if test ! -d libzmq/.git; then git clone $ZMQ_GIT_REPO libzmq; fi
    AC_CONFIG_SUBDIRS([libzmq])
    # Ensure the libzmq/configure script is generated
    cd libzmq
    if test "`git describe --candidates=0 2>/dev/null || echo x`" != "v${ZMQ_VERSION}"; then
        git checkout --quiet master
        git pull --quiet origin
        git checkout "v$ZMQ_VERSION"
        rm -f configure
    fi
    if test ! -e configure; then
        echo "Generating libzmq configure script"
        ./autogen.sh
    fi
    cd ..
    case "${host_os}" in
        *darwin*)
            ;;
        *mingw*|*msys*|*cygwin*)
            export CXXFLAGS="-static-libgcc -static-libstdc++ $CXXFLAGS"
            # Its necessary to pass these to the linker directly since libtool
            # won't build emacs-zmq as a dynamic module otherwise on Windows.
            export LDFLAGS="-Wl,-l:libstdc++.a -Wl,-l:libgcc.a -Wl,-lws2_32 -Wl,-liphlpapi $LDFLAGS"
            ON_WINDOWS=yes
            ;;
        *)
            # Assume GCC compatible compiler
            # TODO: Actually check for this
            # Prevent dynamic linkage of libzmq dependencies
            #
            # PIC is needed since we are most likely building libzmq
            # statically, but would like to link it to the dynamic emacs-zmq
            # library. GCC needs to have this explicitly specified.
            #
            # The -fPIC for CFLAGS is needed even though emacs-zmq is already a
            # dynamic library since libzmq comes packaged with tweetnacl which
            # is a C library and it builds that by default.
            export CXXFLAGS="-fPIC -static-libgcc -static-libstdc++ $CXXFLAGS"
            export CFLAGS="-fPIC $CFLAGS"
            export LDFLAGS="-Wl,-l:libstdc++.a -Wl,-l:libgcc.a $LDFLAGS"
            ;;
    esac
fi

AM_CONDITIONAL(WINDOWS, [test "x$ON_WINDOWS" != x])
AM_CONDITIONAL(ZMQ_BUILD_LOCALLY, [test "x$ZMQ_BUILD_LOCALLY" != x])
if test "x$ZMQ_BUILD_LOCALLY" != x; then
    MAYBE_LIBZMQ=libzmq
else
    MAYBE_LIBZMQ=
fi
AC_SUBST([MAYBE_LIBZMQ])

LT_INIT([win32-dll shared disable-static])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
