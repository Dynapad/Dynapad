AC_PREREQ([2.69])
AC_INIT([pad],[0.9p5])

AC_CONFIG_HEADERS(config.h)

AM_INIT_AUTOMAKE([subdir-objects])

AC_PROG_CXX
AM_PROG_AR
AC_PROG_RANLIB

AC_CONFIG_SRCDIR([tkAppInit.cpp])
AC_CANONICAL_HOST

# On Darwin, X11 (XQuartz) is in a non-standard location.
case "${host_os}" in
    darwin*)
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/opt/X11/lib/pkgconfig/:/opt/X11/share/pkgconfig/"
    ;;
esac

PKG_CHECK_MODULES(X11, [x11])
AC_SUBST(X11_CFLAGS)
AC_SUBST(X11_LIBS)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
