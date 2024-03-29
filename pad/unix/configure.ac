AC_PREREQ([2.69])
AC_INIT([pad],[0.9p5])
AC_CONFIG_SRCDIR([tkAppInit.cpp])
AC_CONFIG_HEADERS(config.h)
AC_CONFIG_MACRO_DIRS([m4])

AC_PROG_CXX
AC_PROG_RANLIB
AC_CANONICAL_HOST

AM_INIT_AUTOMAKE([foreign subdir-objects])

# On Darwin, X11 (XQuartz) is in a non-standard location.
case "${host_os}" in
    darwin*)
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/opt/X11/lib/pkgconfig/:/opt/X11/share/pkgconfig/"
    ;;
esac

PKG_CHECK_MODULES(X11, [x11])
AC_SUBST(X11_CFLAGS)
AC_SUBST(X11_LIBS)

PKG_CHECK_MODULES(FREETYPE2, [freetype2])
AC_SUBST(FREETYPE2_CFLAGS)
AC_SUBST(FREETYPE2_LIBS)

# On Darwin, the ImageMagick@6 homebrew recipe is generally 'keg-only'.
case "${host_os}" in
    darwin*)
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/imagemagick@6/lib/pkgconfig"
    ;;
esac

# REMARK: We need ImageMagick 6, not 7, which deprecates the old api.h.
PKG_CHECK_MODULES(MAGICKCORE, [MagickCore < 7]) 
AC_SUBST(MAGICKCORE_CFLAGS)
AC_SUBST(MAGICKCORE_LIBS)

# Set XSHM to avoid ximage NPEs.
AC_DEFINE([XSHM], [], [Enable X shared memory.])

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
