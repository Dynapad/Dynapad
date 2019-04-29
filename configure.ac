AC_PREREQ([2.69])
AC_INIT([dynapad],[1.0])
AM_INIT_AUTOMAKE([subdir-objects])


AC_CONFIG_HEADERS([config.h])
AC_CONFIG_SRCDIR([dynapad.cpp])

AC_CANONICAL_HOST
AC_PROG_CXX

AC_MSG_CHECKING([minimal-racket])
PLT=/usr/local/opt/minimal-racket
AC_ARG_WITH(plt, 
  [AS_HELP_STRING([--with-plt], 
                  [location of minimal-racket])], 
  [PLT="$withval"])
if test ! -f $PLT/include/racket/scheme.h; then
  AC_MSG_ERROR([Racket not found at $PLT])
fi
AC_SUBST(PLT)
AC_MSG_RESULT([$PLT])


PLT_CFLAGS=[-I$PLT/include/racket/]
AC_SUBST(PLT_CFLAGS)

case "${host_os}" in
    darwin*)
      PLT_LDFLAGS="-F $PLT/lib/racket/ -framework Racket" # link as framework
      AC_SUBST(PLT_LDFLAGS)
      ;;
    linux*)
      PLT_LDADD="/usr/lib/libracket3m.so"
      AC_SUBST(PLT_LDADD)
      ;;
esac

# On Darwin, the ImageMagick@6 homebrew recipe is generally 'keg-only'.
case "${host_os}" in
    darwin*)
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/local/opt/imagemagick@6/lib/pkgconfig"
    ;;
    linux*)
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/lib/imagemagick6/pkgconfig"
    ;;
esac

PKG_CHECK_MODULES(MAGICKCORE, [MagickCore < 7]) 
AC_SUBST(MAGICKCORE_CFLAGS)
AC_SUBST(MAGICKCORE_LIBS)

AC_CHECK_PROG(PDFIMAGES, pdfimages, found, missing)
if test $PDFIMAGES = "missing"; then
  AC_MSG_ERROR(pdfimages not found)
fi

#--------------------------------------------------------------------
# Check for db header file, because hashtab uses it
# instead of Tcl_Hash routines
#--------------------------------------------------------------------
AC_CHECK_HEADER([db.h], DB=found, DB=missing)
if test "$DB" = "missing"; then
  AC_MSG_ERROR(db.h not found)
fi

# On Darwin, X11 (XQuartz) is in a non-standard location.
case "${host_os}" in
    darwin*)
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/opt/X11/lib/pkgconfig/:/opt/X11/share/pkgconfig/"
    ;;
    linux*)
      export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:/usr/lib/pkgconfig"
    ;;
esac

PKG_CHECK_MODULES(X11, [x11 xft xi xext])
AC_SUBST(X11_CFLAGS)
AC_SUBST(X11_LIBS)

PKG_CHECK_MODULES(FREETYPE2, [freetype2])
AC_SUBST(FREETYPE2_CFLAGS)
AC_SUBST(FREETYPE2_LIBS)

PKG_CHECK_MODULES(FONTCONFIG, [fontconfig])
AC_SUBST(FONTCONFIG_CFLAGS)
AC_SUBST(FONTCONFIG_LIBS)

AC_CONFIG_FILES([Makefile])
AC_CONFIG_SUBDIRS([pad/unix])
AC_OUTPUT
