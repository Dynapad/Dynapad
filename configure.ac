AC_PREREQ([2.69])
AC_INIT([Dynapad],[1.0])
AC_CONFIG_SRCDIR([dynapad.cpp])
AM_INIT_AUTOMAKE

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

PKG_CHECK_MODULES(MAGICKCORE, [MagickCore])
AC_SUBST(MAGICKCORE_CFLAGS)
AC_SUBST(MAGICKCORE_LIBS)

# AC_SUBST(MAGICKH)
# AC_SUBST(MAGICKLIB)

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

PKG_CHECK_MODULES(freetype2, [freetype2])
AC_SUBST(FREETYPE2_CFLAGS)
AC_SUBST(FREETYPE2_LIBS)


AC_CONFIG_FILES([makefile])
AC_OUTPUT
[(cd pad/unix; ./configure)]
