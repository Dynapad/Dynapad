AUTOMAKE_OPTIONS = foreign
ACLOCAL_AMFLAGS = -I m4

SUBDIRS = pad/unix
PAD_CPPFLAGS = -Wall -Werror -I/pad/generic

lib_LTLIBRARIES = libdynapad.la
libdynapad_la_SOURCES = dynapad.cpp builtin.cpp tclSchemeNotify.c
libdynapad_la_CPPFLAGS = -Ipad/generic/ \
                   $(PLT_CFLAGS) \
                   $(MAGICKCORE_CFLAGS) \
                   $(X11_CFLAGS) \
                   $(FREETYPE2_CFLAGS) \
                   $(FONTCONFIG_CFLAGS)
libdynapad_la_LDFLAGS = $(PLT_LIBS) \
                  $(MAGICKCORE_LIBS) \
                  $(X11_LIBS) \
                  $(FREETYPE2_LIBS) \
                  $(FONTCONFIG_LIBS) \
                  -L/usr/local/opt/berkeley-db/lib -ldb
libdynapad_la_LIBADD = pad/unix/libpad.a