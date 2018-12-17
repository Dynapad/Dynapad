bin_PROGRAMS = dt_xmouse
dt_xmouse_SOURCES = dt_xmouse.c
dt_xmouse_LDADD = ../libdt/libdt.a $(X_PRE_LIBS) -lX11 -lXtst $(X_EXTRA_LIBS)
dt_xmouse_LDFLAGS = $(X_LIBS) $(AM_LDFLAGS)
AM_CFLAGS = -ggdb -ansi -pedantic -Wall -Wstrict-prototypes -Wnested-externs -U__STRICT_ANSI__
AM_CPPFLAGS = -I../libdt $(X_CFLAGS)
