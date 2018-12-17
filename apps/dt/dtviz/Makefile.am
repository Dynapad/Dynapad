bin_PROGRAMS = dtviz
dtviz_SOURCES = dtviz.c
dtviz_LDADD = ../libdt/libdt.a $(XSSLIBS) $(X_PRE_LIBS) -lX11 -lXmu $(X_EXTRA_LIBS)
dtviz_LDFLAGS = $(X_LIBS) $(AM_LDFLAGS)
AM_CFLAGS = -ggdb -ansi -pedantic -Wall -Wstrict-prototypes -Wnested-externs -U__STRICT_ANSI__
AM_CPPFLAGS = -I../libdt -I$(xssdir) -I$(xssdir)/hacks -I$(xssdir)/utils $(X_CFLAGS)

XSSLIBS = screenhack.o $(XSSUTILS)
XSSUTILS = resources.o visual.o usleep.o yarandom.o hsv.o colors.o xdbe.o

screenhack.o:
	$(MAKE) -C $(xssdir)/hacks $@
	cp $(xssdir)/hacks/$@ .

$(XSSUTILS):
	$(MAKE) -C $(xssdir)/utils $@
	cp $(xssdir)/utils/$@ .
