AUTOMAKE_OPTIONS = foreign

SUBDIRS = pad/unix
PAD_CPPFLAGS = -I/pad/generic

bin_PROGRAMS = dynapad
dynapad_SOURCES = dynapad.cpp
dynapad_CPPFLAGS = -Ipad/generic/ \
                   $(PLT_CFLAGS) \
                   $(MAGICKCORE_CFLAGS) \
                   $(X11_CFLAGS) \
                   $(FREETYPE2_CFLAGS)
dynapad_LDFLAGS = $(PLT_LDFLAGS) \
                  $(MAGICKCORE_LIBS) \
                  $(X11_LIBS) \
                  $(FREETYPE2_LIBS)
dynapad_LDADD = pad/libpad.a