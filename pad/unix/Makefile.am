AUTOMAKE_OPTIONS = foreign

CFLAGS = --pedantic -Wall -std=c99 -O2
# LDFLAGS = 

noinst_LIBRARIES = libpad.a
libpad_a_SOURCES = tkAppInit.cpp \
                   x-win-shape.cpp \
                   shm.cpp \
                   ../generic/*.cpp

libpad_a_CPPFLAGS = $(X11_CFLAGS) \
                    $(TCLTK_CFLAGS)
libpad_a_LIBADD = $(X11_LIBS) \
                  $(TCLTK_LIBS)
