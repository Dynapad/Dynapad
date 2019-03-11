AUTOMAKE_OPTIONS = foreign

CFLAGS = --pedantic -Wall -std=c99 -O2
# LDFLAGS = 

noinst_LIBRARIES = libpad.a
libpad_a_SOURCES = x-win-shape.cpp \
                   shm.cpp \
                   ../generic/xMain.cpp \
                   ../generic/win.cpp \
                   ../generic/view.cpp \
                   ../generic/tree.cpp \
                   ../generic/tree-layout.cpp \
                   ../generic/transform.cpp \
                   ../generic/trait.cpp \
                   ../generic/tkwin.cpp \
                   ../generic/tkcolor.cpp \
                   ../generic/tkborder.cpp \
                   ../generic/text.cpp \
                   ../generic/slist.cpp \
                   ../generic/scrollbar.cpp \
                   ../generic/restorer.cpp \
                   ../generic/resource.cpp \
                   ../generic/renderer.cpp \
                   ../generic/portal.cpp \
                   ../generic/point.cpp \
                   ../generic/plist.cpp \
                   ../generic/pad.cpp \
                   ../generic/pad-string.cpp \
                   ../generic/object.cpp \
                   ../generic/o3.cpp \
                   ../generic/o2.cpp \
                   ../generic/noisedata.cpp \
                   ../generic/noise.cpp \
                   ../generic/misc.cpp \
                   ../generic/menu.cpp \
                   ../generic/list.cpp \
                   ../generic/line.cpp \
                   ../generic/line-font.cpp \
                   ../generic/line-font-data.cpp \
                   ../generic/init.cpp \
                   ../generic/imagemagick.cpp \
                   ../generic/imagedata.cpp \
                   ../generic/image.cpp \
                   ../generic/ilist.cpp \
                   ../generic/hashtab.cpp \
                   ../generic/group.cpp \
                   ../generic/global.cpp \
                   ../generic/gc.cpp \
                   ../generic/fontdata.cpp \
                   ../generic/fontcache.cpp \
                   ../generic/font.cpp \
                   ../generic/flist.cpp \
                   ../generic/events.cpp \
                   ../generic/effect.cpp \
                   ../generic/drawimage.cpp \
                   ../generic/display.cpp \
                   ../generic/curves.cpp \
                   ../generic/container.cpp \
                   ../generic/component.cpp \
                   ../generic/colordb.cpp \
                   ../generic/colorcube.cpp \
                   ../generic/color.cpp \
                   ../generic/clist.cpp \
                   ../generic/callback.cpp \
                   ../generic/button.cpp \
                   ../generic/border.cpp \
                   ../generic/bind.cpp \
                   ../generic/bbox.cpp \
                   ../generic/api.cpp \
                   ../generic/alist.cpp \
                   ../generic/alias.cpp \
                   ../generic/ZFontType1Interp.cpp \
                   ../generic/ZFontType1.cpp \
                   ../generic/ZFont.cpp \
                   ../generic/Buffer.cpp

libpad_a_CPPFLAGS = $(X11_CFLAGS) \
                    $(FREETYPE2_CFLAGS) \
                    $(MAGICKCORE_CFLAGS)
libpad_a_LIBADD = $(X11_LIBS) \
                  $(FREETYPE2_LIBS) \
                  $(MAGICKCORE_LIBS)
