cmake_minimum_required(VERSION 3.14)
project(Pad++ VERSION 1.0 LANGUAGES CXX)

if (APPLE)
  list(APPEND CMAKE_PREFIX_PATH "/usr/local/opt/imagemagick@6")
endif()

find_package(X11 
  MODULE REQUIRED
  COMPONENTS Xi Xext Xft)

find_package(ImageMagick 6
  MODULE REQUIRED
  COMPONENTS MagickCore)

find_package(TCL 
  MODULE REQUIRED)

find_package(Freetype 
  MODULE REQUIRED)

find_package(Fontconfig 
  MODULE REQUIRED)

find_package(BerkeleyDB
  MODULE REQUIRED)

add_library(pad STATIC
  "unix/x-win-shape.cpp"
  "unix/shm.cpp"
  "generic/win.cpp"
  "generic/view.cpp"
  "generic/tree.cpp"
  "generic/tree-layout.cpp"
  "generic/transform.cpp"
  "generic/trait.cpp"
  "generic/tkwin.cpp"
  "generic/tkcolor.cpp"
  "generic/tkborder.cpp"
  "generic/text.cpp"
  "generic/slist.cpp"
  "generic/scrollbar.cpp"
  "generic/restorer.cpp"
  "generic/resource.cpp"
  "generic/renderer.cpp"
  "generic/portal.cpp"
  "generic/point.cpp"
  "generic/plist.cpp"
  "generic/pad.cpp"
  "generic/pad-string.cpp"
  "generic/object.cpp"
  "generic/o3.cpp"
  "generic/o2.cpp"
  "generic/noisedata.cpp"
  "generic/noise.cpp"
  "generic/misc.cpp"
  "generic/menu.cpp"
  "generic/list.cpp"
  "generic/line.cpp"
  "generic/line-font.cpp"
  "generic/line-font-data.cpp"
  "generic/init.cpp"
  "generic/imagemagick.cpp"
  "generic/imagedata.cpp"
  "generic/image.cpp"
  "generic/ilist.cpp"
  "generic/hashtab.cpp"
  "generic/group.cpp"
  "generic/global.cpp"
  "generic/gc.cpp"
  "generic/fontdata.cpp"
  "generic/fontcache.cpp"
  "generic/font.cpp"
  "generic/flist.cpp"
  "generic/events.cpp"
  "generic/effect.cpp"
  "generic/drawimage.cpp"
  "generic/display.cpp"
  "generic/curves.cpp"
  "generic/container.cpp"
  "generic/component.cpp"
  "generic/colordb.cpp"
  "generic/colorcube.cpp"
  "generic/color.cpp"
  "generic/clist.cpp"
  "generic/callback.cpp"
  "generic/button.cpp"
  "generic/border.cpp"
  "generic/bind.cpp"
  "generic/bbox.cpp"
  "generic/api.cpp"
  "generic/alist.cpp"
  "generic/alias.cpp"
  "generic/ZFontType1Interp.cpp"
  "generic/ZFontType1.cpp"
  "generic/ZFont.cpp"
  "generic/Buffer.cpp")

target_include_directories(pad
  PRIVATE 
    ${X11_INCLUDE_DIR}
    ${ImageMagick_INCLUDE_DIRS}
    ${FREETYPE_INCLUDE_DIRS}
    ${Fontconfig_INCLUDE_DIRS}
    ${TCL_INCLUDE_PATH}
    ${TK_INCLUDE_PATH}
    ${BerkeleyDB_INCLUDE_DIRS})

target_link_libraries(pad
  PRIVATE
    ${X11_LIBRARIES}
    ${X11_Xft_LIB}
    ${X11_Xi_LIB}
    ${ImageMagick_LIBRARIES}
    ${FREETYPE_LIBRARIES}
    ${Fontconfig_LIBRARIES}
    ${BerkeleyDB_LIBRARY})