cmake_minimum_required(VERSION 3.14)
project(Dynapad VERSION 1.0 LANGUAGES CXX C)

option (FORCE_COLORED_OUTPUT "Always produce ANSI-colored output (GNU/Clang only)." TRUE)
if (${FORCE_COLORED_OUTPUT})
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
       add_compile_options (-fdiagnostics-color=always)
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
       add_compile_options (-fcolor-diagnostics)
    endif()
endif()

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "AppleClang")
#    set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=address ")
#    set (CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_LINKER_FLAGS_DEBUG} -fno-omit-frame-pointer -fsanitize=address")

#    set (CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=undefined")
#    set (CMAKE_LINKER_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -fsanitize=undefined")
endif()

if (UNIX AND NOT APPLE)
    set(LINUX TRUE)
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules")

find_package(PkgConfig REQUIRED)

find_package(RacketCGC MODULE REQUIRED)

find_package(ImageMagick 6 EXACT MODULE REQUIRED)

find_package(X11 MODULE REQUIRED)

find_package(Freetype MODULE REQUIRED)

find_package(TCL MODULE REQUIRED)

find_package(BerkeleyDB MODULE REQUIRED)

add_definitions(-DXSHM)

add_subdirectory(pad)

add_library(dynapad SHARED
  "extensions/dynapad.cpp"
  "extensions/tclSchemeNotify.c")

target_include_directories(dynapad
  PRIVATE pad/generic  # todo move to dep
  PRIVATE 
    ${RacketCGC_INCLUDE_DIRS}
    ${X11_INCLUDE_DIR}
    ${FREETYPE_INCLUDE_DIRS}
    ${TCL_INCLUDE_PATH}
    ${TK_INCLUDE_PATH}
    ${BerkeleyDB_INCLUDE_DIRS})

target_link_libraries(dynapad
  PRIVATE
    pad
    ${RacketCGC_LIBRARY_mzdyn}
    ${BerkeleyDB_LIBRARIES}
    ${X11_LIBRARIES})

set(DYNAPAD_LINK_OPTIONS)

if (APPLE)
  foreach(SYMBOL IN ITEMS ${RacketCGC_DYNAMIC_SYMBOLS})
    list(APPEND DYNAPAD_LINK_OPTIONS "LINKER:-U,_${SYMBOL}")
  endforeach()
endif()

target_link_options(dynapad
  PRIVATE
    ${DYNAPAD_LINK_OPTIONS}
)
