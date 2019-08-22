cmake_minimum_required(VERSION 3.12)
project(Dynapad VERSION 1.0 LANGUAGES CXX)

find_package(PkgConfig REQUIRED)

if (UNIX AND NOT APPLE)
    set(LINUX TRUE)
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/Modules")

find_package(RacketCGC
  MODULE REQUIRED)

find_package(Freetype 
  MODULE REQUIRED)

find_package(X11 
  MODULE REQUIRED
  COMPONENTS Xi)

add_definitions(-DXSHM)

add_library(dynapad SHARED 
  dynapad.cpp
  builtin.cpp 
  tclSchemeNotify.c)

target_include_directories(dynapad
  PRIVATE pad/generic  # todo move to dep
  PRIVATE 
    ${RACKETCGC_INCLUDE_DIRS}
    ${X11_INCLUDE_DIR}
    ${FREETYPE_INCLUDE_DIRS}
)

target_link_libraries(dynapad
  PRIVATE
    ${RACKETCGC_LIBRARY_mzdyn}
    ${X11_LIBRARIES}
)