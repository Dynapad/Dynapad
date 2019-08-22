find_path(RACKETCGC_INCLUDE_DIR_escheme
    NAMES escheme.h
    PATHS /usr/local/opt/racket/include
          /usr/local/include
    PATH_SUFFIXES racket)

list(APPEND CMAKE_FIND_LIBRARY_SUFFIXES ".o")
find_library(RACKETCGC_LIBRARY_mzdyn
    NAMES mzdyn.o
    PATHS /usr/local/opt/racket/lib
          /usr/local/lib
    PATH_SUFFIXES racket)

# set(RACKETCGC_VERSION ...)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(RACKETCGC
  FOUND_VAR RACKETCGC_FOUND
  REQUIRED_VARS
    RACKETCGC_INCLUDE_DIR_escheme
    RACKETCGC_LIBRARY_mzdyn
  VERSION_VAR RACKETCGC_VERSION
)

set(RACKETCGC_DYNAMIC_SYMBOLS)

if (RACKETCGC_FOUND)
    set(RACKETCGC_INCLUDE_DIRS ${RACKETCGC_INCLUDE_DIR_escheme})
    set(RACKETCGC_LIBRARIES ${RACKETCGC_LIBRARY_mzdyn})
    set(RACKETCGC_DEFINITIONS ${PC_Foo_CFLAGS_OTHER})
endif()

mark_as_advanced(
  RACKETCGC_INCLUDE_DIR_escheme
  RACKET_LIBRARY_mzdyn
  RACKETCGC_DYNAMIC_SYMBOLS
)