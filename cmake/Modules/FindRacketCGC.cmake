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
find_package_handle_standard_args(RacketCGC
  FOUND_VAR RACKETCGC_FOUND
  REQUIRED_VARS
    RACKETCGC_INCLUDE_DIR_escheme
    RACKETCGC_LIBRARY_mzdyn
  VERSION_VAR RACKETCGC_VERSION
)

if (RACKETCGC_FOUND)
  set(RACKETCGC_INCLUDE_DIRS ${RACKETCGC_INCLUDE_DIR_escheme})
  set(RACKETCGC_LIBRARIES ${RACKETCGC_LIBRARY_mzdyn})
  # set(RACKETCGC_DEFINITIONS ${PC_Foo_CFLAGS_OTHER})


  # These are the symbols that will be undefined at compile-time.
  # MacOS is a lot more picky about these, but we don't want to disable 
  # complaining about missing symbols globally, so we list them here.
  set(RACKETCGC_DYNAMIC_SYMBOLS
    "scheme_add_global"
    "scheme_append"
    "scheme_byte_string_to_char_string"
    "scheme_char_string_to_byte_string"
    "scheme_current_config"
    "scheme_dont_gc_ptr"
    "scheme_gc_ptr_ok"
    "scheme_expand_filename"
    "scheme_eval"
    "scheme_check_proc_arity"
    "scheme_get_env"
    "scheme_intern_symbol"
    "scheme_is_list"
    "scheme_list_length"
    "scheme_make_byte_string"
    "scheme_make_cptr"
    "scheme_make_float"
    "scheme_make_pair"
    "scheme_make_path"
    "scheme_make_prim_w_arity"
    "scheme_make_struct_instance"
    "scheme_true"
    "scheme_false"
    "scheme_null"
    "scheme_real_to_double"
    "scheme_thread_local_key"
    "scheme_thread_local_offset"
    "scheme_warning"
    "scheme_wrong_count"
    "scheme_wrong_type")
endif()

mark_as_advanced(
  RACKETCGC_INCLUDE_DIR_escheme
  RACKET_LIBRARY_mzdyn
  RACKETCGC_DYNAMIC_SYMBOLS
)