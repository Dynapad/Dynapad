option (RACKET_DIR "Path to racket [cgc] installation." OFF)

find_path(RacketCGC_INCLUDE_DIR_escheme
    NAMES escheme.h
    PATHS "${RACKET_DIR}/include"
          "/usr/local/opt/racket/include"
          "/usr/local/include"
    PATH_SUFFIXES racket)

list(APPEND CMAKE_FIND_LIBRARY_SUFFIXES ".o")
find_library(RacketCGC_LIBRARY_mzdyn
    NAMES mzdyn.o
    PATHS "${RACKET_DIR}/lib"
          "/usr/local/opt/racket/lib"
          "/usr/local/lib"
          "/usr/lib64/racket"
    PATH_SUFFIXES racket)

# set(RacketCGC_VERSION ...)

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(RacketCGC
  FOUND_VAR RacketCGC_FOUND
  REQUIRED_VARS
    RacketCGC_INCLUDE_DIR_escheme
    RacketCGC_LIBRARY_mzdyn
  VERSION_VAR RacketCGC_VERSION
)

if (RacketCGC_FOUND)
  set(RacketCGC_INCLUDE_DIRS ${RacketCGC_INCLUDE_DIR_escheme})
  set(RacketCGC_LIBRARIES ${RacketCGC_LIBRARY_mzdyn})
  # set(RacketCGC_DEFINITIONS ${PC_Foo_CFLAGS_OTHER})


  # These are the symbols that will be undefined at compile-time.
  # MacOS is a lot more picky about these, but we don't want to disable 
  # complaining about missing symbols globally, so we list them here.
  set(RacketCGC_DYNAMIC_SYMBOLS
    "scheme_add_global"
    "scheme_apply"
    "scheme_append"
    "scheme_block_until"
    "scheme_byte_string_to_char_string"
    "scheme_char_string_to_byte_string"
    "scheme_check_proc_arity"
    "scheme_current_config"
    "scheme_dont_gc_ptr"
    "scheme_eval"
    "scheme_expand_filename"
    "scheme_false"
    "scheme_fdset"
    "scheme_finish_primitive_module"
    "scheme_gc_ptr_ok"
    "scheme_get_env"
    "scheme_get_fdset"
    "scheme_initialize"
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
    "scheme_making_progress"
    "scheme_null"
    "scheme_primitive_module"
    "scheme_real_to_double"
    "scheme_thread_block"
    "scheme_thread_local_key"
    "scheme_thread_local_offset"
    "scheme_true"
    "scheme_void"
    "scheme_warning"
    "scheme_wrong_count"
    "scheme_wrong_type")
endif()

mark_as_advanced(
  RacketCGC_INCLUDE_DIR_escheme
  RACKET_LIBRARY_mzdyn
  RacketCGC_DYNAMIC_SYMBOLS
)
