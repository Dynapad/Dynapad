/*
 * mzc --cc im.c
 * mzc ++ldl -lMagick --ld im.so im.o
 * 
 * mzc.exe -I/home/plt/include -c im.c
 * mzc.exe -shared -o im.so im.o /home/plt/lib/gcc/mzdyn.o \
 *    /home/plt/lib/gcc/libmzsch206_001.lib -lMagick
 */

#include "escheme.h"

#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <magick/api.h>

#include <unistd.h>

#define SCH_UNIXSTR(arg) \
  SCHEME_BYTE_STR_VAL(scheme_char_string_to_byte_string(arg))

#define UNIX_SCHSTR(arg) \
  scheme_byte_string_to_char_string(scheme_make_byte_string(arg))

static  Image *image, *result_image;
static  ExceptionInfo exception;
    
static ImageInfo image_info;
  
Scheme_Object *
sch_resize(int argc, Scheme_Object **argv)
{
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("im/resize", "src filename", 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_type("im/resize", "dst filename", 1, argc, argv);
  if (!SCHEME_INTP(argv[2]))
    scheme_wrong_type("im/resize", "rows", 2, argc, argv);
  if (!SCHEME_INTP(argv[3]))
    scheme_wrong_type("im/resize", "cols", 3, argc, argv);

  char *src = SCH_UNIXSTR(argv[0]);
  char *dst = SCH_UNIXSTR(argv[1]);
  int rows = SCHEME_INT_VAL(argv[2]);
  int cols = SCHEME_INT_VAL(argv[2]);

  if (access(src, R_OK)) {
    scheme_warning("im/resize: source file ~s doesn't exist or isn't readable", src);
    return scheme_false;
  }

  if (!access(dst, F_OK)) {
    scheme_warning("im/resize: destination file ~s already exists", dst);
    return scheme_false;
  }
  /*
    Initialize the image info structure and read an image.
  */
  GetExceptionInfo(&exception);
  GetImageInfo(&image_info);
  (void) strcpy(image_info.filename,src);
  image=ReadImage(&image_info, &exception);
  if (image == (Image *) NULL)
    return scheme_false;
  /*
    Resize the image.
  */
  /*result_image=ScaleImage(image,125,125,&exception);*/
  /*result_image=ResizeImage(image, rows, cols, LanczosFilter, 0, &exception);*/
  image->filter = LanczosFilter;
  result_image=ZoomImage(image, rows, cols, &exception);
  if (result_image != (Image *) NULL)
    {
      DestroyImage(image);
      image=result_image;
    }
  /*
    Write the image and destroy it.
  */
  (void) strcpy(image->filename,dst);
  WriteImage(&image_info,image);
  DestroyImage(image);

  return scheme_true;
}

Scheme_Object *
sch_rotate(int argc, Scheme_Object **argv)
{
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("im/rotate", "src filename", 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_type("im/rotate", "dst filename", 1, argc, argv);
  if (!SCHEME_REALP(argv[2]))
    scheme_wrong_type("im/rotate", "angle", 2, argc, argv);

  char *src = SCH_UNIXSTR(argv[0]);
  char *dst = SCH_UNIXSTR(argv[1]);
  double angle = scheme_real_to_double(argv[2]);

  if (access(src, R_OK)) {
    scheme_warning("im/rotate: source file ~s doesn't exist or isn't readable", src);
    return scheme_false;
  }

  if (!access(dst, F_OK)) {
    scheme_warning("im/rotate: destination file ~s already exists", dst);
    return scheme_false;
  }
  /*
    Initialize the image info structure and read an image.
  */
  GetExceptionInfo(&exception);
  GetImageInfo(&image_info);
  (void) strcpy(image_info.filename,src);
  image=ReadImage(&image_info, &exception);
  if (image == (Image *) NULL)
    return scheme_false;
  /*
    Rotate the image.
  */
  result_image=RotateImage(image, angle, &exception);
  if (result_image != (Image *) NULL)
    {
      DestroyImage(image);
      image=result_image;
    }
  /*
    Write the image and destroy it.
  */
  (void) strcpy(image->filename,dst);
  WriteImage(&image_info,image);
  DestroyImage(image);

  return scheme_true;
}

Scheme_Object *
sch_flip(int argc, Scheme_Object **argv)
{
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("im/flip", "src filename", 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_type("im/flip", "dst filename", 1, argc, argv);

  char *src = SCH_UNIXSTR(argv[0]);
  char *dst = SCH_UNIXSTR(argv[1]);

  if (access(src, R_OK)) {
    scheme_warning("im/flip: source file ~s doesn't exist or isn't readable", src);
    return scheme_false;
  }

  if (!access(dst, F_OK)) {
    scheme_warning("im/flip: destination file ~s already exists", dst);
    return scheme_false;
  }
  /*
    Initialize the image info structure and read an image.
  */
  GetExceptionInfo(&exception);
  GetImageInfo(&image_info);
  (void) strcpy(image_info.filename,src);
  image=ReadImage(&image_info, &exception);
  if (image == (Image *) NULL)
    return scheme_false;
  /*
    Flip the image.
  */
  result_image=FlipImage(image, &exception);
  if (result_image != (Image *) NULL)
    {
      DestroyImage(image);
      image=result_image;
    }
  /*
    Write the image and destroy it.
  */
  (void) strcpy(image->filename,dst);
  WriteImage(&image_info,image);
  DestroyImage(image);

  return scheme_true;
}

Scheme_Object *
sch_flop(int argc, Scheme_Object **argv)
{
  if (!SCHEME_CHAR_STRINGP(argv[0]))
    scheme_wrong_type("im/flop", "src filename", 0, argc, argv);
  if (!SCHEME_CHAR_STRINGP(argv[1]))
    scheme_wrong_type("im/flop", "dst filename", 1, argc, argv);

  char *src = SCH_UNIXSTR(argv[0]);
  char *dst = SCH_UNIXSTR(argv[1]);

  if (access(src, R_OK)) {
    scheme_warning("im/flop: source file ~s doesn't exist or isn't readable", src);
    return scheme_false;
  }

  if (!access(dst, F_OK)) {
    scheme_warning("im/flop: destination file ~s already exists", dst);
    return scheme_false;
  }
  /*
    Initialize the image info structure and read an image.
  */
  GetExceptionInfo(&exception);
  GetImageInfo(&image_info);
  (void) strcpy(image_info.filename,src);
  image=ReadImage(&image_info, &exception);
  if (image == (Image *) NULL)
    return scheme_false;
  /*
    Flop the image.
  */
  result_image=FlopImage(image, &exception);
  if (result_image != (Image *) NULL)
    {
      DestroyImage(image);
      image=result_image;
    }
  /*
    Write the image and destroy it.
  */
  (void) strcpy(image->filename,dst);
  WriteImage(&image_info,image);
  DestroyImage(image);

  return scheme_true;
}

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  InitializeMagick("mzscheme");

  Scheme_Env *mod_env =
    scheme_primitive_module(scheme_intern_symbol("imagemagick"), env);

  scheme_add_global("im/resize",
    scheme_make_prim_w_arity(sch_resize, "im/resize", 4, 4), mod_env);
  scheme_add_global("im/rotate",
    scheme_make_prim_w_arity(sch_rotate, "im/rotate", 3, 3), mod_env);
  scheme_add_global("im/flip",
    scheme_make_prim_w_arity(sch_flip, "im/flip", 2, 2), mod_env);
  scheme_add_global("im/flop",
    scheme_make_prim_w_arity(sch_flop, "im/flop", 2, 2), mod_env);

  scheme_finish_primitive_module(mod_env);

  return UNIX_SCHSTR("ImageMagick");
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  /* This extension does define a module: */
  return scheme_intern_symbol("imagemagick");
}
