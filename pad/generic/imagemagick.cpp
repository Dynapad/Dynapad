#include "defs.h"
#include "imagemagick.h"

#include <stdio.h>
#include <sys/types.h>
#include <magick/api.h>
#include <iostream>
using namespace std;

void
Pad_InitIM()
{
  static int initIM = 1;

  if (initIM) {
    InitializeMagick(NULL);
    initIM = 0;
  }
}

unsigned long *
Pad_ReadIM(FILE *fp, int* ret_width, int* ret_height)
{
  Image *image;
  ExceptionInfo exception;
  ImageInfo *image_info;

  unsigned long *data = NULL;

  GetExceptionInfo(&exception);
  image_info=CloneImageInfo((ImageInfo *) NULL);
  GetImageInfo(image_info);

  image_info->file = fp;
  image = ReadImage(image_info, &exception);
  if (image) {
    /*fprintf(stderr, "%d %d %d\n", image->columns, image->rows, image->depth);*/
    *ret_width = image->columns;
    *ret_height= image->rows;
    unsigned long *outptr;
    data = new unsigned long[image->columns * image->rows];
    outptr = data;
    const PixelPacket *q;
    int r, g, b, a;
    for (int row = 0; row < image->rows; row++) {
      q = AcquireImagePixels(image, 0, row, image->columns, 1, &exception);
      for (int col = 0; col < image->columns; col++, q++, outptr++) {
	r = (q->red*255/MaxRGB);
	g = (q->green*255/MaxRGB);
	b = (q->blue*255/MaxRGB);
	a = 0;
	*outptr = r | (g << 8) | (b << 16) | (a << 24);
      }
    }
    DestroyImage(image);
    DestroyImageInfo(image_info);
    DestroyExceptionInfo(&exception);
  } else {
    *ret_width = *ret_height = 0;
    fprintf(stderr, "ImageMagick exception - %s - %s\n",
      exception.reason, exception.description);
  }
  return data;
}

unsigned long *
Pad_LoadIM(unsigned char *formattedData, int len)
{
  fprintf(stderr, "NOT IMPLEMENTED Pad_LoadIM(unsigned char *formattedData, int len)\n");
  return NULL;
}

unsigned long *
Pad_LoadIM(char *filename, int* ret_width, int* ret_height)
{
  FILE *fp = fopen(filename, "rb");
  unsigned long *data = Pad_ReadIM(fp, ret_width, ret_height);
  fclose(fp);
  return data;
}

Pad_Bool
Pad_Imagep(char *filename)
{
  Image *image;
  ExceptionInfo exception;
  ImageInfo *image_info;
  Pad_Bool result;

  GetExceptionInfo(&exception);
  image_info=CloneImageInfo((ImageInfo *) NULL);
  GetImageInfo(image_info);

  strcpy(image_info->filename, filename);
  image = PingImage(image_info, &exception);

  result = image ? TRUE : FALSE;

  if (image) DestroyImage(image);
  DestroyImageInfo(image_info);
  DestroyExceptionInfo(&exception);

  return result;
}
