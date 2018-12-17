//========================================================================
//
// ImageOutputDev.cc
//
// Copyright 1998 Derek B. Noonburg
//
//========================================================================

#ifdef __GNUC__
#pragma implementation
#endif

#include <aconf.h>
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include <math.h>
#include "gmem.h"
#include "config.h"
#include "Error.h"
#include "GfxState.h"
#include "Object.h"
#include "Stream.h"
#include "ImageOutputDev.h"

ImageOutputDev::ImageOutputDev(char *fileRootA, GBool dumpJPEGA) {
  fileRoot = copyString(fileRootA);
  fileName = (char *)gmalloc(strlen(fileRoot) + 20);
  dumpJPEG = dumpJPEGA;
  imgNum = 0;
  ok = gTrue;
}

ImageOutputDev::~ImageOutputDev() {
  gfree(fileName);
  gfree(fileRoot);
}

void ImageOutputDev::drawImageMask(GfxState *state, Object *ref, Stream *str,
				   int width, int height, GBool invert,
				   GBool inlineImg) {
  FILE *f;
  int c;
  int size, i;

  // dump JPEG file
  if (dumpJPEG && str->getKind() == strDCT && !inlineImg) {

    // open the image file
    sprintf(fileName, "%s-%03d.jpg", fileRoot, imgNum);
    ++imgNum;
    if (!(f = fopen(fileName, "wb"))) {
      error(-1, "Couldn't open image file '%s'", fileName);
      return;
    }

    // initialize stream
    str = ((DCTStream *)str)->getRawStream();
    str->reset();

    // copy the stream
    while ((c = str->getChar()) != EOF)
      fputc(c, f);

    str->close();
    fclose(f);

  // dump PBM file
  } else {

    // open the image file and write the PBM header
    sprintf(fileName, "%s-%03d.pbm", fileRoot, imgNum);
    ++imgNum;
    if (!(f = fopen(fileName, "wb"))) {
      error(-1, "Couldn't open image file '%s'", fileName);
      return;
    }
    fprintf(f, "P4\n");
    fprintf(f, "%d %d\n", width, height);

    // initialize stream
    str->reset();

    // copy the stream
    size = height * ((width + 7) / 8);
    for (i = 0; i < size; ++i) {
      fputc(str->getChar(), f);
    }

    str->close();
    fclose(f);
  }
}

void ImageOutputDev::drawImage(GfxState *state, Object *ref, Stream *str,
			       int width, int height,
			       GfxImageColorMap *colorMap,
			       int *maskColors, GBool inlineImg) {
  FILE *f;
  ImageStream *imgStr;
  int nComps, nVals, nBits;
  GBool dither;
  Guchar pixBuf[4];
  GfxRGB rgb;
  int x, y;
  int c;
  int row, size, i, j;
  double *ctm;
  Guchar *p, *pxmBuf;

  // image parameters
  nComps = colorMap->getNumPixelComps();
  nVals = width * nComps;
  nBits = colorMap->getBits();
  dither = nComps > 1 || nBits > 1;

  // get CTM, check for singular matrix
  ctm = state->getCTM();
  if (fabs(ctm[0] * ctm[3] - ctm[1] * ctm[2]) < 0.000001) {
    error(-1, "Singular CTM in drawImage");
    if (inlineImg) {
      str->reset();
      j = height * ((nVals * nBits + 7) / 8);
      for (i = 0; i < j; ++i) {
	str->getChar();
      }
      str->close();
    }
    return;
  }

  // dump JPEG file
  if (dumpJPEG && str->getKind() == strDCT &&
      colorMap->getNumPixelComps() == 3 &&
      !inlineImg) {

    // open the image file
    sprintf(fileName, "%s-%03d.jpg", fileRoot, imgNum);
    ++imgNum;
    if (!(f = fopen(fileName, "wb"))) {
      error(-1, "Couldn't open image file '%s'", fileName);
      return;
    }

    // initialize stream
    str = ((DCTStream *)str)->getRawStream();
    str->reset();

    // copy the stream
    while ((c = str->getChar()) != EOF)
      fputc(c, f);

    str->close();
    fclose(f);

  // dump PBM file
  } else if (colorMap->getNumPixelComps() == 1 &&
	     colorMap->getBits() == 1) {

    int widthbytes = (width + 7)/8;
    size = height * widthbytes;
    pxmBuf = (Guchar *)gmalloc(size);

    // initialize stream
    str->reset();

    // for each line...
    for (y = 0; y < height; ++y) {

      row = ctm[3]<0 ? y : (height-1-y);
      p = &pxmBuf[widthbytes*row];
      // write the line
      for (x = 0; x < widthbytes; ++x) {
	*p++ = str->getChar();
      }
    }
    str->close();

    // open the image file and write the PBM header
    sprintf(fileName, "%s-%03d.pbm", fileRoot, imgNum);
    ++imgNum;
    if (!(f = fopen(fileName, "wb"))) {
      error(-1, "Couldn't open image file '%s'", fileName);
      return;
    }
    fprintf(f, "P4\n");
    fprintf(f, "%d %d\n", width, height);
    fwrite(pxmBuf, size, 1, f);
    fclose(f);

    gfree(pxmBuf);

  // dump PPM file
  } else {

    size = 3 * width * height;
    pxmBuf = (Guchar *)gmalloc(size);

    // initialize stream
    imgStr = new ImageStream(str, width, colorMap->getNumPixelComps(),
			     colorMap->getBits());
    imgStr->reset();

    // for each line...
    for (y = 0; y < height; ++y) {

      row = ctm[3]<0 ? y : (height-1-y);
      p = &pxmBuf[3*width*row];
      // write the line
      for (x = 0; x < width; ++x) {
	imgStr->getPixel(pixBuf);
	colorMap->getRGB(pixBuf, &rgb);
	*p++ = (int)(rgb.r * 255 + 0.5);
	*p++ = (int)(rgb.g * 255 + 0.5);
	*p++ = (int)(rgb.b * 255 + 0.5);
      }
    }
    delete imgStr;

    // open the image file and write the PPM header
    sprintf(fileName, "%s-%03d.ppm", fileRoot, imgNum);
    ++imgNum;
    if (!(f = fopen(fileName, "wb"))) {
      error(-1, "Couldn't open image file '%s'", fileName);
      return;
    }
    fprintf(f, "P6\n");
    fprintf(f, "%d %d\n", width, height);
    fprintf(f, "255\n");
    fwrite(pxmBuf, size, 1, f);
    fclose(f);

    gfree(pxmBuf);
  }
}
