/*
 * This object represents an Adobe Type 1 font which has been loaded
 * from a file.  It contains a constructor to read the file and methods
 * to generate Polygon, Spline, and Triangle Mesh font objects from
 * this font.
 */

#ifndef ZFontType1_h
#define ZFontType1_h


#include "defs.h"
#include "ZFont.h"
#include "Buffer.h"
#include <stdio.h>


#define MAXCHARS 	1000
#define MAXSUBRS 	1000
#define NASCII		224
#define NSYMBL		224
#define NZAPFD		224
#define NACCENT		14

#define ACCENTBASE	400

typedef struct pschar {
    char *name;
    int code;
} pschar;

class ZFontType1 {
friend class ZFontType1Interp;
friend class ZFontType1InterpPoly;
friend class ZFontType1InterpSpline;
public:
  ZFontType1(const char *infont);
  ZFontPoly *GenFontPoly(double beztol, Pad_Bool fullset);
  ZFontSpline *GenFontSpline(double beztol, Pad_Bool fullset);
  Pad_Bool Is_loaded(void);
private:
  Pad_Bool _loaded;
  void readfontmatrix(FILE *inf);
  void decode(FILE *inf);
  void readhex(FILE *inf, Buffer &hexdat);
  void decryptall();
  int decryptprogram(unsigned char *buf, int len);
  void resetdecrypt(int n);
  void decryptdata(unsigned char *iptr, unsigned char *optr, int n);

  void setcharlist();

  static Pad_Bool hextab_initialized_;
  static unsigned char hextab_[256];

  double mat_[2][2];
  char fname_[100];
  Buffer decoded_;

  int nsubrs_;
  unsigned int sublen_[MAXSUBRS];
  unsigned char *subrs_[MAXSUBRS];

  int nchars_;
  char *charname_[MAXCHARS];
  unsigned int charlen_[MAXCHARS];
  unsigned char *chars_[MAXCHARS];

  unsigned short mr_;		// Decryption state variable

  int charprog_[NASCII];
  int scharprog_[NSYMBL];
  int zcharprog_[NZAPFD];
  int accentprog_[NACCENT];

public:
  static const pschar charlist_[NASCII];
  static const pschar scharlist_[NSYMBL];
  static const pschar zcharlist_[NZAPFD];
  static const pschar accentlist_[NACCENT];
};

#endif
