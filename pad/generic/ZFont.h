#ifndef ZFont_h
#define ZFont_h

#include "slist.h"

#define OFMAGIC		0x93339333

#define TM_TYPE		1
#define PO_TYPE		2
#define SP_TYPE		3

/* ops for tmesh characters */

#define	TM_BGNTMESH	(1)
#define	TM_SWAPTMESH	(2)
#define	TM_ENDBGNTMESH	(3)
#define	TM_RETENDTMESH	(4)
#define	TM_RET		(5)

/* ops for poly characters */

#define	PO_BGNLOOP	(1)
#define	PO_ENDBGNLOOP	(2)
#define	PO_RETENDLOOP	(3)
#define	PO_RET		(4)

/* ops for spline  characters */

#define	SP_MOVETO	(1)
#define	SP_LINETO	(2)
#define	SP_CURVETO	(3)
#define	SP_CLOSEPATH	(4)
#define	SP_RETCLOSEPATH	(5)
#define	SP_RET		(6)

#define MIN_ASCII 	' '
#define MAX_ASCII 	'~'

class chardesc {
public:
  short movex, movey;		/* advance */
  short llx, lly;		/* bounding box */
  short urx, ury;
  Pad_SList data;		/* char data */
};

typedef void (*pfss)(short, short, float);


class ZFont {
  friend class ZFontType1Interp;
  friend class ZFontType1;
public:
  virtual void Run(int code, pfss f, float theta=0.0) = 0;
  short char_width(unsigned char c) {
    return getchardesc(c) ? getchardesc(c)->movex : 0;}
  short char_height(unsigned char c) {
      chardesc *cd = getchardesc(c);
      if (cd)
	return (cd->ury - cd->lly);
      else
	return 0;
  }
  int char_bbox(unsigned char c, 
                 short &xmin, short &ymin, short &xmax, short &ymax) {
      chardesc *cd = getchardesc(c);
      if (cd) {
          xmin = cd->llx;
          ymin = cd->lly;
          xmax = cd->urx;
          ymax = cd->ury;
          return 1;
      } else {
          xmin = xmax = ymin = ymax = 0;
          return 0;
      }
  }

protected:
public:
  ZFont(int cmin, int cmax, int fs);
  virtual ~ZFont();
  void fakechar(int c, int width);
  void addchardata(int c, const short *data, int nshorts);
  void addcharmetrics(int c, int movex, int movey);
  int chartoindex(int c);
  chardesc *getchardesc(int c);
  const short *chardata(int c);
  const char *asciiname(int c);

  void calccharbboxes();

  char *name_;
  short charmin_;
  short charmax_;
  short nchars_;
  short scale_;
  chardesc *chars_;
};

class ZFontPoly : public ZFont {
  friend class ZFontType1Interp;
  friend class ZFontType1;
public:
  virtual void Run(int code, pfss f, float theta=0.0);
  void print();
  int printchar(int c);
private:
  ZFontPoly(int cmin, int cmax, int fscale)
    : ZFont(cmin, cmax, fscale) {}
};

class ZFontSpline : public ZFont {
  friend class ZFontType1Interp;
  friend class ZFontType1;
public:
  virtual void Run(int code, pfss f, float theta=0.0);
private:
  ZFontSpline(int cmin, int cmax, int fscale)
    : ZFont(cmin, cmax, fscale) {}
};

#endif
