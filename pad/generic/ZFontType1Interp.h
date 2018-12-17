#ifndef ZFontType1Interp_h
#define ZFontType1Interp_h

#include "ZFont.h"
#include "alist.h"
#include "ilist.h"
#include "defs.h"

class ZFont;
class ZFontType1;

typedef double matrix[2][2];

class ZFontType1Interp {
public:
  ZFontType1Interp(ZFontType1 &, double beztol);
  void reset(ZFont *f, int c);
  int nshorts();
  //const short *chardata();
  void beginchar(int c);
  void set_offset(int xo, int yo);
  void endchar();
  void drawchar(int c, int n, ZFont &f);
  void runprog(ZFont &f);
  Pad_Bool docommand(ZFont &f, int cmd);
  void rmoveto(Pad_Bool incusp, int x, int y);
  void rlineto(matrix &mat, int x, int y);
  void rcurveto(int dx1, int dy1, int dx2, int dy2,
		int dx3, int dy3, double beztol, matrix &mat);
  void closepath(matrix &mat);
  void savestart(int x, int y);
  int width(matrix &mat, int x);
  void sbpoint(int x, int y);
  void seac(ZFont &c, int /*asb*/, int adx, int ady, int bchar, int achar);
  void drawchar_seac(ZFont &cd, int c, int accentflag);
  void subr0(ZFont &c);
  void subr1();
  void subr2(ZFont &c);
  void getpos(int *x, int *y);
  void getmove(int *x, int *y);

  virtual void drawline(double x0, double y0, double x1, double y1, 
			double, double, double, double, matrix &mat) = 0;
  virtual void drawbez(double x0, double y0, double x1, double y1, 
		       double x2, double y2, double x3, double y3,
		       double beztol, matrix &mat) = 0;
  virtual void close() = 0;

protected:
  double beztol_;

  ZFontType1 &type1_;

#if 0
  ZArray<short> chardata_;
#else
  chardesc *char_;
#endif

  int npnts_, nloops_;

  Pad_AList pcstack_;  // drawchar_seac docommand drawchar
  Pad_IList stack_;
  Pad_IList retstack_;	// subr0 drawchar_seac docommand drawchar
  unsigned char *pc_;
  Pad_Bool incusp_;

  int x_offset_, y_offset_;	/* offsets for accents */
  int startx_, starty_;
  int curx_, cury_;
  int nextx_, nexty_;
  int delx_, dely_;
  Pad_Bool started_;
  int thecharwidth_;

  int coordpos_;
  int coordsave_[7][2];
};

class ZFontType1InterpPoly : public ZFontType1Interp {
public:
  ZFontType1InterpPoly(ZFontType1 &t, double beztol);
  virtual void drawline(double x0, double y0, double x1, double y1, 
			double, double, double, double, matrix &mat);
  virtual void drawbez(double x0, double y0, double x1, double y1, 
		       double x2, double y2, double x3, double y3,
		       double beztol, matrix &mat);
  virtual void close();

  void poly_pnt(double x, double y);
  void bezadapt(double x0, double y0, double x1, double y1, 
		double x2, double y2, double x3, double y3, 
		double beztol, matrix &mat);
private:
  int nvertpos_;
};

class ZFontType1InterpSpline : public ZFontType1Interp {
public:
  ZFontType1InterpSpline(ZFontType1 &t, double beztol);
  virtual void drawline(double x0, double y0, double x1, double y1, 
			double, double, double, double, matrix &mat);
  virtual void drawbez(double x0, double y0, double x1, double y1, 
		       double x2, double y2, double x3, double y3,
		       double beztol, matrix &mat);
  virtual void close();
private:
};

inline void ZFontType1Interp::reset(ZFont *f, int c)
{
#if 0
  chardata_.make_empty();
#else
  char_ = f->getchardesc(c);
  char_->data.Make_empty();
#endif
}

inline int ZFontType1Interp::nshorts()
{
  return char_->data.Length();
}

/*
inline const short *ZFontType1Interp::chardata()
{
  return char_->data.Pointer();
}
*/

inline void ZFontType1Interp::set_offset(int xo, int yo)
{
  x_offset_ = xo; y_offset_ = yo;
}

inline void ZFontType1Interp::endchar()
{
  char_->data.Push_last((nloops_ == 0) ? PO_RET : PO_RETENDLOOP);
}

inline ZFontType1InterpPoly::ZFontType1InterpPoly(ZFontType1 &t, double beztol)
  : ZFontType1Interp(t, beztol)
{
}

inline ZFontType1InterpSpline::ZFontType1InterpSpline(ZFontType1 &t,
						      double beztol)
  : ZFontType1Interp(t, beztol)
{
}

#endif
