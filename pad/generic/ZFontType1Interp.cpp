#include "ZFontType1Interp.h"
#include "ZFontType1.h"
#include <math.h>


inline void 
applymat(matrix mat, double *x, double *y)	
{
  /* use doubles don't change!! */
  double tx, ty;

  tx = (*x)*mat[0][0]+(*y)*mat[0][1];
  ty = (*x)*mat[1][0]+(*y)*mat[1][1];
  *x = tx;
  *y = ty;
}

ZFontType1Interp::ZFontType1Interp(ZFontType1 &t, double beztol)
  : beztol_(beztol), type1_(t), char_(0), incusp_(FALSE), started_(FALSE)
{
}

void ZFontType1Interp::drawchar(int c, int n, ZFont &f)
{
  char_ = f.getchardesc(n);
  beginchar(c);
  set_offset(0, 0);
  runprog(f);
  endchar();
  f.addcharmetrics(n, thecharwidth_, 0);
}

void ZFontType1Interp::beginchar(int c)
{
  npnts_ = 0;
  nloops_ = 0;
  stack_.Make_empty();
  pcstack_.Make_empty();
  retstack_.Make_empty();
  pc_ = type1_.chars_[c];
}

void 
ZFontType1Interp::runprog(ZFont &f)
{
  long v, w, num, cmd;

  while(1) {
    v  = *pc_++;
    if (v>=0 && v<=31) {
      if (v == 12) {
	w  = *pc_++;
	cmd = 256+w;
      }
      else 
	cmd = v;
      if (!docommand(f, (int)cmd)) {
	return;
      }
    }
    else if (v>=32 && v<=246) {
      num = v-139;
      stack_.Push_last((int)num);
    }
    else if (v>=247 && v<=250) {
      w  = *pc_++;
      num = (v-247)*256+w+108;
      stack_.Push_last((int)num);
    }
    else if (v>=251 && v<=254) {
      w  = *pc_++;
      num = -(v-251)*256-w-108;
      stack_.Push_last((int)num);
    }
    else if (v == 255) {
      num  = *pc_++;
      num <<= 8;
      num |= *pc_++;
      num <<= 8;
      num |= *pc_++;
      num <<= 8;
      num |= *pc_++;
      stack_.Push_last((int)num);
    }
  }
}

#define HSTEM		1
#define VSTEM		3
#define VMOVETO		4
#define RLINETO		5
#define HLINETO		6
#define VLINETO		7
#define RRCURVETO	8
#define CLOSEPATH	9
#define CALLSUBR	10
#define RETURN		11
#define HSBW		13
#define ENDCHAR		14
#define UNDOC15		15	/* undocumented, but used by Adobe */
#define RMOVETO		21
#define HMOVETO		22
#define VHCURVETO	30
#define HVCURVETO	31
#define DOTSECTION	(256+0)
#define VSTEM3		(256+1)
#define HSTEM3		(256+2)
#define SEAC		(256+6)
#define SBW		(256+7)
#define DIV		(256+12)
#define CALLOTHERSUBR	(256+16)
#define POP		(256+17)
#define SETCURRENTPOINT	(256+33)
#define WHAT0		0

Pad_Bool
ZFontType1Interp::docommand(ZFont &f, int cmd)
{
  int x, y, w;
  int dx1, dy1;
  int dx2, dy2;
  int dx3, dy3;
  int sub, n;
  int	achar, bchar, adx, ady, asb;
  unsigned char *subpc;

  switch(cmd) {
  case WHAT0:
    fprintf(stderr, "\nYUCK: WHAT0\n"); 
    break;
  case HSTEM:
    stack_.Pop_last();
    stack_.Pop_last();
    break;
  case VSTEM:		
    stack_.Pop_last();
    stack_.Pop_last();
    break;
  case VMOVETO:
    y = stack_.Pop_last();
    rmoveto(incusp_, 0, y);
    break;
  case RLINETO:
    y = stack_.Pop_last();
    x = stack_.Pop_last();
    rlineto(type1_.mat_, x, y);
    break;
  case HLINETO:
    x = stack_.Pop_last();
    rlineto(type1_.mat_, x, 0);
    break;
  case VLINETO:
    y = stack_.Pop_last();
    rlineto(type1_.mat_, 0, y);
    break;
  case RRCURVETO:
    dy3 = stack_.Pop_last();
    dx3 = stack_.Pop_last();
    dy2 = stack_.Pop_last();
    dx2 = stack_.Pop_last();
    dy1 = stack_.Pop_last();
    dx1 = stack_.Pop_last();
    rcurveto(dx1, dy1, dx1+dx2, dy1+dy2, dx1+dx2+dx3, dy1+dy2+dy3,
	       beztol_, type1_.mat_);
    break;
  case CLOSEPATH:
    closepath(type1_.mat_);
    break;
  case CALLSUBR:
    sub = stack_.Pop_last();
    subpc = type1_.subrs_[sub];
    if (!subpc) {
      fprintf(stderr, "\nYUCK no sub addr\n");
    }
    pcstack_.Push_last(pc_);
    pc_ = subpc;
    break;
  case RETURN:
    pc_ = (unsigned char *)pcstack_.Pop_last();
    break;
  case HSBW:
    w = stack_.Pop_last();
    x = stack_.Pop_last();
    thecharwidth_ = width(type1_.mat_, w);
    sbpoint(x, 0);
    break;
  case ENDCHAR:
    closepath(type1_.mat_);
    break;
  case UNDOC15:
    break;
  case RMOVETO:
    y = stack_.Pop_last();
    x = stack_.Pop_last();
    rmoveto(incusp_, x, y);
    break;
  case HMOVETO:
    x = stack_.Pop_last();
    rmoveto(incusp_, x, 0);
    break;
  case VHCURVETO:
    dy3 = 0;
    dx3 = stack_.Pop_last();
    dy2 = stack_.Pop_last();
    dx2 = stack_.Pop_last();
    dy1 = stack_.Pop_last();
    dx1 = 0;
    rcurveto(dx1, dy1, dx1+dx2, dy1+dy2, dx1+dx2+dx3, dy1+dy2+dy3,
	       beztol_, type1_.mat_);
    break;
  case HVCURVETO:
    dy3 = stack_.Pop_last();
    dx3 = 0;
    dy2 = stack_.Pop_last();
    dx2 = stack_.Pop_last();
    dy1 = 0;
    dx1 = stack_.Pop_last();
    rcurveto(dx1, dy1, dx1+dx2, dy1+dy2, dx1+dx2+dx3, dy1+dy2+dy3,
	       beztol_, type1_.mat_);
    break;
  case DOTSECTION:
    break;
  case VSTEM3:
    stack_.Pop_last();
    stack_.Pop_last();
    stack_.Pop_last();
    stack_.Pop_last();
    stack_.Pop_last();
    stack_.Pop_last();
    break;
  case HSTEM3:
    stack_.Pop_last();
    stack_.Pop_last();
    stack_.Pop_last();
    stack_.Pop_last();
    stack_.Pop_last();
    stack_.Pop_last();
    break;
  case SEAC:
    achar = stack_.Pop_last();
    bchar = stack_.Pop_last();
    ady   = stack_.Pop_last();
    adx   = stack_.Pop_last();
    asb   = stack_.Pop_last();
    seac(f, asb, adx, ady, bchar, achar);
    return FALSE;
  case SBW:
    x = stack_.Pop_last();
    y = stack_.Pop_last();
    fprintf(stderr, "sbw: width: %d %d\n", x, y);
    thecharwidth_ = width(type1_.mat_, x);
    y = stack_.Pop_last();
    x = stack_.Pop_last();
    fprintf(stderr, "sbw: side: %d %d\n", x, y);
    sbpoint(x, y);
    break;
  case DIV:
    x = stack_.Pop_last();
    y = stack_.Pop_last();
    stack_.Push_last(x/y);
    break;
  case CALLOTHERSUBR:
    sub = stack_.Pop_last();
    n = stack_.Pop_last();
    if (sub == 0) subr0(f);
    else if (sub == 1) subr1();
    else if (sub == 2) subr2(f);
    else {
      //dfprintf(stderr, "\nYUCK nargs %d\n", n);
      for (int i = 0; i < n; i++) {
	//dfprintf(stderr, "\nYUCK pretpush %d\n", i);
	retstack_.Push_last(stack_.Pop_last());
      }
    }
    break;
  case POP:
    stack_.Push_last(retstack_.Pop_last());
    break;
  case SETCURRENTPOINT:
    y = stack_.Pop_last();
    x = stack_.Pop_last();
    sbpoint(x, y);
    break;
  default:
    fprintf(stderr, "\nYUCK bad instruction %d\n", cmd);
    break;
  }
  return !(pc_ == 0 || cmd == ENDCHAR || cmd == WHAT0 || cmd == SEAC);
}

void 
ZFontType1Interp::rmoveto(Pad_Bool incusp, int x, int y)
{
  if (incusp) {
    delx_ = x;
    dely_ = y;
  }
  else {
    curx_ += x;
    cury_ += y;
    savestart(curx_, cury_);
  }
}

void 
ZFontType1Interp::rlineto(matrix &mat, int x, int y)
{
    double dx, dy;

    nextx_ = curx_ + x;
    nexty_ = cury_ + y;
    dx = nextx_ - curx_;
    dy = nexty_ - cury_;
    drawline((double)curx_, (double)cury_, (double)nextx_, (double)nexty_,
	     dx, dy, dx, dy, mat);
    curx_ = nextx_;
    cury_ = nexty_;
}

void 
ZFontType1Interp::rcurveto(int dx1, int dy1, int dx2, int dy2,
			   int dx3, int dy3, double beztol, matrix &mat)
{
  int x0 = curx_;
  int y0 = cury_;
  int x1 = curx_ + dx1;
  int y1 = cury_ + dy1;
  int x2 = curx_ + dx2;
  int y2 = cury_ + dy2;
  int x3 = curx_ + dx3;
  int y3 = cury_ + dy3;
  drawbez((double)x0, (double)y0, (double)x1, (double)y1, 
	  (double)x2, (double)y2, (double)x3, (double)y3, beztol, mat);
  curx_ = x3;
  cury_ = y3;
}

void 
ZFontType1InterpPoly::drawbez(double x0, double y0, double x1, double y1, 
		  double x2, double y2, double x3, double y3,
		  double beztol, matrix &mat)
{
  bezadapt(x0, y0, x1, y1, x2, y2, x3, y3, beztol, mat);
}

void 
ZFontType1InterpPoly::bezadapt(double x0, double y0, double x1, double y1, 
			       double x2, double y2, double x3, double y3, 
			       double beztol, matrix &mat)
{
  double midx = (x0+3*x1+3*x2+x3)/8.0;
  double midy = (y0+3*y1+3*y2+y3)/8.0;
  double linx = (x0+x3)/2.0;
  double liny = (y0+y3)/2.0;
  double dx = midx-linx;
  double dy = midy-liny;
  double mag = dx*dx+dy*dy;
  if (mag<(beztol*beztol))
    drawline(x0, y0, x3, y3, x1-x0, y1-y0, x3-x2, y3-y2, mat);
  else {
    double ax0 = x0;
    double ay0 = y0;
    double ax1 = (x0+x1)/2;
    double ay1 = (y0+y1)/2;
    double ax2 = (x0+2*x1+x2)/4;
    double ay2 = (y0+2*y1+y2)/4;
    double ax3 = midx;
    double ay3 = midy;
    bezadapt(ax0, ay0, ax1, ay1, ax2, ay2, ax3, ay3, beztol, mat);

    double bx0 = midx;
    double by0 = midy;
    double bx1 = (x1+2*x2+x3)/4;
    double by1 = (y1+2*y2+y3)/4;
    double bx2 = (x2+x3)/2;
    double by2 = (y2+y3)/2;
    double bx3 = x3;
    double by3 = y3;
    bezadapt(bx0, by0, bx1, by1, bx2, by2, bx3, by3, beztol, mat);
  }
}

void 
ZFontType1InterpSpline::drawbez(double x0, double y0, double x1, double y1, 
				double x2, double y2, double x3, double y3,
				double /*beztol*/, matrix &mat)
{
  applymat(mat, &x0, &y0);
  applymat(mat, &x1, &y1);
  applymat(mat, &x2, &y2);
  applymat(mat, &x3, &y3);
  if (npnts_ == 0) {
    char_->data.Push_last(SP_MOVETO);
    char_->data.Push_last((short)floor(x0));
    char_->data.Push_last((short)floor(y0));
    npnts_++;
    nloops_++;
  }
  char_->data.Push_last(SP_CURVETO);
  char_->data.Push_last((short)floor(x1));
  char_->data.Push_last((short)floor(y1));
  char_->data.Push_last((short)floor(x2));
  char_->data.Push_last((short)floor(y2));
  char_->data.Push_last((short)floor(x3));
  char_->data.Push_last((short)floor(y3));
}

void 
ZFontType1Interp::closepath(matrix &mat)
{
  double dx, dy;

  if (started_) {
    dx = startx_ - curx_;
    dy = starty_ - cury_;
    drawline((double)curx_, (double)cury_, 
	     (double)startx_, (double)starty_,
	     dx, dy, dx, dy, mat);
    close();
    started_ = FALSE;
  }
}

void 
ZFontType1InterpPoly::close()
{
  if (npnts_) {
    char_->data.Set(nvertpos_, npnts_);
    npnts_ = 0;
  }
}

void 
ZFontType1InterpSpline::close()
{
  char_->data.Push_last(SP_CLOSEPATH);
  npnts_ = 0;
}

int 
ZFontType1Interp::width(matrix &mat, int x)
{
  double fx = x;
  double fy = 0.0;
  applymat(mat, &fx, &fy);
  return (int)(fx + 0.5);
}

void 
ZFontType1Interp::sbpoint(int x, int y)
{
  curx_ = x;
  cury_ = y;
}

void 
ZFontType1Interp::seac(ZFont &c, 
		       int /*asb*/, int adx, int ady, int bchar, int achar)
{
  int	i, j;

  for (i = 0; i < NACCENT; i++) {
    if (achar == type1_.accentlist_[i].code) {
      set_offset(adx, ady);
      drawchar_seac(c, type1_.charprog_[ACCENTBASE + i], 0);
      set_offset(adx, ady);
      for (j = 0; j < NACCENT; j++) {	/* look through accents first */
	if (bchar == type1_.accentlist_[j].code) {
	  drawchar_seac(c, type1_.charprog_[ACCENTBASE + j], 1);
	  break;
	}
      }

      if (j == NACCENT) {
	for (j = 0; j < NASCII; j++) {
	  if (bchar == type1_.charlist_[j].code) {
	    drawchar_seac(c, type1_.charprog_[j], 1);
	    break;
	  }
	}
      }
      break;
    }
  }
  if (i == NACCENT) {
    fprintf(stderr, "Drawing accent, not sure this is implemented right.\n");
    drawchar(achar, -1, c);
  }
}

void 
ZFontType1Interp::drawchar_seac(ZFont &f, int c, int accentflag)
{
  if (!accentflag) beginchar(c);
  runprog(f);
  if (accentflag) endchar();
}

void 
ZFontType1Interp::subr0(ZFont &)
{
  /*int ypos =*/ stack_.Pop_last();
  /*int xpos =*/ stack_.Pop_last();
  /*int noise =*/ stack_.Pop_last();
  if (coordpos_!=7) {
    fprintf(stderr, "subr0: bad poop\n");
    exit(1);
  }
  int x0 =  coordsave_[0][0];
  int y0 =  coordsave_[0][1];

  int x1 =  coordsave_[1][0]+x0;
  int y1 =  coordsave_[1][1]+y0;
  int x2 =  coordsave_[2][0];
  int y2 =  coordsave_[2][1];
  int x3 =  coordsave_[3][0];
  int y3 =  coordsave_[3][1];
  rcurveto(x1, y1, x1+x2, y1+y2, x1+x2+x3, y1+y2+y3, beztol_, type1_.mat_);
  x1 =  coordsave_[4][0];
  y1 =  coordsave_[4][1];
  x2 =  coordsave_[5][0];
  y2 =  coordsave_[5][1];
  x3 =  coordsave_[6][0];
  y3 =  coordsave_[6][1];
  rcurveto(x1, y1, x1+x2, y1+y2, x1+x2+x3, y1+y2+y3, beztol_, type1_.mat_);
  getpos(&x0, &y0);
  retstack_.Push_last(y0);
  retstack_.Push_last(x0);
  incusp_ = FALSE;
}

void 
ZFontType1Interp::subr1()
{
  coordpos_ = 0;
  incusp_ = TRUE;
}

void 
ZFontType1Interp::subr2(ZFont &)
{
  int x, y;

  getmove(&x, &y);
  if (coordpos_>=7) {
    fprintf(stderr, "subr2: bad poop\n");
    exit(1);
  }
  coordsave_[coordpos_][0] = x;
  coordsave_[coordpos_][1] = y;
  coordpos_++;
}

void 
ZFontType1Interp::savestart(int x, int y)
{
  startx_ = x;
  starty_ = y;
  started_ = TRUE;
}

/*
 * drawline calls:
 *   poly_pnt
 * uses:
 *   mat_, x_offset_, y_offset_
 */

void 
ZFontType1InterpPoly::drawline(double x0, double y0, double x1, double y1, 
			       double /*dx0*/, double /*dy0*/,
			       double /*dx1*/, double /*dy1*/, matrix &mat)
{
  if (x0 != x1 || y0 != y1) {
    double x = x1;
    double y = y1;
    applymat(mat, &x, &y);
    poly_pnt(x, y);
  }
}

void 
ZFontType1InterpPoly::poly_pnt(double x, double y)
{
  int ix = (int)floor(x);
  int iy = (int)floor(y);
  ix += x_offset_;
  iy += y_offset_;
  if (npnts_ == 0) {
    char_->data.Push_last((nloops_ == 0) ? PO_BGNLOOP : PO_ENDBGNLOOP);
    nvertpos_ = char_->data.Length();
    char_->data.Push_last(0);
    nloops_++;
  }
  char_->data.Push_last(ix);
  char_->data.Push_last(iy);
  npnts_++;
}

void 
ZFontType1InterpSpline::drawline(double x0, double y0, double x1, double y1,
				 double, double, double, double, matrix &mat)
{
  applymat(mat, &x0, &y0);
  applymat(mat, &x1, &y1);
  if (npnts_ == 0) {
    char_->data.Push_last(SP_MOVETO);
    char_->data.Push_last((short)floor(x0));
    char_->data.Push_last((short)floor(y0));
    npnts_++;
    nloops_++;
  }
  char_->data.Push_last(SP_LINETO);
  char_->data.Push_last((short)floor(x1));
  char_->data.Push_last((short)floor(y1));
  npnts_++;
}

void 
ZFontType1Interp::getpos(int *x, int *y)
{
  *x = curx_;
  *y = cury_;
}

void 
ZFontType1Interp::getmove(int *x, int *y)
{
  *x = delx_;
  *y = dely_;
}
