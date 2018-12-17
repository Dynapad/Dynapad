#include "ZFont.h"
#include "ZFontType1.h"
#include "point.h"
#include <stdio.h>

#define NOBBOX		30000

const char *
ZFont::asciiname(int c)
{
  c -= MIN_ASCII;
  if (c >= 0 && c < NASCII) {
    if (ZFontType1::charlist_[c].name)
      return ZFontType1::charlist_[c].name + 1;
    else return 0;
  } else return 0;
}

ZFont::ZFont(int cmin, int cmax, int fs)
{
  //freeaddr_ = 0;
  name_ = 0;

  //type_ = t;
  scale_ = fs;
  charmin_ = cmin;
  charmax_ = cmax;
  nchars_ = cmax - cmin + 1;
  chars_ = new chardesc[nchars_];
}

ZFont::~ZFont()
{
    if (chars_) {
	delete [] chars_;
	chars_ = NULL;
    }
}

void
ZFont::fakechar(int c, int width)
{
  short chardata[1];

  chardata[0] = PO_RET;
  addchardata(c, chardata, 1);
  addcharmetrics(c, width, 0);
}

void
ZFont::addchardata(int c, const short *data, int nshorts)
{
  int index = chartoindex(c);
  if (index < 0) {
    fprintf(stderr,"Addchardata bad poop\n");
    return;
  }
  chardesc *cd = chars_ + index;
  //freeaddr_ = 0;
  while (nshorts--) cd->data.Push_last(*data++);
}

void
ZFont::addcharmetrics(int c, int movex, int movey)
{
  int index = chartoindex(c);
  if (index<0) {
    fprintf(stderr,"Addcharmetrics bad poop\n");
    return;
  }
  chardesc *cd = chars_ + index;
  cd->movex = movex;
  cd->movey = movey;
}

chardesc *ZFont::getchardesc(int c)
{
    int index;

    index = chartoindex(c);
    if (index == -1) {
	return NULL;
    } else {
	return &chars_[index];
    }
}

const short *ZFont::chardata(int c)
{
    int index;

    index = chartoindex(c);

    if (index == -1) {
	return NULL;
    } else {
	return chars_[index].data.Pointer();
    }
}

int ZFont::chartoindex(int c)
{
  if (c < charmin_) return -1;
  if (c > charmax_) return -1;
  return c - charmin_;
}

#if 0
void ZFont::applytocharverts(int c, void (*func)(ZFont*, short*))
{
  int index;
  chardesc *cd;
  short *sptr;
  int nverts;

  index = chartoindex(c);
  if (index<0)
    return;
  cd = chars_ + index;
  if (!cd->data.Is_empty()) {
    sptr = cd->data.Pointer();
    switch(Type()) {
    case TM_TYPE:
      while(1) {
	switch((short)*sptr++) {	
	case TM_BGNTMESH:
	case TM_SWAPTMESH:
	case TM_ENDBGNTMESH:
	  break;
	case TM_RETENDTMESH:
	case TM_RET:
	  return;
	default:
	  fprintf(stderr,"applytocharverts: bad TM op\n");
	  return;
	}
	nverts = *sptr++;
	while(nverts--) {
	  (func)(this, sptr);
	  sptr+=2;
	}
      }
      break;
    case PO_TYPE:
      while(1) {
	switch(*sptr++) {	
	case PO_BGNLOOP:
	case PO_ENDBGNLOOP:
	  break;
	case PO_RETENDLOOP:
	case PO_RET:
	  return;
	default:
	  fprintf(stderr,"applytocharverts: bad PO op\n");
	  return;
	}
	nverts = *sptr++;
	while(nverts--) {
	  (func)(this, sptr);
	  sptr+=2;
	}
      }
      break;
    case SP_TYPE:
      while(1) {
	switch(*sptr++) {	
	case SP_MOVETO:
	  (func)(this, sptr);
	  sptr+=2;
	  break;
	case SP_LINETO:
	  (func)(this, sptr);
	  sptr+=2;
	  break;
	case SP_CURVETO:
	  (func)(this, sptr+0);
	  (func)(this, sptr+2);
	  (func)(this, sptr+4);
	  sptr+=6;
	  break;
	case SP_CLOSEPATH:
	  break;
	case SP_RETCLOSEPATH:
	  return;
	case SP_RET:
	  return;
	default:
	  fprintf(stderr,"applytocharverts: bad SP op\n");
	  return;
	}
      }
      break;
    default:
      fprintf(stderr,"applytocharverts: bad obj font type\n");
    }
  }
}
#endif

static chardesc *currdesc;

static void bboxcalc(short x, short y, float)
{
  if (x < currdesc->llx) currdesc->llx = x;
  if (y < currdesc->lly) currdesc->lly = y;
  if (x > currdesc->urx) currdesc->urx = x;
  if (y > currdesc->ury) currdesc->ury = y;
}

void ZFont::calccharbboxes()
{
  for (int c = charmin_; c <= charmax_; c++) {
    currdesc = getchardesc(c);
    currdesc->llx = NOBBOX;
    currdesc->lly = NOBBOX;
    currdesc->urx = -NOBBOX;
    currdesc->ury = -NOBBOX;
    Run(c, bboxcalc, 0.0);
    if (currdesc->llx == NOBBOX) {
      currdesc->lly = NOBBOX;
      currdesc->urx = NOBBOX;
      currdesc->ury = NOBBOX;
    }
#if 0
    else printf("bbox `%c': %d %d %d %d\n",
		c, currdesc->llx, currdesc->lly, currdesc->urx, currdesc->ury);
#endif
  }
}

static void poly_print(const short *sptr)
{
  int nverts;

  while(1) {
    switch(*sptr++) {	
    case PO_BGNLOOP:
      printf("bgnloop\n");
      break;
    case PO_ENDBGNLOOP:
      printf("endbgnloop\n\n");
      break;
    case PO_RETENDLOOP:
      printf("retendloop\n\n");
      return;
    case PO_RET:
      printf("ret\n\n");
      return;
    default:
      fprintf(stderr,"poly_print: bad PO op\n");
      return;
    }
    nverts = *sptr++;
    while(nverts--) {
      printf("	vert %d %d\n",sptr[0],sptr[1]);
      sptr+=2;
    }
  }
}

int ZFontPoly::printchar(int c)
{
  int index;
  chardesc *cd;
  const char *aname;

  index = chartoindex(c);
  if (index<0)
    return 0;
  cd = chars_ + index;
  aname = asciiname(c);
  if (aname)
    printf("Charname: \"%s\"  code is %d\n",aname,c);
  else
    printf("Charname: NULL code is %d\n", c);
  printf("Advance %d %d\n", cd->movex, cd->movey);
  printf("Bbox %d %d to %d %d\n", cd->llx, cd->lly, cd->urx, cd->ury);
  printf("Data %d tokens\n\n", cd->data.Length() / 2);
  if (cd->data.Length()) poly_print(cd->data.Pointer());
  return 1;
}

void ZFontPoly::print()
{
  int i;

  printf("Object font type is Poly\n");
  printf("First char is %d\n", charmin_);
  printf("Last char is %d\n", charmax_);
  printf("Num chars is %d\n", nchars_);
  printf("Fnt scale is %d\n\n", scale_);
  for (i=charmin_; i<=charmax_; i++) printchar(i);
}

/*            s     e
 *  <---o<----o<----o<-----0<-----
 *            |
 *            |
 *            v
 *  --->o---->o---->
 *      e     s
 */
void ZFontPoly::Run(int code, pfss f, float theta)
{
  chardesc *desc = getchardesc(code);
  if (!desc) {
      return;
  }
  const short *prog = desc->data.Pointer();
  if (!prog) {
      return;
  }

  int char_vertex_count = 0;
  int loop_vertex_count;
  Pad_SList xstart, ystart;

  while(1) {
    switch (*prog++) {	
    case PO_ENDBGNLOOP:
      //end_loop();
      // Fall through
    case PO_BGNLOOP:
      // start_loop();
      // If this is not the first loop
      if (char_vertex_count) { 
	(*f)((short)xstart.Last(), (short)ystart.Last(), theta);
	char_vertex_count += 1;
      }
      loop_vertex_count = 0;
      break;

    case PO_RETENDLOOP:
      //end_loop();
      // Fall through
    case PO_RET:
      // Pop all the saved start points except for the last and add
      // them to the polygon's outline.
      while (xstart.Length() > 1) {	
	(*f)((short)xstart.Pop_last(), (short)ystart.Pop_last(), theta);
	char_vertex_count += 1;
      }
#if 0
      v->FillPolygon((double*)xv.pointer(),
		     (double*)yv.pointer(),
		     xv.count(), /*Complex*/Nonconvex, CoordModeOrigin);
#endif
      return;
    default:
      fprintf(stderr,"poly_draw: bad PO op\n");
      return;
    }
    int nverts = *prog++;
    while (nverts--) {
      (*f)(prog[0], prog[1], theta);
      char_vertex_count += 1;
      // Save the first point of the loop.
      if (loop_vertex_count == 0) {
	xstart.Push_last(prog[0]);
	ystart.Push_last(prog[1]);
      }
      loop_vertex_count += 1;
      prog += 2;
    }
  }
}

void ZFontSpline::Run(int , pfss ,float) {};

