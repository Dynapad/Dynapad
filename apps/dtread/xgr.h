

typedef struct Point {
      float x,y;
} Point;


EXTERN void drawcirc( float x, float y, float size);
EXTERN void drawline(float x, float y, float x2, float y2);
EXTERN void drawrect(float x,float y,float x2,float y2);
EXTERN void drawpoly( Point *xy, int size);
EXTERN void fillcirc( float x, float y, float size, int color);
EXTERN void setview(float x0,float y0,float scl);
EXTERN void fixview(float x0,float y0,float scl);
EXTERN void drawtext(float x0,float y0,char *str);
EXTERN void drawtextscreen(int x0,int y0,char *str);
EXTERN void clearscreen();
EXTERN void clearmarks();
EXTERN void focusview();
EXTERN void x_update();

EXTERN void x_draw_frame(unsigned char *x,unsigned char *y);


EXTERN void xinit();

