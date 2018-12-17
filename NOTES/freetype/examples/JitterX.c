/*

  HelloX.C
  ========
  (c) Copyright Paul Griffiths 1999
  Email: paulgriffiths@cwcom.net

  "Hello, World!", X Window System style.

*/


#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>
#include <X11/keysym.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <sys/time.h>
#include <signal.h>
#include <math.h>

/*########################################################*/
/*# BEGIN:  TT/AA message */
#include <X11/Xft/Xft.h>
/*# END : TT/AA message */
/*########################################################*/

/*  Global variables  */

Display *     display;
int           screen_num;
static char * appname;

/*  Window variables  */

Window       win;
Pixmap       pixmap;
int          x, y;
unsigned int width, height;
unsigned int border_width;
char *       window_name = "Hello, X Window System!";
char *       icon_name   = "HelloX";

/*  Display variables  */

char *       display_name = NULL;
unsigned int display_width, display_height;


/* Xft variables */
int size = 10;
int zoom = 1;
char * msg = "Hello, X FreeType AA!";
XftDraw *xftdraw;
XftFont *xftfont;

void
renderstring()
{
      XGlyphInfo after;

      if (xftfont) XftFontClose(display, xftfont);
      xftfont = XftFontOpen(display, screen_num,
          XFT_FAMILY, XftTypeString, "Helvetica",
           XFT_ANTIALIAS, XftTypeBool, 0,
           /*XFT_SIZE, XftTypeInteger, size,*/
           /*XFT_PIXEL_SIZE, XftTypeDouble, (double)1.4*size,*/
           XFT_PIXEL_SIZE, XftTypeDouble, (double)size,
           0);
      if (!xftfont) {
           perror("XftFontOpen(): Failed"); exit(1);}
      XftTextExtents8(display, xftfont,
                          (unsigned char*)msg, strlen(msg),
		          &after);

      printf("%d %d %d %d %f %f %f\n",
        size,
	-after.x,
	after.width,
	after.width - after.x,
	(float)(after.width - after.x)/size,
	9.859*size,
	(after.width - after.x) - 9.859*size);
}

/*  main() function  */

int main( int argc, char * argv[] ) {
    
    appname = argv[0];

    /*  Connect to X server  */

    if ( (display = XOpenDisplay(display_name)) == NULL ) {
	fprintf(stderr, "%s: couldn't connect to X server %s\n",
		appname, display_name);
	exit(EXIT_FAILURE);
    }


    /*  Get screen size from display structure macro  */

    screen_num     = DefaultScreen(display);
    display_width  = DisplayWidth(display, screen_num);
    display_height = DisplayHeight(display, screen_num);


    /*  Set initial window size and position, and create it  */

    x = y = 0;
    width  = display_width / 3;
    height = display_width / 3;

    win = XCreateSimpleWindow(display, RootWindow(display, screen_num),
			      x, y, width, height, border_width,
			      BlackPixel(display, screen_num),
			      WhitePixel(display, screen_num));

    pixmap = XCreatePixmap(display, win, width, height,
                           DefaultDepth(display, DefaultScreen(display)));

    xftfont = XftFontOpen(display, screen_num,
        XFT_FAMILY, XftTypeString, "Helvetica",
         /*XFT_ANTIALIAS, XftTypeBool, antialias,*/
         XFT_ANTIALIAS, XftTypeBool, 0,
         /*XFT_SIZE, XftTypeInteger, size,*/
         XFT_PIXEL_SIZE, XftTypeDouble, (double)1.4*size,
         /*XFT_PIXEL_SIZE, XftTypeDouble, (double)size,*/
         0);
    if (!xftfont) {
         perror("XftFontOpen(): Failed"); exit(1);}

    for (size = 10; size < 800; size += zoom)
      renderstring();

    /*  Clean up and exit  */
    XCloseDisplay(display);

    return EXIT_SUCCESS;   /*  We shouldn't get here  */
}
