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


/*  Miscellaneous X variables  */

XSizeHints *  size_hints;
XWMHints   *  wm_hints;
XClassHint *  class_hints;
XTextProperty windowName, iconName;
XEvent        report;
XFontStruct * font_info;
XGCValues     values;
GC            gc;

/* Xft variables */
float msg_x, msg_y;
float rect_x, rect_y;
int size = 25;
char * msg = "Hello, X FreeType AA!";
XftDraw *xftdraw;
XftFont *xftfont;
XftColor color_fg;
XColor dummyc,fg;
Status retval;
int zoom = 0;

int start_x, start_y;
int moving = 0;
int antialias = 1;
int reload = 1;

struct itimerval interval, zero;

KeySym keysym;

void
renderstring()
{
      /* static so don't need to calculate it during panning, only during zooming. */
      static XGlyphInfo before, after;
      static float omsg_x = -1, omsg_y = -1;

      if (reload) {

	  /* I don't remember why I limit the minimum size */
          if (size < 10)  size = 10;
	  /* For a while I was also limiting the maximum size, because
	   * xft/xrender crashed when the size exceeded some threshold.
	   */

          if (xftfont) XftTextExtents8(display, xftfont,
              (unsigned char*)msg, strlen(msg),
              &before);
          if (xftfont) XftFontClose(display, xftfont);
          xftfont = XftFontOpen(display, screen_num,
              XFT_FAMILY, XftTypeString, "Helvetica",
	      XFT_ANTIALIAS, XftTypeBool, 0,
	      /*XFT_SIZE, XftTypeInteger, size,*/
	      XFT_PIXEL_SIZE, XftTypeDouble, (double)size,
	      0);
          if (!xftfont) {
	      perror("XftFontOpen(): Failed"); exit(1);}
          XftTextExtents8(display, xftfont,
                          (unsigned char*)msg, strlen(msg),
		          &after);
          if (zoom) {
              Window root, child;
              int root_x, root_y, x, y;
              unsigned int keys;

              XQueryPointer(display, win, &root, &child,
	          &root_x, &root_y, &x, &y, &keys);

	      /* This method of computing the string origin attempts to keep
	       * the current point under the mouse still under the mouse after
	       * zooming.  It's not really the correct thing to do, because
	       * it moves the string origin.
	       */
	      /*
	      msg_x = msg_x -
	        ((after.width - before.width)*(x - msg_x))/before.width;
	      msg_y = msg_y -
	        ((after.height - before.height)*(y - msg_y))/before.height;
	      */
	      /* This method of computing the sting origin, moves the string
	       * origin in proportion to the amount of zoom.  Which is what
	       * we want.  Alas, then the end of the string jitters.
	       */
	      msg_x = msg_x - (zoom*(x - msg_x))/(size - zoom);
	      msg_y = msg_y - (zoom*(y - msg_y))/(size - zoom);
          }
      }

      /* filling the entire window is slow, so try to just fill the previous string */
      /*XFillRectangle(display, pixmap, gc, 0, 0, width, height);*/
      if (omsg_x < 0) omsg_x = 0;
      if (omsg_y < 0) omsg_y = 0;
      XFillRectangle(display, pixmap, gc,
        (int)(omsg_x),
	(int)(omsg_y - before.y),
	(int)(before.xOff + 1),
	(int)(before.height + 1));  /* +1 because of Rectangle below */
      omsg_x = msg_x;
      omsg_y = msg_y;

      XftDrawString8(xftdraw, &color_fg, xftfont, (int)msg_x, (int)msg_y,
		     (unsigned char *) msg, strlen(msg));

      XSetForeground(display, gc, WhitePixel(display, screen_num));
      XDrawRectangle(display, pixmap, gc,
        (int)(msg_x - after.x),
	(int)(msg_y - after.y),
	(int)(after.width),
	(int)after.height);
      XDrawLine(display, pixmap, gc,
        (int)msg_x,
	(int)msg_y,
	(int)(msg_x - after.x),
	(int)msg_y);
      XDrawLine(display, pixmap, gc,
        (int)(msg_x - after.x + after.width),
	(int)(msg_y + after.yOff),
	(int)(msg_x + after.xOff),
	(int)(msg_y + after.yOff));
      XSetForeground(display, gc, BlackPixel(display, screen_num));

      /* copying the entire window is also slow */
      XCopyArea(display, pixmap, win, gc,
          0, 0, width, height, 0, 0);
      XFlush(display);
}

void
renderrect()
{
      static float orect_x, orect_y;
      static float width = 250, height = 20;
      Window root, child;
      int root_x, root_y, x, y;
      unsigned int keys;

      XQueryPointer(display, win, &root, &child,
	          &root_x, &root_y, &x, &y, &keys);

      if (orect_x < 0) orect_x = 0;
      if (orect_y < 0) orect_y = 0;
      XFillRectangle(display, pixmap, gc,
        (int)orect_x,
	(int)orect_y,
	(int)(width + 1),
	(int)(height + 1));  /* +1 because of Rectangle below */

      rect_x = rect_x - (zoom*(x - rect_x))/(size - zoom);
      rect_y = rect_y - (zoom*(y - rect_y))/(size - zoom);
      width += (width * zoom)/(size-zoom);
      height += (height * zoom)/(size-zoom);

      orect_x = rect_x;
      orect_y = rect_y;

      XSetForeground(display, gc, WhitePixel(display, screen_num));
      XDrawRectangle(display, pixmap, gc,
        (int)rect_x,
	(int)rect_y,
	(int)width,
	(int)height);
      XSetForeground(display, gc, BlackPixel(display, screen_num));
}

void
sigalrm(int sig)
{
    signal(sig, SIG_IGN);
    size += zoom;
    reload = 1;
    renderrect();
    renderstring();
    reload = 0;
    signal(sig, sigalrm);
}


/*  main() function  */

int main( int argc, char * argv[] ) {
    
    appname = argv[0];


    /*  Allocate memory for our structures  */

    if ( !( size_hints  = XAllocSizeHints() ) || 
	 !( wm_hints    = XAllocWMHints()   ) ||
	 !( class_hints = XAllocClassHint() )    ) {
	fprintf(stderr, "%s: couldn't allocate memory.\n", appname);
	exit(EXIT_FAILURE);
    }


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
    msg_x = width/2;
    msg_y = height/2;

    rect_x = msg_x;
    rect_y = msg_y + 10;

    win = XCreateSimpleWindow(display, RootWindow(display, screen_num),
			      x, y, width, height, border_width,
			      BlackPixel(display, screen_num),
			      WhitePixel(display, screen_num));

    pixmap = XCreatePixmap(display, win, width, height,
                           DefaultDepth(display, DefaultScreen(display)));

    /*  Set hints for window manager before mapping window  */

    if ( XStringListToTextProperty(&window_name, 1, &windowName) == 0 ) {
	fprintf(stderr, "%s: structure allocation for windowName failed.\n",
		appname);
	exit(EXIT_FAILURE);
    }

    if ( XStringListToTextProperty(&icon_name, 1, &iconName) == 0 ) {
	fprintf(stderr, "%s: structure allocation for iconName failed.\n",
		appname);
	exit(EXIT_FAILURE);
    }

    size_hints->flags       = PPosition | PSize | PMinSize;
    size_hints->min_width   = 200;
    size_hints->min_height  = 100;

    wm_hints->flags         = StateHint | InputHint;
    wm_hints->initial_state = NormalState;
    wm_hints->input         = True;

    class_hints->res_name   = appname;
    class_hints->res_class  = "hellox";

    XSetWMProperties(display, win, &windowName, &iconName, argv, argc,
		     size_hints, wm_hints, class_hints);


    /*  Choose which events we want to handle  */

    XSelectInput(display, win, ExposureMask | KeyPressMask | PointerMotionMask |
		 ButtonPressMask | ButtonReleaseMask | StructureNotifyMask);


    /*  Create graphics context  */

    gc = XCreateGC(display, win, 0, &values);

    XSetForeground(display, gc, BlackPixel(display, screen_num));
    XFillRectangle(display, win, gc, 0, 0, width, height);
    XFillRectangle(display, pixmap, gc, 0, 0, width, height);

    retval = XAllocNamedColor(display,DefaultColormap(display,screen_num), 
			      "green", &fg, &dummyc);
    if (!retval) {
	perror("XAllocNamedColor(): Failed"); exit(1);}

    color_fg.color.red = dummyc.red;
    color_fg.color.green = dummyc.green;
    color_fg.color.blue = dummyc.blue;
    color_fg.color.alpha = 0xffff;
    color_fg.pixel = fg.pixel;


    /*  Display Window  */

    XMapWindow(display, win);

    signal(SIGALRM, sigalrm);
    interval.it_value.tv_usec = 50 * 1000;
    interval.it_interval.tv_usec = 50 * 1000;
    
    /*  Enter event loop  */

    while ( 1 ) {
	XNextEvent(display, &report);

	switch ( report.type ) {

	case Expose:

	    if ( report.xexpose.count != 0 )
		break;

	    /*########################################################*/
	    /*# BEGIN:  TT/AA message */
	    if (!xftdraw) {


	      xftdraw = XftDrawCreate(display, (Drawable) pixmap, 
				      DefaultVisual(display, screen_num), 
				      DefaultColormap(display, screen_num));
	      if (!xftdraw) {
		perror("XftDrawCreate(): Failed"); exit(1);}

	      xftfont = XftFontOpen(display, screen_num,
	          XFT_FAMILY, XftTypeString, "Helvetica",
		  /*XFT_SIZE, XftTypeInteger, size,*/
		  XFT_PIXEL_SIZE, XftTypeDouble, (double)size,
		  0);
	      if (!xftfont) {
		perror("XftFontOpen(): Failed"); exit(1);}
	    }
	      
	    renderrect();
	    renderstring();
	    /*# END : TT/AA message */
	    /*########################################################*/

	    break;

	case ButtonPress:
	    antialias = 0;
	    switch (report.xbutton.button) {
		case Button1:
		    moving = 1;
		    start_x = report.xbutton.x;
		    start_y = report.xbutton.y;
		    break;
	        case Button2:
		    zoom = 1;
	            if (report.xbutton.state & ShiftMask)
	                sigalrm(SIGALRM);
	            else
	                setitimer(ITIMER_REAL, &interval, 0);
		    break;
	        case Button3:
		    zoom = -1;
	            if (report.xbutton.state & ShiftMask)
	                sigalrm(SIGALRM);
	            else
	                setitimer(ITIMER_REAL, &interval, 0);
		    break;
	    }
	    break;

	case ButtonRelease:
	    switch (report.xbutton.button) {
		case Button1:
		    moving = 0;
		    break;
	        case Button2:
	        case Button3:
		    zoom = 0;
	            setitimer(ITIMER_REAL, &zero, 0);
		    break;
	    }
	    reload = 1;
	    antialias = 1;
	    renderrect();
	    renderstring();
	    reload = 0;
	    break;

	case MotionNotify:
	    if (moving) {
	        msg_x += report.xmotion.x - start_x;
	        msg_y += report.xmotion.y - start_y;
	        rect_x += report.xmotion.x - start_x;
	        rect_y += report.xmotion.y - start_y;
		start_x = report.xmotion.x;
		start_y = report.xmotion.y;
		renderrect();
	        renderstring();
	    }
	    break;

	case ConfigureNotify:

	    /*  Store new window width & height  */

	    width  = report.xconfigure.width;
	    height = report.xconfigure.height;

	    XFreePixmap(display, pixmap);
            pixmap = XCreatePixmap(display, win, width, height,
                                   DefaultDepth(display, DefaultScreen(display)));
            XFillRectangle(display, pixmap, gc, 0, 0, width, height);
	    if (xftdraw) XftDrawDestroy(xftdraw);
	    xftdraw = XftDrawCreate(display, (Drawable) pixmap, 
			            DefaultVisual(display, screen_num), 
			            DefaultColormap(display, screen_num));
	    if (!xftdraw) {
		perror("XftDrawCreate(): Failed"); exit(1);}

	    break;


	case KeyPress:

	    keysym = XLookupKeysym(&report.xkey, ShiftMapIndex);

	    if (keysym == XK_Shift_L || keysym == XK_Shift_R) break;

	    /*  Clean up and exit  */

	    XFreeGC(display, gc);
	    XCloseDisplay(display);
	    exit(EXIT_SUCCESS);

	    break;

	case MappingNotify:
	    XRefreshKeyboardMapping(&report.xmapping);
	    break;

	}
    }

    return EXIT_SUCCESS;   /*  We shouldn't get here  */
}
