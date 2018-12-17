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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/*########################################################*/
/*# BEGIN:  TT/AA message */
#include <X11/Xft/Xft.h>
/*# END : TT/AA message */
/*########################################################*/

/*  Global variables  */

Display *     display;
int           screen_num;
static char * appname;


/*  main() function  */

int main( int argc, char * argv[] ) {

    /*  Window variables  */

    Window       win;
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

    /*########################################################*/
    /*# BEGIN:  TT/AA message */
    /* XRENDER */

    static char * msg = "Hello, X FreeType AA!";
    XftDraw *xftdraw;
    XftFont *xftfont;
    XftColor color_bg,color_fg,colortwo;
    XRenderColor colortmp;

    /*# END : TT/AA message */
    /*########################################################*/
    
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

    /*
    win = XCreateSimpleWindow(display, RootWindow(display, screen_num),
			      x, y, width, height, border_width,
			      BlackPixel(display, screen_num),
    			      WhitePixel(display, screen_num));
    */

    win = XCreateWindow(display,RootWindow(display,screen_num),
			x,y,width,height,border_width,
			CopyFromParent, // depth
			CopyFromParent, // class
			CopyFromParent, // visual
			0,              // valuemask
			0);    // attributes


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

    XSelectInput(display, win, ExposureMask | KeyPressMask |
		 ButtonPressMask | StructureNotifyMask);


    /*  Load a font called "9x15"  */

    if ( (font_info = XLoadQueryFont(display, "9x15")) == NULL ) {
	fprintf(stderr, "%s: cannot open 9x15 font.\n", appname);
	exit(EXIT_FAILURE);
    }


    /*  Create graphics context  */

    gc = XCreateGC(display, win, 0, &values);

    XSetFont(display, gc, font_info->fid);
    XSetForeground(display, gc, BlackPixel(display, screen_num));


    /*  Display Window  */

    XMapWindow(display, win);



	    /*########################################################*/
	    /*# BEGIN:  TT/AA message */





	      xftdraw = XftDrawCreate(display, (Drawable) win, 
				      DefaultVisual(display, screen_num), 
				      DefaultColormap(display, screen_num));
	      if (!xftdraw) {
		perror("XftDrawCreate(): Failed"); exit(1);}

	      xftfont = XftFontOpen(display, screen_num,XFT_FAMILY, XftTypeString,
				    "lucidux",XFT_SIZE, XftTypeInteger, 25,0);
	      if (!xftfont) {
		perror("XftFontOpen(): Failed"); exit(1);}

	      colortmp.red   = 0000;
	      colortmp.green = 0000;
	      colortmp.blue  = 0000;
	      colortmp.alpha = 0x004000;
	      XftColorAllocValue(display,
				 DefaultVisual(display, screen_num), 
				 DefaultColormap(display,screen_num),
				 &colortmp,
				 &color_bg);

	      colortmp.red   = 0;
	      colortmp.green = -3000;
	      colortmp.blue  = 0;
	      colortmp.alpha = 0x001000;
	      XftColorAllocValue(display,
				 DefaultVisual(display, screen_num), 
				 DefaultColormap(display,screen_num),
				 &colortmp,
				 &color_fg);

	      colortmp.red   = 0;
	      colortmp.green = 0;
	      colortmp.blue  = -1;
	      colortmp.alpha = 0x004000;
	      XftColorAllocValue(display,
				 DefaultVisual(display, screen_num), 
				 DefaultColormap(display,screen_num),
				 &(colortmp),
				 &colortwo);


	      /*# END : TT/AA message */
	      /*########################################################*/




    
    /*  Enter event loop  */

    while ( 1 ) {
	static char * message = "Hello, X Window System!";
	static int    length;
	static int    font_height;
	static int    msg_x, msg_y;

	XNextEvent(display, &report);

	switch ( report.type ) {

	case ButtonPress:
	case Expose:

	    if ( report.xexpose.count != 0 )
		break;

	    /*  Output message centrally in window  */

	    length = XTextWidth(font_info, message, strlen(message));
	    msg_x  = (width - length) / 2;

	    font_height = font_info->ascent + font_info->descent;
	    msg_y  = (height + font_height) / 2;


	    /*########################################################*/
	    /*# BEGIN:  TT/AA message */
	    {

	      XftDrawRect(xftdraw,&color_bg,0,0,width,height);

	      XDrawString(display, win, gc, msg_x, msg_y,
			  message, strlen(message));
	      
	      XftDrawString8(xftdraw, &color_fg, xftfont, msg_x-30, msg_y+30,
			     (unsigned char *) msg, strlen(msg));

	      XftDrawString8(xftdraw, &colortwo, xftfont, msg_x-30, msg_y+30+15,
			     (unsigned char *) msg, strlen(msg));

	      XftDrawRect(xftdraw,&color_fg,10,10,100,100);
	     
	      XftDrawRect(xftdraw,&colortwo,60,60,100,100);	      
	    }
	    /*# END : TT/AA message */
	    /*########################################################*/

	    break;


	case ConfigureNotify:

	    /*  Store new window width & height  */

	    width  = report.xconfigure.width;
	    height = report.xconfigure.height;

	    break;


	    //case ButtonPress:            /*  Fall through  */
	case KeyPress:

	    /*  Clean up and exit  */

	    XUnloadFont(display, font_info->fid);
	    XFreeGC(display, gc);
	    XCloseDisplay(display);
	    exit(EXIT_SUCCESS);

	}
    }

    return EXIT_SUCCESS;   /*  We shouldn't get here  */
}
