#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#define EXTERN 
#include "xgr.h"

float nodesize=1.0;

/*
unsigned sleepamount=100000;
unsigned sleepamount=500000;
*/
unsigned sleepamount=10000;

int viewspeed=2;
int zooming=0;
int POSTSCRIPT=0;

FILE *fps;

typedef struct MARKLINE{
 float x,y,x2,y2;
} MARKLINE;
MARKLINE mark[100];
int markcnt;

MARKLINE edgedisplaylist[100];
int edgedisplaylistcnt;

typedef struct STRM{
 float x,y;
 char str[20];
} STRM;
STRM strmark[100];
int strmarkcnt;

void
x_draw_frame(unsigned char *x,unsigned char *y)
{
int i;
  for(i=0;i<128;i++){
    drawline( i, 96, i, 96+0.25*(float)x[i] );
  }
  for(i=0;i<96;i++){
    drawline( 0, i, 0-0.25*(float)y[i], i );
  }
  x_update();
}


/*-----------------------------------------------------------------*/
not_main()
{
 int i;
 float x,y,x2,y2;
 char buff[40];

  xinit();
 
  sprintf(buff,"hello world");
  drawtextscreen(5,35,buff);

  while(1) {
      drawcirc(0.0,0.0,104.0);
      drawcirc(3-0.5,3-0.5,40.0);
      //fillcirc(3-0.5,3-0.5,40.0,1);
      //fillcirc(3-1.5,3-1.5,40.0,1);
      fillcirc(25,5,3,1);

    drawcirc(0.0,0.0,100.0);
    drawcirc(-50.0,-50.0,1.0);
    drawrect(10.0,10.0,100.0,10.0);
    drawrect(15.0,15.0,10.0,10.0);
    //fillcirc(15.0,15.0,1.0,1);
    //fillcirc(15.0,15.0,1.0,1);
  
    drawline( 0.0, 0.0, 100.0, 120.0 );
  
    x_update();
  }
}


/***********************************************************************/
/************************* X GRAPHICS CODE *****************************/
/***********************************************************************/

#define screenNumber     DefaultScreen(display)
#define rootWindow       RootWindow(display,screenNumber)
#define displayWidth     DisplayWidth(display,screenNumber)
#define displayHeight    DisplayHeight(display,screenNumber)
#define blackPixel       BlackPixel(display,screenNumber)
#define whitePixel       WhitePixel(display,screenNumber)
#define defaultGC        DefaultGC(display,screenNumber)
#define defaultColormap  DefaultColormap(display,screenNumber)
#define x_start		       500
#define y_start		       500
#define window_size	       500
#define xwindow_size	       700
#define ywindow_size	       600
#define increment		20
#define windowX                x_start
#define windowY                y_start
#define draw_windowX           0
#define draw_windowY           0

#define windowWidth            600
#define windowHeight           500
#define draw_windowWidth       windowWidth
#define draw_windowHeight      windowHeight
#define borderWidth            1
#define windowName             "X Test"
#define forever                while(1)

/*
static void cleardisplay();        
static void circle();                      
static void rect1();
static void change_visual();          
static void change();
*/

GC	gc;
XColor	red, green, background, foreground, exact, blackcolor, whitecolor;
char	*color = "white";
Window 	window, draw_window;
Display 	*display;

XGCValues	gc_values;        
int 		status;


/* scaling declarations */

float gscale;
float gx0;
float gy0;

#define XMAP(x) ( (x-(gx0))*gscale )
#define YMAP(y) ( draw_windowHeight - (y-(gy0))*gscale )
#define SMAP(s) ( (s)*gscale )

/*-----------------------------------------------------------------*/
void
xinit()
{
 static int first=1;

 if(first) {
   if (!(display = XOpenDisplay(NULL))) {
      fprintf(stderr,"Can't open display\n");
      exit(0);
   }

   gc_values.foreground = blackPixel;
   gc_values.line_width = 1;
   gc = XCreateGC( display,
			rootWindow,
			GCForeground | GCLineWidth, 
			&gc_values);

   status = XAllocNamedColor(display,
				defaultColormap,
				"black",
				&blackcolor,
				&exact);
   status = XAllocNamedColor(display,
				defaultColormap,
				"white",
				&whitecolor,
				&exact);

   status = XAllocNamedColor(display,
				defaultColormap,
				"black",
				&red,
				&exact);
   if(!status)
	printf("Can't allocate named color 'red'.\n"), exit(0);

   status = XAllocNamedColor(display,
				defaultColormap,
				color,
				&background,
				&exact);
   if(!status)
	printf("Can't allocate named color %s.\n", color), exit(0);

   status = XAllocNamedColor(display,
				defaultColormap,
				"white",
				&green,
				&exact);
   if(!status)
	printf("Can't allocate named color 'green'.\n"), exit(0);

   window = XCreateSimpleWindow(
      display,             
      rootWindow,          
      windowX,             
      windowY,             
      windowWidth,         
      windowHeight,        
      borderWidth,         
      blackPixel,          
      background.pixel);         

   XChangeProperty(display,window,
                   XA_WM_NAME,             
                   XA_STRING,              
                   8,                      
                   PropModeReplace,        
                   (unsigned char*)windowName,             
                   strlen(windowName));    


   draw_window = XCreateSimpleWindow(
      display,             
      window,          
      draw_windowX,             
      draw_windowY,             
      draw_windowWidth,         
      draw_windowHeight,        
      borderWidth,         
      blackPixel,          
      whitePixel);         


   if(!XAllocNamedColor(display, defaultColormap, "black", &foreground, &exact))
     fprintf(stderr, "can't allocate named color"), exit(0);

   if(!XAllocNamedColor(display, defaultColormap, "white", &background, &exact))
     fprintf(stderr, "can't allocate named color"), exit(0);
 }

 first=0;

   XMapWindow(display,window);
   XMapWindow(display,draw_window);
   
   XSetWindowBackground(display, window, whitecolor.pixel);
   XSetForeground(display, defaultGC, blackcolor.pixel);

   markcnt=0;
   strmarkcnt=0;

  /*
  setview( 25.0, 15.0, 14.0);
  */
  setview( 35.0, 15.0, 12.0);
}

/*-----------------------------------------------------------------*/
void
x_update()
{
      XFlush(display);
}  

/*-----------------------------------------------------------------*/
void change_visual(
Display		*display,
Window		window,
char		*string,
int		reason)
{
   if(reason == 1) {
	XSetWindowBackground(display, window, red.pixel);
	XSetForeground(display, defaultGC, green.pixel);
   }
   if(reason == 2) {
	XSetWindowBackground(display, window, green.pixel);
	XSetForeground(display, defaultGC, red.pixel);
   }
   XClearWindow(display, window);
}

/*-----------------------------------------------------------------*/
void cleardisplay(
   Display *display,
   Window draw_window)
{
   XClearWindow(display,draw_window);
}   


/*-----------------------------------------------------------------*/
void
drawtext(float x, float y, char *string)
{
 int ix,iy;
 ix = (int)XMAP(x);
 iy = (int)YMAP(y);

   if( POSTSCRIPT ){
     fprintf(fps,"/Helvetica  findfont 0.5 scalefont setfont\n");
     fprintf(fps,"%d %d moveto (%s) show\n", ix,iy,string);
   }
   else
     XDrawString(display,draw_window,gc, ix,iy,string,strlen(string));
}

/*-----------------------------------------------------------------*/
void
drawtextscreen(int ix, int iy, char *string)
{
   XDrawString(display,draw_window,gc, ix,iy,string,strlen(string));
}

/*-----------------------------------------------------------------*/
void
clearscreen()
{
   cleardisplay(display,draw_window);
}

/*-----------------------------------------------------------------*/
void
drawcirc( float x, float y, float size)
{
 int ix,iy,isize;
 ix = (int)XMAP(x);
 iy = (int)YMAP(y);
 isize = (int)SMAP(size);

 if( POSTSCRIPT ){
   fprintf(fps,"%f      %f add   %f moveto\n",ix+isize*0.5,isize*0.5,iy-isize*0.5);
   fprintf(fps,"%f %f   %f 0 360 arc stroke\n",ix+isize*0.5,iy-isize*0.5,isize*0.5);
 }
 else
   XDrawArc(display,draw_window, gc, ix,iy-isize, isize, isize, 0, 360*64);
}

/*-----------------------------------------------------------------*/
void
fillcirc( float x, float y, float size, int color)
{
 int ix,iy,isize;
 ix = (int)XMAP(x);
 iy = (int)YMAP(y);
 isize = (int)SMAP(size);

 if(color){
	/*
	XSetWindowBackground(display, window, whitecolor.pixel);
	*/
	XSetForeground(display, gc, blackcolor.pixel);
 } 
 else {
	/*
	XSetWindowBackground(display, window, blackcolor.pixel);
	*/
	XSetForeground(display, gc, whitecolor.pixel);
 }

	/*
	XSetWindowBackground(display, window, whitecolor.pixel);
	XSetForeground(display, defaultGC, blackcolor.pixel);
	*/
 if( POSTSCRIPT ){
   fprintf(fps,"gsave\n");
   if(color)
     fprintf(fps,"0 setgray\n");
   else
     fprintf(fps,"1 setgray\n");

   fprintf(fps,"%f      %f add   %f moveto\n",ix+isize*0.5,isize*0.5,iy-isize*0.5);
   fprintf(fps,"%f %f   %f 0 360 arc fill\n",ix+isize*0.5,iy-isize*0.5,isize*0.5);
   fprintf(fps,"grestore\n");
 }
 else {
   XFillArc(display,draw_window, gc, ix,iy-isize, isize, isize, 0, 360*64);
   XSetWindowBackground(display, window, whitecolor.pixel);
   XSetForeground(display, gc, blackcolor.pixel);
 }
} 

/*-----------------------------------------------------------------*/
void
drawline(float x, float y, float x2, float y2)
{
 int ix,iy,ix2,iy2;
 ix  = (int)XMAP(x);
 iy  = (int)YMAP(y);
 ix2 = (int)XMAP(x2);
 iy2 = (int)YMAP(y2);

   //XDrawLine(display,draw_window, gc, 10,10, 50,50);
 
 if( POSTSCRIPT ){
   fprintf(fps,"%d %d moveto %d %d lineto stroke\n",ix,iy,ix2,iy2);
 }
 else
   XDrawLine(display,draw_window, gc, ix,iy, ix2,iy2);
} 

/*-----------------------------------------------------------------*/
void
drawrect(float x,float y,float x2,float y2)
{
  drawline(x,y,x2,y);
  drawline(x2,y,x2,y2);
  drawline(x2,y2,x,y2);
  drawline(x,y2,x,y);
}

/*-----------------------------------------------------------------*/
void
drawpoly(Point *xy,int cnt)
{
 int i;
 for(i=1;i<cnt;i++){
   drawline(xy[i].x,xy[i].y,xy[i-1].x,xy[i-1].y);
 }
 drawline(xy[0].x,xy[0].y,xy[cnt-1].x,xy[cnt-1].y);
}

/*-----------------------------------------------------------------*/
void
fixview(float x0,float y0,float scl)
{
  gx0 = x0;
  gy0 = y0;
  gscale = scl;

}

/*-----------------------------------------------------------------*/
void
setview(float x0,float y0,float scl)
{
  gx0  =  x0 - 0.5*draw_windowWidth/scl; 
  gy0 =   y0 - 0.5*draw_windowHeight/scl ;
  gscale = scl;
}


/*-----------------------------------------------------------------*/
void
addline(float x, float y, float x2, float y2)
{
  mark[markcnt].x  = x;
  mark[markcnt].y  = y;
  mark[markcnt].x2 = x2;
  mark[markcnt].y2 = y2;
  markcnt++;

}

/*-----------------------------------------------------------------*/
void
add_display_edge(float x, float y, float x2, float y2)
{
  edgedisplaylist[ edgedisplaylistcnt ].x  = x;
  edgedisplaylist[ edgedisplaylistcnt ].y  = y;
  edgedisplaylist[ edgedisplaylistcnt ].x2 = x2;
  edgedisplaylist[ edgedisplaylistcnt ].y2 = y2;
                   edgedisplaylistcnt++;

}

/*-----------------------------------------------------------------*/
void
addtext(float x, float y, char *str)
{
  strmark[strmarkcnt].x  = x;
  strmark[strmarkcnt].y  = y;
  strcpy( strmark[strmarkcnt].str, str);
  strmarkcnt++;

 
}



/*-----------------------------------------------------------------*/
void
openpostscript(char *filename)
{
  fps = fopen(filename,"w");
  if( fps ){
    fprintf(fps,"%%! ps\n");
    fprintf(fps,"90 rotate\n");
    fprintf(fps,"50 50  translate\n");
    fprintf(fps,"1 -1  scale \n");
    fprintf(fps,"0.001 setlinewidth \n");
  }
  else
    fps = stdout;

  POSTSCRIPT=1;
 
}
/*-----------------------------------------------------------------*/
void
closepostscript()
{
  fprintf(fps,"\n");
  fprintf(fps,"showpage\n");
  fclose(fps);
  POSTSCRIPT=0;
 
}


