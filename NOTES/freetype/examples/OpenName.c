#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <X11/Xft/Xft.h>

Display *     display;
int           screen;

char *
print_xftresult(XftResult result)
{
  switch (result) {
    case XftResultMatch:
      return "XftResultMatch";
      break;
    case XftResultNoMatch:
      return "XftResultNoMatch";
      break;
    case XftResultTypeMismatch:
      return "XftResultTypeMismatch";
      break;
    case XftResultNoId:
      return "XftResultNoId";
      break;
    default:
      return "unknown";
      break;
  }
}

int main(int argc, char **argv)
{

    XftFont *xftfont;

    if ( (display = XOpenDisplay(NULL)) == NULL ) {
	printf("couldn't connect to X server\n");
	exit(EXIT_FAILURE);
    }
    screen = DefaultScreen(display);

    xftfont = XftFontOpenName(display, screen,
        "/usr/X11R6/lib/X11/fonts/Type1/UTRG____.pfa");

    return EXIT_SUCCESS;
}
