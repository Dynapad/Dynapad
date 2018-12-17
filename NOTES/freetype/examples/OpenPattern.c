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
    XftPattern *pat;
    XftFont *xftfont;
    char *s;

    if ( (display = XOpenDisplay(NULL)) == NULL ) {
	printf("couldn't connect to X server\n");
	exit(EXIT_FAILURE);
    }
    screen = DefaultScreen(display);

    pat = XftPatternCreate();
    if (XftPatternAddString(pat, "file", argv[1]) != True) {
      printf("XFtPatternAddString %s failed\n", argv[1]);
      exit(EXIT_FAILURE);
    }
    if (XftPatternGetString(pat, "file", 0, &s) != XftResultMatch) {
      printf("XFtPatternGetString pat failed\n");
      exit(EXIT_FAILURE);
    }
    printf("pat %s\n", s);
    if (XftPatternAddDouble(pat, "pixelsize", 20) != True) {
      printf("XFtPatternAddDouble failed\n");
      exit(EXIT_FAILURE);
    }

    xftfont = XftFontOpenPattern(display, pat);
    if (!xftfont) {
      printf("XFtFontOpenPattern failed\n");
    }

    return EXIT_SUCCESS;
}
