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
    XftPattern *pat;
    XftResult result;
    char *s;
    double d;

    if ( (display = XOpenDisplay(NULL)) == NULL ) {
	printf("couldn't connect to X server\n");
	exit(EXIT_FAILURE);
    }
    screen = DefaultScreen(display);

    printf("using family/size\n");
    xftfont = XftFontOpen(display, screen,
        XFT_FAMILY, XftTypeString, "Utopia",
        XFT_SIZE, XftTypeInteger, 25,
        0);
    pat = xftfont->pattern;
    result = XftPatternGetString(pat, "file", 0, &s);
    if (XftPatternGetString(pat, "file", 0, &s) != XftResultMatch) {
        print_xftresult(result);
    }
    printf("file %s\n", s);
    if (XftPatternGetDouble(pat, "pixelsize", 0, &d) != XftResultMatch) {
        print_xftresult(result);
    }
    printf("pixelsize %f\n", d);

    printf("using file\n");
    xftfont = XftFontOpen(display, screen,
        XFT_FILE, XftTypeString, "/usr/X11R6/lib/X11/fonts/Type1/UTRG____.pfa",
        0);
    pat = xftfont->pattern;
    result = XftPatternGetString(pat, "file", 0, &s);
    if (XftPatternGetString(pat, "file", 0, &s) != XftResultMatch) {
        print_xftresult(result);
    }
    printf("file %s\n", s);

    return EXIT_SUCCESS;
}
