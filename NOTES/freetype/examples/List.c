#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/Xatom.h>

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include <X11/Xft/Xft.h>

Display *     display;
int           screen_num;

int main(int argc, char **argv)
{
    XftFontSet *fs;
    XftPattern **fonts;
    int i;
    char *family, *style, *file;

    if ( (display = XOpenDisplay(NULL)) == NULL ) {
	fprintf(stderr, "couldn't connect to X server\n");
	exit(EXIT_FAILURE);
    }
    screen_num     = DefaultScreen(display);

    fs = XftListFonts(display, screen_num,
        /*XFT_FAMILY, XftTypeString, "Courier",*/
	0,
        "family",
	"style",
	"file",
	0);
    if (fs == NULL) {
      fprintf(stderr, "XftListPatternObjects failed\n");
      exit(-1);
    }

    printf("nfont sfont %d %d\n", fs->nfont, fs->sfont);
    for (i = 0, fonts = fs->fonts; i < fs->nfont; i++, fonts++) {
      if (XftPatternGetString(*fonts, "family", 0, &family) != XftResultMatch) {
        fprintf(stderr, "XftPatternGetString family failed\n");
	exit(-1);
      }
      /*printf("%s\n", family);*/
      if (XftPatternGetString(*fonts, "style", 0, &style) != XftResultMatch) {
        fprintf(stderr, "XftPatternGetString style failed\n");
	exit(-1);
      }
      if (XftPatternGetString(*fonts, "file", 0, &file) != XftResultMatch) {
        fprintf(stderr, "XftPatternGetString file failed\n");
	exit(-1);
      }
      printf("%s - %s - %s\n", family, style, file);
    }

    return EXIT_SUCCESS;
}
