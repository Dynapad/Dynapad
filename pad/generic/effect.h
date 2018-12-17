
#include "defs.h"
#include <X11/X.h>

class Pad_Effect {
  public:
    Pad_Effect(Display *dpy, Drawable dbl, int width, int height, int depth);
    ~Pad_Effect();
    void Dissolve(Drawable src,	int sx, int sy, int swidth, int sheight, 
		  int dx, int dy, int freq, Pad_Bool (*interrupt_p)());

  private:
    Display *dpy;
    Drawable dst;

    // Resources
    Pixmap mask, strip, result;
    GC gc, and_gc, or_gc, and_inv_gc;
};
