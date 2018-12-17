/*
"(c) Copyright 1993-1997 Pad++ Consortium {University of New Mexico (UNM),
and New York University (NYU)}, All Rights Reserved."  
Licensee can not remove or obscure any of the
copyright or trademark notices in this software.

IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.

See the file "License" for general information on usage and
redistribution, and the file "LicenseTerms" for the specific license
agreement on usage and redistribution of this file, and the Pad++
software in general.
*/


#ifndef _PAD_FONTCACHE_H
#define _PAD_FONTCACHE_H

#define PAD_FC_CHAR_WIDTH 96
#define PAD_FC_CHAR_HEIGHT 96
#define PAD_FC_BASE_CHAR 32
#define PAD_FC_NUM_CHARS 96

class ZFontPoly;
class Pad_Renderer;

class Pad_FontCacheEntry {
  public:
    ZFontPoly *font;
    Pixmap stipple; // transparency stipple used to draw character
    unsigned char size; // font size
    unsigned char width, height;
    short x, xoffset, yoffset;
};

class Pad_FontCache {
  public:    
    Pad_FontCache();
    ~Pad_FontCache();
    Pad_Bool Allocate(Display *disp);
    void Setup(Pad_Renderer *r, Drawable dbl, GC gc, Pixmap stipple);
    void Draw_char(ZFontPoly *font, int c, float xoffs, int mag, Pixmap stipple);

  private:

    Pad_FontCacheEntry cache[PAD_FC_NUM_CHARS];
    Pixmap bitmap; // font cache characters
    GC bitGC;
    GC fillGC;
    GC myGC;

    Display *display;
    Pad_Renderer *renderer;

    // Per-render details
    Drawable drawable;
    GC fgGC;
    Pixmap orig_stipple; 

    Pad_FontCacheEntry *Update_cache(ZFontPoly *font, int c, int size, 
			    float xoffs, Pad_Point &origin, Pixmap stipple);

    void Draw_direct(ZFontPoly *font, int c, float xoffs);

    Pad_FontCacheEntry *In_cache(ZFontPoly *font, 
				 int c, int size, Pixmap stipple);

};

class Pad_FontCacheList {
  public:
    Pad_FontCacheList(Display *disp, int size);
    ~Pad_FontCacheList();

    inline int Length() { return length; }

    inline Pad_FontCache *Get(int n) {
	return (&list[n % length]);
    }

  private:
    int length;
    Pad_FontCache *list;
};


#endif /* _PAD_FONTCACHE_H */
