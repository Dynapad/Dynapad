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

#ifndef _PAD_COLOR_H
#define _PAD_COLOR_H

#include "defs.h"
#include "pad-string.h"
#include "resource.h"

//
// Pad_Color class record
//
class Pad_Color : public Pad_Resource {

public:
    // constructors, in various forms
    Pad_Color() : r(0), g(0), b(0), name(0) {};

    Pad_Color(const int red, const int green, const int blue) : r(red), g(green), b(blue), name(0) {};

    Pad_Color(const char *name);

    Pad_Color(const Pad_Color *copy) : r(copy->r), g(copy->g), b(copy->b), name(copy->name) {};

    virtual ~Pad_Color();

    //
    // methods for getting the color data stored in a 
    // Pad_Color. Note that colors are immutable - if you
    // want to change a color, delete the old color and make a new one.
    //
    inline void Get(intptr_t &red, intptr_t &green, intptr_t &blue) const {
        red = r;
        green = g;
        blue = b;
    };

    void Get(Pad_String &string) const;

    // returns the internal color name, or NULL if the color has no name.
    char *Name() const { return name; }

    Pad_Bool Equals(const Pad_Color *otherColor) const; // true if two colors are equal


    // methods for allocating or freeing a color for a specific display device.
    virtual void *Alloc(Pad_Display *dpy);

    virtual void Free(Pad_Display *dpy, void *value);

    // standard colors
    static Pad_Color black, white, gray;

protected:
    unsigned char r, g, b;    // color data (kept in rgb form)
    char *name;                 // color name (or NULL, if none is given)
};



//
// Pad_ColorRef 
//
// Wrapper on Pad_Color that uses a hashtable with reference counting,
// to avoid allocating multiple Pad_Color's for the same r/g/b. Note that
// it is legal to directly use Pad_Color - this wrapper is primarily cosmetic.
//

class Pad_ColorRef {
    friend class Pad_Renderer;

    friend class Pad_BorderRef;

public:
    Pad_ColorRef() : color(NULL) {};

    ~Pad_ColorRef();

    inline Pad_Bool Is_set() const { return color ? TRUE : FALSE; }

    // Set the current color
    void Set(intptr_t r, intptr_t g, intptr_t b);

    void Set(const char *name);

    void Set(const Pad_Color *color);

    void Set(const Pad_ColorRef &colorref) { Set(colorref.color); }

    void Set_contrasting(int r, int g, int b);

    // Compare with other colors
    Pad_Bool Equals(const Pad_Color *otherColor) const {
        return (color == otherColor) ||
               (color && color->Equals(otherColor));
    }

    Pad_Bool Equals(Pad_ColorRef &colorref) const { return Equals(colorref.color); }

    // Get the current color
    void Get(intptr_t &red, intptr_t &green, intptr_t &blue) const;

    void Get(Pad_String &string) const;

protected:
    // pointer to the underlying color
    Pad_Color *color;
};

//
// used by Tcl API (for Pad commands like alloccolor, freecolor, setcolor etc.)
//
extern Pad_Color *Pad_LookupColorByName(char *name);

extern Pad_Color *Pad_AllocColorByName(char *name);

extern int Pad_FreeColorByName(char *name);

//
// Used by color.cpp and border.cpp to map X color names to their
// RGB values and visa-versa. See colordb.cpp
//
extern int _Pad_colorname_to_rgb(const char *name, intptr_t &r, intptr_t &g, intptr_t &b);

#endif /* _PAD_COLOR_H */
