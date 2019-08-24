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


#include "defs.h"
#include "color.h"
#include "display.h"
#include "hashtab.h"
#include "misc.h"

#include <string.h>
#include <stdio.h>

// Pad_Color 
//
// This represents a device-independent color. Device-dependent
// color information is attached to a Pad_Color using the perdisplay
// slot (see resource.h / resource.cpp)
//

Pad_Color::~Pad_Color() {
    Delete_per_display();
}

//
// Note: dealing with color names is fiddly.
//
// For example, color names that start with a '#' are treated specially: 
// We avoid calling Pad_GetUid on color names which start with
// a '#', since there are (in principal) 2^24 names starting with a #, and
// we don't want to fill up Tk's uid table.
//


// standard colors

Pad_Color Pad_Color::black(0, 0, 0);
Pad_Color Pad_Color::white(255, 255, 255);
Pad_Color Pad_Color::gray(128, 128, 128);

Pad_Color::Pad_Color(const char *colorname) {
    intptr_t red, green, blue;

    if (colorname[0] == '#') {    // RGB encoded name - don't remember it
        name = NULL;
    } else {                    // remember the Uid for the name
        name = Pad_GetUid(colorname);
    }
    // convert the name to rgb
    if (_Pad_colorname_to_rgb(colorname, red, green, blue)) {
        r = red;
        g = green;
        b = blue;
    } else {
        r = g = b = 0;        // probably better to print a warning?
    }

}

void
Pad_Color::Get(Pad_String &colorname) const {
    if (name) {
        colorname = name;
    } else {
        // print name using RGB encoding.
        char buf[50];
        sprintf(buf, "#%02x%02x%02x", (int) r, (int) g, (int) b);
        colorname = buf;
    }
}

Pad_Bool
Pad_Color::Equals(const Pad_Color *otherColor) const {
    if (!otherColor) {
        return FALSE;
    } else {
        return (r == otherColor->r && g == otherColor->g && b == otherColor->b
                && name == otherColor->name);
    }
}

//
// Interface to an underlying display that performs device-dependent color
// allocation.
//

void *
Pad_Color::Alloc(Pad_Display *dpy) {
    return (void *) dpy->Alloc_color(r, g, b);
}

void
Pad_Color::Free(Pad_Display *dpy, void *value) {
    dpy->Free_color((XColor *) value);
}


//
// Pad_ColorRef - wrapper on Pad_Color that uses a hash table
//                to reduce memory cost and simplify color usage inside Pad objects
//

static Pad_HashTable rgbtable;
static Pad_HashTable nametable;

// _Release()
//
// decreases counter on color, deleting the color when    
// the count reaches zero.
//
static void
_Release(Pad_Color *color) {
    if (color && color->Deref()) {
        if (color->Name()) {
            nametable.Remove((void *) color->Name());
        } else {
            intptr_t red, green, blue;
            color->Get(red, green, blue);
            rgbtable.Remove((void *) CC_RGBA(red, green, blue, 0));
        }
        delete color;
    }
}

#define RELEASE() { _Release(color); color = NULL; }

Pad_ColorRef::~Pad_ColorRef() {
    RELEASE();
}

void
Pad_ColorRef::Set(intptr_t r, intptr_t g, intptr_t b) {
    r = MIN(255, r);
    r = MAX(0, r);
    g = MIN(255, g);
    g = MAX(0, g);
    b = MIN(255, b);
    b = MAX(0, b);

    void *key = (void *) CC_RGBA(r, g, b, 0);

    RELEASE();   // unreference old color

    color = (Pad_Color *) rgbtable.Get(key);

    if (!color) {
        // need to allocate new color
        color = new Pad_Color(r, g, b);
        rgbtable.Set(key, color);
    }

    color->Addref();
}

void
Pad_ColorRef::Set(const char *name) {
    intptr_t red, green, blue;

    RELEASE();   // unreference old color

    if (_Pad_colorname_to_rgb(name, red, green, blue)) {     // yes, I recognize the name ...
        if (name[0] == '#') {                     // its an #rrggbb style name.
            Set(red, green, blue);
        } else {                                     // its an X color name
            void *key = (void *) Pad_GetUid(name);
            color = (Pad_Color *) nametable.Get(key);

            if (!color) {
                // need to allocate new color
                color = new Pad_Color(name);
                rgbtable.Set(key, color);
            }
            color->Addref();
        }
    }
}

void
Pad_ColorRef::Set(const Pad_Color *otherColor) {
    RELEASE();

    if (otherColor) {
        if (otherColor->Name()) {
            Set(otherColor->Name());
        } else {
            intptr_t red, green, blue;
            otherColor->Get(red, green, blue);
            Set(red, green, blue);
        }
    }
}

//
// Sets color to either black or white...
// 
void
Pad_ColorRef::Set_contrasting(int red, int green, int blue) {
    if ((.30 * red + .61 * green + .11 * blue) <= 127) {
        // other color is dark - so use white
        Set(255, 255, 255);
    } else {
        // use black
        Set(0, 0, 0);
    }
}

void
Pad_ColorRef::Get(intptr_t &red, intptr_t &green, intptr_t &blue) const {
    if (color)
        color->Get(red, green, blue);
    else
        red = green = blue = 0;
}

void
Pad_ColorRef::Get(Pad_String &name) const {
    if (color) {
        color->Get(name);
    } else {
        name = "none";
    }
}

//
// String based color allocation/lookup - used by Tcl
//


Pad_Color *
Pad_LookupColorByName(char *name) {
    if (name && name[0] == '#') {
        // uses #rrggbb encoding - look up color in rgb table.
        intptr_t red, green, blue;
        _Pad_colorname_to_rgb(name, red, green, blue);
        return (Pad_Color *) rgbtable.Get((void *) CC_RGBA(red, green, blue, 0));
    } else {
        // an X color name - look it up in the name table.
        return (Pad_Color *) nametable.Get((void *) Pad_GetUid(name));
    }
}

Pad_Color *
Pad_AllocColorByName(char *name) {
    Pad_Color *color = Pad_LookupColorByName(name);

    if (!color) {    // unknown color ...
        color = new Pad_Color(name);
        if (color->Name() == NULL) {
            // use rgb table
            intptr_t red, green, blue;
            color->Get(red, green, blue);
            rgbtable.Set((void *) CC_RGBA(red, green, blue, 0), (void *) color);
        } else {
            // use name table
            nametable.Set((void *) Pad_GetUid(name), (void *) color);
        }
    }
    color->Addref();
    return color;
}

int
Pad_FreeColorByName(char *name) {
    Pad_Color *color = Pad_LookupColorByName(name);
    _Release(color);
    return (color ? TRUE : FALSE);
}






