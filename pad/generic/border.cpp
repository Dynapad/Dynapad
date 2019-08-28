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


#include "global.h"
#include "defs.h"
#include "border.h"
#include "display.h"
#include "hashtab.h"
#include "misc.h"

#include <string.h>

// Pad_Border
//
// This represents a device-independent border. Device-dependent
// border information is attached to a Pad_Border using the perdisplay
// slot (see resource.h / resource.cpp)
//

Pad_Border::~Pad_Border() {
    Delete_per_display();
}

//
// Interface to an underlying display that performs device-dependent border
// allocation.
//

void *
Pad_Border::Alloc(Pad_Display *dpy) {
    return (void *) dpy->Alloc_border(r, g, b);
}

void
Pad_Border::Free(Pad_Display *dpy, void *value) {
    dpy->Free_border((Pad_3DBorder) value);
}


//
// Pad_BorderRef - wrapper on Pad_Border that uses a hash table
//                to reduce memory cost and simplify border
//                usage inside Pad objects
//

static Pad_HashTable nametable;
static Pad_HashTable rgbtable;

// _Release()
//
// decreases counter on border, deleting the border when
// the count reaches zero.
//
static void
_Release(Pad_Border *border) {
    if (border && border->Deref()) {
        if (border->Name()) {
            nametable.Remove((void *) border->Name());
        } else {
            intptr_t red, green, blue;
            border->Get(red, green, blue);
            rgbtable.Remove((void *) CC_RGBA(red, green, blue, 0));
        }
        delete border;
        border = NULL;
    }
}

#define RELEASE() { _Release(border); border = NULL; }

Pad_BorderRef::~Pad_BorderRef() {
    RELEASE();
}

void
Pad_BorderRef::Set(intptr_t r, intptr_t g, intptr_t b) {
    void *key = (void *) CC_RGBA(r, g, b, 0);

    RELEASE();   // unreference old border

    border = (Pad_Border *) rgbtable.Get(key);

    if (!border) {
        // need to allocate new border
        border = new Pad_Border(r, g, b);
        rgbtable.Set(key, border);
    }

    border->Addref();
}

void
Pad_BorderRef::Set(const char *name) {
    intptr_t red, green, blue;

    RELEASE();        // unreference old border

    if (_Pad_colorname_to_rgb(name, red, green, blue)) {
        if (name[0] == '#') {
            Set(red, green, blue);
        } else {
            void *key = (void *) Pad_GetUid(name);
            border = (Pad_Border *) nametable.Get(key);

            if (!border) {
                // need to allocate new color
                border = new Pad_Border(name);
                nametable.Set(key, border);
            }
            border->Addref();
        }
    }
}

void
Pad_BorderRef::Set(const Pad_Color *otherColor) {
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
// Sets border to either black or white...
//
void
Pad_BorderRef::Set_contrasting(intptr_t red, intptr_t green, intptr_t blue) {
    if ((.30 * red + .61 * green + .11 * blue) <= 127) {
        // other color is dark - so use white
        Set(255, 255, 255);
    } else {
        // use black
        Set(0, 0, 0);
    }
}

void
Pad_BorderRef::Get(intptr_t &red, intptr_t &green, intptr_t &blue) const {
    if (border)
        border->Get(red, green, blue);
    else
        red = green = blue = 0;
}

void
Pad_BorderRef::Get(Pad_String &name) const {
    if (border) {
        border->Get(name);
    } else {
        name = "none";
    }
}



//
// String based border allocation/lookup - used by Tcl
//

Pad_Border *
Pad_LookupBorderByName(char *name) {
    if (name && name[0] == '#') {
        // uses #rrggbb encoding - look up color in rgb table.
        intptr_t red, green, blue;
        _Pad_colorname_to_rgb(name, red, green, blue);
        return (Pad_Border *) rgbtable.Get((void *) CC_RGBA(red, green, blue, 0));
    } else {
        // an X color name - look it up in the name table.
        return (Pad_Border *) nametable.Get((void *) Pad_GetUid(name));
    }
}

Pad_Border *
Pad_AllocBorderByName(char *name) {
    Pad_Border *border = Pad_LookupBorderByName(name);

    if (!border) {
        border = new Pad_Border(name);
        if (border->Name() == NULL) {
            // use rgb table
            intptr_t red, green, blue;
            border->Get(red, green, blue);
            rgbtable.Set((void *) CC_RGBA(red, green, blue, 0), (void *) border);
        } else {
            // use name table
            nametable.Set((void *) border->Name(), (void *) border);
        }
    }
    border->Addref();
    return border;
}

int
Pad_FreeBorderByName(char *name) {
    Pad_Border *border = Pad_LookupBorderByName(name);
    _Release(border);
    return (border ? TRUE : FALSE);
}


//
// Given a relief type, return the relief name.
// Return TRUE if valid relief type, or FALSE otherwise.
//
Pad_Bool
Pad_Relief_type_to_name(int relief, Pad_String &borderReliefName) {
    Pad_Bool rc = TRUE;

    switch (relief) {
        case PAD_RELIEF_FLAT:
            borderReliefName = "flat";
            break;
        case PAD_RELIEF_RAISED:
            borderReliefName = "raised";
            break;
        case PAD_RELIEF_SUNKEN:
            borderReliefName = "sunken";
            break;
        case PAD_RELIEF_RIDGE:
            borderReliefName = "ridge";
            break;
        case PAD_RELIEF_GROOVE:
            borderReliefName = "groove";
            break;
        default:
            rc = FALSE;
            Pad_errorString = "Invalid borderrelief type: ";
            Pad_errorString += relief;
            break;
    }

    return (rc);
}

//
// Given a relief name, return the relief type.
// Return TRUE if valid relief type, or FALSE otherwise.
//
Pad_Bool
Pad_Relief_name_to_type(char *reliefName, int &borderReliefType) {
    if (!strcmp(reliefName, "flat")) {
        borderReliefType = PAD_RELIEF_FLAT;
    } else if (!strcmp(reliefName, "raised")) {
        borderReliefType = PAD_RELIEF_RAISED;
    } else if (!strcmp(reliefName, "sunken")) {
        borderReliefType = PAD_RELIEF_SUNKEN;
    } else if (!strcmp(reliefName, "groove")) {
        borderReliefType = PAD_RELIEF_GROOVE;
    } else if (!strcmp(reliefName, "ridge")) {
        borderReliefType = PAD_RELIEF_RIDGE;
    } else {
        Pad_errorString = "Invalid borderrelief name: ";
        Pad_errorString += reliefName;
        return (FALSE);
    }
    return (TRUE);
}
