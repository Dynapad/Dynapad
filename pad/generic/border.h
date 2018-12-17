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

#ifndef _PAD_BORDER_H
#define _PAD_BORDER_H

#include "defs.h"
#include "color.h"

#include <string.h>
//
// Pad_Border class record
//
class Pad_Border : public Pad_Color {
  public:
    // constructors, in various forms
    Pad_Border() : Pad_Color() { } ;
    Pad_Border(int red, int green, int blue) : Pad_Color(red, green, blue) { } ;
    Pad_Border(const char *name) : Pad_Color(name) { };
    Pad_Border(const Pad_Border *copy) : Pad_Color(copy) { } ;

   virtual ~Pad_Border();

  protected:
    // methods for allocating or freeing a color for a specific display device.
    virtual void *Alloc(Pad_Display *dpy);
    virtual void Free(Pad_Display *dpy, void *value);
};

//
// Pad_BorderRef
//
// Wrapper on Pad_Border that uses a hashtable with reference counting,
// to avoid allocating multiple Pad_Border's for the same r/g/b. Note that
// it is legal to directly use Pad_Border - this wrapper is primarily cosmetic.
//

class Pad_BorderRef
{
  friend class Pad_Renderer;

  public:
    Pad_BorderRef() : border(NULL) { };
    ~Pad_BorderRef();

    inline Pad_Bool Is_set() const { return border ? TRUE : FALSE ; }

    // Set the current border
    void Set(int r, int g, int b);
    void Set(const char *name);
    void Set(const Pad_Color *color);
    void Set(const Pad_BorderRef &borderref) { Set(borderref.border); }
    void Set(const Pad_ColorRef &colorref) { Set(colorref.color); }
    void Set_contrasting(int red, int green, int blue);

    // Compare with other borders
    Pad_Bool Equals(const Pad_Border *otherBorder) const { 
	return (border == otherBorder) ||
	  (border && border->Equals(otherBorder)); 
    }
    Pad_Bool Equals(const Pad_BorderRef &borderref) const {
	return Equals(borderref.border); 
    }

    // Get the current border
    void Get(int &r, int &g, int &b) const;
    void Get(Pad_String &string) const;

  protected:
    // pointer to the underlying border
    Pad_Border *border;    
};

//
// used by Tcl API (for Pad commands like allocborder,
// freeborder, setborder etc.)
//
extern Pad_Border *Pad_LookupBorderByName(char *name);
extern Pad_Border *Pad_AllocBorderByName(char *name);
extern int        Pad_FreeBorderByName(char *name);

extern Pad_Bool Pad_Relief_type_to_name(int relief, Pad_String &borderReliefName);
extern Pad_Bool Pad_Relief_name_to_type(char *reliefName, int &borderReliefType);

#endif /* _PAD_BORDER_H */
