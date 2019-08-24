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

#ifndef _FONTDATA_H
#define _FONTDATA_H

#include "defs.h"
#include "pad-string.h"

#include <X11/Xft/Xft.h>

class ZFontType1;
class ZFontPoly;

// Pad_FontData is a class for locating Type-1 Postscript font data for a
// given fontname/style.

class Pad_FontData {
  public:
    friend class Pad_XRenderer;

    static Pad_FontData lineFontData;
    static Pad_FontData systemFontData;	  // Historical line font

    // information available for all fonts (loaded and unloaded)
    Pad_Uid name;              // my logical font name
    unsigned char style;      // style flags
    Pad_String filename;      // file containing .pfa data for font
    Pad_Bool loaded;          // set true when font is loaded from file
    Pad_Bool substituted;     // set true if font gets substituted

    // information that is set once a font is loaded
    Pad_Bool useLineFont;     // true if this should be drawn using line font
    int id;                   // font ID number
    ZFontType1 *zfont;        // postscript font
    ZFontPoly  *lowResPoly;   // low res polygonal version
    ZFontPoly  *highResPoly;  // high res polygonal version    

    XftFont *xftfont;
    static Pad_Bool xftenable;
    float previousmag;
    float xftfactor;
    Pad_Bool xftfactorchanged;

    // used internally in fontdata.cpp
    Pad_FontData(const char *filename, const char *name, int style);
    Pad_FontData(const char *name);
    Pad_FontData(Pad_FontData *copy);
    ~Pad_FontData();

    // Methods for querying the underlying font geometry (once its loaded)
    float Char_height(char c = 'A');
    float Char_width(char c = 'A');
    void String_extents(char *string, float &width, float &height);
    void Load();

    // Font Data Manager API
    // Sets the path used to locate postscript font files
    static void Set_path(char *path);
    
    // Returns the current path used to locate font files
    static void Get_path(Pad_String &path);

    // Adds single directory to end of font path
    static void Append_to_path(char *dirname);

    static void Set_default_font_path();

    // Gets list of all available fonts
    static void Get_available_fonts(Pad_List &fonts);

    // Given a postscript type 1 font filename, obtains its logical name/style.
    // Returns true if the file can be found, false otherwise.
    static int File_name_and_style(const char *filename, 
				   Pad_String &name, int &style);

    // Retrieves font with given name/style, loading it if required.
    // May return a substitute font if the name/style isn't available.
    static void *Get_font_data(char *name, int style);

    // Utility function to map style number to style name
    static const char *Style_as_string(int style);
};

#endif
