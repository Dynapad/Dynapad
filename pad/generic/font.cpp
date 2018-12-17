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

// Pad_Font
//
// This represents a device-independent font. Device-dependent
// color information is attached to a Pad_Font using the perdisplay
// slot (see resource.h / resource.cpp)
//
#include <stdlib.h>
#include <string.h>
#include <iostream>
using namespace std;

#include "font.h"
#include "misc.h"
#include "display.h"
#include "hashtab.h"
#include "fontdata.h"

// one pixel high system font
Pad_Font Pad_Font::systemFont("System", FONT_PLAIN, 1.0);

// twelve pixel high line font
Pad_Font Pad_Font::lineFont("Line", FONT_PLAIN, 12.0);

Pad_Font::~Pad_Font()
{
    Delete_per_display(); 
}

//
// basic constructor for fonts
//
Pad_Font::Pad_Font(const char *newName, int newStyle, float newSize)
{
    if (newName && newName[0]) {
	name = Pad_GetUid(newName);
    } else {
	name = Pad_GetUid("Line");
    }
    style = newStyle;
    size  = newSize;
}


//
// Constructor which takes names in the form:
// fontname-style-size. Also takes a default font size to use
// if none is provided.
//
void
Pad_Font::Parse_name(const char *fullname, float defaultSize) 
{
    int len = strlen(fullname);
    Pad_String buf;
    int i;
    Pad_Bool foundSep = FALSE;

    // determine family

    for (i = 0; i < len; i++) {
	char c = fullname[i];
	if (c == '-') {
	    foundSep = TRUE;
	    break;
	} 
	if (c == '/' | c == '.') {
	    // Aha! old style font name - perform conversion
	    if (Pad_FontData::File_name_and_style(fullname, buf, i)) {
		name  = Pad_GetUid(buf.Get());
		style = i;
	    } else {
		cerr << "Pad_Font:: Cannot convert " << fullname 
                     << " to new naming scheme. Using line font instead." 
                     << endl;
		name = Pad_Font::lineFont.name;
		style = FONT_PLAIN;		
	    }
	    size = 1.0;
	    return;
	}
	buf.Append(fullname[i]);
    }

    name = Pad_GetUid(buf.Get());

    if (!foundSep) {
	// no size or style given - use plain, default point
	style = FONT_PLAIN;
	if (name == Pad_GetUid(systemFont.name)) {
	    size = 1.0;
	} else {
	    size  = defaultSize;
	}
	return;
    }

    // now determine style and size
        
    i++; // skip '-'
    foundSep = FALSE;
    buf = "";

    for (; i < len; i++) {
	if (fullname[i] == '-') {
	    foundSep = TRUE;
	    break;
	}
	buf.Append(fullname[i]);
    }

    if (!foundSep) {
	// only size given - use plain font
	style = FONT_PLAIN;
	size = atof(buf.Get());
	if (size != 0)
	  return;
    }

    // if we found a '-', then both size and style are given - decode 
    // style first

    if (!strcasecmp(buf.Get(), "bold")) {
	style = FONT_BOLD;
    } else if (!strcasecmp(buf.Get(), "italic")) {
	style = FONT_ITALIC;
    } else if (!strcasecmp(buf.Get(), "bolditalic")) {
	style = FONT_BOLD | FONT_ITALIC;
    } else {
	// should we check (!strcmp(buf.Get(), "plain")) ?
	style = FONT_PLAIN;
    }

    // now determine size

    i++; // skip '-'
    if (i >= len) {
	size = defaultSize; // empty size field - use 12 instead
    } else {
	size = atof(fullname + i);
    }
} 

//
// Returns logical font name (the name given to the constructor)
// 
void
Pad_Font::Get_name(Pad_String &result) const
{
    result = name;
}

//
// Returns platform-specific font used to display this logical font
//
void
Pad_Font::Get_family(Pad_String &result) const
{
    // no distinction yet between logical name and family name
    result = name;
}

//
// gets the font, using the "fontname-style-size" format.
//
void
Pad_Font::Get(Pad_String &result) const
{
    result = name;

    if (style != FONT_PLAIN) {
	switch (style) {
	  case FONT_BOLD:
	    result += "-bold"; 
	    break;
	  case FONT_ITALIC:
	    result += "-italic"; 
	    break;
	  case (FONT_BOLD|FONT_ITALIC):
	    result += "-bolditalic"; 
	    break;
	}
    }

    if (size != FONT_DEFAULT_SIZE) {
	result += "-";
	result += size;
    }
}

float
Pad_Font::Get_size() const
{
    return size;
}

int
Pad_Font::Get_style() const
{
    return style;
}

Pad_Bool
Pad_Font::Is_plain() const
{
    return (style == FONT_PLAIN);
}

Pad_Bool
Pad_Font::Is_bold() const
{
    return (style & FONT_BOLD ? TRUE : FALSE);
}

Pad_Bool
Pad_Font::Is_italic() const
{
    return (style & FONT_ITALIC ? TRUE : FALSE);
}


void *
Pad_Font::Alloc(Pad_Display *dpy) 
{
    return (void*)dpy->Get_font_data((char*)name, style);    
}

void
Pad_Font::Free(Pad_Display *, void *) 
{
    // no unload defined for fonts yet...
}

//
// Pad_FontRef
//

Pad_FontRef::~Pad_FontRef()
{
    if (font) delete font; 
}

//
// For KPL and Tcl:
//

static Pad_HashTable nameTable;

Pad_Font *
Pad_LookupFontByName(char *name) {
    Pad_Uid nameKey = Pad_GetUid(name);
    Pad_Font *font;

    if ((font = (Pad_Font*)nameTable.Get(nameKey))) {
	return font;
    }

    font = new Pad_Font(name);
    nameTable.Set(nameKey, font);
    return font;
}


