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

#ifndef _PAD_FONT_H
#define _PAD_FONT_H

#include "defs.h"
#include "pad-string.h"
#include "resource.h"

// font styles
#define FONT_PLAIN  0
#define FONT_BOLD   1
#define FONT_ITALIC 2

#define FONT_DEFAULT_SIZE 12
//
// Structure to store font information
//
class Pad_Font : public Pad_Resource
{
  public:
    static Pad_Font systemFont; // Historical system font - 1 point default
    static Pad_Font lineFont;	// Line font - 12 point default

    Pad_Font(const char *name, int style, float size);

    Pad_Font(const char *fullname) {
	Parse_name(fullname, FONT_DEFAULT_SIZE);
    };

    Pad_Font(const char *fullname, float defaultSize) { 
	Parse_name(fullname, defaultSize); 
    };

    Pad_Font(Pad_Font *other) {
	style = other->style;
	name = other->name;
	size = other->size;
    }

    virtual ~Pad_Font();

    void Get(Pad_String &fullname) const; 
    void Get_family(Pad_String &name) const;
    void Get_name(Pad_String &name) const;
    float Get_size() const;
    int Get_style() const;

    char *Name() const { return name; } // returns handle to internal name

    Pad_Bool Is_plain() const;
    Pad_Bool Is_bold() const;
    Pad_Bool Is_italic() const;
    
  protected:
    // methods for allocating or freeing a font for a specific display device.
    virtual void *Alloc(Pad_Display *dpy);
    virtual void Free(Pad_Display *dpy, void *value);
    
  private:
    unsigned char style;
    char *name;
    float size;

    void Parse_name(const char *fullname, float defaultSize);
};

//
// Wrapper on Pad_Font that simplifies allocation/deletion (currently
// doesn't cache fonts)
//
class Pad_FontRef {
  friend class Pad_Renderer;
  
 public:
  Pad_FontRef()  { font = NULL; }
  ~Pad_FontRef();
  
  void Set(const char *name, int style, float size) {
      if (font) delete font;
      font = (name ? new Pad_Font(name, style, size) : (Pad_Font *)NULL);
  }
  
  void Set(const char *fullname) {	
      if (font) delete font;
      font = (fullname ? new Pad_Font(fullname) : (Pad_Font *)NULL);
  }

  void Set(const char *fullname, float defaultSize) {
      if (font) delete font;
      font = (fullname ? new Pad_Font(fullname, defaultSize) : (Pad_Font *)NULL);
  }

  void Set(Pad_FontRef &other) {
      if (font) delete font;
      font = other.font ? new Pad_Font(other.font) : (Pad_Font *)NULL;
  }
  
  void Set_size(float size) {
      int style;
      Pad_String name;
      
      Get_name(name);
      style = Get_style();
      
      Set(name.Get(), style, size);
  }

  inline Pad_Bool Is_set() const { return font ? TRUE : FALSE ; }

  void Get(Pad_String &fullname) const {
      if (font) font->Get(fullname);
      else fullname = "Line";
  }

  void Get_name(Pad_String &name) const {
      if (font) font->Get_name(name);
      else name = "Line";
  }

  int Get_style() const {
      if (font) return font->Get_style();
      else return FONT_PLAIN;
  }

  float Get_size() const {
      if (font) return font->Get_size();
      else return 0;
  }

  protected:
    Pad_Font *font;    
};

Pad_Font *Pad_LookupFontByName(char *name);

#endif /* _PAD_FONT_H */

