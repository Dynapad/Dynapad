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

#ifndef COMPONENT_H
#define COMPONENT_H 1

#include "defs.h"
#include "object.h"
#include "point.h"
#include "color.h"
#include "font.h"

class Pad;

#define COMPONENT_DEFAULT_PEN         "black"          // Default pen color
#define COMPONENT_DEFAULT_FILL        "#c0c0c0"        // Default fill color
#define COMPONENT_DEFAULT_FONT        "Times-12"       // Default font
#define COMPONENT_DEFAULT_HILITEWIDTH 1                // Default width of hilite border

//
// Utility classes
//
class Pad_Dimension {
  public:
    Pad_Dimension();
    Pad_Dimension(float newWidth, float newHeight);

    float width;
    float height;

    void  Set(float newWidth, float newHeight);
};

class Pad_Area {
  public:
    Pad_Area();
    Pad_Area(float newX, float newY, float newWidth, float newHeight);

    float x;
    float y;
    float width;
    float height;

    Pad_Bool Contains(Pad_Point &point);
    void     Set(float newX, float newY, float newWidth, float newHeight);
};

				       // Bits for _componentFlags
#define COMPONENT_DISABLED    (1<<0)   // Set if component is disabled
#define COMPONENT_PEN_SET     (1<<1)   // Set if pen is non-default
#define COMPONENT_FILL_SET    (1<<2)   // Set if fill is non-default
#define COMPONENT_FONT_SET    (1<<3)   // Set if font is non-default
#define COMPONENT_RESHAPE_SET (1<<4)   // Set if component has been reshaped

#define COMPONENT_ALIGN_LEFT   0	       // Align item on left side
#define COMPONENT_ALIGN_CENTER 1	       // Align item centered
#define COMPONENT_ALIGN_RIGHT  2	       // Align item on right side

class Pad_Component : public Pad_Object {
  public:
				// Construct a new Pad_Component on the pad surface.
    Pad_Component(Pad *pad);
    virtual ~Pad_Component();

				// Private data used and managed by the application
    void *clientData;

				// Same as map/unmap 
    virtual void       Show(void);
    virtual void       Hide(void);

				// At least refuse focus and button presses when disabled
    virtual void       Enable(void);
    virtual void       Disable(void);
    virtual Pad_Bool   Is_enabled(void);

    virtual Pad_Bool   Is_active(void);	     // True if pointer within frame that component is in

				// Default render method
    virtual Pad_Bool   Render(void);

				// If hasn't been reshaped, then this auto-shapes
    virtual void       Compute_bounding_box(void);

				// Reshape to bounding box (x, y, width, height)
    virtual void       Reshape(float x, float y, float width, float height);

				// Set the width and height of component (wrappers around Reshape)
    virtual Pad_Bool   Set_height(float height);
    virtual Pad_Bool   Set_width(float width);

				// The minimum dimensions necessary for the component to look normal
    virtual void       Minimum_size(Pad_Dimension &dimension);

				// The preferred (relaxed) dimension of the component (often 
				// same as Minimum_size())
    virtual void       Preferred_size(Pad_Dimension &dimension);

    
    virtual Pad_Area * Get_active_area(void);      // Return the active portion of this component

				// The width of the motif hilite border on the focus component
    virtual void       Set_hilite_width(float hiliteWidth);
    virtual float      Get_hilite_width(void);

				// Change the fg, bg and font
    virtual Pad_Bool   Set_pen(char *colorName);
    virtual Pad_Bool   Set_pen(int r, int g, int b);
    virtual void       Get_pen_name(Pad_String &penname);
    virtual Pad_Bool   Set_fill(char *colorName);
    virtual Pad_Bool   Set_fill(int r, int g, int b);
    virtual void       Pen_changed(void);   // This gets called when the pen changed, can be overridden
    virtual void       Fill_changed(void);  // This gets called when the fill changed, can be overridden
    virtual void       Set_font(char *fullname);
				// Change the fg, bg and font to default values
    virtual void       Set_pen_default(void);
    virtual void       Set_fill_default(void);
    virtual void       Set_font_default(void);
				// Retrieve the fg, bg and font
    virtual void Get_font(Pad_String &fullname);

				// True if user is currently interacting with this component
    virtual Pad_Bool   Is_user_interacting(void);

    virtual void       Request_focus(void);  // Set this component as its frame's keyboard focus
    virtual void       Next_focus(void);     // Tell component's frame to go to next focus-able object
    virtual void       Unset_focus(void);    // This object is no longer the focus object
    virtual void       Set_focus(void);      // This object has become the focus object
				// Get and set options

  protected:
    int          _componentFlags;     // OR'd combination of flags
    float        _hiliteWidth;	      // Width of hilite border in pixels
    Pad_ColorRef _penColor;	      // Pen color
    Pad_ColorRef _fillColor;	      // Fill color
    Pad_FontRef  _font;		      // Font
    Pad_Area     _activeArea;         // Active area of component (not including hilite border)
};

class Pad_Canvas : public Pad_Component {
  public:
    Pad_Canvas(Pad *pad);
    virtual ~Pad_Canvas();

    virtual Pad_Bool Render(void);
				// Get and set options

    PAD_TYPE("canvas");

  protected:
    int      _canvasFlags;     // OR'd combination of flags
};

#endif
