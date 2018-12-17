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
#include "component.h"
#include "pad.h"
#include "win.h"
#include "view.h"
#include "renderer.h"
#include "global.h"
#include "group.h"

//////////////////////////////////////////////////////////////
//
// Pad_Dimension class
//
//////////////////////////////////////////////////////////////

Pad_Dimension::Pad_Dimension()
{
    width = 0.0;
    height = 0.0;
}

Pad_Dimension::Pad_Dimension(float newWidth, float newHeight)
{
    width = newWidth;
    height = newHeight;
}

void
Pad_Dimension::Set(float newWidth, float newHeight)
{
    width = newWidth;
    height = newHeight;
}

//////////////////////////////////////////////////////////////
//
// Pad_Area class
//
//////////////////////////////////////////////////////////////

Pad_Area::Pad_Area()
{
    x = 0.0;
    y = 0.0;
    width = 0.0;
    height = 0.0;
}

Pad_Area::Pad_Area(float newX, float newY, float newWidth, float newHeight)
{
    x = newX;
    y = newY;
    width = newWidth;
    height = newHeight;
}

void
Pad_Area::Set(float newX, float newY, float newWidth, float newHeight)
{
    x = newX;
    y = newY;
    width = newWidth;
    height = newHeight;
}

//
// Returns TRUE if <point> is within area
//
Pad_Bool
Pad_Area::Contains(Pad_Point &point)
{
    if ((point.x >= x) &&
	(point.y >= y) &&
	(point.x <= x+width) &&
	(point.y <= y+height)) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

//////////////////////////////////////////////////////////////
//
// Pad_Component class
//
//////////////////////////////////////////////////////////////

Pad_Component::Pad_Component(Pad *pad) :
Pad_Object(pad)
{
    _type = PAD_COMPONENT;

    clientData = NULL;
    _componentFlags = PAD_NO_MASK;
    _hiliteWidth = COMPONENT_DEFAULT_HILITEWIDTH;

    Set_anchor(PAD_ANCHOR_SW);
    Set_pen_default();
    Set_fill_default();
    Set_font_default();
}

Pad_Component::~Pad_Component()
{
}

//
// Component is active if pointer is within frame that
// component is in
//
Pad_Bool
Pad_Component::Is_active(void)
{
    if (group) {
	return(group->Is_active());
    } else {
	return(pad->Get_active());
    }
}

//
// Some Components can be interacted with directly by users,
// and it is useful to know when this is ocurring, or if
// the user is interacting with a group that this component
// is a member of.
//
Pad_Bool
Pad_Component::Is_user_interacting(void)
{
    if (group) {
	return(group->Is_user_interacting());
    } else {
	return(FALSE);
    }
}

//
// Render a button, depending on its state 
// (enabled/disabled, normal/active/armed)
//
Pad_Bool
Pad_Component::Render(void)
{
    if ((_hiliteWidth > 0) && (Has_focus() && Is_active())) {
	float h2 = _hiliteWidth * 0.5;
	Pad_BBox bb;

	Get_bbox(bb);
	Pad_renderer->Set_color(&Pad_Color::black);
	Pad_renderer->Set_line_style(_hiliteWidth, CapButt, JoinMiter);
	Pad_renderer->Draw_box(bb.Xmin()+h2, bb.Ymin()+h2, bb.Xmax()-h2, bb.Ymax()-h2);
    }

    return(TRUE);
}

//
// Make component visible (i.e., map it)
//
void
Pad_Component::Show(void)
{
    Set_transparency(1.0);
}

//
// Make component invisible (i.e., unmap it)
//
void
Pad_Component::Hide(void)
{
    Set_transparency(0.0);
}

//
// Make component respond to events
//
void
Pad_Component::Enable(void)
{
    _componentFlags &= ~COMPONENT_DISABLED;
}

//
// Make component stop responding to events
//
void
Pad_Component::Disable(void)
{
    _componentFlags |= COMPONENT_DISABLED;
}

//
// Return true if component is enabled
//
Pad_Bool
Pad_Component::Is_enabled(void)
{
    return((_componentFlags & COMPONENT_DISABLED) ? FALSE : TRUE);
}

//
// Set bounding box of component to specification
// Position is relative to parent's SW corner
//
void
Pad_Component::Reshape(float x, float y, float width, float height)
{
				// Can't make negatively-sized components
    if ((width < 0) || (height < 0)) {
	return;
    }
    
    Damage();

    Set_bbox(x, y, x + width, y + height);
    _activeArea.Set(x + _hiliteWidth, y + _hiliteWidth, 
		    width - (2 * _hiliteWidth), height - (2 * _hiliteWidth));
				// Position relative to parent
				// Since anchorpt is global, we need to do it
				// this way instead of using transforms.
    transform.Set_offset(0, 0);
    if (group && group->Is_container()) {
	float gx, gy, gs;
	group->Get_abs_position(gx, gy, gs);
        x += gx;
        y += gy;
    }
    Set_abs_position_xy(x, y);
    
    _componentFlags |= COMPONENT_RESHAPE_SET;

    Damage();
}

//
// If component has been reshaped, then this does nothing.
// If it hasn't been reshaped, then this computes the
// component's preferred size.
//
void
Pad_Component::Compute_bounding_box(void)
{
    Pad_Dimension dimension;

    if (!(_componentFlags & COMPONENT_RESHAPE_SET)) {
	Preferred_size(dimension);
	Reshape(Get_rel_position_x(), Get_rel_position_y(), dimension.width, dimension.height);
	_componentFlags &= ~COMPONENT_RESHAPE_SET;    // Reset because Reshape sets it
    }

    Pad_Object::Compute_bounding_box();
}

Pad_Bool
Pad_Component::Set_height(float height)
{
    Pad_BBox globalBB, localBB, parentBB;
    Pad_Point sw;

                        // Reshape using the sw corner (relative to parent)
    Get_global_bbox(globalBB);
    Get_bbox(localBB);
    if (group && group->Is_container()) {
        group->Get_global_bbox(parentBB);
	sw.Set(globalBB.Xmin() - parentBB.Xmin(),
	       globalBB.Ymin() - parentBB.Ymin());
    } else {
        sw.Set(globalBB.Xmin(), globalBB.Ymin());
    }

    Reshape(sw.x, sw.y, localBB.Width(), height);
    optionFlags |= PAD_HEIGHT_SET;

    return(TRUE);
}

Pad_Bool
Pad_Component::Set_width(float width)
{
    Pad_BBox globalBB, localBB, parentBB;
    Pad_Point sw;

                        // Reshape using the sw corner (relative to parent)
    Get_global_bbox(globalBB);
    Get_bbox(localBB);
    if (group && group->Is_container()) {
        group->Get_global_bbox(parentBB);
	sw.Set(globalBB.Xmin() - parentBB.Xmin(),
	       globalBB.Ymin() - parentBB.Ymin());
    } else {
        sw.Set(globalBB.Xmin(), globalBB.Ymin());
    }

    Reshape(sw.x, sw.y, width, localBB.Height());
    optionFlags |= PAD_WIDTH_SET;

    return(TRUE);
}

//
// Return the minimum dimensions necessary for the component to look normal
//
void
Pad_Component::Minimum_size(Pad_Dimension &dimension)
{
    dimension.Set(10, 10);
}

//
// Return the preferred (relaxed) dimension of the component
// If the size has been set by -width or -height, then use
// that.  Otherwise, use it's minimum nice size.
//
void
Pad_Component::Preferred_size(Pad_Dimension &dimension)
{
    Pad_BBox bb;

    Get_bbox(bb);
    Minimum_size(dimension);
    if (optionFlags & PAD_WIDTH_SET) {
	dimension.width = bb.Width();
    }
    if (optionFlags & PAD_HEIGHT_SET) {
	dimension.height = bb.Height();
    }
}

//
// Return the active portion of this component
//
Pad_Area *
Pad_Component::Get_active_area(void)
{
    return(&_activeArea);
}

//
// Specify this component's pen color
//
Pad_Bool
Pad_Component::Set_pen(char *name)
{
    _penColor.Set(name);
    Pen_changed();

    return(TRUE);
}

void 
Pad_Component::Get_pen_name(Pad_String &penname)
{
    _penColor.Get(penname);
}

Pad_Bool
Pad_Component::Set_pen(int r, int g, int b)
{
    _penColor.Set(r, g, b);
    Pen_changed();

    return(TRUE);
}

void
Pad_Component::Set_pen_default(void)
{
    Set_pen(COMPONENT_DEFAULT_PEN);
    _componentFlags &= ~COMPONENT_PEN_SET;
}


//
// Specify this component's pen color
//
Pad_Bool
Pad_Component::Set_fill(char *name)
{
    _fillColor.Set(name);
    Fill_changed();

    return(TRUE);
}

Pad_Bool
Pad_Component::Set_fill(int r, int g, int b)
{
    _fillColor.Set(r, g, b);
    Fill_changed();

    return(TRUE);
}

void
Pad_Component::Set_fill_default(void)
{
    Set_fill(COMPONENT_DEFAULT_FILL);
    _componentFlags &= ~COMPONENT_FILL_SET;
}

//
// These get called whenever the pen or fill are changed,
// and are typically overridden by derived types to catch these events.
//
void
Pad_Component::Pen_changed(void)
{
    _componentFlags |= COMPONENT_PEN_SET;
    Damage();
}

void
Pad_Component::Fill_changed(void)
{
    _componentFlags |= COMPONENT_FILL_SET;
    Damage();
}

//
// Specify this component's font
//
void
Pad_Component::Set_font(char *fullname)
{
    _componentFlags |= COMPONENT_FONT_SET;
    _font.Set(fullname);
    Update();
}

void
Pad_Component::Set_font_default(void)
{
    Set_font(COMPONENT_DEFAULT_FONT);
    _componentFlags &= ~COMPONENT_FONT_SET;
}

void
Pad_Component::Get_font(Pad_String &name)
{
    _font.Get(name);
}

//
// The width of the motif hilite border on the focus component
//
void
Pad_Component::Set_hilite_width(float hiliteWidth)
{
    _hiliteWidth = hiliteWidth;
}

float
Pad_Component::Get_hilite_width(void)
{
    return(_hiliteWidth);
}

//
// Set this component as its frame's keyboard focus
//
void
Pad_Component::Request_focus(void)
{
    pad->view->Update_focus(this);
}

//
// Tell component's frame to go to next focus-able object
//
void
Pad_Component::Next_focus(void)
{
				// Not implemented yet, this is a place-holder
    pad->view->Update_focus(NULL);
}

//
// This object has become the focus object
//
void
Pad_Component::Set_focus(void)
{
    Damage();
}

//
// This object is no longer the focus object
//
void
Pad_Component::Unset_focus(void)
{
    Damage();
}

//////////////////////////////////////////////////////////////
//
// Pad_Canvas class
//
//////////////////////////////////////////////////////////////

Pad_Canvas::Pad_Canvas(Pad *pad) :
Pad_Component(pad)
{
    _type = PAD_CANVAS;

    _canvasFlags = PAD_NO_MASK;
    Update();

    Add_tag("Canvas");
}

Pad_Canvas::~Pad_Canvas()
{
}

//
// Render a Canvas
//
Pad_Bool
Pad_Canvas::Render(void)
{
    if (_fillColor.Is_set()) {
	Pad_renderer->Set_color(_fillColor);
	Pad_renderer->Draw_filled_box(_activeArea.x, _activeArea.y, 
				      _activeArea.x + _activeArea.width, _activeArea.y + _activeArea.height);
    }

    Pad_Component::Render();

    return(TRUE);
}
