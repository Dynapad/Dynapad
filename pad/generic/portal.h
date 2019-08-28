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

#ifndef PORTAL_H
#define PORTAL_H

#include "view.h"
#include "pad.h"
#include "defs.h"
#include "object.h"
#include "point.h"
#include "plist.h"
#include "font.h"
#include <X11/X.h>

class Event;
class Pad_Text;
class Pad_Win;
class Pad_String;
class Pad_Event;

#define PORTAL_BORDER_SET        (1<<0)  // Set if -border      has been configured
#define PORTAL_BORDERWIDTH_SET   (1<<1)  // Set if -borderwidth has been configured
#define PORTAL_FILL_SET          (1<<2)  // Set if -fill        has been configured
#define PORTAL_FONT_SET          (1<<3)  // Set if -font        has been configured
#define PORTAL_LOOKON_SET        (1<<4)  // Set if -lookon      has been configured
#define PORTAL_PEN_SET           (1<<5)  // Set if -pen         has been configured
#define PORTAL_RELIEF_SET        (1<<6)  // Set if -relief      has been configured
#define PORTAL_TITLE_SET         (1<<7)  // Set if -title       has been configured
#define PORTAL_VIEW_SET          (1<<8)  // Set if -view        has been configured

class Pad_Portal : public Pad_View
{
  public:
    unsigned short portalFlags;           // OR'd combination of flags
    float         lineWidth;		  // Line width of portal object
    Pad_Win *     lookon;                 // What window does this portal see?
    int           recursiveRenderLevel;
    Pad_BorderRef border;		  // Border
    Pad_ColorRef  fillColor;	          // Fill color
    Pad_FontRef   font;                   // Font
    int           relief;		  // Relief style of border
    Pad_String *  title;		  // Title string
    Pad_ColorRef  titleColor;		  // Color to use for titles
    unsigned long serial;		  // Id # for events to avoid circular following
    Pad_Bool      currentlyVisible;	  // True if portal visible on its surface
					  // (Pad_View's dirtyPortals flag must be false)
    Pad_Bool      rectangle;		  // True if portal is an aligned rectangle
    Pad_PList     points;		  // Points of portal (rectangular or polygonal)

    virtual Pad_Bool    Append_coords(Pad_PList &pts, Pad_Bool objectCoords);
    virtual Pad_Bool    Append_coords(Pad_PList &pts, Pad_Bool objectCoords, Pad_Bool recomputeView);
    virtual Pad_Bool    Check_render_clipped();
            void        Clip_bbox(float *result_bbox, float *bbox1, float *bbox2);
            void        Clip_view_bbox(float *clip_bbox);
    virtual void        Compute_bounding_box(void);
            void        Compute_view_bounding_box(void);
    virtual int         Create_obj_args(int argc, char **argv);   // Called when an item is created with string args
    virtual void        Damage(void);
    virtual void        Get_bordername(Pad_String &bordername);
    virtual int         Get_borderrelief(void);
    virtual float       Get_borderwidth(void);
    virtual void        Get_coords(Pad_PList &pts, Pad_Bool object_coords);
    virtual void        Get_fillname(Pad_String &fillname);
    virtual void        Get_font(Pad_String &name);
    virtual Pad_Win *   Get_lookon(void);
    virtual void        Get_penname(Pad_String &penname);
    virtual const char *Get_title(void);
            void        Pass_through(Pad_Event *event);
            void        Pass_through(float *bbox);
    virtual Pad_Bool    Is_rotatable(void);
    virtual Pad_Bool    Pick(Pad_Event *event, float halo);
            Pad_Bool    Pick_border(Pad_Event *event, float halo);
    virtual Pad_Bool    Render(void);
    virtual Pad_Bool    Render(Pad_Bool fast);
    virtual Pad_Bool    Render(Pad_Restorer *);
    virtual Pad_Bool    Render_fast(void);
    virtual Pad_Bool    Rotate(float dtheta);
    virtual Pad_Bool    Rotate(float dtheta, Pad_Point &center);
    virtual void        Set_border(char *colorname);
    virtual void        Set_border_default(void);
    virtual void        Set_borderrelief(int relief);
    virtual void        Set_borderrelief_default(void);
    virtual void        Set_borderwidth(float borderwidth);
    virtual void        Set_borderwidth_default(void);
    virtual Pad_Bool    Set_coords(Pad_PList &pts, Pad_Bool object_coords);
    virtual Pad_Bool    Set_coords(Pad_PList &pts, Pad_Bool object_coords, Pad_Bool recomputeView);
    virtual Pad_Bool    Set_fill(const char *colorname);
    virtual Pad_Bool    Set_fill(int red, int green, int blue);
    virtual void        Set_fill_default(void);
    virtual void        Set_font(const char *fontname);
    virtual void        Set_font_default(void);
    virtual Pad_Bool    Set_height(float height);
            void        Set_lookon(Pad_Win *win);
            void        Set_lookon_default(void);
    virtual Pad_Bool    Set_pen(const char *colorname);
    virtual Pad_Bool    Set_pen(int red, int green, int blue);
    virtual void        Set_pen_default(void);
    virtual void        Set_title(const char *title);
    virtual void        Set_title_default(void);
    virtual Pad_Bool    Set_view(float newXview, float newYview, float newZoom, Pad_Bool render_now=FALSE);
            void        Set_view_default();
    virtual Pad_Bool    Set_width(float width);
    virtual void        Update_visibility(void);

    virtual ~Pad_Portal();
    Pad_Portal(Pad *pad);

    PAD_TYPE("portal");
};


#endif
