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

#ifndef PAD_VIEW_H
#define PAD_VIEW_H

#include "pad.h"
#include "defs.h"
#include "object.h"
#include "point.h"
#include "bbox.h"

extern "C" {
#  include <X11/X.h>
#  include <X11/Xlib.h>
}

class Event;
class Pad_Text;
class Pad_Win;
class Pad_Restorer;
class Pad_Portal;
class Pad_DisplayItem;

#define VIEW_VISIBLELAYERS_SET (1<<0)	 // Set if -visiblelayers has been configured

class Pad_View : public Pad_Object
{
  public:
    Pad_Win * win;                   // Window pointer for X graphics
    unsigned char viewFlags;	     // OR'd combination of flags
    float     xview;		     // Current view onto the surface
    float     yview;
    float     zoom;
    int       layers;		     // Length of layer lists
    Pad_Bool *visibleLayers;	     // Array of flags specifying if layer is visible (layer 0 means 'all')
    Pad_Bool *invisibleLayers;	     // Array of flags specifying if layer is visible
    Pad_List  viewScriptObjects;     // Objects to be notified when view changes
    Pad_List  stickyObjects;         // Objects to be transformed when view changes
    Pad_BBox  viewBBox;		     // BBox of current view;

    Pad_List  visiblePortals;	     // List of portals visible (on any surface) looking
				     // onto this view.
    Pad_Bool  dirtyPortals;	     // Specifies if 'visiblePortals' is up-to-date.
    int       renderTime;	     // Time (in ms) of most recent render

    Pad_DisplayItem *Add_to_display_list(Pad_Object *obj);
            void     Add_sticky(Pad_Object *obj);
            Pad_Bool Animate_to(float newXview, float newYview, float newZoom, int animation_time=0, Pad_Bool twostep=FALSE);
            void     Apply_view(Pad_Object *obj);
            void     Apply_view(float &x, float &y, float &s);
            void     Apply_view(float *bb);
            void     Center(Pad_Object *obj, int animation_time=0, Pad_Bool twostep=FALSE,
			    float x=0.5, float y=0.5, float z=0.75, Pad_View *view=NULL);
            void     Center(Pad_List &objs, int animation_time=0, Pad_Bool twostep=FALSE,
			    float x=0.5, float y=0.5, float z=0.75, Pad_View *view=NULL);
            void     Center(float x1, float y1, float x2, float y2, int animation_time=0, Pad_Bool twostep=FALSE,
			    float x=0.5, float y=0.5, float z=0.75, Pad_View *view=NULL);
    virtual void     Compute_bounding_box(void);
    virtual void     Compute_view_bounding_box(void);
    virtual void     Damage(void);                          // Mark the pad surface as changed
    virtual Pad_Win *Get_lookon(void);
            void     Get_view(float &xview, float &yview, float &zoom);
    virtual void     Get_visiblelayers(Pad_String &layers);
            void     Invert_view(Pad_Object *obj);
            void     Invert_view(float &x, float &y, float &s);
            void     Invert_view(float *bb);
            void     Invert_view(Pad_BBox &bb);
            void     Print_debugging_info(void);
            void     Remove_sticky(Pad_Object *obj);
    virtual Pad_Bool Render(void);
    virtual Pad_Bool Render(Pad_Restorer *);
    virtual Pad_Bool Set_view(float newXview, float newYview, float newZoom, Pad_Bool render_now=FALSE);
    virtual void     Set_visiblelayers(const char *layers);
    virtual void     Set_visiblelayers_default(void);
            void     Update_display(void);
            void     Update_display(int dissolveSpeed, Pad_Bool withRefinements);
            void     Update_focus(Pad_Object *obj);
            void     Update_layers(void);
    virtual void     Update_visibility(void);
            void     Zoom_around(float x, float y, float zmult, int animation_time=0);
    
    virtual ~Pad_View();
    Pad_View(Pad_Win *win);	// This one used to create a new top-level window
    Pad_View(Pad *pad); 	// This one used to create a portal (derived from Pad_View)
    void Init(void);		// Used by constructors

    PAD_TYPE("pad_view");
};

#endif
