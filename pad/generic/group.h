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

#ifndef GROUP_H
#define GROUP_H 1

#include "defs.h"
#include "object.h"
#include "component.h"

//
// An inset describes the space available within a container
// for laying out other objects.
//
class Pad_Inset
{
  public:
    float bottom;
    float left;
    float right;
    float top;

    void Set(float b, float l, float r, float t) {
	bottom = b;
	left = l;
	right = r;
	top = t;
    }

    Pad_Inset() {
	bottom = 0.0;
	left = 0.0;
	right = 0.0;
	top = 0.0;
    }
};

#define GROUP_DIVISIBLE_SET          (1<<0)  // Set if -divisible has been configured
#define GROUP_MEMBERS_SET            (1<<1)  // Set if -members   has been configured

//
// Group Area
// 
class Pad_Group : public Pad_Component
{
  public:
    unsigned char groupFlags;	// OR'd combination of flags
    Pad_List      members;	// Members of this group (Pad_Objects), in *reverse* display-list order
    Pad_Bool      divisible;	// True if events go to members

    virtual Pad_Bool Add(Pad_Object *obj, Pad_Bool transform=FALSE);     // Move object into group
            void     Add_to_drawing_order(Pad_Object *obj, Pad_Object *prev_obj);
    virtual void     Add_to_layer(const char *name);
    virtual Pad_Bool Check_render_clipped(void);
    virtual void     Compute_bounding_box(void);
    virtual void     Create_obj_error(void);    // Called if there is an error configuring obj during create
    virtual Pad_Bool Delete(Pad_Object *obj);   // Remove object from group (don't put on surface)
    virtual Pad_Bool Get_divisible(void);
    virtual Pad_Inset *Get_inset(void);         // Return inset within group
            void     Get_members(Pad_List &list);
            Pad_Bool Is_group(void);
    virtual Pad_Bool Is_rotatable();
    virtual Pad_Bool Lower_member(Pad_Object *obj);
    virtual Pad_Bool Lower_member(Pad_Object *obj, Pad_Object *ref);
            void     Make_empty(Pad_Bool transform);
    virtual int      Next_drawing_order(void);
    virtual Pad_Bool Pick_group(Pad_Event *event);         // True if group object should be picked
    virtual void     Pointer_in(Pad_Event *padEvent);
    virtual void     Pointer_out(Pad_Event *padEvent);
    virtual Pad_Bool Raise_member(Pad_Object *obj);
    virtual Pad_Bool Raise_member(Pad_Object *obj, Pad_Object *ref);
    virtual Pad_Bool Remove(Pad_Object *obj, Pad_Bool transform=FALSE);  // Move object from group onto surface
    virtual Pad_Bool Render(void);
    virtual Pad_Bool Render_fast(void);
    virtual Pad_Bool Rotate(float theta);
    virtual Pad_Bool Rotate(float theta, Pad_Point &center);
    virtual void     Set_divisible(Pad_Bool divisible);
    virtual void     Set_divisible_default(void);
    virtual Pad_Bool Set_height(float height);
    virtual void     Set_members(Pad_List *new_members);
    virtual void     Set_members_default(void);
    virtual Pad_Bool Set_width(float width);
    virtual void     Update_global_bounding_box(void);

    Pad_Group(Pad *pad);
    virtual ~Pad_Group();

    PAD_TYPE("group");
protected:
    Pad_Inset _inset;		// Inset to layout items within
                                // These methods are for overloading by subclasses of Pad_Group to
                                // provide subclass specific functionality for adding or
                                // removing items.
    Pad_Bool _Add(Pad_Object *obj, Pad_Bool transform); 
    Pad_Bool _Remove(Pad_Object *obj, Pad_Bool transform);  
};

#endif
