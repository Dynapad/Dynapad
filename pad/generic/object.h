/*
"(c) Copyright 1993-1997 Pad++ Consortium University of New Mexico (UNM),
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

#ifndef INCL_OBJ
#define INCL_OBJ

#include "defs.h"
#include "events.h"
#include "point.h"
#include "types.h"
#include "list.h"
#include "pad-string.h"
#include "restorer.h"
#include "bbox.h"
#include "transform.h"
#include "tree-layout.h"

// not used directly by object, but by almost all its subclasses
#include "color.h" 
#include "border.h"

#include <iostream>
using namespace std;

//
// Function Prototypes
//
void     Pad_Delete_callback(ClientData clientData);

//
// All objects sitting on a pad are derived from a Pad_Object
//

//
// Sticky types
//
#define PAD_STICKY_NONE   0	// Not sticky
#define PAD_STICKY_ALL    1	// Sticky
#define PAD_STICKY_X      2	// Sticky only in X and Z, i.e., item doesn't zoom or pan in X
#define PAD_STICKY_Y      3	// Sticky only in Y and Z, i.e., item doesn't zoom or pan in Y
#define PAD_STICKY_Z      4	// Sticky only in Z, i.e., item doesn't zoom
#define PAD_STICKY_VIEW   5	// Sticks to the current view bbox
  
class Pad;                // Forward declarations
class Pad_Callback;
class Pad_DCons;
class Pad_Display;
class Pad_Event;
class Pad_Group;
class Pad_Handle;
class Pad_NoiseData;
class Pad_String;
class Pad_Text;
class Pad_TreeNode;
class Pad_Trait;
class Pad_View;

//
// A class for storing information about the
// zoomaction callbacks.  For each object, a list
// these are stored, and whenever the object is rendered
// at a size across the actionSize, one of the callbacks
// is fired.
//
class Pad_ZoomAction
{
  public:
    Pad_Callback *growScript;    // Call-back script when object grows
    Pad_Callback *shrinkScript;  // Call-back script when object shrinks
    float actionSize;          // Size of object about which to execute zoom action.
    float userActionSize;      // actionSize in caller's units (to avoid rounding errors)

    ~Pad_ZoomAction();
    Pad_ZoomAction();
};

// Used to store size of object within view
class Pad_ZoomActionSize
{
  public:
    Pad_View *view;
    float pixelDim;
};

//
// Specifies the sticky attributes of an object.
//
class Pad_Sticky
{
  public:
    int           type;             // Sticky type
    int           id;		    // Id of related item
    Pad_Transform stickyTransform;  // Requested transform of sticky items

    Pad_Sticky() {
	type = PAD_STICKY_NONE;
	id = 0;
    }
};

//
// Specifies a layer.
// Keeps track of the first and last object on this layer.
// Layers are indexed by both name (layerNameTable),
// and by id (layerIdTable).
//
class Pad_Layer
{
  public:
    int         id;		// Unique id for this layer.  0 means not on a layer
    int         objects;        // Number of objects on this layer
    Pad_Uid      name;		// Name of layer
    Pad_Object *first;		// First object in drawing order on this layer
    Pad_Object *last;		// Last object in drawing order on this layer
    void       *userdata;       // whatever ... Scheme_Object * for drscheme
    Pad        *pad;		// Pad this object is on

    Pad_Layer(Pad *newpad) {
	id = 0;
	objects = 0;
	name = NULL;
	first = NULL;
	last = NULL;
	userdata = NULL;
	pad = newpad;
    }
};

//
// A specification for each defined type. A list of these is stored in pad->userTypes.
// pad->userTypes also contains one of these for each built-in type with a null createScript.
//
class Pad_Type
{
  public:
    Pad_Uid name;		// Name of type
    int id;	        	// Pad type id (PAD_USER_TYPE for user-defined types)
    Pad_Callback *createScript;	// Tcl script to evaluate when type is created
    Pad_List options;		// List of Pad_Option's

    Pad_Type();
    ~ Pad_Type();
};

//
// A specification for each option. Each Pad_UserType has a list of these.
// Both built-in types and user-defined types can have user-defined options.
//
class Pad_Option
{
  public:
    int id;			// Id specifying type (PAD_USER_OPTION for user-defined option)
    Pad_Uid option;		// Name of option
    Pad_Bool write;		// True if option should be written out
    Pad_String defaultValue;   // Default value (only used for user-defined options)
    Pad_Callback *optionScript;	// Tcl script to evaluate when option is accessed
    
    Pad_Option();
    ~Pad_Option();
};

class Pad_OptionKey
{
  public:
    Pad_Uid optionUid;
    Pad_Uid typeUid;
};

				// Options for Pad_Object

// IMPORTANT NOTE:  DO NOT REUSE THESE NUMBERS!
// This is crucial for backward compatibility and correct reading
// of binary pad files.  Add new options to the end and do not reuse
// id numbers of deleted options.

#define PAD_USER_OPTION    0    // User-defined option
#define PAD_ALIASES        1    // Option number for -aliases
#define PAD_ALWAYSRENDER   2    // Option number for -alwaysrender
#define PAD_ANCHOR         3    // Option number for -anchor
#define PAD_ANGLE          4    // Option number for -angle
#define PAD_CLIPPING       5    // Option number for -clipping
#define PAD_EVENTS         6    // Option number for -events
#define PAD_FADERANGE      7    // Option number for -faderange
#define PAD_HEIGHT         8	// Option number for -height
#define PAD_INFO           9    // Option number for -info
#define PAD_LAYER          10   // Option number for -layer
#define PAD_LOCK           11   // Option number for -lock
#define PAD_MAXSIZE        12   // Option number for -maxsize
#define PAD_MINSIZE        13   // Option number for -minsize
#define PAD_NOISEDATA      14   // Option number for -noisedata
#define PAD_RENDERSCRIPT   15   // Option number for -renderscript
#define PAD_STICKY         16   // Option number for -sticky
#define PAD_TAGS           17   // Option number for -tags
#define PAD_TIMERSCRIPT    18   // Option number for -timerscript
#define PAD_TIMERRATE      19   // Option number for -timerrate
#define PAD_TRANSPARENCY   20   // Option number for -transparency
#define PAD_VIEWSCRIPT     21   // Option number for -viewscript
#define PAD_WIDTH          22   // Option number for -width
#define PAD_ZOOMACTION     23   // Option number for -zoomaction
#define PAD_PLACE          24   // Option number for -place
#define PAD_X              25   // Option number for -x
#define PAD_Y              26   // Option number for -y
#define PAD_Z              27   // Option number for -z
#define PAD_SCALE          28   // Option number for -scale
#define PAD_POSITION       29   // Option number for -position
#define PAD_RPOSITION      30   // Option number for -rposition
#define PAD_ANCHORPT       31   // Option number for -anchorpt
#define PAD_ANGLECTR       32   // Option number for setting angle with center (also -angle from Tcl)

				// Options for derived types
				// These types are not used anywhere in the definition of
				// Pad_Object, but are defined centrally so that all derived
				// types use the same indices.
#define PAD_ARROW          101  // Option number for -arrow
#define PAD_ARROWSHAPE     102  // Option number for -arrowshape
#define PAD_BBOX           103  // Option number for -bb
#define PAD_BORDER         104  // Option number for -border
#define PAD_BORDERWIDTH    105  // Option number for -borderwidth
#define PAD_CAPSTYLE       106  // Option number for -capstyle
#define PAD_DITHER         107  // Option number for -dither
#define PAD_DIVISIBLE      108  // Option number for -divisible
#define PAD_DONESCRIPT     109  // Option number for -donescript
#define PAD_ERRORSCRIPT    110  // Option number for -errorscript
#define PAD_FILE           111  // Option number for -file
#define PAD_FILL           112  // Option number for -fill
#define PAD_FONT           113  // Option number for -font
#define PAD_HTMLANCHORS    114  // Option number for -htmlanchors
#define PAD_HTMLOPTION     115  // Option number for -html
#define PAD_IMAGEDATA      116  // Option number for -image
#define PAD_ISMAP          117  // Option number for -ismap
#define PAD_JOINSTYLE      118  // Option number for -joinstyle
#define PAD_LOOKON         119  // Option number for -lookon
#define PAD_MEMBERS        120  // Option number for -members
#define PAD_PEN            121  // Option number for -pen
#define PAD_PENWIDTH       122  // Option number for -penwidth
#define PAD_REFERENCE      123  // Option number for -reference
#define PAD_RELIEF         124  // Option number for -relief
#define PAD_SIZE           125  // Option number for -size
#define PAD_STATE          126  // Option number for -state
#define PAD_TITLE          127  // Option number for -title
#define PAD_TEXTOPTION     128  // Option number for -text
#define PAD_UPDATESCRIPT   129  // Option number for -updatescript
#define PAD_URL            130  // Option number for -url
#define PAD_VIEWOPTION     131  // Option number for -view
#define PAD_VISIBLELAYERS  132  // Option number for -visiblelayers
#define PAD_WRITEFORMAT    133  // Option number for -writeformat
#define PAD_COMMAND        134  // Option number for -command
#define PAD_EDITABLE       135  // Option number for -editable
#define PAD_FROM           136  // Option number for -from
#define PAD_TO             137  // Option number for -to
#define PAD_ECHOCHAR       138  // Option number for -echochar
#define PAD_ORIENTATION    139  // Option number for -orientation
#define PAD_VALUE          140  // Option number for -value
#define PAD_PAGESIZE       141  // Option number for -pagesize
#define PAD_LINESIZE       142  // Option number for -linesize
#define PAD_MENUBAROPTION  143  // Option number for -menubar
#define PAD_MEMBERLABELS   144  // Option number for -memberlabels
#define PAD_LASTOPTION     145  // Equal to the last option.  Update this so it is always last

#define PAD_WRITE_COPY       1  // Possible values for the PAD_WRITEFORMAT option
#define PAD_WRITE_REFERENCE  2

			    	          // Bits which get OR'd together to fill up
					  // 'flags' slot of Pad_Object
#define PAD_NO_MASK               0
#define PAD_MASK_CREATED          (1<<0)  // Object has been created (and <Create> binding fired)
#define PAD_MASK_DELETED          (1<<1)  // Object has been deleted (and <Delete> binding fired)
#define PAD_MASK_ID_EVENT_FIRST   (1<<2)  // Events should fire on id first and all last (not vice versa)
#define PAD_MASK_MINSIZE_RELATIVE (1<<3)  // minsize is specified as a percentage (between 1 and 100).
#define PAD_MASK_MAXSIZE_RELATIVE (1<<4)  // maxsize is specified as a percentage (between 1 and 100).
#define PAD_MASK_GRID_SLAVE       (1<<5)  // True if object is in a grid
#define PAD_MASK_ALWAYS_RENDER    (1<<6)  // True if object should always be rendered, even if small
#define PAD_MASK_EVENTS           (1<<7)  // True if object gets events
#define PAD_MASK_LOCK             (1<<8)  // True if object is locked
#define PAD_MASK_WRITTEN          (1<<9)  // True if object has been written (during a write loop)
#define PAD_MASK_CLIPPING         (1<<10 | 1 <<11) // Two bits used for storing clipping type
#define PAD_CLIPPING_FALSE        0     
#define PAD_CLIPPING_TRUE         (1<<10)
#define PAD_CLIPPING_AUTO         (1<<11)
#define PAD_MASK_CACHEDOUT        (1<<12) // True if obj is cached out
#define PAD_MASK_NOT_FINDABLE     (1<<13) // True if object should not respond to find commands
#define PAD_ZAWT                  (1<<14) // True if this object was created by ZAWT

				// Bits which get OR'd together to fill up
				// 'optionFlags' slot of Pad_Object
#define PAD_ALIASES_SET      (1<<0)  // Set if -aliases      has been configured
#define PAD_ALWAYSRENDER_SET (1<<1)  // Set if -alwaysrender has been configured
#define PAD_ANCHOR_SET       (1<<2)  // Set if -anchor       has been configured
#define PAD_ANGLE_SET        (1<<3)  // Set if -angle        has been configured
#define PAD_CLIPPING_SET     (1<<4)  // Set if -clipping     has been configured
#define PAD_EVENTS_SET       (1<<5)  // Set if -events       has been configured
#define PAD_FADERANGE_SET    (1<<6)  // Set if -faderange    has been configured
#define PAD_HEIGHT_SET       (1<<7)  // Set if -height       has been configured
#define PAD_INFO_SET         (1<<8)  // Set if -info         has been configured
#define PAD_LAYER_SET        (1<<9)  // Set if -layer        has been configured
#define PAD_LOCK_SET         (1<<10) // Set if -lock         has been configured
#define PAD_MAXSIZE_SET      (1<<11) // Set if -maxsize      has been configured
#define PAD_MINSIZE_SET      (1<<12) // Set if -minsize      has been configured
#define PAD_NOISEDATA_SET    (1<<13) // Set if -noisedata    has been configured
#define PAD_RENDERSCRIPT_SET (1<<14) // Set if -renderscript has been configured
#define PAD_STICKY_SET       (1<<15) // Set if -sticky       has been configured
#define PAD_TAGS_SET         (1<<16) // Set if -tags         has been configured
#define PAD_TIMERRATE_SET    (1<<17) // Set if -timerrate    has been configured
#define PAD_TIMERSCRIPT_SET  (1<<18) // Set if -timerscript  has been configured
#define PAD_TRANSPARENCY_SET (1<<19) // Set if -transparency has been configured
#define PAD_VIEWSCRIPT_SET   (1<<20) // Set if -viewscript   has been configured
#define PAD_WIDTH_SET        (1<<21) // Set if -width        has been configured
#define PAD_ZOOMACTION_SET   (1<<22) // Set if -zoomaction   has been configured
#define PAD_PLACE_SET        (1<<23) // Set if -place        has been configured
#define PAD_X_SET            (1<<24) // Set if -x            has been configured
#define PAD_Y_SET            (1<<25) // Set if -y            has been configured
#define PAD_Z_SET            (1<<26) // Set if -z            has been configure

                                       // Bits which get or'ed together to fill
                                       // up _traitFlags slot of Pad_Object.
#define PAD_ALIASES_TRAIT      (1<<0)  // Set if obj has alias option set
#define PAD_INFO_TRAIT         (1<<1)  // Set if obj has info option set
#define PAD_NOISEDATA_TRAIT    (1<<2)  // Set if obj has noise option set
#define PAD_RENDERSCRIPT_TRAIT (1<<3)  // Set if obj has renderscript option set
#define PAD_TIMERSCRIPT_TRAIT  (1<<4)  // Set if obj has timer option set
#define PAD_VIEWSCRIPT_TRAIT   (1<<5)  // Set if obj has viewerscript option set
#define PAD_ZOOMACTION_TRAIT   (1<<6)  // Set if obj has zoomaction option set
#define PAD_USERTYPE_TRAIT     (1<<7)  // Set if obj has userType
#define PAD_TREENODE_TRAIT     (1<<8)  // Set if obj has a treenode
#define PAD_TREELAYOUT_TRAIT   (1<<9)  // Set if obj has a treelayout
#define PAD_STICKY_TRAIT       (1<<10) // Set if obj has sticky option set
#define PAD_TRANSPARENCY_TRAIT (1<<11) // Set if obj has non-default transparency
#define PAD_ANGLE_TRAIT        (1<<12) // Set if obj has non-default angle


class Pad_Object
{
  public:
    int           _type;
    unsigned int  flags;	// OR'd combination of flags for all objects
    unsigned int  optionFlags;	// OR'd combination of flags for object options
    Pad_Transform transform;	// This object's transformation

    float         pixelDim;     // Current max dimension of object (in pixels)
    float         percentDim;   // Current max size of object in percentage of view
    float         minSize;      // Minimum size to render object (in pixels or percentage)
    float         maxSize;      // Maximum size to render object (in pixels or percentage)

    int           layerId;	// Id of layer this object is on
    float         fadeRange;    // Range over which to fade (in and out)

    Pad_Object *  next;		// Next Pad_Object in drawing order
    Pad_Object *  prev;		// Previous Pad_Object in drawing order
    Pad_Group *   group;	// Group item is in (or NULL if not a group member)
    Pad *         pad;		// Pad this object is on
    int           drawingOrder; // Number in drawing order list

    int           id;		// Id of object
    Pad_List      tags;		// List of tags (Pad_Uid's)
    Pad_Anchor    anchor;	// Anchor type (n, s, sw, etc.)
    Pad_Point     anchorpt;	// Anchor point (The point this object is positioned by
				// according to its anchor.  Note that anchorpt is relative
				// to objects parent.  So, if this object is not a member of
				// of a group, then anchorpt is in absolute coordinates.  If
				// this object is a group member, then anchorpt is relative
				// to the group.  So, it is unsafe to use anchorpt directly.
				// You should use the Get/Set_abs/rel_position() methods.

    Pad_BBox      bbox;         // Bounding box of object in local coordinates
    Pad_BBox      globalBBox;   // Bounding box of object in global coordinates
    void *        userdata;     // whatever ... Scheme_Object * for drscheme
    Pad_Bool      findme;       // Used for determining display order
    Pad_Bool      findable;     // Should find/below/above find this?  Eg padselect% shouldn't be findable.
    Pad_Bool      visible;      // Used to determine if object's visibility changed
    Pad_Bool      viewable;     // Used to determine if object entered/left the view
    Pad_Bool      boundable;    // Does not affect group bbox

public:
            Pad_Bool Add_alias(Pad_Object *obj);
            void     Add_tag(const char *tag);
            void     Add_to_drawing_order(Pad_Object *prev_obj);
    virtual void     Add_to_layer(const char *name);
    virtual Pad_Bool Append_coords(Pad_PList &pts, Pad_Bool object_coords);
    virtual Pad_Bool Check_for_render(void);
    virtual Pad_Bool Check_render_clipped(void); // tests render_clipped
            Pad_Bool Check_render_layer(Pad_View *view);
            Pad_Bool Check_render_size(void);
    virtual void     Compute_bounding_box(void);
    virtual float    Compute_dimensions(float &global_width, float &global_height);
    virtual float    Compute_dimensions(Pad_Event *event, float &global_width, float &global_height);
    virtual float    Compute_dimensions(Pad_List &views, float &global_width, float &global_height);
    virtual float    Compute_fade(void);
    virtual float    Compute_scale_within_frame(float *frame);
    virtual Pad_Bool Contains_text(char *text);
    virtual Pad_Bool Continue_refinement(void);         // True if object wants more refinement
    virtual int      Create_obj_args(int argc, char **argv);   // Called when an item is created with string args
    virtual void     Create_obj_error(void);            // Called if there is an error configuring obj during create
    virtual void     Damage(void);                      // Mark an object as changed
            Pad_Bool Delete_all_tags(void);
            Pad_Bool Delete_id(void);
            Pad_Bool Delete_tag(char *tag);
            Pad_Bool Enclosed(Pad_Object *pad_object);
            Pad_Bool Enclosed_or_same(Pad_Object *pad_object);
            Pad_Bool Fire_render_script(Pad_Bool &refine);
            int      Generate_delete(void);
            int      Generate_event(int type, Pad_String &result);
            int      Generate_event(int type, Pad_List *portals);
            int      Generate_event(int type, Pad_List *portals, char *info);
            void     Get_aliases(Pad_List&);
            Pad_List* Get_aliases(void);
            Pad_Bool  Get_alwaysrender(void);
            Pad_Anchor Get_anchor(void);
    virtual float     Get_angle(void);
    inline  Pad_BBox *Get_bbox(void) {return(&bbox);}
            void     Get_bbox(float *bb);
            void     Get_bbox(Pad_BBox &bb);

            enum Clipping {CLIPPING_FALSE=PAD_CLIPPING_FALSE, CLIPPING_TRUE=PAD_CLIPPING_TRUE, CLIPPING_AUTO=PAD_CLIPPING_AUTO};

            Clipping Get_clipping(void);

    virtual void     Get_coords(Pad_PList &pts, Pad_Bool object_coords);
            char *   Get_event_order(void);
            Pad_Bool Get_events(void);
            float    Get_faderange(void);
    virtual void     Get_fillname(Pad_String &fillname);
    inline  Pad_BBox *Get_global_bbox(void) {return(&globalBBox);}
            void     Get_global_bbox(float *bb);
            void     Get_global_bbox(Pad_BBox &bb);
            float    Get_global_size(void);
            float    Get_height(void);
    virtual char *   Get_info(void);
            char *   Get_layer(void);
            Pad_Bool Get_lock(void);
            void     Get_minsize(Pad_String &minsize);
            float    Get_minsize(void);
            Pad_Bool Get_minsizep(void);
            void     Get_maxsize(Pad_String &maxsize);
            float    Get_maxsize(void);
            Pad_Bool Get_maxsizep(void);
    virtual Pad_Bool Get_name(Pad_String &string);
            Pad_NoiseData * Get_noisedata();
    virtual void     Get_pen_name(Pad_String &penname);
    virtual float    Get_penwidth(void);
    virtual Pad_Bool Get_abslinestyle();
            void     Get_abs_position(float &x, float &y, float &s, Pad_Bool useCoordFrames=TRUE);
            float    Get_abs_position_x(void);
            float    Get_abs_position_y(void);
            float    Get_abs_position_z(void);
            void     Get_rel_position(float &x, float &y, float &s, Pad_Bool useCoordFrames=TRUE);
            float    Get_rel_position_x(void);
            float    Get_rel_position_y(void);
            float    Get_rel_position_z(void);
    virtual Pad_Callback *  Get_renderscript(void);
            int             Get_sticky(void);
    virtual Pad_Bool        Get_text(Pad_String &string);
    virtual Pad_Text *      Get_text_obj(void);
            Pad_Transform * Get_transform(void);
            float           Get_transparency(void);
            Pad_TreeLayout* Get_treelayout();
            Pad_TreeNode*   Get_treenode();
            Pad_String *    Get_usertype(void);
    virtual Pad_Callback *  Get_viewscript(void);
    virtual float           Get_width(void);
            Pad_List *      Get_zoomaction(void);
            Pad_List *      Get_prevsizes(void);  // Returns the size list for zoomactions
            Pad_Bool        Gets_events(void);
    virtual Pad_Bool        Get_written(void);
            Pad_Bool        Has_focus(void);
    virtual int             Initialize(Pad_View &pad_view, char *filename);
            void            _Insert_forward(Pad_List &elements);
            void            _Insert_reverse(Pad_List &elements);
    virtual Pad_Bool Is_container(void);    // True if a container (or derived from one)
    virtual Pad_Bool Is_group(void);        // True if a group (or derived from one)
            Pad_Bool Is_findable(void);	    // True if object should respond to find commands
    virtual Pad_Bool Is_rotatable(void);    // True if object can be rotated
    virtual Pad_Bool Is_sticky(void);       // True if sticky or member of a sticky group
    virtual Pad_Bool Is_text(void);         // True if a type of text object (derives from Pad_Text)
    virtual Pad_Bool Is_visible(void);      // True if object is visible within the view.
    virtual Pad_Bool Is_visible(float viewBB[4], Pad_View*);
            void     Local_to_screen(float &s);
            void     Local_to_screen(float &x, float &y, float &s);
            void     Local_to_screen(float *bb);
            void     Local_to_screen(Pad_Point &pt);
            void     Local_to_screen(Pad_Event &event);
            Pad_Bool Lower(void);
            Pad_Bool Lower(Pad_Object *obj);
    virtual int      Next_drawing_order(void);
            Pad_Bool Overlaps(Pad_Object *pad_object);
    virtual Pad_Bool Pick(Pad_Event *event, float halo);
    virtual void     Pointer_in(Pad_Event *padEvent);
    virtual void     Pointer_out(Pad_Event *padEvent);
            void     Propogate_drawing_order(void);
            void     Remove_from_layer(void);
            Pad_Bool Raise(void);
            Pad_Bool Raise(Pad_Object *obj);
            void     Remove_from_drawing_order(void);
            Pad_Bool Remove_alias(Pad_Object *obj);
            void     Remove_all_aliases(void);
    virtual Pad_Bool Render(void);
    virtual Pad_Bool Render_medium(void);
    virtual Pad_Bool Render_fast(void);
    virtual Pad_Bool Rotate(float theta);
    virtual Pad_Bool Rotate(float theta, Pad_Point &center);
            Pad_Bool Scale(float x, float y, float zmult, Pad_Bool transform=FALSE);
            void     Screen_to_local(float &s);
            void     Screen_to_local(float &x, float &y, float &s);
            void     Screen_to_local(float *bb);
            void     Screen_to_local(Pad_Point &pt);
            void     Screen_to_local(Pad_Event &event);
            void     Set_anchor_from_position(void);
            Pad_Bool Set_alwaysrender(Pad_Bool alwaysrender);
            void     Set_alwaysrender_default(void);
            Pad_Bool Set_angle(float theta, Pad_Point *ctrpt=NULL);
            void     Set_angle_default(void);
            Pad_Bool Set_anchor(Pad_Anchor new_anchor);
            void     Set_anchor_default(void);
            void     Set_bbox(float *bb);
            void     Set_bbox(Pad_BBox &bb);
            void     Set_bbox(float xmin, float ymin, float xmax, float ymax);
            void     Set_bbox_xmin(float xmin);
            void     Set_bbox_ymin(float ymin);
            void     Set_bbox_xmax(float xmax);
            void     Set_bbox_ymax(float ymax);
    virtual void     Set_bounding_box_to_enclose_list(Pad_List &list);
            void     Set_clipping(Clipping clipping);
            void     Set_clipping_default(void);
    virtual Pad_Bool Set_coords(Pad_PList &pts, Pad_Bool object_coords);
            Pad_Bool Set_event_order(char *type);
            Pad_Bool Set_events(Pad_Bool events);
            void     Set_events_default(void);
            Pad_Bool Set_faderange(float faderange);
            void     Set_faderange_default(void);
    virtual Pad_Bool Set_fill(int red, int green, int blue);
    virtual Pad_Bool Set_fill(char *colorName);
            void     Set_findable(Pad_Bool findable);
    virtual void     Set_focus(void);                  // This object is now the focus object
    virtual Pad_Bool Set_height(float height);
    virtual void     Set_height_default(void);
            Pad_Bool Set_id(void);
            Pad_Bool Set_id(int new_id);
            Pad_Bool Set_info(char *info);
            void     Set_info_default(void);
            Pad_Bool Set_layer(char *l);
            void     Set_layer_default(void);
            Pad_Bool Set_lock(Pad_Bool visible);
            void     Set_lock_default(void);
            Pad_Bool Set_minsize_abs(float minsize);
            Pad_Bool Set_minsize_rel(float minsize);
            void     Set_minsize(char *minsize);
            void     Set_minsize_default(void);
            Pad_Bool Set_maxsize_abs(float minsize);
            Pad_Bool Set_maxsize_rel(float minsize);
            void     Set_maxsize(char *minsize);
            void     Set_maxsize_default(void);
            Pad_Bool Set_noisedata(float, float, float, float);
            void     Set_noisedata_default(void);
            Pad_Bool Set_abs_position(float x, float y, float s, Pad_Bool useCoordFrames=TRUE);
            void     Set_abs_position_xy(float x, float y);
            void     Set_abs_position_x(float x);
            void     Set_abs_position_y(float y);
            Pad_Bool Set_abs_position_z(float z);
            void     Set_abs_position_center(void);
            Pad_Bool Set_rel_position(float x, float y, float s, Pad_Bool useCoordFrames=TRUE);
            void     Set_rel_position_xy(float x, float y);
            void     Set_rel_position_x(float x);
            void     Set_rel_position_y(float y);
            Pad_Bool Set_rel_position_z(float z);
            void     Set_rel_position_center(void);
    virtual Pad_Bool Set_pen(int red, int green, int blue);
    virtual Pad_Bool Set_pen(char *colorName);
    virtual Pad_Bool Set_penwidth(float penwidth, Pad_Bool absLineStyle = FALSE);
            void     Set_position_from_anchor(void);
    virtual Pad_Bool Set_renderscript(Pad_Callback *callback);
    virtual void     Set_renderscript_default(void);
            Pad_Bool Set_sticky(int sticky);
            void     Set_sticky_default(void);
    virtual Pad_Bool Set_text(char *new_text);
    virtual Pad_Bool Set_text_default(void);
            Pad_Bool Set_transparency(float trans);
            void     Set_transparency_default(void);
            Pad_Bool Set_treelayout(Pad_TreeLayout *val);
            Pad_Bool Set_treenode(Pad_TreeNode *val);
            Pad_Bool Set_usertype(Pad_String*);
    virtual Pad_Bool Set_view(float newXview, float newYview, float newZoom, Pad_Bool render_now=FALSE);
    virtual Pad_Bool Set_viewscript(Pad_Callback *new_viewscript);
    virtual void     Set_viewscript_default(void);
    virtual Pad_Bool Set_width(float width);
    virtual void     Set_width_default(void);
    virtual Pad_Bool Set_written(Pad_Bool, Pad_Bool groupMembers=TRUE);
            void     Modify_zoomaction(Pad_ZoomAction *zoomAction);
            void     Set_zoomaction_default(void);
            int      Sizeof_tree(void);
            Pad_Bool Slide(float dx, float dy, Pad_Bool transform=FALSE);
            void     Transform_sticky(void);
    virtual void     Unset_focus(void);	                // This object is no longer the focus object
            void     Update(void);                      // Object has changed, compute bbox, damage, etc.
            void     Update_and_compute_anchor(void);   // Object has changed, update it *and* compute new anchor
    virtual void     Update_global_bounding_box(void);
            void     Update_sticky(void);               // View has changed, update sticky object for new view

    virtual ~Pad_Object();
    Pad_Object(Pad *pad);	// Constructor for most Pad_Objects
    Pad_Object();		// Constructor for creating Pad (root of tree) and Pad_View (can't be protected)
    void Init(void);		// Used by constructors

    PAD_TYPE("pad_object");

  protected:
    friend class Pad_ObjHandle;

    Pad_Handle *_handle;        // Pointer to first handle to this object, or NULL if none
    Pad_List _traitList;   // For infrequently used options
    int      _traitFlags;  // Bit field of used traits         
    virtual Pad_Trait *_Get_trait(int);  // To get a trait for an option

    virtual Pad_Trait *_Get_trait(int, char*&);     // Convinient routines for 
    virtual Pad_Trait *_Get_trait(int, Pad_String*&); // setting trait values.
    virtual Pad_Trait *_Get_trait(int, Pad_Callback*&);
    virtual Pad_Trait *_Get_trait(int, Pad_List*&);
    virtual Pad_Bool   _Set_trait(int, char*);
    virtual Pad_Bool   _Set_trait(int, Pad_String*);
    virtual Pad_Bool   _Set_trait(int, Pad_Callback*);
    virtual Pad_Bool   _Set_trait(int, Pad_List*);
    virtual Pad_Bool   _Remove_trait(int);
    virtual void       _Set_angle_data(float);

    float _Compute_dimensions(Pad_List &views, float mag, float &global_width, float &global_height);
};


//
// A class for identifying a Pad_Object by id and the pad surface it is on.
// This is a safe structure for storing a handle to an object where the 
// object may have been deleted by the time it is accessed.  
// Use Get_object() to get the Pad_Object pointer, and to
// determine if it still exists.  It will be NULL if the object
// no longer exists.
//
class Pad_ObjectHandle
{
  public:
    Pad_ObjectHandle(Pad_Object *obj);
    Pad_Object *Get_object(void);

  private:
    int  id;			// Id of object
    Pad *pad;			// Pad surface object is on
};

#endif
