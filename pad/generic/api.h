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

/*
  File: api.h

  This defines the official published C++ API to Pad++.
  It contains a set of classes which define all functionality 
  that is available to applications.

  All communication between applications and Pad++ should occur through
  this API.  This includes applications written in C++, and applications
  written in other languages.  In the case of other languages (such as
  Tcl, Scheme, or Perl, etc.), they should communicate with a set of wrapper
  procedures for that language which communicate with Pad++ through these 
  procedures.  

  While there is nothing stopping an application from accessing Pad++
  internals directly, this is considered unsafe and is unsupported.
  In particular, Pad++ makes no promises about how internals may change
  in the future - but we will make a great effort to support this API
  in a backwards compatible way.

  There are several variable types that are used that the caller must know about.
  These include:
     Pad_List        - A list of (void *)'s.  The user must cast these carefully.
     Pad_PList       - A list of (Pad_Point)'s.  Note that it is a list of actual
                       Pad_Point instances, and not pointers to Pad_Point.
     Pad_Bool        - A 1 byte boolean.  Can be TRUE or FALSE.
     Pad_String      - A dynamically growable string class.
     Pad_Anchor      - A anchor type specifying a point on a bounding box of an object
     Pad_Callback    - A reference to a callback that can be in any of the supported
                       languages.  This includes at least C++ and Tcl, but may also
                       include Scheme or Perl if they are compiled in.
		       See the callback API in generic/callback.h
     Pad_Renderer    - The system renderer that can be used in a render callback
                       to draw directly onto the screen.  When in a render callback,
		       the renderer can be accessed through the global variable,
		       Pad_renderer.  See the renderer API in generic/renderer.h

  The API is (will be...) completely documented in files in the PADHOME/doc directory.

  WARNING: This is a work in progress!
 */

#ifndef API_H
#define API_H 1

#include "defs.h"
#include "list.h"
#include "ilist.h"
#include "plist.h"
#include "pad-string.h"
#include "types.h"
#include "bbox.h"
#include "object.h"

#include <stdarg.h>

//////////////////////////////////////////////////////////////////////
//
// Forward References
//
//////////////////////////////////////////////////////////////////////

class Pad;
class Pad_Object;
class Pad_Text;
class Pad_View;
class Pad_Win;
class Pad_Callback;
class Pad_Event;
class Pad_Renderer;
class Pad_AnimPath;
class Pad_Animation;

//////////////////////////////////////////////////////////////////////
//
// Global variables used by the Pad++ API
//
//////////////////////////////////////////////////////////////////////

extern Pad_String      Pad_errorString; // Contains descriptive string of current error
extern Pad_Renderer   *Pad_renderer;    // Current system renderer


//////////////////////////////////////////////////////////////////////
//
// Types used by the Pad++ API
//
//////////////////////////////////////////////////////////////////////


         // Pad++ object types that are used to specify which type of
         // object to create, or othertimes when object types are
         // specified.  Applications should use the lower case
         // versions below.  The upper case versions are for
         // internal use only.
enum Pad_ObjectType {
    Pad_NoType        = 0,
    Pad_AliasType     = PAD_ALIAS,
    Pad_ButtonType    = PAD_BUTTON,
    Pad_CanvasType    = PAD_CANVAS,
    Pad_ContainerType = PAD_CONTAINER,
    Pad_FrameType     = PAD_FRAME,
    Pad_GridType      = PAD_GRID,
    Pad_GroupType     = PAD_GROUP,
    Pad_HTMLType      = PAD_HTML,
    Pad_ImageType     = PAD_IMAGE,
    Pad_KPLType       = PAD_KPL,
    Pad_LabelType     = PAD_LABEL,
    Pad_LineType      = PAD_LINE,
    Pad_PanelType     = PAD_PANEL,
    Pad_OvalType      = PAD_OVAL,
    Pad_PadType       = PAD_PAD,
    Pad_PolygonType   = PAD_POLYGON,
    Pad_PortalType    = PAD_PORTAL,
    Pad_RectangleType = PAD_RECTANGLE,
    Pad_ScrollbarType = PAD_SCROLLBAR,
    Pad_SplineType    = PAD_SPLINE,
    Pad_TCLType       = PAD_TCL,
    Pad_TextType      = PAD_TEXT,
    Pad_TextareaType  = PAD_TEXTAREA,
    Pad_TextfieldType = PAD_TEXTFIELD,
    Pad_TextfileType  = PAD_TEXTFILE,
    Pad_WindowType    = PAD_WINDOW
};

         // Pad++ object option types that are used to specify which option
         // of an object is being specified.
         // Applications should use the lower case versions below.  
         // The upper case versions are for internal use only.
enum Pad_OptionType {
         // These are options that every object type has.
    Pad_NoOption            = 0,
    Pad_AliasesOption       = PAD_ALIASES,
    Pad_AlwaysRenderOption  = PAD_ALWAYSRENDER,
    Pad_AnchorOption        = PAD_ANCHOR,
    Pad_AngleOption         = PAD_ANGLE,
    Pad_AngleCtrOption      = PAD_ANGLECTR,
    Pad_EventsOption        = PAD_EVENTS,
    Pad_FadeRangeOption     = PAD_FADERANGE,
    Pad_HeightOption        = PAD_HEIGHT,
    Pad_InfoOption          = PAD_INFO,
    Pad_LayerOption         = PAD_LAYER,
    Pad_LockOption          = PAD_LOCK,
    Pad_MaxSizeAbsOption    = PAD_LASTOPTION + 1,
    Pad_MaxSizeRelOption    = PAD_MAXSIZE,
    Pad_MinSizeAbsOption    = PAD_LASTOPTION + 2,
    Pad_MinSizeRelOption    = PAD_MINSIZE,
    Pad_NoiseDataOption     = PAD_NOISEDATA,
    Pad_RenderScriptOption  = PAD_RENDERSCRIPT,
    Pad_StickyOption        = PAD_STICKY,
    Pad_TagsOption          = PAD_TAGS,
    Pad_TimerScriptOption   = PAD_TIMERSCRIPT,
    Pad_TimerRateOption     = PAD_TIMERRATE,
    Pad_TransparencyOption  = PAD_TRANSPARENCY,
    Pad_ViewScriptOption    = PAD_VIEWSCRIPT,
    Pad_WidthOption         = PAD_WIDTH,
    Pad_ZoomActionOption    = PAD_ZOOMACTION,
    Pad_RPositionOption     = PAD_RPOSITION,
    Pad_PositionOption      = PAD_POSITION,

	// Options for derived types. These options are only valid
        // for the relevant object types.
    Pad_ArrowOption         = PAD_ARROW,
    Pad_ArrowshapeOption    = PAD_ARROWSHAPE,
    Pad_BBoxOption          = PAD_BBOX,
    Pad_BorderOption        = PAD_BORDER,
    Pad_BorderWidthOption   = PAD_BORDERWIDTH,
    Pad_CapStyleOption      = PAD_CAPSTYLE,
    Pad_CommandOption       = PAD_COMMAND,
    Pad_DitherOption        = PAD_DITHER,
    Pad_DivisibleOption     = PAD_DIVISIBLE,
    Pad_DoneScriptOption    = PAD_DONESCRIPT,
    Pad_EchoCharOption      = PAD_ECHOCHAR,
    Pad_EditableOption      = PAD_EDITABLE,
    Pad_ErrorScriptOption   = PAD_ERRORSCRIPT,
    Pad_FileOption          = PAD_FILE,
    Pad_FillOption          = PAD_FILL,
    Pad_FontOption          = PAD_FONT,
    Pad_FromOption          = PAD_FROM,
    Pad_HTMLAnchorsOption   = PAD_HTMLANCHORS,
    Pad_ImageDataOption     = PAD_IMAGEDATA,
    Pad_IsMapOption         = PAD_ISMAP,
    Pad_JoinStyleOption     = PAD_JOINSTYLE,
    Pad_LineSizeOption      = PAD_LINESIZE,
    Pad_LookOnOption        = PAD_LOOKON,
    Pad_MembersOption       = PAD_MEMBERS,
    Pad_OrientationOption   = PAD_ORIENTATION,
    Pad_PageSizeOption      = PAD_PAGESIZE,
    Pad_PenOption           = PAD_PEN,
    Pad_PenWidthOption      = PAD_PENWIDTH,
    Pad_ReferenceOption     = PAD_REFERENCE,
    Pad_ReliefOption        = PAD_RELIEF,
    Pad_SizeOption          = PAD_SIZE,
    Pad_StateOption         = PAD_STATE,
    Pad_TitleOption         = PAD_TITLE,
    Pad_TextOption          = PAD_TEXTOPTION,
    Pad_ToOption            = PAD_TO,
    Pad_UpdateScriptOption  = PAD_UPDATESCRIPT,
    Pad_URLOption           = PAD_URL,
    Pad_ValueOption         = PAD_VALUE,
    Pad_ViewOption          = PAD_VIEWOPTION,
    Pad_VisibleLayersOption = PAD_VISIBLELAYERS,
    Pad_WriteFormatOption   = PAD_WRITEFORMAT
};

/*
 *----------------------------------------------------------------------
 *
 * class Pad_Handle --
 *
 *    A class for referencing zero, one or more things within Pad++.
 *    There are different handle types to reference different kinds of
 *    things.  Handles have special code to reference Pad_Objects.  
 *    They can store either a pointer to an object, or a tag that 
 *    implicitly specifies all the objects that share that tag.  
 *    Derived handle types can use _data slot to reference any kind of data.
 * 
 *    This is a safe structure for storing a handle to an object where the 
 *    object may have been deleted by the time it is accessed.  A linked
 *    list of all handles that point to an object is maintained, and when
 *    an object is deleted, all handles sharing that object have their
 *    pointers NULLed.
 *
 *    Note that some data that is used only by derived types is in
 *    this base class so that all Pad_Handles are the same size, and
 *    we can take over memory management for them.
 *
 *    This is an abstract base class, and can not be instantiated.
 *    It is useful only because we can have types derived from this one.
 *
 *----------------------------------------------------------------------
 */

class Pad_Handle
{
  public:
    Pad_Handle();
    Pad_Handle(Pad_Handle &handle);
    virtual ~Pad_Handle();

    void *       operator new(size_t size);            // Do our own memory management to avoid memory fragmentation
    void         operator delete(void *object);
    Pad_Handle & operator=(Pad_Handle &handle);        // Assignment operator
    Pad_Bool     operator==(Pad_Handle &handle);       // Equality operator
    Pad_Bool     operator!=(Pad_Handle &handle);       // InEquality operator

  protected:
    friend class Pad_ObjHandle;
    friend class Pad_Object;

				// A handle can specify either an object directly (with the _obj) slot,
				// or a group of objects indirectly using a tag in which case the
				// _obj slot is used to specify the pad widget the tag references.
				// Or, a handle can reference another kind of object specified by _data.
				// 
				// Note: All data for every handle type is stored in the base class
				// so that every Handle is the same size, and thus we can easily
				// do our own memory management.

    Pad_Object *_obj;	// Pointer to object.  This will be set to NULL when obj is deleted
    Pad_Handle *_next;	// Pointer to next handle sharing this object (or pad), or NULL if last one
    Pad_String *_tag;   // Tag that specifies objects.
    void       *_data;  // Pointer to type-specific data
};

/*
 *----------------------------------------------------------------------
 *
 * class Pad_ObjHandle --
 *
 *    A handle to one or more Pad++ Objects.  A handle can specify either a
 *    single object with very quick access, or a group of objects specified
 *    by a tag with access through a hash table.  Handles are memory-safe,
 *    even when the object the handle references has been deleted.
 *
 *    Use Get_id() to determine if the handle actually references any objects.
 *    It will return the id of the first object referenced, or 0 if none.
 *
 *---------------------------------------------------------------------- */

class Pad_ObjHandle : public Pad_Handle
{
  public:
    Pad_ObjHandle();
    Pad_ObjHandle(Pad_ObjHandle &objHandle);
    Pad_ObjHandle(Pad_Object *obj);
    virtual ~Pad_ObjHandle();
    Pad_ObjHandle & operator=(Pad_ObjHandle &objHandle); // Assignment operator

            Pad_Bool       Attach(Pad_Object *obj);     // Set object referred to by this handle
            Pad_Bool       Attach(Pad_ObjHandle &objHandle); // Attach to object(s) referenced by handle

  protected:
    friend class Pad_Event;

    Pad_Object * _Get_object(void)       const;     // Return object specified by handle, or NULL if handle specifies tag (or nothing)
};

#endif    // Endif API_H

