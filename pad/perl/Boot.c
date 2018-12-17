/*
"(c) Copyright 1993-1995 Pad++ Consortium {University of New Mexico (UNM),
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

#undef bool
#include "../generic/api.h"
#include "../generic/global.h"
#include "pad-perl.h"

extern "C" {
#define explicit expl  
#include "EXTERN.h"
#include "perl.h"
#undef explicit                 // These names are defined in perl and they
#undef Null                     // conflict with stuff elsewhere in Pad++
}

struct Perl_ObjectType {
    Pad_Event *event;
    Pad_ObjHandle *object;
    Pad_SurfaceHandle *surface;
};

static SV   *       Get_slot_from_ref(SV *padRef, const Pad_String slot);
static Pad_Bool     Init_object (SV *objRef, Pad_String &surfaceName, int &objectId);
static void         xs_init (void);

static SV          *Make_reference(SV *sv);
static SV          *struct_sv(void *ptr, STRLEN sz);
static SV          *Blessed(char *package, SV *sv);
Pad_SurfaceHandle  *Get_surface_from_obj (SV *obj);
Perl_ObjectType    *Get_object_from_obj (SV *obj);

void Pad_Main(int argc, char **argv, Tcl_AppInitProc *appInitProc);

////////////////////////////////////////
//
// Global variables
//
//
////////////////////////////////////////
static Pad_String Perl_resultString;

// These Pad_Strings are used for getting slot values of objects
static const Pad_String PAD  = "pad";
static const Pad_String NAME  = "name";
static const Pad_String OBJECT_ID = "object_id";

static Pad_Bool DEBUG_PERL = FALSE;

void
Boot_glue(void)
{
        // Initialize perl variables that have C++ coupling
        // The type values are defined in api.h
    sv_setiv(perl_get_sv("Pad::ALIAS_TYPE",        TRUE), Pad_AliasType);
    sv_setiv(perl_get_sv("Pad::BUTTON_TYPE",       TRUE), Pad_ButtonType);
    sv_setiv(perl_get_sv("Pad::CANVAS_TYPE",       TRUE), Pad_CanvasType);
    sv_setiv(perl_get_sv("Pad::CONTAINER_TYPE",    TRUE), Pad_ContainerType);
    sv_setiv(perl_get_sv("Pad::FRAME_TYPE",        TRUE), Pad_FrameType);
    sv_setiv(perl_get_sv("Pad::GRID_TYPE",         TRUE), Pad_GridType);
    sv_setiv(perl_get_sv("Pad::GROUP_TYPE",        TRUE), Pad_GroupType);
    sv_setiv(perl_get_sv("Pad::HTML_TYPE",         TRUE), Pad_HTMLType);
    sv_setiv(perl_get_sv("Pad::IMAGE_TYPE",        TRUE), Pad_ImageType);
    sv_setiv(perl_get_sv("Pad::KPL_TYPE",          TRUE), Pad_KPLType);
    sv_setiv(perl_get_sv("Pad::LABEL_TYPE",        TRUE), Pad_LabelType);
    sv_setiv(perl_get_sv("Pad::LINE_TYPE",         TRUE), Pad_LineType);
    sv_setiv(perl_get_sv("Pad::PANEL_TYPE",        TRUE), Pad_PanelType);
    sv_setiv(perl_get_sv("Pad::POLYGON_TYPE",      TRUE), Pad_PolygonType);
    sv_setiv(perl_get_sv("Pad::PORTAL_TYPE",       TRUE), Pad_PortalType);
    sv_setiv(perl_get_sv("Pad::RECTANGLE_TYPE",    TRUE), Pad_RectangleType);
    sv_setiv(perl_get_sv("Pad::SCROLLBAR_TYPE",    TRUE), Pad_ScrollbarType);
    sv_setiv(perl_get_sv("Pad::SPLINE_TYPE",       TRUE), Pad_SplineType);
    sv_setiv(perl_get_sv("Pad::TCL_TYPE",          TRUE), Pad_TCLType);
    sv_setiv(perl_get_sv("Pad::TEXT_TYPE",         TRUE), Pad_TextType);
    sv_setiv(perl_get_sv("Pad::TEXTAREA_TYPE",     TRUE), Pad_TextareaType);
    sv_setiv(perl_get_sv("Pad::TEXTFIELD_TYPE",    TRUE), Pad_TextfieldType);
    sv_setiv(perl_get_sv("Pad::TEXTFILE_TYPE",     TRUE), Pad_TextfileType);
    sv_setiv(perl_get_sv("Pad::WINDOW_TYPE",       TRUE), Pad_WindowType);


         // Pad++ object option types that are used to specify which option
         // of an object is being specified.

         // These are options that every object type has.
    sv_setiv(perl_get_sv("Pad::NOOPTION_OPTION",     TRUE), Pad_NoOption);
    sv_setiv(perl_get_sv("Pad::ALIASES_OPTION",      TRUE), Pad_AliasesOption);
    sv_setiv(perl_get_sv("Pad::ALWAYSRENDER_OPTION", TRUE), Pad_AlwaysRenderOption);
    sv_setiv(perl_get_sv("Pad::ANCHOR_OPTION",       TRUE), Pad_AnchorOption);
    sv_setiv(perl_get_sv("Pad::ANGLE_OPTION",        TRUE), Pad_AngleOption);
    sv_setiv(perl_get_sv("Pad::EVENTS_OPTION",       TRUE), Pad_EventsOption);
    sv_setiv(perl_get_sv("Pad::FADERANGE_OPTION",    TRUE), Pad_FadeRangeOption);
    sv_setiv(perl_get_sv("Pad::HEIGHT_OPTION",       TRUE), Pad_HeightOption);
    sv_setiv(perl_get_sv("Pad::INFO_OPTION",         TRUE), Pad_InfoOption);
    sv_setiv(perl_get_sv("Pad::LAYER_OPTION",        TRUE), Pad_LayerOption);
    sv_setiv(perl_get_sv("Pad::LOCK_OPTION",         TRUE), Pad_LockOption);
    sv_setiv(perl_get_sv("Pad::MAXSIZE_OPTION",      TRUE), Pad_MaxSizeRelOption);
    sv_setiv(perl_get_sv("Pad::MINSIZE_OPTION",      TRUE), Pad_MinSizeRelOption);
    sv_setiv(perl_get_sv("Pad::NOISEDATA_OPTION",    TRUE), Pad_NoiseDataOption);
    sv_setiv(perl_get_sv("Pad::RENDERSCRIPT_OPTION", TRUE), Pad_RenderScriptOption);
    sv_setiv(perl_get_sv("Pad::STICKY_OPTION",       TRUE), Pad_StickyOption);
    sv_setiv(perl_get_sv("Pad::TAGS_OPTION",         TRUE), Pad_TagsOption);
    sv_setiv(perl_get_sv("Pad::TIMERSCRIPT_OPTION",  TRUE), Pad_TimerScriptOption);
    sv_setiv(perl_get_sv("Pad::TIMERRATE_OPTION",    TRUE), Pad_TimerRateOption);
    sv_setiv(perl_get_sv("Pad::TRANSPARENCY_OPTION", TRUE), Pad_TransparencyOption);
    sv_setiv(perl_get_sv("Pad::VIEWSCRIPT_OPTION",   TRUE), Pad_ViewScriptOption);
    sv_setiv(perl_get_sv("Pad::WIDTH_OPTION",        TRUE), Pad_WidthOption);
    sv_setiv(perl_get_sv("Pad::ZOOMACTION_OPTION",   TRUE), Pad_ZoomActionOption);
    sv_setiv(perl_get_sv("Pad::POSITION_OPTION",     TRUE), Pad_PositionOption);
    sv_setiv(perl_get_sv("Pad::RPOSITION_OPTION",    TRUE), Pad_RPositionOption);
    sv_setiv(perl_get_sv("Pad::MAXSIZEABS_OPTION",   TRUE), Pad_MaxSizeAbsOption);
    sv_setiv(perl_get_sv("Pad::MINSIZEABS_OPTION",   TRUE), Pad_MinSizeAbsOption);

	// Options for derived types. These options are only valid
        // for the relevant object types.
    sv_setiv(perl_get_sv("Pad::ARROW_OPTION",        TRUE), Pad_ArrowOption);
    sv_setiv(perl_get_sv("Pad::ARROWSHAPE_OPTION",   TRUE), Pad_ArrowshapeOption);
    sv_setiv(perl_get_sv("Pad::BBOX_OPTION",         TRUE), Pad_BBoxOption);
    sv_setiv(perl_get_sv("Pad::BORDER_OPTION",       TRUE), Pad_BorderOption);
    sv_setiv(perl_get_sv("Pad::BORDERWIDTH_OPTION",  TRUE), Pad_BorderWidthOption);
    sv_setiv(perl_get_sv("Pad::CAPSTYLE_OPTION",     TRUE), Pad_CapStyleOption);
    sv_setiv(perl_get_sv("Pad::COMMAND_OPTION",      TRUE), Pad_CommandOption);
    sv_setiv(perl_get_sv("Pad::DITHER_OPTION",       TRUE), Pad_DitherOption);
    sv_setiv(perl_get_sv("Pad::DIVISIBLE_OPTION",    TRUE), Pad_DivisibleOption);
    sv_setiv(perl_get_sv("Pad::DONESCRIPT_OPTION",   TRUE), Pad_DoneScriptOption);
    sv_setiv(perl_get_sv("Pad::ECHOCHAR_OPTION",     TRUE), Pad_EchoCharOption);
    sv_setiv(perl_get_sv("Pad::EDITABLE_OPTION",     TRUE), Pad_EditableOption);
    sv_setiv(perl_get_sv("Pad::ERRORSCRIPT_OPTION",  TRUE), Pad_ErrorScriptOption);
    sv_setiv(perl_get_sv("Pad::FILE_OPTION",         TRUE), Pad_FileOption);
    sv_setiv(perl_get_sv("Pad::FILL_OPTION",         TRUE), Pad_FillOption);
    sv_setiv(perl_get_sv("Pad::FONT_OPTION",         TRUE), Pad_FontOption);
    sv_setiv(perl_get_sv("Pad::FROM_OPTION",         TRUE), Pad_FromOption);
    sv_setiv(perl_get_sv("Pad::HTMLANCHORS_OPTION",  TRUE), Pad_HTMLAnchorsOption);
    sv_setiv(perl_get_sv("Pad::IMAGEDATA_OPTION",    TRUE), Pad_ImageDataOption);
    sv_setiv(perl_get_sv("Pad::ISMAP_OPTION",        TRUE), Pad_IsMapOption);
    sv_setiv(perl_get_sv("Pad::JOINSTYLE_OPTION",    TRUE), Pad_JoinStyleOption);
    sv_setiv(perl_get_sv("Pad::LINESIZE_OPTION",     TRUE), Pad_LineSizeOption);
    sv_setiv(perl_get_sv("Pad::LOOKON_OPTION",       TRUE), Pad_LookOnOption);
    sv_setiv(perl_get_sv("Pad::MEMBERS_OPTION",      TRUE), Pad_MembersOption);
    sv_setiv(perl_get_sv("Pad::ORIENTATION_OPTION",  TRUE), Pad_OrientationOption);
    sv_setiv(perl_get_sv("Pad::PAGESIZE_OPTION",     TRUE), Pad_PageSizeOption);
    sv_setiv(perl_get_sv("Pad::PEN_OPTION",          TRUE), Pad_PenOption);
    sv_setiv(perl_get_sv("Pad::PENWIDTH_OPTION",     TRUE), Pad_PenWidthOption);
    sv_setiv(perl_get_sv("Pad::REFERENCE_OPTION",    TRUE), Pad_ReferenceOption);
    sv_setiv(perl_get_sv("Pad::RELIEF_OPTION",       TRUE), Pad_ReliefOption);
    sv_setiv(perl_get_sv("Pad::SIZE_OPTION",         TRUE), Pad_SizeOption);
    sv_setiv(perl_get_sv("Pad::STATE_OPTION",        TRUE), Pad_StateOption);
    sv_setiv(perl_get_sv("Pad::TITLE_OPTION",        TRUE), Pad_TitleOption);
    sv_setiv(perl_get_sv("Pad::TEXT_OPTION",         TRUE), Pad_TextOption);
    sv_setiv(perl_get_sv("Pad::TO_OPTION",           TRUE), Pad_ToOption);
    sv_setiv(perl_get_sv("Pad::UPDATESCRIPT_OPTION", TRUE), Pad_UpdateScriptOption);
    sv_setiv(perl_get_sv("Pad::URL_OPTION",          TRUE), Pad_URLOption);
    sv_setiv(perl_get_sv("Pad::VALUE_OPTION",        TRUE), Pad_ValueOption);
    sv_setiv(perl_get_sv("Pad::VIEW_OPTION",         TRUE), Pad_ViewOption);
    sv_setiv(perl_get_sv("Pad::VISIBLELAYERS_OPTION",TRUE), Pad_VisibleLayersOption);
    sv_setiv(perl_get_sv("Pad::WRITEFORMAT_OPTION",  TRUE), Pad_WriteFormatOption);

         // Pad++ anchor type
    sv_setiv(perl_get_sv("Pad::ANCHORN",      TRUE), Pad_AnchorN);
    sv_setiv(perl_get_sv("Pad::ANCHORNE",     TRUE), Pad_AnchorNE);
    sv_setiv(perl_get_sv("Pad::ANCHORE",      TRUE), Pad_AnchorE);
    sv_setiv(perl_get_sv("Pad::ANCHORSE",     TRUE), Pad_AnchorSE);
    sv_setiv(perl_get_sv("Pad::ANCHORS",      TRUE), Pad_AnchorS);
    sv_setiv(perl_get_sv("Pad::ANCHORSW",     TRUE), Pad_AnchorSW);
    sv_setiv(perl_get_sv("Pad::ANCHORW",      TRUE), Pad_AnchorW);
    sv_setiv(perl_get_sv("Pad::ANCHORNW",     TRUE), Pad_AnchorNW);
    sv_setiv(perl_get_sv("Pad::ANCHORCENTER", TRUE), Pad_AnchorCenter);

	// Initialize the predefined XSUBs
    xs_init();
    
	// Call the Pad main loop
    char **argv;
    argv[0] = "Pad.pm";
    argv[1] = NULL;
    Pad_Main(1,argv,Tcl_AppInit);
}

//////////////////////////////////////////////////////////////////////
//
// The following rotuines are helper functions for the Perl XSUBS
//
//////////////////////////////////////////////////////////////////////


static SV *
Make_reference(SV *sv)
{
 SV *rv = newRV(sv);              /* REFCNT of sv now 2 */
 SvREFCNT_dec(sv);
 return rv;
}

static SV *
Blessed(char *package, SV *sv)
{
 HV *stash = gv_stashpv(package, TRUE);
 return sv_bless(sv, stash);
}

static SV *
struct_sv(void *ptr, STRLEN sz)
{
 SV *sv = (ptr) ? newSVpv((char *) ptr, sz) : newSV(sz);
 if (ptr)
  {
   SvREADONLY_on(sv);
  }
 else
  {
   Zero(SvPVX(sv),sz+1,char);
   SvCUR_set(sv,sz);
   SvPOK_only(sv);
  }
 return sv;
}

/*
 *----------------------------------------------------------------------
 *
 * Init_Object
 *
 *   Dereferences the perl object <objRef>, which is presumed to be a
 *   reference to an object of class Pad, and sets <surfaceName> and
 *   <objectId> to the values in the slots of the object.
 *
 * Result:
 *   Returns TRUE or FALSE.
 *
 * Side effects:
 *   Changes the values of <surfaceName> and <objectId> on success.
 *
 *----------------------------------------------------------------------
 */
static Pad_Bool
Init_object (SV *objRef, Pad_String &surfaceName, int &objectId) {

        // get the name of the pad
    SV *slotVal = Get_slot_from_ref (objRef, PAD);
    if (slotVal == NULL) {
        Pad_errorString += " pad ref";
        return FALSE;
    }
    slotVal = Get_slot_from_ref (slotVal, NAME);
    if (slotVal == NULL) {
        Pad_errorString += " pad name";
        return FALSE;
    }
        // set the pad name
    surfaceName = SvPV (slotVal, na);

        // get the object's id
    slotVal = Get_slot_from_ref (objRef, OBJECT_ID);
    if (slotVal == NULL) {
        Pad_errorString += " object id";
        return FALSE;
    }
        // set the object id
    objectId = SvIV (slotVal);
}

/*
 *----------------------------------------------------------------------
 *
 * Init_Object
 *
 *   Dereferences the perl object <objRef>, which could a reference to
 *   any class in Perl, and sets <slotName>  to the appropriate
 *   values stored in the object.
 *
 * Result:
 *   Returns TRUE or FALSE.
 *
 * Side effects:
 *   Changes the values of <slotName> on success.
 *
 *----------------------------------------------------------------------
 */
static SV *
Get_slot_from_ref(SV *ojbRef, const Pad_String slotName) {
        // all slots in Pad objects are stored in an anonymous hash. This
        // is why SvRV() is cast to an HV*, and then hv_fetch is called to
        // get the value of the slot. This is standard Perl OO procedure.


        // check to make sure this is really a reference
    if (! SvROK (ojbRef)) {
        Pad_errorString = "Bad ref";
        return NULL;
    }
  
    SV *hash = SvRV (ojbRef);  
    SV **slot = hv_fetch((HV *) hash,          // Get the hash from the ref
                         slotName.Get(),     // the slot name
                         slotName.Length(),  // the lenght of the name
                         FALSE);              // don't create a new key

        // If the return value is NULL the key wasn't in the hash
    if (slot == NULL) {
        Pad_errorString = "Bad slot name: ";
        Pad_errorString += slotName;
        return NULL;
    } else {
        return (*slot);
    }
}

Pad_SurfaceHandle *
Get_surface_from_obj (SV *obj)
{
    Pad_SurfaceHandle *surfacePtr = NULL;

            // Get the surface * off the stack 
    if( sv_isobject(obj) && (SvTYPE(SvRV(obj)) == SVt_PVMG) ){
        surfacePtr = (Pad_SurfaceHandle *)SvIV((SV*)SvRV( obj ));
        if (surfacePtr == NULL){
                // the surface got deleted
            Pad_errorString = " -- Surface no longer valid";
        }
    } else {
        Pad_errorString = " -- not a blessed SV reference";
    }
    return surfacePtr;
}


Perl_ObjectType *
Get_object_from_obj (SV *obj)
{
    Perl_ObjectType *objectPtr = NULL;

            // Get the surface * off the stack 
    if( sv_isobject(obj) && (SvTYPE(SvRV(obj)) == SVt_PVMG) ){
        objectPtr = (Perl_ObjectType *)SvIV((SV*)SvRV( obj ));
        if (objectPtr == NULL){
                // something went wrong
            Pad_errorString = " -- Bad reference";
        } else if (objectPtr->object == NULL){
                // the object got deleted
            Pad_errorString = " -- Object no longer valid";
            objectPtr = NULL;
        } else if (objectPtr->surface == NULL){
                // the surface got deleted
            Pad_errorString = " -- Surface no longer valid";
            objectPtr = NULL;
        }
    } else {
        Pad_errorString = " -- not a blessed SV reference";
    }
    return objectPtr;
}


//////////////////////////////////////////////////////////////////////
//
// Perl C++ API
//
//    The following routines are used to register an External Subroutine
//    (XSUB or XS) with perl5. It is used to implement the various glue
//    funcions that are needed to connect Perl to Pad++. If you want to
//    know what these do and how they work, read the following perl man
//    pages: perlcall, perlguts, and perlxs. I'm not going to explain it
//    here.
//
//////////////////////////////////////////////////////////////////////

extern "C" {                    // Perl5 include files don't yet deal with 
#include <XSUB.h>               // the C/C++ name conflict, so we have to.
}

//////////////////////////////////////////////////////////////////////
//
// Set_top_level()
//   function enables switching between the available top level
//   interpreters
//
//////////////////////////////////////////////////////////////////////

XS(XS_SetTopLevel)
{
    dXSARGS;
    if (items != 1)
        croak("Usage: SetTopLevel(language)");
    {
        char * langStr = (char *)SvPV (ST (0),na);
        Pad_Bool rc;
        Pad_Language *language;
    
        rc = Pad_GetLanguage(langStr, &language);
        if (!rc) {
            croak(Pad_errorString.Get());
        } else {
            Pad_topLevelLanguage = language;
        }
    }
    XSRETURN_YES;
}

//////////////////////////////////////////////////////////////////////
//
//  Pad::test()
//    a function for experimenting with the Perl API
//
//
//////////////////////////////////////////////////////////////////////
XS (XS__Pad_test) {
    dXSARGS;
    if (items != 2)
        croak("Usage: pad_ref->test(name)");
    {
        SV *ref = ST (0);
        SV *arg = ST (1);
        SV *slotVal = Get_slot_from_ref (ref, NAME);
    
        if (slotVal == NULL) {
            Perl_resultString = "Pad::test: ";
            Perl_resultString += Pad_errorString;
            croak(Perl_resultString.Get());
        }
    
        char *name = (char *) SvPV(slotVal, na);
    
        cout << "You are "        << (char *)SvPV (ref,   na) << endl;
        cout << "You're name is " << name                     << endl;
        cout << "You said "       << (char *)SvPV (arg,   na) << endl;
    
    }
    XSRETURN_YES;
}

//////////////////////////////////////////////////////////////////////
//
// Pad::Get_surfaces()
//   returns a list of the available pad surfaces
//
//
//////////////////////////////////////////////////////////////////////
XS (XS__Pad_Get_surfaces)
{
    dXSARGS;
    if (items != 1) {
        croak("Usage: Pad::Get_surfaces()");
    }
    SP -= items;

    Pad_Bool first;
    Pad_Name padName;
    Pad_SurfaceHandle *surface;
    Pad_HandleList pads;
    Pad_Iterator oi;
    int i = 0;
    Pad_UniverseMgr *universe = Pad_UniverseMgr::Get_instance();

        // Use the Pad_Universe manager
    universe->Get_surfaces(pads);

    DOLIST(oi, pads, Pad_SurfaceHandle, surface) {
            // add the name to the stack
        padName = surface->Get_name();
        XPUSHs (sv_2mortal(newSVpv((char*) padName, strlen ((char*)padName))));
    }
    PUTBACK;
    return;
}


//////////////////////////////////////////////////////////////////////
//
// Pad::Surface::new()
//   creates a new pad surface
//
//
//////////////////////////////////////////////////////////////////////
XS (XS__Pad_Surface_new)
{
    dXSARGS;
    if (items != 2)
        croak("Usage: Pad::Surface::new(classname, pad_name)");
    {
        PerlData *data;
	Pad_WinHandle win;
        Pad_Bool rc;
        Pad_SurfaceHandle *surface = NULL;
        
        SV *CLASS = ST (0);
        SV *surfaceName = ST (1);
        Perl_resultString = (char *) SvPV(surfaceName, na);
	Pad_UniverseMgr *universe = Pad_UniverseMgr::Get_instance();

	rc = universe->Create_win (Perl_tcl_interp, Perl_resultString.Get(), win);
        if (!rc){
            croak ("Couldn't create new surface");
        }
        surface = new Pad_SurfaceHandle (win);
        if (!surface){
            croak ("Couldn't create new surface");
        }
        
            // Set the result on the stack
        ST(0) = sv_newmortal();
        sv_setref_pv(ST(0), (char*) SvPV(CLASS,na), (void*)surface);
    }
    XSRETURN(1);
}

//////////////////////////////////////////////////////////////////////
//
// Pad::Surface::_Call()
//    implements a path through the Pad++
//    string interface using Pad_Command().
//
//////////////////////////////////////////////////////////////////////

XS(XS__Pad_Surface__Call)
{
    dXSARGS;
    if (items < 2)
        croak("Usage: pad_ref->_Call(...)");
    {
        PerlData *data;
        int i;
        Pad_TkWin *tkwin;
        Tcl_Interp *interp;
        int retVal;
        int argc;
        char **argv;
        Pad_SurfaceHandle *surface;

            // Get the surface *
        surface = Get_surface_from_obj (ST(0));
        if (surface == NULL){
            Perl_resultString = "Pad::Surface::_Call:";
            Perl_resultString += Pad_errorString;
            croak (Perl_resultString.Get());
            XSRETURN_UNDEF;
        }
            
            // start creating the argc argv that Pad_Command() wants
        argc = items;
        argv = new char * [argc];
        argv[0] = surface->Get_name();
            
        for(i=1;i<argc;i++) {
            argv[i] = (char *)SvPV(ST(i), na);
        }

        data = (PerlData *)Perl_Table->Get(argv[0]);
        if (data) {
            tkwin = data->tkwin;
            interp = data->interp;

                // we really should check ret_val here...
            if (! Pad_Command((ClientData)tkwin, interp, argc, argv) == TCL_OK) {
                Pad_errorString =  "Pad::_Call: ";
                Pad_errorString += interp->result;
                Tcl_ResetResult (interp); // So Tcl doesn't echo the error
                croak (Pad_errorString.Get());
            }
            Perl_resultString = interp->result;
            Tcl_ResetResult (interp); // So Tcl doesn't echo the result
        } else {
            Pad_errorString =  "Pad::_Call: non-existent pad widget: ";
            Pad_errorString += argv[0];
            croak (Pad_errorString.Get());
        }
        ST(0) = sv_newmortal();
        sv_setpv(ST(0), Perl_resultString.Get());
    }
    XSRETURN(1);
}

//////////////////////////////////////////////////////////////////////
//
// Pad::Surface::Pack() uses the Tk packer to pack a pad widget
//
//
//////////////////////////////////////////////////////////////////////
XS (XS__Pad_Surface_Pack)
{
    dXSARGS;
    if (items != 1)
        croak("Usage: pad_ref->Pack()");
    {
        Pad_SurfaceHandle *surface;

            // Get the surface *
        surface = Get_surface_from_obj (ST(0));
        if (surface == NULL){
            Perl_resultString = "Pad::Surface::_Call:";
            Perl_resultString += Pad_errorString;
            croak (Perl_resultString.Get());
            XSRETURN_UNDEF;
        }
            
        Perl_resultString = "pack ";
        Perl_resultString += surface->Get_name();

            // Pack the new pad using Tcl -- Yucchh!
        Tcl_Eval (Perl_tcl_interp, Perl_resultString.Get());

            // Push the result on the stack
        Perl_resultString = Perl_tcl_interp->result;
        ST(0) = sv_newmortal();
        sv_setpvn(ST(0), Perl_resultString.Get(), Perl_resultString.Length());
    }
    XSRETURN(1);
}

/*
 *----------------------------------------------------------------------
 *
 * Pad::Surface::Get_option()
 *
 *   Takes a char *key, and returns the option type enum associated with
 *   that option
 *
 * Result:
 *   Returns a Pad_OptionType enum
 *
 * Side effects:
 *   none.
 *
 *----------------------------------------------------------------------
 */
XS(XS__Pad_Surface_Get_option)
{
    dXSARGS;
    if (items != 2) {
        croak ("Usage: Pad::Surface::Get_option(key)");
    }
    Pad_OptionType type;
    char * key;
    Pad_ObjHandle *objectPtr;
    Pad_SurfaceHandle *surfacePtr;
    Perl_ObjectType *obj;
        
            // Get the surfacePtr off the stack
    surfacePtr = Get_surface_from_obj (ST(0));
    if (surfacePtr == NULL){
        Perl_resultString = "Pad::Surface::_Call:";
        Perl_resultString += Pad_errorString;
        croak (Perl_resultString.Get());
        XSRETURN_UNDEF;
    }
            
        // Get the option name off the stack
    key = (char *) SvPV (ST (1),na);
    type = surfacePtr->Get_option_type (key);

        // return the enum
    ST(0) = sv_newmortal();
    sv_setiv(ST(0), type);
    XSRETURN(1);
}

//////////////////////////////////////////////////////////////////////
//
// Pad::Surface::_Create_object() creates a Pad_Object of the specified type
//
//
//////////////////////////////////////////////////////////////////////
XS (XS__Pad_Surface__Create_object)
{
    dXSARGS;
    if (items != 2)
        croak("Usage: pad_ref->Create_object(object_type)");
    {
        Pad_ObjHandle *objectPtr;
        Pad_SurfaceHandle *surfacePtr;
        Perl_ObjectType *obj;
        
            // Get the surfacePtr off the stack
        surfacePtr = Get_surface_from_obj (ST(0));
        if (surfacePtr == NULL){
            Perl_resultString = "Pad::Surface::_Call:";
            Perl_resultString += Pad_errorString;
            croak (Perl_resultString.Get());
            XSRETURN_UNDEF;
        }
            
            // Get the object type off the stack
        Pad_ObjectType obj_type = (Pad_ObjectType) SvIV (ST (1));

            // create an object of the specified type
        objectPtr = new Pad_ObjHandle();
        if (! surfacePtr->Create_obj(obj_type, *objectPtr)) {
            croak ("Pad::_Create_object: Bad object type");
        }

        obj = new Perl_ObjectType;
        obj->surface = surfacePtr;
        obj->object  = objectPtr;
        obj->event   = NULL;
        
            // Put the object reference on the stack as the return value
            // the NULL arg means don't bless the reference into an actual
            // object, as it will be done by the calling function
        ST(0) = sv_newmortal();
        sv_setref_pv(ST(0), (char*) NULL, (void*)obj);
    }
    XSRETURN(1);
}

//////////////////////////////////////////////////////////////////////
//
// Pad::Object::Bind() creates an event binding from command to the
//   object
//
//
//////////////////////////////////////////////////////////////////////
XS (XS__Pad__Object_Bind)
{
    dXSARGS;
    {
        Pad *pad;
        Pad_Win *win;
        Pad_Callback *callback;
        Pad_Bool rc;
        Pad_ObjHandle *objectPtr;
        Perl_ObjectType *obj;
    
            // Get the Perl_ObjectType * off the stack
        obj = Get_object_from_obj (ST(0));
        if (obj == NULL){
            Perl_resultString = "Pad::Surface::Bind:";
            Perl_resultString += Pad_errorString;
            croak (Perl_resultString.Get());
            XSRETURN_UNDEF;
        }
            
            // get the object information from the reference
            // we don't need to check the pointers, because
            // Get_object_from_obj has already done it
        objectPtr = obj->object;

        if (items == 1) {
                // no event given so get all bindings
            rc = objectPtr->Get_all_bindings ();
            if (rc) {
                ST(0) = sv_newmortal ();
                    // Get_all_bindings() sets Pad_resultString
                sv_setpv (ST(0), Pad_resultString.Get());
            } else {
                Pad_errorString = "Pad::Object::Bind: bad binding: ";
                Pad_errorString += Pad_resultString;
                croak (Pad_errorString.Get());
            }
        } else if (items == 2) {
                // no command given so get binding for event
            char *event = SvPV(ST(1), na);
            rc = objectPtr->Get_binding (event);
            if (rc) {
                ST(0) = sv_newmortal ();
                    // Get_binding() sets Pad_resultString
                sv_setpv (ST(0), Pad_resultString.Get());
            } else {
                Pad_errorString = "Pad::Object::Bind: bad binding: ";
                Pad_errorString += Pad_resultString;
                croak (Pad_errorString.Get());
            }
        } else {                // items == 3
                // got both event and command so set the binding
            Perl_CallbackType *call = new Perl_CallbackType;
            char *event = SvPV(ST(1), na);
                                        
                // we need to make a copy of the code, because as soon
                // as we return, Perl will clean up the stack. It also
                // must not be mortal, otherwise it will also get cleaned up

            call->command = newSVsv(ST(2));
            call->obj = newSVsv(ST(0));
            callback = new Pad_Callback (Pad_Perl_callback_event, call);
      
            rc = objectPtr->Bind(event,callback);
            if (!rc) {
                Pad_resultString = "Pad::Object::Bind: couldn't create binding: ";
                Pad_resultString += Pad_errorString;
                warn (Pad_resultString.Get());
            }
            ST(0) = sv_newmortal ();
            sv_setiv (ST(0), rc);
        }
    }
    XSRETURN(1);
}

//////////////////////////////////////////////////////////////////////
//
// Pad::Object::Get_id()
//   returns the object Id
//
//
//////////////////////////////////////////////////////////////////////
XS (XS__Pad__Object_Get_id)
{
  
    dXSARGS;
    if (items != 1)
        croak("Usage: Pad::Object->Get_id()");
    {
        Perl_ObjectType *obj;
        Pad_ObjHandle *objectPtr;
        Pad_SurfaceHandle *surfacePtr;

            // Get the Perl_ObjectType * off the stack
        obj = Get_object_from_obj (ST(0));
        if (obj == NULL){
            Perl_resultString = "Pad::Surface::_Call:";
            Perl_resultString += Pad_errorString;
            croak (Perl_resultString.Get());
            XSRETURN_UNDEF;
        }
            
            // get the object information from the reference
            // we don't need to check the pointers, because
            // Get_object_from_obj has already done it
        surfacePtr = obj->surface;
        objectPtr = obj->object;
        ST(0) = sv_newmortal ();
        sv_setiv (ST(0), objectPtr->Get_id());
    }
    XSRETURN(1);
    
}

//////////////////////////////////////////////////////////////////////
//
// Pad::Object::Get_surface()
//   returns the surface the object is on
//
//
//////////////////////////////////////////////////////////////////////
XS (XS__Pad__Object_Get_surface)
{
  
    dXSARGS;
    if (items != 1)
        croak("Usage: Pad::Object->Get_id()");
    {
        Perl_ObjectType *obj;
        Pad_ObjHandle *objectPtr;
        Pad_SurfaceHandle *surfacePtr;

            // Get the Perl_ObjectType * off the stack
        obj = Get_object_from_obj (ST(0));
        if (obj == NULL){
            Perl_resultString = "Pad::Surface::_Call:";
            Perl_resultString += Pad_errorString;
            croak (Perl_resultString.Get());
            XSRETURN_UNDEF;
        }
            
            // get the object information from the reference
            // we don't need to check the pointers, because
            // Get_object_from_obj has already done it
        surfacePtr = obj->surface;
        objectPtr = obj->object;
        ST(0) = sv_newmortal ();
        sv_setref_pv(ST(0), (char*) "Pad::Surface", (void*)surfacePtr);
    }
    XSRETURN(1);
    
}

/*
 *----------------------------------------------------------------------
 *
 * Pad::Object::_Set_options()
 *
 *   Takes a list of option value pairs and for each option sets it to the
 *   value given.
 *
 * Result:
 *   Returns TRUE or FALSE.
 *
 * Side effects:
 *   Changes the values of the options given.
 *
 *----------------------------------------------------------------------
 */
XS (XS__Pad__Object__Set_options)
{
    dXSARGS;
    if (items < 2)
        croak("Usage: Pad::Object->_Set_coords(optionType, optionValue, ...)");
    {
        Perl_ObjectType *obj;
        Pad_ObjHandle *objectPtr;
        Pad_SurfaceHandle *surfacePtr;
        
            // Get the Perl_ObjectType * off the stack
        obj = Get_object_from_obj (ST(0));
        if (obj == NULL){
            Perl_resultString = "Pad::Surface::_Call:";
            Perl_resultString += Pad_errorString;
            croak (Perl_resultString.Get());
            XSRETURN_UNDEF;
        }
            
            // get the object information from the reference
            // we don't need to check the pointers, because
            // Get_object_from_obj has already done it
        surfacePtr = obj->surface;
        objectPtr = obj->object;
    
            // Get the coords of the stack
        SV *arrayRef = ST(1);
            // Check to make sure that it's really an array refernece
        if ((!SvROK(arrayRef)) ||
            (SvROK (arrayRef) && (!(SvTYPE(SvRV (arrayRef)) == SVt_PVAV)))) {
            croak("Usage: Pad::Object->_Set_options(array_ref)");
        }
        AV *options = (AV *) SvRV (arrayRef);
    
            // make sure that there are pairs of values
            // av_len returns the highest index in the array, so one is
            // added to get the number of elements
        int len = av_len (options) + 1;
        if (len % 2) {
            croak("Usage: Pad::Object->Set_coords(array_ref)");
        }
    
            // make calls to Set_option one pair at a time
        Pad_Bool rc;
        int ret_val = TRUE;
        Pad_OptionType type;
        AV *array;
        int intVal1, intVal2, intVal3;
        Pad_Anchor anchorVal;
        Pad_Bool boolVal;
        char * charVal;
        double doubleVal;
        int intVal;
        double doubleVal1, doubleVal2, doubleVal3;
        for (int i=0;i < len ; i += 2) {
            type = (Pad_OptionType) SvIV(*av_fetch(options,i,0));
            switch (type) {
                case Pad_AnchorOption:
                    anchorVal = (Pad_Anchor) SvIV(*av_fetch(options,i+1,0));
                    rc = objectPtr->Set_option (type,anchorVal);
                    break;
                case Pad_AlwaysRenderOption:
                case Pad_EventsOption:
                case Pad_LockOption:
                    boolVal = (Pad_Bool) SvIV(*av_fetch(options,i+1,0));
                    rc = objectPtr->Set_option (type,boolVal);
                    break;
                case Pad_InfoOption:
                case Pad_LayerOption:
                case Pad_TextOption:
                    charVal = (char*) SvPV(*av_fetch(options,i+1,0),na);
                    rc = objectPtr->Set_option (type,charVal);
                    break;
                case Pad_AngleOption:
                case Pad_FadeRangeOption:
                case Pad_HeightOption:
                case Pad_MaxSizeAbsOption:
                case Pad_MaxSizeRelOption:
                case Pad_MinSizeAbsOption:
                case Pad_MinSizeRelOption:
                case Pad_PenWidthOption:
                case Pad_TransparencyOption:
                case Pad_WidthOption:
                    doubleVal = SvNV(*av_fetch(options,i+1,0));
                    rc = objectPtr->Set_option (type,doubleVal);
                    break;
                    
                case Pad_StickyOption:
                    intVal = SvIV(*av_fetch(options,i+1,0));
                    rc = objectPtr->Set_option (type,intVal);
                    break;
                    
                case Pad_ViewOption:
                case Pad_PositionOption:
                case Pad_RPositionOption:
                        // check that the next arg is an array reference
                        // with three values in the array
                    if (!SvROK (*av_fetch(options,i+1,0))){
                        warn ("Pad::Object::_Set_options: wrong arg type");
                        rc = FALSE;
                        break;
                    }
                    if (!SvTYPE(SvRV(*av_fetch(options,i+1,0))) == SVt_PVAV) {
                        warn ("Pad::Object::_Set_options: wrong arg ref type");
                        rc = FALSE;
                        break;
                    }
                    array = (AV*) SvRV(*av_fetch(options,i+1,0));
                    if (av_len (array) != 2) {
                        warn ("Pad::Object::_Set_options: wrong number of args");
                        rc = FALSE;
                        break;
                    }
                    doubleVal1 = SvNV(*av_fetch(array,0,0));
                    doubleVal2 = SvNV(*av_fetch(array,1,0));
                    doubleVal3 = SvNV(*av_fetch(array,2,0));
                    rc = objectPtr->Set_option (type, doubleVal1, doubleVal2, doubleVal3);
                    break;

                case Pad_PenOption:
                case Pad_FillOption:
                        // check that the next arg is an array reference
                        // with three values in the array
                    if (!SvROK (*av_fetch(options,i+1,0))){
                        warn ("Pad::Object::_Set_options: wrong arg type");
                        rc = FALSE;
                        break;
                    }
                    if (!SvTYPE(SvRV(*av_fetch(options,i+1,0))) == SVt_PVAV) {
                        warn ("Pad::Object::_Set_options: wrong arg ref type");
                        rc = FALSE;
                        break;
                    }
                    array = (AV*) SvRV(*av_fetch(options,i+1,0));
                    if (av_len (array) != 2) {
                        warn ("Pad::Object::_Set_options: wrong number of args");
                        rc = FALSE;
                        break;
                    }
                    intVal1 = SvIV(*av_fetch(array,0,0));
                    intVal2 = SvIV(*av_fetch(array,1,0));
                    intVal3 = SvIV(*av_fetch(array,2,0));
                    rc = objectPtr->Set_option (type, intVal1, intVal2, intVal3);
                    break;

                case Pad_NoiseDataOption:
                case Pad_RenderScriptOption:
                case Pad_TagsOption:
                case Pad_TimerScriptOption:
                case Pad_TimerRateOption:
                case Pad_ViewScriptOption:
                case Pad_ZoomActionOption:
                case Pad_ArrowOption:
                case Pad_ArrowshapeOption:
                case Pad_BBoxOption:
                case Pad_BorderOption:
                case Pad_BorderWidthOption:
                case Pad_CapStyleOption:
                case Pad_CommandOption:
                case Pad_DitherOption:
                case Pad_DivisibleOption:
                case Pad_DoneScriptOption:
                case Pad_EchoCharOption:
                case Pad_EditableOption:
                case Pad_ErrorScriptOption:
                case Pad_FileOption:
                case Pad_FontOption:
                case Pad_FromOption:
                case Pad_HTMLAnchorsOption:
                case Pad_ImageDataOption:
                case Pad_IsMapOption:
                case Pad_JoinStyleOption:
                case Pad_LineSizeOption:
                case Pad_LookOnOption:
                case Pad_MembersOption:
                case Pad_OrientationOption:
                case Pad_PageSizeOption:
                case Pad_ReferenceOption:
                case Pad_ReliefOption:
                case Pad_SizeOption:
                case Pad_StateOption:
                case Pad_TitleOption:
                case Pad_ToOption:
                case Pad_UpdateScriptOption:
                case Pad_URLOption:
                case Pad_ValueOption:
                case Pad_VisibleLayersOption:
                case Pad_WriteFormatOption:
                    Pad_errorString = "Pad::Object::_Set_options: option not yet implemented: ";
                    Pad_errorString += type;
                    croak (Pad_errorString.Get());
                    break;

                default:
                    Pad_errorString = "Pad::Object::_Set_options: Invalid option: ";
                    Pad_errorString += type;
                    croak (Pad_errorString.Get());
                    break;

            }
            if (!rc) {
                ret_val == FALSE;
            }
        }
        ST(0) = sv_newmortal();
        sv_setiv(ST(0), ret_val);
    }
    XSRETURN(1);
}

//////////////////////////////////////////////////////////////////////
//
// Pad::Object::Set_coords() Sets the coordinates for the object
//
//
//////////////////////////////////////////////////////////////////////
XS (XS__Pad__Object_Set_coords)
{
  
    dXSARGS;
    if (items != 2)
        croak("Usage: Pad::Object->Set_coords(array_ref)");
    {
        Pad_PList plist;
        Pad_Point *point;
        float x,y;
        Perl_ObjectType *obj;
        Pad_ObjHandle *objectPtr;
        Pad_SurfaceHandle *surfacePtr;

            // Get the Perl_ObjectType * off the stack
        obj = Get_object_from_obj (ST(0));
        if (obj == NULL){
            Perl_resultString = "Pad::Surface::_Call:";
            Perl_resultString += Pad_errorString;
            croak (Perl_resultString.Get());
            XSRETURN_UNDEF;
        }
            
            // get the object information from the reference
            // we don't need to check the pointers, because
            // Get_object_from_obj has already done it
        surfacePtr = obj->surface;
        objectPtr = obj->object;
    
            // Get the coords of the stack
        SV *arrayRef = ST(1);
            // Check to make sure that it's really an array refernece
        if ((!SvROK(arrayRef)) ||
            (SvROK (arrayRef) && (!(SvTYPE(SvRV (arrayRef)) == SVt_PVAV)))) {
            croak("Usage: Pad::Object->Set_coords(array_ref)");
        }
        AV *coords = (AV *) SvRV (arrayRef);
    
            // make sure that there are the right number of points
            // av_len returns the highest index in the array, so one is
            // added to get the number of elements
        int len = av_len (coords) + 1;
        if (len % 2) {
            croak("Usage: Pad::Object->Set_coords(array_ref)");
        }
    
            // make the PList one point at a time
        for (int i=0;i < len ; i += 2) {
            x = SvNV(*av_fetch(coords,i,0));
            y = SvNV(*av_fetch(coords,i+1,0));
            point = new Pad_Point();
            point->Set(x,y);
            plist.Push_last (point);
        }
    
        ST(0) = sv_newmortal();
        sv_setiv(ST(0), objectPtr->Set_coords (plist));
    }
    XSRETURN(1);
}

// XS(XS__Pad_Option__FETCH)
// {
//     dXSARGS;
//     if (items != 2) {
//         croak ("Usage: Pad_Option::FETCH(key)");
//     }
//     Pad_OptionType type;

//         //
//     XSRETURN(1);
// }

//////////////////////////////////////////////////////////////////////
//
// xs_init() is used during initialization of the Perl interpereter.
//   it declares the bindings between perl subroutines and the C++
//   implemenations 
//
//
//////////////////////////////////////////////////////////////////////
extern "C" void boot_DynaLoader _((CV* cv));

static void
xs_init()
{
    char* file = __FILE__;

    newXS("Pad::Object::Bind",           XS__Pad__Object_Bind,           file);
    newXS("Pad::Object::Get_id",         XS__Pad__Object_Get_id,         file);
    newXS("Pad::Object::Get_surface",    XS__Pad__Object_Get_surface,    file);
    newXS("Pad::Object::Set_coords",     XS__Pad__Object_Set_coords,     file);
    newXS("Pad::Object::_Set_options",   XS__Pad__Object__Set_options,   file);
    newXS("Pad::Surface::Pack",          XS__Pad_Surface_Pack,           file);
    newXS("Pad::Surface::_Create_object",XS__Pad_Surface__Create_object, file);
    newXS("Pad::Surface::new",           XS__Pad_Surface_new,            file);
    newXS("Pad::Surface::_Call",         XS__Pad_Surface__Call,          file);
    newXS("Pad::Surface::Get_option",    XS__Pad_Surface_Get_option,     file);
    newXS("Pad::test",                   XS__Pad_test,                   file);
    newXS("Pad::Get_surfaces",           XS__Pad_Get_surfaces,           file);
    newXS("Set_top_level",               XS_SetTopLevel,                 file);
//    newXS("Pad_Option::FETCH",           XS__Pad_Option__FETCH,          file);
    newXS("DynaLoader::boot_DynaLoader", boot_DynaLoader,                file);
}

