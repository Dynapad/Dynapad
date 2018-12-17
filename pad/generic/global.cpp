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

#include "global.h"
#include "hashtab.h"
#include "pad-string.h"

//
// Various Pad_Uid's used by this module (set up during initialization):
//
Pad_Uid Pad_allUid =     NULL;
Pad_Uid Pad_noneUid =    NULL;
Pad_Uid Pad_currentUid = NULL;

//
// Pad_Uid's for each built-in type
//
Pad_Uid Pad_aliasUid =      NULL;
Pad_Uid Pad_buttonUid =     NULL;
Pad_Uid Pad_checkboxUid =   NULL;
Pad_Uid Pad_menuitemUid =   NULL;
Pad_Uid Pad_checkboxmenuitemUid =   NULL;
Pad_Uid Pad_menuUid =       NULL;
Pad_Uid Pad_menubarUid =    NULL;
Pad_Uid Pad_choicemenuUid = NULL;
Pad_Uid Pad_canvasUid =     NULL;
Pad_Uid Pad_containerUid =  NULL;
Pad_Uid Pad_frameUid =      NULL;
Pad_Uid Pad_gridUid =       NULL;
Pad_Uid Pad_groupUid =      NULL;
Pad_Uid Pad_htmlUid =       NULL;
Pad_Uid Pad_htmlAnchorUid = NULL;
Pad_Uid Pad_imageUid =      NULL;
Pad_Uid Pad_kplUid =        NULL;
Pad_Uid Pad_labelUid =      NULL;
Pad_Uid Pad_lineUid =       NULL;
Pad_Uid Pad_splineUid =     NULL;
Pad_Uid Pad_padUid =        NULL;
Pad_Uid Pad_panelUid =      NULL;
Pad_Uid Pad_ovalUid =       NULL;
Pad_Uid Pad_polygonUid =    NULL;
Pad_Uid Pad_portalUid =     NULL;
Pad_Uid Pad_rectangleUid =  NULL;
Pad_Uid Pad_scrollbarUid =  NULL;
Pad_Uid Pad_tclUid =        NULL;
Pad_Uid Pad_textUid =       NULL;
Pad_Uid Pad_textfileUid =   NULL;
Pad_Uid Pad_textfieldUid =  NULL;
Pad_Uid Pad_textareaUid =   NULL;
Pad_Uid Pad_windowUid =     NULL;

//
// Version string
//
char *Pad_version = PAD_VERSION;

//
// Pad_tk flag indicating if we are running under Tk or Java+X
//
Pad_Bool Pad_tk = TRUE;

//
// Hash tables to deal with Pad_ImageDatas.  
// Pad_imageTokenTable hashes from image tokens to Pad_ImageDatas.
// Pad_imageNameTable hashes from image file names to Pad_ImageDatas.
//
Pad_HashTable Pad_imageTokenTable;
Pad_HashTable Pad_imageNameTable;

//
// Hash table to deal with writing of resources such as Pad_ImageDatas.
// Pad_resourceWriteTable hashes from resource names to a flag indicating 
// if the resource has already been written.
//
Pad_HashTable Pad_resourceWriteTable;

//
// Global used to return if rendering should be interrupted because of
// waiting X event.
//
Pad_Bool Pad_eventCheckerResult;

//
// Global list of active events, most recent first.
// There can be more than one if an event generates enter or leave events.
//
Pad_List Pad_events;

//
// Global buffer for creating strings to return to Tcl.
//
char Pad_result[PAD_RESULT_SIZE];

//
// Global pointer to the single Pad_Display in the system.
// Used to force multiple pads to use the same display.
//

//
// Pad Render Context (set for active render)
//
Pad_RenderContext *Pad_prc = NULL;

//
// Pad Renderer (current output device)
// List of all renderers
//
Pad_Renderer *Pad_renderer = NULL;
Pad_List Pad_renderers;

//
// Current keyboard focus
//
Pad_Object *Pad_focus = NULL;

//
// Current item that has selection
//
Pad_Object *Pad_selection = NULL;

//
// Language new callback scripts should be interpreted as.
//
Pad_Language *Pad_callbackLanguage = NULL;

//
// Language top-level interpreter should run
//
Pad_Language *Pad_topLevelLanguage = NULL;

//
// Hash table for supported scripting languages.
// One Pad_Language per entry.
//
Pad_HashTable Pad_languages;

//
// List of coordinate frames used for hierarchical coordinates.
// If no coordinate frames (bounding boxes) are on the stack, then
// the regular coordinates are used.  If  there any, then all Pad++
// coordinates (in and out) are specified within the range (0.0, 1.0)
// that apply to this bounding box.
//
Pad_List Pad_coordFrames;	       // List of coordinate frames for hierarchical coords

//
// Contains descriptive string of current error
// (if there is one).
//
Pad_String Pad_errorString;

//
// Contains descriptive string of current result
// (if there is one).
//
Pad_String Pad_resultString;

//
// True if shared memory should be accessed
//

#ifdef CYGWIN
int Pad_sharedMemory = FALSE;
#else
int Pad_sharedMemory = TRUE;
#endif

//
//
// True if images should store RGB information
//
int Pad_rgb = TRUE;

