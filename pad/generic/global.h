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

#ifndef GLOBAL_H
#define GLOBAL_H 1

#include "defs.h"
#include "misc.h"

class Pad_RenderContext;
class Pad_Renderer;
class Pad_Language;
class Pad_Object;
class Pad_TkWin;
class Pad_String;
class Pad_List;
class Pad_HashTable;

extern Pad_Uid        Pad_allUid;	 // Universal id for "all"
extern Pad_Uid        Pad_noneUid;	 // Universal id for "none"
extern Pad_Uid        Pad_currentUid;	 // Universal id for "current"
extern Pad_Uid        Pad_aliasUid;	 // Universal id for "alias" type
extern Pad_Uid        Pad_buttonUid;	 // Universal id for "button" type
extern Pad_Uid        Pad_checkboxUid;	 // Universal id for "checkbox" type
extern Pad_Uid        Pad_menuitemUid;	 // Universal id for "menuitem" type
extern Pad_Uid        Pad_checkboxmenuitemUid;	 // Universal id for "checkboxmenuitem" type
extern Pad_Uid        Pad_menuUid;	 // Universal id for "menu" type
extern Pad_Uid        Pad_menubarUid;	 // Universal id for "menubar" type
extern Pad_Uid        Pad_choicemenuUid;	 // Universal id for "choicemenu" type
extern Pad_Uid        Pad_canvasUid;	 // Universal id for "canvas" type
extern Pad_Uid        Pad_containerUid;	 // Universal id for "container" type
extern Pad_Uid        Pad_frameUid;	 // Universal id for "frame" type
extern Pad_Uid        Pad_gridUid;	 // Universal id for "grid" type
extern Pad_Uid        Pad_groupUid;	 // Universal id for "group" type
extern Pad_Uid        Pad_htmlUid;	 // Universal id for "html" type
extern Pad_Uid        Pad_htmlAnchorUid;	 // Universal id for "htmlanchor" type
extern Pad_Uid        Pad_imageUid;	 // Universal id for "image" type
extern Pad_Uid        Pad_kplUid;	 // Universal id for "kpl" type
extern Pad_Uid        Pad_labelUid;	 // Universal id for "label" type
extern Pad_Uid        Pad_lineUid;	 // Universal id for "line" type
extern Pad_Uid        Pad_splineUid;	 // Universal id for "spline" type
extern Pad_Uid        Pad_padUid;	 // Universal id for "pad" type
extern Pad_Uid        Pad_panelUid;	 // Universal id for "panel" type
extern Pad_Uid        Pad_ovalUid;	 // Universal id for "oval" type
extern Pad_Uid        Pad_polygonUid;	 // Universal id for "polygon" type
extern Pad_Uid        Pad_portalUid;	 // Universal id for "portal" type
extern Pad_Uid        Pad_rectangleUid;	 // Universal id for "rectangle" type
extern Pad_Uid        Pad_scrollbarUid;	 // Universal id for "scrollbar" type
extern Pad_Uid        Pad_tclUid;	 // Universal id for "tcl" type
extern Pad_Uid        Pad_textUid;	 // Universal id for "text" type
extern Pad_Uid        Pad_textfileUid;	 // Universal id for "textfile" type
extern Pad_Uid        Pad_textfieldUid;	 // Universal id for "textfield" type
extern Pad_Uid        Pad_textareaUid;	 // Universal id for "textarea" type
extern Pad_Uid        Pad_windowUid;	 // Universal id for "window" type

extern Pad_RenderContext *Pad_prc;       // Current render context
extern Pad_Renderer *Pad_renderer;       // Current renderer
extern char         *Pad_version;        // Version string
extern int           Pad_sharedMemory;   // True if shared memory should be tried
extern int           Pad_rgb;            // True if images should stored RGB information
extern Pad_Language *Pad_callbackLanguage; // Language new scripts should be interpreted as
extern Pad_Language *Pad_topLevelLanguage; // Current top-level language interpreter
extern Pad_HashTable Pad_languages;      // One Pad_Language for each supported language
extern Pad_List      Pad_events;         // List of active events, most recent first
extern Pad_HashTable Pad_imageTokenTable;// Hash table for Pad_ImageDatas from image tokens
extern Pad_HashTable Pad_imageNameTable; // Hash table for Pad_ImageDatas from image names
extern Pad_HashTable Pad_resourceWriteTable; // Hash table for writing resources (e.g. Pad_ImageDatas)

extern Pad_Bool      Pad_eventCheckerResult; // Used to check for interrupting X events
extern char          Pad_result[PAD_RESULT_SIZE];  // Space for pad result to return to Tcl.
extern Pad_List      Pad_coordFrames;    // List of coordinate frames for hierarchical coords
extern Pad_List      Pad_renderers;      // List of all renderers
extern Pad_Object   *Pad_focus;	         // Current key focus
extern Pad_Object   *Pad_selection;      // Current item that has selection
extern Pad_String    Pad_errorString;    // Contains descriptive string of current error
extern Pad_String    Pad_resultString;    // Contains descriptive string of current result
                                         // It is cleared before each pad-tcl
                                         // command is executed.  Most substrate
                                         // methods should append to it.

#endif


