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

#include "init.h"
#include "line-font.h"
#include "global.h"
#include "api.h"

class Pad_Animatable;

void 
Pad_MasterInit(void)
{
    static Pad_Bool init = FALSE;

    if (init == TRUE) {
	return;
    }
    init = TRUE;

    Pad_allUid =       Pad_GetUid("all");
    Pad_noneUid =      Pad_GetUid("none");
    Pad_currentUid =   Pad_GetUid("current");

    Pad_aliasUid =     Pad_GetUid("alias");
    Pad_buttonUid =    Pad_GetUid("button");
    Pad_checkboxUid =  Pad_GetUid("checkbox");
    Pad_menuitemUid =  Pad_GetUid("menuitem");
    Pad_checkboxmenuitemUid =  Pad_GetUid("checkboxmenuitem");
    Pad_menuUid =      Pad_GetUid("menu");
    Pad_menubarUid =   Pad_GetUid("menubar");
    Pad_choicemenuUid =Pad_GetUid("choicemenu");
    Pad_canvasUid =    Pad_GetUid("canvas");
    Pad_containerUid = Pad_GetUid("container");
    Pad_frameUid =     Pad_GetUid("frame");
    Pad_gridUid =      Pad_GetUid("grid");
    Pad_groupUid =     Pad_GetUid("group");
    Pad_htmlUid =      Pad_GetUid("html");
    Pad_htmlAnchorUid =Pad_GetUid("htmlanchor");
    Pad_imageUid =     Pad_GetUid("image");
    Pad_kplUid =       Pad_GetUid("kpl");
    Pad_labelUid =     Pad_GetUid("label");
    Pad_lineUid =      Pad_GetUid("line");
    Pad_splineUid =    Pad_GetUid("spline");
    Pad_padUid =       Pad_GetUid("pad");
    Pad_panelUid =     Pad_GetUid("panel");
    Pad_ovalUid =      Pad_GetUid("oval");
    Pad_polygonUid =   Pad_GetUid("polygon");
    Pad_portalUid =    Pad_GetUid("portal");
    Pad_rectangleUid = Pad_GetUid("rectangle");
    Pad_scrollbarUid = Pad_GetUid("scrollbar");
    Pad_tclUid =       Pad_GetUid("tcl");
    Pad_textUid =      Pad_GetUid("text");
    Pad_textfileUid =  Pad_GetUid("textfile");
    Pad_textfieldUid = Pad_GetUid("textfield");
    Pad_textareaUid =  Pad_GetUid("textarea");
    Pad_windowUid =    Pad_GetUid("window");

    Pad_LineFont::InitLineFonts();
}




