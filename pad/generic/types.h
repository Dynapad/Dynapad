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

#ifndef TYPES_H
#define TYPES_H 1

#define PAD_TYPE(N) \
  virtual int Type(void) { return (_type); } \
  virtual char *Type_name(void) { return (char *)(N); }

//////////////////////////////////////////////
// Pad Classes
//////////////////////////////////////////////

// IMPORTANT NOTE:
// DO NOTE CHANGE OR REUSE EXISTING TYPE ID NUMBERS!  THIS IS REQUIREED
// IN ORDER TO ENSURE BACKWARD COMPATIBILITY WITH FILE FORMAT.

#define PAD_USER_TYPE        0
#define PAD_OBJECT         100
#define PAD_PAD            101
#define PAD_VIEW           102
#define PAD_LINE           103
#define PAD_RECTANGLE      104
#define PAD_POLYGON        105
#define PAD_TEXT           106
#define PAD_TEXTFILE       107
#define PAD_IMAGE          108
#define PAD_PORTAL         109
#define PAD_GROUP          110
#define PAD_ALIAS          111
#define PAD_SPLINE         112
#define PAD_GRID           113
#define PAD_OVAL           114

#define PAD_HTML           201
#define PAD_HTML_ANCHOR    202
#define PAD_HTML_ANCHOR_IMAGE 203
#define PAD_HTML_ANCHOR_TEXT  204
#define PAD_TCL            205
#define PAD_KPL            206

#define PAD_BUTTON         300
#define PAD_CANVAS         301
#define PAD_COMPONENT      302
#define PAD_CONTAINER      303
#define PAD_FRAME          304
#define PAD_LABEL          305
#define PAD_PANEL          306
#define PAD_SCROLLBAR      307
#define PAD_TEXTFIELD      308
#define PAD_TEXTAREA       309
#define PAD_TEXTAREACOMP   310
#define PAD_WINDOW         311
#define PAD_CHECKBOX       312
#define PAD_MENUITEM       313
#define PAD_MENU           314
#define PAD_MENUBAR        315
#define PAD_CHOICEMENU     316
#define PAD_CHECKBOXMENUITEM 317

//
// special symbols and values used for binary reading and writing
//
#define PAD_OBJ_SCRIPT     3000
#define PAD_END_INPUT      3001
#define PAD_ENDOBJ_OPTIONS 4000
#define PAD_ENDOBJ_OPTION  4001

#endif
