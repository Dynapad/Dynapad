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

#include "defs.h"
#include "pad-string.h"
#include "list.h"
#include "pad.h"
#include "text.h"
#include "point.h"
#include "view.h"
#include "restorer.h"
#include "renderer.h"
#include "bind.h"
#include "events.h"
#include "win.h"
#include "global.h"
#include "callback.h"

#include <math.h>
#include <iostream>
using namespace std;
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <fstream>


#include <X11/X.h>
#include <X11/keysym.h>


#define TEXT_DEFAULT_FILE  ""
#define TEXT_DEFAULT_PEN   "black"
#define TEXT_DEFAULT_TEXT  ""
#define TEXT_DEFAULT_WRITEFORMAT   PAD_WRITE_REFERENCE
#define TEXT_DEFAULT_FONT  "Times-12"                  // Default font for text items
#define TEXT_DEFAULT_EDITABLE      0                   // Regular text not editable by default
#define TEXT_DEFAULT_HILITEWIDTH   0

#define TEXTFIELD_DEFAULT_FILL        "white"          // Default background color
#define TEXTFIELD_BORDER_WIDTH        3                // Border width of textfield
#define TEXTFIELD_OFFSET              2                // Offset of text from border
#define TEXTFIELD_DEFAULT_WIDTH       200              // Default width of textfield
#define TEXTFIELD_DEFAULT_EDITABLE    1                // Textfield editable by default
#define TEXTFIELD_DEFAULT_HILITEWIDTH 1

#define TEXTAREA_DEFAULT_FILL         "white"           // Default background color
#define TEXTAREA_BORDER_WIDTH         3                // Border width of textarea
#define TEXTAREA_OFFSET               2                // Offset of text from border
#define TEXTAREA_DEFAULT_WIDTH        400              // Default width of textara
#define TEXTAREA_DEFAULT_ROWS         4                // Default height of textarea
#define TEXTAREA_DEFAULT_EDITABLE     1                // Textarea editable by default
#define TEXTAREA_SCROLLBAR_THICKNESS  15
#define TEXTAREA_DEFAULT_HILITEWIDTH  1

static Pad_String origFile;  // used by Write_obj_pre() and Write_obj_post()
                             // when writing Tcl script for textfiles
                             // with "copy" writeformat.

///////////////////////////////////////////////////////////////////////////////////
//
// Pad_TextIndex Class
//
//   This describes a location within a text object.
//   The location is stored with line and char # within the line,
//   but may also be accessed by char # from start of text object.
//
///////////////////////////////////////////////////////////////////////////////////

Pad_TextIndex::Pad_TextIndex(Pad_Text *newText) 
{
    line = 0;
    chr = 0;
    text = newText;
}

Pad_TextIndex::Pad_TextIndex(Pad_Text &newText) 
{
    line = 0;
    chr = 0;
    text = &newText;
}

Pad_TextIndex::Pad_TextIndex(Pad_TextArea *newTextArea) 
{
    line = 0;
    chr = 0;
    text = &newTextArea->_textArea;
}

Pad_TextIndex::Pad_TextIndex(Pad_TextIndex &index)
{
    *this = index;
}

//
// Return index as a single int which specifies the char #
// from the start of the text object.
//
int
Pad_TextIndex::Get(void)
{
    int i;
    int intIndex = 0;
    
    DOTIMES(i, line) {
	intIndex += text->text[i].Length();
    }
    intIndex += chr;

    return(intIndex);
}

//
// Output an index to a stream
//
ostream & 
operator << (ostream& os, Pad_TextIndex &index) {
    os << "(" << index.line << ", " << index.chr << ")";

    return os;
}

//
// Assignment operators
//
Pad_TextIndex&
Pad_TextIndex::operator=(int intIndex) 
{
    chr = intIndex;
    line = 0;
    text->Round_index(this);
    return(*this);
}

Pad_TextIndex&
Pad_TextIndex::operator=(Pad_TextIndex *index) 
{
    *this = *index;
    return(*this);
}

Pad_TextIndex&
Pad_TextIndex::operator=(Pad_TextIndex &index) 
{
    line = index.line;
    chr = index.chr;
    name = index.name;
    text = index.text;
    return(*this);
}

//
// Equality operators
//
Pad_Bool
Pad_TextIndex::operator<(Pad_TextIndex &index) 
{
    if ((line < index.line) || 
	((line == index.line) && (chr < index.chr))) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

Pad_Bool
Pad_TextIndex::operator<=(Pad_TextIndex &index) 
{
    if ((line < index.line) || 
	((line == index.line) && (chr <= index.chr))) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

Pad_Bool
Pad_TextIndex::operator>(Pad_TextIndex &index) 
{
    if ((line > index.line) || 
	((line == index.line) && (chr > index.chr))) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

Pad_Bool
Pad_TextIndex::operator>=(Pad_TextIndex &index) 
{
    if ((line > index.line) || 
	((line == index.line) && (chr >= index.chr))) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

Pad_Bool
Pad_TextIndex::operator==(Pad_TextIndex &index) 
{
    if ((line == index.line) &&
	(chr == index.chr) &&
	(text == index.text) &&
	(name == index.name)) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

Pad_Bool
Pad_TextIndex::operator!=(Pad_TextIndex &index) 
{
    return(!(*this==index));
}

///////////////////////////////////////////////////////////////////////////////////
//
// Pad_Text Class
//
///////////////////////////////////////////////////////////////////////////////////

Pad_Text::~Pad_Text(void)
{
    Pad_TextIndex *index;
    Pad_HashTableIterator hi;
    
    Generate_delete();

    if (text) {
	delete [] text;
	text = NULL;
    }
    if (marks) {
	DOTABLE(hi, *marks, Pad_TextIndex, index) {
	    if (index != &point) {
		delete index;
	    }
	}
	delete marks;
	marks = NULL;
    }
}

Pad_Text::Pad_Text(Pad *pad) :
Pad_Component(pad), point(this), selStart(this), selEnd(this)
{
    Init();
}

//
// Create a new text object not getting more than 'width'
// standard character widths wide, and with the first line starting at
// an index of char_pos pixels.
//
Pad_Text::Pad_Text(Pad *pad, int len, char *b, float width, float char_pos) :
Pad_Component(pad), point(this), selStart(this), selEnd(this)
{
    int i;
    char *token;
    char *buf;
    float token_width;
    Pad_String *string_ptr;
    Pad_List new_text;
    Pad_BBox line_bbox;
        
    Init();

    if (text) 
      {
	  delete [] text;
	  text = NULL;
      }
    lines = 0;

    buf = new char[len + 1];
    strncpy(buf, b, len);
    buf[len] = 0;

    string_ptr = new Pad_String;
    Pad_renderer->Set_font(_font);
    if (char_pos > 0.0) 
      {
	  DOTIMES(i, (int)(char_pos / Pad_renderer->Get_char_width(' ')))
	    {
		string_ptr->Append(" ");
	    }
      }
    
    token = strtok(buf, ", \t\n\r");
    while (token) 
      {
	  Pad_renderer->Get_string_bbox(token, &line_bbox);
	  token_width = line_bbox.Width();
	  if ((char_pos == 0.0) || ((char_pos + token_width < width)))
	    {
		if (string_ptr->Length() > 0) 
		  {
		      string_ptr->Append(" ");
		      char_pos += Pad_renderer->Get_char_width(' ');
		  }
		string_ptr->Append(token);
		Pad_renderer->Get_string_bbox(token, &line_bbox);
		token_width = line_bbox.Width();
		char_pos += token_width;
		token = strtok(NULL, ", \t\n\r");
	    }
	  else 
	    {
		new_text.Push_last(string_ptr);
		string_ptr = new Pad_String;
		char_pos = 0.0;
		lines++;
	    }
      }
    if (string_ptr->Length() > 0)
      {
	  new_text.Push_last(string_ptr);
	  lines++;
      }
    else 
      {
	  delete string_ptr;
      }
    
    delete [] buf;
    
    if (text) {
	delete [] text;
    }
    text = new Pad_String[lines];
    
    DOTIMES(i, lines) 
      {
	  string_ptr = (Pad_String *)new_text.Pop();
	  text[i].Set(string_ptr);
	  delete string_ptr;
      }
}

void 
Pad_Text::Init(void)
{  
				// Initialize default event handlers for text
    //_Initialize_events(pad->view->win);

    _type = PAD_TEXT;
    textFlags = PAD_NO_MASK;
    lines = 1;
    text = new Pad_String[1];
    point.chr = 0;
    point.line = 0;
    point.name = "point";
    maxSize = -1;  /* not 10000 */
    marks = new Pad_HashTable(PAD_STRING_TABLE);
    marks->Set((void *)"point", &point);
    marks->Set((void *)"insert", &point);
    Set_font_default();
    Set_pen_default();
    _xoffset = 0.0;
    _yoffset = 0.0;
    Set_editable_default();
    echoChar = '\0';
    _hiliteWidth = TEXT_DEFAULT_HILITEWIDTH;
    refineNeeded = FALSE;
}

//
// Returns true if object derived from Pad_Text
//
Pad_Bool 
Pad_Text::Is_text(void)
{
    return(TRUE);
}

//
// Returns text object associated with this object
//
Pad_Text *
Pad_Text::Get_text_obj(void)
{
    return(this);
}

//////////////////////////////////////////////
//              Utilities
//////////////////////////////////////////////  

//
// Use a different default font size than components
//
void
Pad_Text::Set_font_default(void)
{
    _font.Set(TEXT_DEFAULT_FONT);
    _componentFlags &= ~COMPONENT_FONT_SET;
}

//
// Setters and getters for text
//
//
// Create a multi-line text object from a string, 
// with lines separated by '\n's.
//
Pad_Bool
Pad_Text::Set_text(char *buf)
{
    int i;
    int new_line_ctr;
    char *token, *ptr;
    Pad_String *string_ptr;
    Pad_String buf_copy;
    Pad_List new_text;

    textFlags |= TEXT_TEXT_SET;

    if (text) {
	delete [] text;
	text = NULL;
    }

    lines = 0;
    new_line_ctr = 0;
    buf_copy.Set(buf);
    token = buf_copy.Get();
    ptr = token; 
    while (*ptr && (*ptr != '\n')) {
	ptr++;
    }
    if (*ptr) {
	new_line_ctr++;
	*ptr++ = 0;
    }
    while (*ptr || *token)
      {
	  string_ptr = new Pad_String(token);
	  new_text.Push_last(string_ptr);
	  lines++;
	  token = ptr;
	  while (*ptr && (*ptr != '\n')) {
	      ptr++;
	  }
	  if (*ptr) {
	      new_line_ctr++;
	      *ptr++ = 0;
	  }
      }
    if (new_line_ctr == lines) {
	lines++;
	string_ptr = new Pad_String("");
	new_text.Push_last(string_ptr);
    }
    
    if (lines == 0) {
	text = new Pad_String[1];
	lines = 1;
    } else {
	text = new Pad_String[lines];

	DOTIMES(i, lines) 
	  {
	      string_ptr = (Pad_String *)new_text.Pop();
	      text[i].Set(string_ptr);
	      delete string_ptr;
	  }
    }

    point.line = 0;
    point.chr = 0;

    Round_index(&selStart);
    Round_index(&selEnd);
    Update();

    return(TRUE);
}

Pad_Bool
Pad_Text::Set_text_default(void)
{
    Set_text(TEXT_DEFAULT_TEXT);
    textFlags &= ~TEXT_TEXT_SET;
    
    return(TRUE);
}

Pad_Bool
Pad_Text::Get_text(Pad_String &result)
{
    int i;

    result = "";
    for (i=0; i<lines; i++) {
	result += text[i];
	if (i != (lines - 1)) {
	    result += "\n";
	}
    }
    return(TRUE);
}

//
// Setter and getter for editable
//
void 
Pad_Text::Set_editable(Pad_Bool editable)
{
    if (editable) {
	textFlags |= TEXT_EDITABLE;
    } else {
	textFlags &= ~TEXT_EDITABLE;
    }
    textFlags |= TEXT_EDITABLE_SET;
}

void 
Pad_Text::Set_editable_default(void)
{
    Set_editable(TEXT_DEFAULT_EDITABLE);
    textFlags &= ~TEXT_EDITABLE_SET;
}

Pad_Bool
Pad_Text::Get_editable(void)
{
    if ((textFlags & TEXT_EDITABLE) && (echoChar == '\0')) {
	return(TRUE);
    } else {
	return(FALSE);
    }
}

//
// Gets called when text pen is changed to 
// change the cursor color appropriately.
//
void 
Pad_Text::Pen_changed(void)
{
    Pad_Component::Pen_changed();

    if (_penColor.Is_set()) {
	int red, green, blue;
	_penColor.Get(red, green, blue);
	cursorColor.Set_contrasting(red, green, blue);
	if (cursorColor.Equals(_fillColor)) {
	    int fgred, fggreen, fgblue;
	    int bgred, bggreen, bgblue;
	    _penColor.Get(fgred, fggreen, fgblue);
	    _fillColor.Get(bgred, bggreen, bgblue);
	    cursorColor.Set((int)(LERP(0.2, bgred, fgred)),
			    (int)(LERP(0.2, bggreen, fggreen)),
			    (int)(LERP(0.2, bgblue, fgblue)));
	}
    }
}

//
// Set width of text item.  Redefine because Pad_Component
// sets the width using local bbox which is no good for text.
//
Pad_Bool
Pad_Text::Set_width(float width)
{
    Pad_Object::Set_width(width);

    return(TRUE);
}

//
// Set height of text item.  Redefine because Pad_Component
// sets the height using local bbox which is no good for text.
//
Pad_Bool
Pad_Text::Set_height(float height)
{
    Pad_Object::Set_height(height);

    return(TRUE);
}

//
// Make this object the focus object.
// Nothing special for most objects, but this may be overrided by sub-classes.
//
void
Pad_Text::Set_focus(void)
{
    Update();
}

//
// This object no longer the focus object.
// If the text item was empty, delete it.
//
void
Pad_Text::Unset_focus(void)
{
    Damage();
    if (!text || ((lines == 1) && (strlen(text[0].Get()) == 0))) {
	delete this;
    } else {
	Pad_focus = NULL;
	Update();
    }
}

//
// Rotate sets the angle at which the 
// text is to be renedered, by setting
// the value of theta and calling
// Update()
//
Pad_Bool
Pad_Text::Is_rotatable(void)
{
    return(TRUE);
}

Pad_Bool 
Pad_Text::Rotate(float dtheta)
{
    float curAngle;
    optionFlags |= PAD_ANGLE_SET; // Set bit specifying that angle has been set

    curAngle = Get_angle();
    _Set_angle_data(curAngle + dtheta);
    Update();
    return(TRUE);
}

Pad_Bool 
Pad_Text::Rotate(float dtheta, Pad_Point &center)
{
    float curAngle;
    optionFlags |= PAD_ANGLE_SET; // Set bit specifying that angle has been set

				// Move center point to same coord system as anchorpt.
				// This is local * transform.
    Pad_Point centerCopy = center;
    Screen_to_local(centerCopy);
    transform.Apply(centerCopy);
    anchorpt.Rotate(dtheta, centerCopy); // Causes the text to rotate about a point other than the anchor of the text

    curAngle = Get_angle();
    _Set_angle_data(curAngle + dtheta);
    Update();

    return(TRUE);
}

//
// Return current selection
//
void
Pad_Text::Get_selection(Pad_String &text)
{
    if (selStart == selEnd) {
	text = "";
    } else {
	if (selStart <= selEnd) {
	    Get_chars(text, selStart, selEnd);
	} else {
	    Get_chars(text, selEnd, selStart);
	}
    }
}

//
// Return position of last character in selection
//
int
Pad_Text::Get_selection_end(void)
{
    if (selStart <= selEnd) {
	return(selEnd.Get());
    } else {
	return(selStart.Get());
    }
}

//
// Return position of first character in selection
//
int
Pad_Text::Get_selection_start(void)
{
    if (selStart <= selEnd) {
	return(selStart.Get());
    } else {
	return(selEnd.Get());
    }
}

//
// Set selection (start=end means no selection).
// Note that start does not necessarily come before end.
//
void
Pad_Text::Select(int newSelStart, int newSelEnd)
{
    Pad_TextIndex s1(this), s2(this);

    s1 = newSelStart;
    s2 = newSelEnd;
    Select(s1, s2);
}

void
Pad_Text::Select(Pad_TextIndex &newSelStart, Pad_TextIndex &newSelEnd)
{
    if (Pad_selection && (Pad_selection != this)) {

	if (Pad_selection->Is_text()) {
	    ((Pad_Text *)Pad_selection)->Select(0, 0);
	}
    }
    Pad_selection = this;
				// Assign selection.  But be careful because this
				// could be called with the indices we are assigning to
    if (&selStart == &newSelEnd) {
	Pad_TextIndex tmp = selStart;
	selStart = newSelStart;
	selEnd = tmp;
    } else {
	selStart = newSelStart;
	selEnd = newSelEnd;
    }

    Damage();
}

//
// Char to display instead of actual text
//
void
Pad_Text::Set_echo_char(char c)
{
    echoChar = c;
    Damage();
}
    

//////////////////////////////////////////////
//              render definitions
//////////////////////////////////////////////  

float
Pad_Text::Compute_dimensions(float &globalWidth, float &globalHeight)
{
    Pad_Object::Compute_dimensions(globalWidth, globalHeight);
    _Compute_dimensions(globalWidth, globalHeight);

    return(pixelDim);
}

float
Pad_Text::Compute_dimensions(Pad_Event *event, float &globalWidth, float &globalHeight)
{
    Pad_Object::Compute_dimensions(event, globalWidth, globalHeight);
    _Compute_dimensions(globalWidth, globalHeight);

    return(pixelDim);
}

float
Pad_Text::Compute_dimensions(Pad_List &views, float &globalWidth, float &globalHeight)
{
    Pad_Object::Compute_dimensions(views, globalWidth, globalHeight);
    _Compute_dimensions(globalWidth, globalHeight);

    return(pixelDim);
}

void
Pad_Text::_Compute_dimensions(float &, float &globalHeight)
{
    float localLineHeight;
    float localHeight;
    Pad_BBox *bb;
    
    if (lines == 0) {		// Shouldn't ever get here, but it does sometimes
	level = 0;
	return;
    }

    bb = Get_bbox();
    localHeight = bb->Height();
    Pad_renderer->Set_font(_font);
    localLineHeight = Pad_renderer->Get_font_height();
    renderedFontSize = localLineHeight * globalHeight / localHeight;
	  
    if (renderedFontSize <= TEXT_LOD0_SIZE)
      level = 0;
    else if (renderedFontSize <= TEXT_LOD1_SIZE+2)
      level = 1;
    else
      level = 2;
				// Skip intermediate refinement levels
    if (pad->level > 0) {
	level = TEXT_LEVELS;
    }
}

//
// Wrapper around Renderer's draw_string to use echo character if there is one
//
static void
Draw_string(Pad_Text *text, char *string, int level, float xoff, float yoff, float dtheta=0.0)
{
    if (text->echoChar == '\0') {
	text->refineNeeded |= Pad_renderer->Draw_string(string, level, xoff, yoff, dtheta);
    } else {
	int i, len;
	Pad_String str;

	len = strlen(string);
	DOTIMES(i, len) {
	    str += text->echoChar;
	}
	text->refineNeeded |= Pad_renderer->Draw_string(str.Get(), level, xoff, yoff, dtheta);
    }
}

//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_Text::Render(void)
{
    int i;
    float height;
    float offset;
    float xoffset, yoffset;
    float descender;
    float xmin, ymin, xmax, ymax;
    int startLine, endLine;
    Pad_View *view;
    float xshift, yshift;
    Pad_BBox lineBBox;
    Pad_String str;
    Pad_Bool selection = FALSE;
    Pad_TextIndex *sStart, *sEnd;
    Pad_Bool rc = TRUE;
    Pad_BBox bb;

    if (lines == 0) {		// Shouldn't ever get here, but it does sometimes
	return(TRUE);
    }

    refineNeeded = FALSE;       // Set to true by Draw_string if refinement is needed

    view = (Pad_View *)Pad_prc->views.Last();
    if (_penColor.Is_set()) {
	if (Pad_focus == this) {
	    Render_cursor();
	}
				// Compute text clipping
				// (i.e., which lines of the text are actually visible)
	if (Get_angle() == 0) {
	    Pad_prc->win->activeRestorer->Get_visibility(this, xmin, ymin, xmax, ymax);
	    startLine = (int)((1 - ymax) * lines);
	    endLine = MIN((int)((1 - ymin) * lines), lines - 1);
	} else {
	    startLine = 0;
	    endLine = lines - 1;
	}

	Get_bbox(bb);
        xoffset = _xoffset + _activeArea.x;
	yoffset = _yoffset + _activeArea.y + bb.Ymax() - bb.Ymin();

	Pad_renderer->Set_font(_font);
	height = Pad_renderer->Get_font_height();
	descender = Pad_renderer->Get_font_descender();

	if (selStart <= selEnd) {
	    sStart = &selStart;
	    sEnd = &selEnd;
	} else {
	    sStart = &selEnd;
	    sEnd = &selStart;
	}
	
				// If there is some selected text, then render the background 
				// of the selected portion
	if (*sStart != *sEnd) {
	    selection = TRUE;
	    Pad_renderer->Set_color(_penColor);

	    str = text[sStart->line];        // First calculate offset from start of line
	    str.Insert('\0', sStart->chr);
	    Pad_renderer->Get_string_bbox(str.Get(), &lineBBox);
	    offset = lineBBox.Width();

	    str = text[sStart->line];        // Then calculate length of selection
	    if (sStart->line == sEnd->line) {
		str.Insert('\0', sEnd->chr);
	    }
				              // Draw background of first line
	    Pad_renderer->Get_string_bbox(str.Get(sStart->chr), &lineBBox);
	    lineBBox.Set_x(offset + lineBBox.Xmin(), offset + lineBBox.Xmax());
	    i = sStart->line;
	    Pad_renderer->Draw_filled_box(xoffset + lineBBox.Xmin(), 
					  yoffset - height * (i+1) + lineBBox.Ymin() - descender, 
					  xoffset + lineBBox.Xmax(), 
					  yoffset - height * i);

				              // Draw background of middle lines
	    for (i=sStart->line+1; i<=sEnd->line-1; i++) {
		Pad_renderer->Get_string_bbox(text[i].Get(), &lineBBox);
		Pad_renderer->Draw_filled_box(xoffset + lineBBox.Xmin(), 
					      yoffset - height * (i+1) + lineBBox.Ymin() - descender, 
					      xoffset + lineBBox.Xmax(), 
					      yoffset - height * i);
	    }
	    
				              // Draw background of last line
	    if (sEnd->line > sStart->line) {
		i = sEnd->line;
		str = text[i];
		str.Insert('\0', sEnd->chr);
		Pad_renderer->Get_string_bbox(str.Get(), &lineBBox);
		Pad_renderer->Draw_filled_box(xoffset + lineBBox.Xmin(), 
					      yoffset - height * (i+1) + lineBBox.Ymin() - descender, 
					      xoffset + lineBBox.Xmax(), 
					      yoffset - height * i);
	    }
	}

				// Render text differently depending on render level
	Pad_renderer->Set_color(_penColor);
	if (Get_angle() == 0) {
	switch (level)
	  {
	    case 0:
				// For render level 0, Don't check for interruption
	      for (i=startLine; i<=endLine; i++) {
		  Draw_string(this, text[i].Get(), 0, xoffset, yoffset - height * (i + 1), Get_angle());
	      }
	      break;
	    case 1:
				// For render level 1, Check for interruption every 10 lines
	      for (i=startLine; i<=endLine; i++) {
		  if (i%10 == 9) {
		      if ((view->pad->level > 0) && Pad_renderer->Is_interrupted(Pad_prc->win)) {
			  rc = FALSE;
			  break;
		      }
		  }
		  Draw_string(this, text[i].Get(), 1, xoffset, yoffset - height * (i + 1), Get_angle()); 
	      }
	      break;
	    case 2:
				// For render level 2, Check for interruption every 5 lines
	      for (i=startLine; i<=endLine; i++) {
		  if (selection) {
		      if (i < sStart->line) {
				// Render first full lines of unselected text
			  Draw_string(this, text[i].Get(), 2, xoffset, yoffset - height * (i + 1), Get_angle());
		      } else if (i == sStart->line) {
				// Render partial line of unselected text
			  str = text[i];
			  str.Insert('\0', sStart->chr);
			  Draw_string(this, str.Get(), 2, xoffset, yoffset - height * (i + 1), Get_angle());
				// Render first partial line of selected text
			  Pad_renderer->Set_color(_fillColor);
			  Pad_renderer->Get_string_bbox(str.Get(), &lineBBox);
			  str = text[i].Get(sStart->chr);
			  if (sStart->line == sEnd->line) {
			      str.Insert('\0', (sEnd->chr - sStart->chr));
			  }
			  Draw_string(this, str.Get(), 2, xoffset + lineBBox.Width(), 
						    yoffset - height * (i + 1), Get_angle());
				// If only one line of selected text, render end partial unselected text
			  if (i == sEnd->line) {
			      Pad_renderer->Set_color(_penColor);
			      str = text[i].Get();
			      str.Insert('\0', sEnd->chr);
			      Pad_renderer->Get_string_bbox(str.Get(), &lineBBox);
			      str = text[i].Get(sEnd->chr);
			      Draw_string(this, str.Get(), 2, xoffset + lineBBox.Width(), 
							yoffset - height * (i + 1), Get_angle());
			  }
		      } else if ((i > sStart->line) && (i < sEnd->line)) {
				// Render middle full lines of selected text
			  Draw_string(this, text[i].Get(), 2, xoffset, yoffset - height * (i + 1), Get_angle());
		      } else if (i == sEnd->line) {
				// Render partial line of selected text
			  str = text[i];
			  str.Insert('\0', sEnd->chr);
			  Draw_string(this, str.Get(), 2, xoffset, yoffset - height * (i + 1), Get_angle());
				// Render partial line of unselected text
			  Pad_renderer->Set_color(_penColor);
			  Pad_renderer->Get_string_bbox(str.Get(), &lineBBox);
			  str = text[i].Get(sEnd->chr);
			  Draw_string(this, str.Get(), 2, xoffset + lineBBox.Width(), 
						    yoffset - height * (i + 1), Get_angle());
		      } else {
				// Render last full lines of unselected text
			  Draw_string(this, text[i].Get(), 2, xoffset, yoffset - height * (i + 1), Get_angle());
		      }
		  } else {
				// No selection, render normally
		      Draw_string(this, text[i].Get(), 2, xoffset, yoffset - height * (i + 1), Get_angle());
		  }

		  if ((i%5 == 4) && 
		      (pad->level > 0) && Pad_renderer->Is_interrupted(Pad_prc->win))
                   {
			  rc = FALSE;
			  break;
		   }
	      }
	      break;
	  }
	} else { // If text is rotated
	    switch (level)
		{
		case 0:
				// For render level 0, Don't check for interruption
		    for (i=startLine; i<=endLine; i++) {
			xshift =  (height * (i + 1)) * sin(DEG2RAD(Get_angle()));
			yshift = (- height * (i + 1)) * cos(DEG2RAD(Get_angle()));
			Draw_string(this, text[i].Get(), 2, xshift, yshift,  Get_angle());
		    }
		    break;
		case 1:
				// For render level 1, Check for interruption every 10 lines
		    for (i=startLine; i<=endLine; i++) {
			if (i%10 == 9) {
			    if ((view->pad->level > 0) && Pad_renderer->Is_interrupted(Pad_prc->win)) {
				rc = FALSE;
				break;
			    }
			}
			xshift =  (height * (i + 1)) * sin(DEG2RAD(Get_angle()));
			yshift = (- height * (i + 1)) * cos(DEG2RAD(Get_angle()));
			Draw_string(this, text[i].Get(), 1, xshift, yshift, Get_angle());
		    }
		    break;
		case 2:
				// For render level 2, Check for interruption every 5 lines
		    for (i=startLine; i<=endLine; i++) {
			xshift =  (height * (i + 1)) * sin(DEG2RAD(Get_angle()));
			yshift = (- height * (i + 1)) * cos(DEG2RAD(Get_angle()));
			Draw_string(this, text[i].Get(), 2, xshift, yshift, Get_angle());
		    }
		    
		    if ((i%5 == 4) && 
			(view->pad->level > 0) && Pad_renderer->Is_interrupted(Pad_prc->win))
			{
			    rc = FALSE;
			    break;
			}
		    break;
		}
	}
    }

    return(rc);
}

//
// This gets called whenever the system is slow, and the object
// should render itself faster - even if that makes it a bt ugly.
//
Pad_Bool
Pad_Text::Render_medium(void)
{
				// If the font is not too big, we'll
				// lower it's rendering quality a level
    if (level > 0) {
	if (level == 1) {
	    level = 0;
	} else {
	    if (renderedFontSize < TEXT_FAST_SIZE) {
		level = 1;
	    }
	}
    }
    Render();

    return(TRUE);
}

//
// This gets called whenever the system is very slow, and the object
// should render itself very fast - even if that makes it ugly.
//
Pad_Bool
Pad_Text::Render_fast(void)
{
    Pad_renderer->Set_color(&Pad_Color::gray);

    if (Get_angle() == 0) {
	float bb[4];

	Get_bbox(bb);
	Pad_renderer->Draw_filled_box(bb[XMIN], bb[YMIN], bb[XMAX],bb[YMAX]);
    } else {
	float theta;
	float width, height;

	theta = Get_angle();
	width = Compute_width();
	Pad_renderer->Set_font(_font);
	height = Pad_renderer->Get_font_height() * (lines + 0.3);

	Pad_renderer->Begin_polygon();
	Pad_renderer->V2f(0.0, -height, theta);
	Pad_renderer->V2f(width, -height, theta);
	Pad_renderer->V2f(width, 0.0, theta);
	Pad_renderer->V2f(0.0, 0.0, theta);
	Pad_renderer->End_polygon();
    }

    return(TRUE);
}

//
// Return scale factor that will transform object so height of object
// fits within height of frame.
//
float
Pad_Text::Compute_scale_within_frame(float *frame)
{
    float s;

				// Text gets drawn one pixel high
				// The magic number 0.3 here comes from the way text
				// objects compute their bounding box.
				// See Text::Compute_bounding_box().
    Pad_renderer->Set_font(_font);
    s = (frame[YMAX] - frame[YMIN]) / (Pad_renderer->Get_font_height() + 0.3);

    return(s);
}

//
// Returns true if object needs more refinement.
//
Pad_Bool 
Pad_Text::Continue_refinement(void)
{
	return (refineNeeded);
}

void 
Pad_Text::Render_cursor(void)
{
    int j;
    int tabpos = 0;
    char c;
    char *str;
    float width;
    float cursorWidth;
    float lineHeight;
    float descender;
    float cursorXpos = 0.0;
    float xoffset, yoffset;
    Pad_Point rotPts[4];
    Pad_BBox bb;

    if (!cursorColor.Is_set()) {
	return;
    }
    
    Get_bbox(bb);
    xoffset = _xoffset + _activeArea.x;
    yoffset = _yoffset + _activeArea.y + bb.Ymax() - bb.Ymin();

    str = text[point.line].Get();
    
    Pad_renderer->Set_font(_font);
    cursorWidth = Pad_renderer->Get_char_width(str[point.chr]);
    lineHeight = Pad_renderer->Get_font_height();
    descender = Pad_renderer->Get_font_descender();
    
    DOTIMES(j, point.chr)
      {
	  c = str[j];
	  if (c == '\t') {
	      width = Pad_renderer->Get_char_width(' ');
	      cursorXpos += (8 - (tabpos % 8)) * width;
	      tabpos = 0;
	  } else {
	      width = Pad_renderer->Get_char_width(c);
	      cursorXpos += width;
	      tabpos++;
	  }
      }

      if (Get_angle() ==0){ // If text is not rotated do normal
	  if (pad->Get_active()) {// do not draw fill box if not active
	      Pad_renderer->Set_color(cursorColor);
	      Pad_renderer->Draw_filled_box(xoffset + cursorXpos, 
					    yoffset - lineHeight * (point.line + 1) - descender,
					    xoffset + cursorXpos + cursorWidth,
					    yoffset - lineHeight * point.line);
	  }

	  Pad_renderer->Set_color(&Pad_Color::white);
	  Pad_renderer->Set_abs_line_width(1);
	  Pad_renderer->Draw_box(xoffset + cursorXpos, 
				 yoffset - lineHeight * (point.line + 1) - descender,
				 xoffset + cursorXpos + cursorWidth,
				 yoffset - lineHeight * point.line);
      } else { // If text is rotated, rotate cursor
	
	  // Find the four corners of the original box
	  rotPts[0].x = cursorXpos;
	  rotPts[0].y = - lineHeight * (point.line + 1);
	  rotPts[1].x = cursorXpos + cursorWidth;
	  rotPts[1].y = - lineHeight * (point.line + 1);
	  rotPts[2].x = cursorXpos + cursorWidth;
	  rotPts[2].y = - lineHeight * point.line;
	  rotPts[3].x = cursorXpos;
	  rotPts[3].y = - lineHeight * point.line;
	  
	  // Rotate the corners about the anchorpt 
	  Pad_Point tmppt;
	  tmppt.x = 0.0;
	  tmppt.y = 0.0;
	  rotPts[0].Rotate(Get_angle(), tmppt);
	  rotPts[1].Rotate(Get_angle(), tmppt);
	  rotPts[2].Rotate(Get_angle(), tmppt);
	  rotPts[3].Rotate(Get_angle(), tmppt);
	  
	  if(pad->Get_active()) {
	      Pad_renderer->Set_color(cursorColor);
	      Pad_renderer->Draw_polygon(4, rotPts);
	  } 
	  
	  Pad_renderer->Set_color(&Pad_Color::white);
	  Pad_renderer->Set_abs_line_width(1);
	  
	  Pad_renderer->Draw_box(rotPts);
	  
      }
}

//////////////////////////////////////////////
//              Tcl interface
//////////////////////////////////////////////  

//
// index_comparison
//
//   Compares the two indices to see which is larger.  Returns
//   -1, index1 < index2
//    0, index1 = index2
//    1, index1 > index2
//
static int
Index_comparison(Pad_TextIndex &index1, Pad_TextIndex &index2)
{
    int result;

    if (index1.line < index2.line) {
	result = -1;
    } else if (index1.line > index2.line) {
	result = 1;
    } else {
	if (index1.chr < index2.chr) {
	    result = -1;
	} else if (index1.chr > index2.chr) {
	    result = 1;
	} else {
	    result = 0;
	}
    }

    return(result);
}

int 
Pad_Text::Compare(int argc, char **argv)
{
    int result, value;
    char *p;
    Pad_TextIndex index1(this), index2(this);

    if (argc != 4) {
	fprintf(stderr, "Pad_Text::Compare wrong # args %d should be %d\n",
	    argc, 4);
	return(PAD_ERROR);
    }
    if (Get_index_from_string(argv[1], &index1) != PAD_OK) {
	return(PAD_ERROR);
    }
    if (Get_index_from_string(argv[3], &index2) != PAD_OK) {
	return(PAD_ERROR);
    }

    result = Index_comparison(index1, index2);

    p = argv[2];
    if (p[0] == '<') {
	value = (result == -1 ? 1 : 0);
	if ((p[1] == '=') && (p[2] == 0)) {
	    value = (result <= 0 ? 1 : 0);
	} else if (p[1] != 0) {
	  compareError:
	    fprintf(stderr, "Pad_Text::Compare bad comparison operator %s\n",
		argv[3]);
	    return(PAD_ERROR);
	}
    } else if (p[0] == '>') {
	value = (result == 1 ? 1 : 0);
	if ((p[1] == '=') && (p[2] == 0)) {
	    value = (result >= 0 ? 1 : 0);
	} else if (p[1] != 0) {
	    goto compareError;
	}
    } else if ((p[0] == '=') && (p[1] == '=') && (p[2] == 0)) {
	value = (result == 0 ? 1 : 0);
    } else if ((p[0] == '!') && (p[1] == '=') && (p[2] == 0)) {
	value = (result == 0 ? 0 : 1);
    } else {
	goto compareError;
    }

    Pad_resultString = (char *)((value) ? "1" : "0");
    return(PAD_OK);
}

int 
Pad_Text::Del(int argc, char **argv) 
{
    Pad_TextIndex index1(this), index2(this);

    if ((argc != 2) && (argc != 3)) {
	fprintf(stderr, "Pad_Text::Del wrong # args %d should be %s\n",
	    argc, "2 or 3");
	return(PAD_ERROR);
    }

    if (Get_lock()) {
	return(PAD_OK);		// Can't modify if locked
    }

    if (Get_index_from_string(argv[1], &index1) != PAD_OK) {
	return(PAD_ERROR);
    }
    if (argc == 2) {
	index2.line = index1.line;
	index2.chr = index1.chr + 1;
	Round_index(&index2);
    } else if (Get_index_from_string(argv[2], &index2) != PAD_OK) {
	return(PAD_ERROR);
    }

    Delete_chars(index1, index2, TRUE);

    return(PAD_OK);
}

int 
Pad_Text::Get(int argc, char **argv)
{
    Pad_TextIndex index1(this), index2(this), temp(this);
    Pad_String string;

    if ((argc != 2) && (argc != 3)) {
	fprintf(stderr, "Pad_Text::Get wrong # args %d should be %s\n",
	    argc, "2 or 3");
	return(PAD_ERROR);
    }

    if (Get_index_from_string(argv[1], &index1) != PAD_OK) {
	return(PAD_ERROR);
    }
    if (argc == 2) {
	index2.line = index1.line;
	index2.chr = index1.chr + 1;
	Round_index(&index2);
    } else if (Get_index_from_string(argv[2], &index2) != PAD_OK) {
	return(PAD_ERROR);
    }

				// Wrong order, so swap
    if (index2 < index1) {
	temp.line = index1.line;
	temp.chr = index1.chr;
	index1.line = index2.line;
	index1.chr = index2.chr;
	index2.line = temp.line;
	index2.chr = temp.chr;
    }

    Get_chars(string, index1, index2);
    Pad_resultString += string.Get();

    return(PAD_OK);
}

int 
Pad_Text::Bbox_cmd(int argc, char **argv)
{
    Pad_TextIndex index1(this), index2(this), temp(this);
    float bb[4], bb1[4], bb2[4];
    Pad_BBox tmp_bbox;

    if ((argc != 2) && (argc != 3)) {
	fprintf(stderr, "Pad_Text::Bbox_cmd wrong # args %d should be %s\n",
	    argc, "2 or 3");
	return(PAD_ERROR);
    }

    if (Get_index_from_string(argv[1], &index1) != PAD_OK) {
	return(PAD_ERROR);
    }
    if (argc == 2) {
	index2.line = index1.line;
	index2.chr = index1.chr;
    } else if (Get_index_from_string(argv[2], &index2) != PAD_OK) {
	return(PAD_ERROR);
    }

				// Wrong order, so swap
    if (index2 < index1) {
	temp.line = index1.line;
	temp.chr = index1.chr;
	index1.line = index2.line;
	index1.chr = index2.chr;
	index2.line = temp.line;
	index2.chr = temp.chr;
    }

    Bbox_of_index(index1, bb1);
    Bbox_of_index(index2, bb2);
    Pad_Merge_bounds(bb1, bb2, bb);
    if (index2.line > index1.line) {
	Get_global_bbox(tmp_bbox);
	bb[XMIN] = tmp_bbox.Xmin();
	bb[XMAX] = tmp_bbox.Xmax();
    }
    sprintf(Pad_result, "%f %f %f %f", 
	    XTOUNITS(pad->view->win, bb[0]),
	    YTOUNITS(pad->view->win, bb[1]),
	    XTOUNITS(pad->view->win, bb[2]),
	    YTOUNITS(pad->view->win, bb[3]));
    Pad_resultString = Pad_result;

    return(PAD_OK);
}

int 
Pad_Text::Index(int argc, char **argv)
{
    int type;
    Pad_TextIndex index(this);

    if ((argc != 2) && (argc != 3)) {
	fprintf(stderr, "Pad_Text::Index wrong # args %d should be %s\n",
	    argc, "2 or 3");
	return(PAD_ERROR);
    }

    type = 0;
    if (argc == 3) {
	if (!strcmp(argv[2], "char")) {
	    type = 1;
	} else {
	    Pad_resultString = "";
	    fprintf(stderr, "Pad_Text::Index optional argument must be char\n");
	    return(PAD_ERROR);
	}
    }

    if (Get_index_from_string(argv[1], &index) != PAD_OK) {
	return(PAD_ERROR);
    }

    Print_index(index, Pad_result, type);
    Pad_resultString = Pad_result;

    return(PAD_OK);
}

int 
Pad_Text::Insert(int argc, char **argv)
{
    Pad_TextIndex index(this);

    if (argc != 3) {
	fprintf(stderr, "Pad_Text::Insert wrong # args %d, should be %d\n",
		argc, 3);
	return(PAD_ERROR);
    }

    if (Get_lock()) {
	return(PAD_OK);		// Can't modify if locked
    }

    if (Get_index_from_string(argv[1], &index) != PAD_OK) {
	return(PAD_ERROR);
    }

    Insert_chars(argv[2], index, TRUE);

    Print_index(index, Pad_result, 0);
    Pad_resultString = Pad_result;
    Generate_event(Pad_ModifyNotify, NULL, "text");

    return(PAD_OK);
}

int 
Pad_Text::Mark(int argc, char **argv)
{
    int i;
    Pad_Bool pointFound;
    Pad_TextIndex index(this);
    Pad_TextIndex *new_index;
    Pad_HashTableIterator hi;

    if (argc < 2) {
	fprintf(stderr, "Pad_Text::Mark wrong # args %d should be >= %d\n",
		argc, 2);
	return(PAD_ERROR);
    }

    if (!strcmp(argv[1], "names"))
      {
          Pad_resultString = "";
	  pointFound = FALSE;
	  DOTABLE(hi, *marks, Pad_TextIndex, new_index) {
				// Only show "point" index once.
				// There are two.  Once for "point" and once for "insert"
	      if (new_index == &point) {
		  if (pointFound) {
		      continue;
		  } else {
		      pointFound = TRUE;
		  }
	      }
	      if (Pad_resultString != "")
		  Pad_resultString += " ";
	      Pad_resultString += new_index->name.Get();
	  }
      }
    else if (!strcmp(argv[1], "set"))
      {
	  if (argc != 4) {
	      fprintf(stderr, "Pad_Text::Mark wrong # args %d should be %d\n",
	          argc, 4);
	      return(PAD_ERROR);
	  }
	  if (Get_index_from_string(argv[3], &index) != PAD_OK) {
	      return(PAD_ERROR);
	  }
	  if ((new_index = (Pad_TextIndex *)marks->Get(argv[2])) == NULL) {
	      new_index = new Pad_TextIndex(this);
	      new_index->name = argv[2];
	      marks->Set(argv[2], new_index);
	  }
	  new_index->chr = index.chr;
	  new_index->line = index.line;
	  if (new_index == &point) {
	      Damage();
	  }
      } 
    else if (!strcmp(argv[1], "unset"))
      {
	  for (i=1; i<argc; i++) {
	      new_index = (Pad_TextIndex *)marks->Get(argv[i]);
	      if (new_index != &point) {
		  marks->Remove(argv[i]);
	      }
	  }
      } 
    else 
      {
	  fprintf(stderr, "Pad_Text::Mark unknown option %s\n",
	      argv[1]);
	  return(PAD_ERROR);
      }

    return(PAD_OK);
}

void 
Pad_Text::Round_index(Pad_TextIndex *index)
{
    while (index->line < lines - 1) {
	if (index->chr > text[index->line].Length()) {
	    index->chr -= text[index->line].Length() + 1;
	    index->line++;
	} else {
	    break;
	}
    }
    while (index->line > 0) {
	if (index->chr < 0) {
	    index->line--;
	    index->chr += text[index->line].Length() + 1;
	} else {
	    break;
	}
    }

    if (index->line >= lines) {
	index->line = lines - 1;
    }
    if (index->line < 0) {
	index->line = 0;
    }
    if (index->chr > text[index->line].Length()) {
	index->chr = text[index->line].Length();
    }
    if (index->chr < 0) {
	index->chr = 0;
    }
}

// 
// Update index based on insertion (start_index < end_index),
// or deletion (start_index > end_index).
//
void 
Pad_Text::Update_index(Pad_TextIndex &start_index, Pad_TextIndex &end_index)
{
    Pad_Bool pointFound;
    Pad_TextIndex *index;
    Pad_HashTableIterator hi;

    pointFound = FALSE;
    DOTABLE(hi, *marks, Pad_TextIndex, index) {
				// Only process "point" once.  There are two
				// instances - once for "point" and once for "insert"
	if (index == &point) {
	    if (pointFound) {
		continue;
	    } else {
		pointFound = TRUE;
	    }
	}
				// Insertion
	  if (start_index < end_index)
	    {
		if (*index >= start_index) {
		    if ((start_index.line == end_index.line) && (end_index.line == index->line)) {
			index->chr += (end_index.chr - start_index.chr);
		    } else {
			index->line += (end_index.line - start_index.line);
			if (end_index.line == index->line) {
			    index->chr -= start_index.chr - end_index.chr;
			}
		    }
		}
	    } 
	  else {		      
				// Deletion
	      if (start_index > end_index) {
		  if (*index >= start_index) {
		      if ((start_index.line == end_index.line) && (end_index.line == index->line)) {
			  index->chr -= (start_index.chr - end_index.chr);
		      } else {
			  if (start_index.line == index->line) {
			      index->chr -= start_index.chr - end_index.chr;
			  }
			  index->line -= (start_index.line - end_index.line);
		      }
		  }
	      } 
	  }
	  Round_index(index);
      }
}

void 
Pad_Text::Bbox_of_index(Pad_TextIndex &index, float *bb)
{
    int i;
    unsigned char c;
    float width;
    
    bb[XMIN] = 0.0;
    Pad_renderer->Set_font(_font);
    bb[YMIN] = - (Pad_renderer->Get_font_height() * (index.line + 1));
    bb[YMAX] = - (Pad_renderer->Get_font_height() * index.line);
    for (i=0; i<index.chr; i++) {
	c = *text[index.line].Get(i);
	bb[XMIN] += Pad_renderer->Get_char_width(c);
    }
    c = *text[index.line].Get(index.chr);
    if (c == '\0') {
	width = 1.0;
    } else {
	width = Pad_renderer->Get_char_width(c);
    }
    bb[XMAX] = bb[XMIN] + width;
    Local_to_screen(bb);
}

void 
Pad_Text::Forward_chars(Pad_TextIndex *index, int count)
{
    index->chr += count;
    Round_index(index);
}

void 
Pad_Text::Backward_chars(Pad_TextIndex *index, int count)
{
    index->chr -= count;
    Round_index(index);
}

//
// Insert <str> at <index>
//
void
Pad_Text::Insert_chars(char *str, Pad_TextIndex &index, Pad_Bool reshape)
{
    int i;
    int len, bufLen;
    char *token;
    Pad_String *s;
    Pad_String string;
    Pad_Bool ending_cr;
    Pad_TextIndex startIndex(this);

    startIndex.line = index.line;
    startIndex.chr = index.chr;

    string.Set(str);	// Make copy because strtok is destructive
    bufLen = string.Length();
    token = strtok(string.Get(), "\n\r");
    if ((str[bufLen - 1] == '\n') || (str[bufLen - 1] == '\r')) {
	ending_cr = TRUE;
    } else {
	ending_cr = FALSE;
    }
    while (token || ending_cr) {
	if (token) {
	    text[index.line].Insert(token, index.chr);
	    len = strlen(token);
	    index.chr += len;
	    token = strtok(NULL, "\n");
	}
				// Catch internal *and* trailing carriage returns
	if (token || ending_cr) {
				// Insert a newline (i.e., split up line)
	    s = text;
	    text = new Pad_String[lines + 1];
	  
				// Copy before cursor
	    for (i = 0; i < index.line; i++)
	      {
		  text[i].Set(&s[i]);
	      }
				// Split up line
	    text[index.line].Set(s[index.line].Get(), index.chr);
	    text[index.line + 1].Set(s[index.line].Get(index.chr));
				// Copy after cursor
	    for (i = index.line + 1; i < lines; i++)
	      {
		  text[i + 1].Set(&s[i]);
	      }
	    
	    delete [] s;
	    lines++;
	    index.line++;
	    index.chr = 0;

	    if (!token) {
		break;
	    }
	}
    }

    Round_index(&selStart);
    Round_index(&selEnd);

    textFlags |= TEXT_TEXT_SET;
				// Update marks
    Update_index(startIndex, index);

    if (reshape) {
	Update();
    } else {
	Damage();
    }
}

//
// Delete the characters between index1 and index2
//
void
Pad_Text::Delete_chars(Pad_TextIndex &index1, Pad_TextIndex &index2, Pad_Bool reshape)
{
    int i, delta;
    Pad_TextIndex temp(this);
				// Wrong order, so swap
    if (index2 < index1) {
	temp.line = index1.line;
	temp.chr = index1.chr;
	index1.line = index2.line;
	index1.chr = index2.chr;
	index2.line = temp.line;
	index2.chr = temp.chr;
    }

    if (index1.line == index2.line) {
	text[index1.line].Erase(index1.chr, index2.chr - index1.chr);
    } else {
	text[index1.line].Insert('\0', index1.chr);
	text[index1.line].Append(text[index2.line].Get(index2.chr));
	delta = index2.line - index1.line;
	for (i=index1.line + 1; i < (lines - delta); i++) {
	    text[i].Set(&text[i + delta]);
	}
	for (i=(lines - delta); i<lines; i++) {
	    text[i].Set("");
	}
	lines -= delta;
    }

    Round_index(&selStart);
    Round_index(&selEnd);
				// Update marks
    Update_index(index2, index1);
    textFlags |= TEXT_TEXT_SET;

    if (reshape) {
	Update();
    } else {
	Damage();
    }

    Generate_event(Pad_ModifyNotify, NULL, "text");
}

//
// Return the text between index1 and index2
//
void
Pad_Text::Get_chars(Pad_String &result, Pad_TextIndex &index1, Pad_TextIndex &index2)
{
    int i;
    Pad_String string;

    result = "";
    for (i=index1.line; i<=index2.line; i++) {
	if (index1.line == index2.line) {
	    string.Set(text[i].Get(index1.chr), index2.chr - index1.chr);
	    result += string;
	} else {
	    if (i == index1.line) {
		result += text[i].Get(index1.chr);
	    } else if (i == index2.line) {
		string.Set(text[i].Get(), index2.chr);
		result += string;
	    } else {
		result += text[i].Get();
	    }
	}
	if (i < index2.line) {
	    result += "\n";
	}
    }
}

//
// Forw_back
//   This procedure handles +/- modifiers for indices to adjust
//   the index forwards or backwards.
//
// Results:
//   If the modifier is successfully parsed then the return value
//   is the address of the first character after the modifier, and
//   the index is updated to reflect the modifier.  If there is a
//   syntax error in the modifier then NULL is returned.
//

static char *
Forw_back(Pad_Text *text, char *string, Pad_TextIndex *index)
{
    char *p;
    char *end, *units;
    int count, length;
    
    //
    // Get the count (how many units forward or backward).
    //
    
    p = string+1;
    while (isspace(UCHAR(*p))) {
	p++;
    }
    count = (int)strtoul(p, &end, 0);
    if (end == p) {
	return NULL;
    }
    p = end;
    while (isspace(UCHAR(*p))) {
	p++;
    }
    
    //
    // Find the end of this modifier (next space or + or - character),
    // then parse the unit specifier and update the position
    // accordingly.
    //
    
    units = p; 
    while ((*p != 0) && !isspace(UCHAR(*p)) && (*p != '+') && (*p != '-')) {
	p++;
    }
    length = p - units;
    if ((*units == 'c') && (strncmp(units, "chars", length) == 0)) {
	text->Round_index(index);
	if (*string == '+') {
	    text->Forward_chars(index, count);
	} else {
	    text->Backward_chars(index, count);
	}
    } else if ((*units == 'l') && (strncmp(units, "lines", length) == 0)) {
	if (*string == '+') {
	    index->line += count;
	    if (index->line >= text->lines) {
		index->line = text->lines - 1;
	    }
	} else {
	    index->line -= count;
	    if (index->line < 0) {
		index->line = 0;
	    }
	}
	if (index->chr > text->text[index->line].Length()) {
	    index->chr = text->text[index->line].Length();
	}
	if (index->chr < 0) {
	    index->chr = 0;
	}
    } else {
	return NULL;
    }
    return p;
}

static Pad_Bool whitespace(char c)
{
    if (isalnum(UCHAR(c))) {
	return(FALSE);
    } else {
	return(TRUE);
    }
}

//
// Start_end
//   This procedure handles modifiers like "wordstart" and "lineend"
//   to adjust indices forwards or backwards.
//
// Results:
//   If the modifier is successfully parsed then the return value
//   is the address of the first character after the modifier, and
//   index is updated to reflect the modifier.  f there is a syntax
//   error in the modifier then NULL is returned.
//

static char *
Start_end(Pad_Text *text, char *string, Pad_TextIndex *index)
{
    char *p, c;
    int length;
    int linelen;
    
    //
    // Find the end of the modifier word.
    //
    
    for (p = string; isalnum(UCHAR(*p)); p++) {
	// Empty loop body.
    }
    length = p-string;
    if ((*string == 'l') && (strncmp(string, "lineend", length) == 0)
	&& (length >= 5)) {
	index->chr = text->text[index->line].Length();
    } else if ((*string == 'l') && (strncmp(string, "linestart", length) == 0)
	       && (length >= 5)) {
	index->chr = 0;
    } else if ((*string == 'w') && (strncmp(string, "wordend", length) == 0)
	       && (length >= 5)) {
	c = *text->text[index->line].Get(index->chr);
	linelen = text->text[index->line].Length();
	// Skip whitespace
	while (whitespace(c)) {
	    index->chr++;
	    if (index->chr > linelen) {
		if (index->line < (text->lines - 1)) {
		    index->line++;
		    index->chr = 0;
		    linelen = text->text[index->line].Length();
		} else {
		    index->chr--;
		    break;
		}
	    }
	    c = *text->text[index->line].Get(index->chr);
	}
	// Skip non-whitespace
	while (!whitespace(c)) {
	    index->chr++;
	    if (index->chr > linelen) {
		index->chr--;
		break;
	    }
	    c = *text->text[index->line].Get(index->chr);
	}
    } else if ((*string == 'w') && (strncmp(string, "wordstart", length) == 0)
	       && (length >= 5)) {
	c = *text->text[index->line].Get(index->chr);
	linelen = text->text[index->line].Length();
	// Skip whitespace
	while (whitespace(c)) {
	    index->chr--;
	    if (index->chr < 0) {
		if (index->line > 0) {
		    index->line--;
		    linelen = text->text[index->line].Length();
		    index->chr = linelen;
		} else {
		    index->chr++;
		    break;
		}
	    }
	    c = *text->text[index->line].Get(index->chr);
	}
	// Skip non-whitespace
	while (!whitespace(c)) {
	    index->chr--;
	    if (index->chr < 0) {
		index->chr++;
		break;
	    }
	    c = *text->text[index->line].Get(index->chr);
	}
	if ((index->chr > 0) && (index->chr < linelen)) {
	    index->chr++;
	}
    } else {
	return NULL;
    }
    return p;
}

int 
Pad_Text::Get_index_from_string(char *given_string, Pad_TextIndex *index)
{
    int num;
    char *p;
    char *end, *endOfBase;
    char save_char;
    Pad_TextIndex *new_index;
    
    // work with a copy of given_string
    char *string =  new char[strlen(given_string)+1];
    strcpy(string, given_string);
    
    //------------------------------------------------
    // Stage 1: parse the base index.
    //------------------------------------------------
    
    if (string[0] == '@') {
	/*
	  * Find character at a given x,y location in the window.
	    */
	
	float padx, pady;
	
	p = string+1;
	padx = ATOXFE(pad->view->win, p, &end);
	if ((end == p) || (*end != ',')) {
	    goto error;
	}
	p = end+1;
	pady = ATOYFE(pad->view->win, p, &end);
	if (end == p) {
	    goto error;
	}
	Get_index_from_xy(padx, pady, index);
	endOfBase = end;
	goto gotBase; 
    } else if (isdigit(UCHAR(string[0])) || (string[0] == '-')) {
	/*
	  * Base is identified with line and/or character indices.
	    */
	
	num = (int)strtol(string, &end, 0);
	if (end == string) {
	    goto error;
	}
	// Base identified only by character
	if (*end != '.') {
	    index->chr = num;
	    index->line = 0;
	    Round_index(index);
	    endOfBase = end;
	    goto gotBase;
	}
	
	// Base identified by line.char
	index->line = num;
	p = end+1;
	if ((*p == 'e') && (strncmp(p, "end", 3) == 0)) {
	    if ((index->line < lines) && (index->line >= 0)) {
		index->chr = text[index->line].Length();
	    } else {
		fprintf(stderr, "Pad_Text::Get_index_from_string Bad index %s\n",
			string);
		delete[] string;
		return(PAD_ERROR);
	    }
	    endOfBase = p+3;
	} else {
	    index->chr = (int)strtol(p, &end, 0);
	    if (end == p) {
		goto error;
	    }
	    endOfBase = end;
	}
	Round_index(index);
	
	goto gotBase;
    }
    
    for (p = string; *p != 0; p++) {
	if (isspace(UCHAR(*p)) || (*p == '+') || (*p == '-')) {
	    break;
	}
    }
    endOfBase = p;
    if ((string[0] == 'e')
	&& (strncmp(string, "end", endOfBase-string) == 0)) {
				// Base position is end of text.
	index->line = lines - 1;
	index->chr = text[index->line].Length();
	goto gotBase;
    } else {
				// See if the base position is the name of a mark.
	save_char = *endOfBase;
	*endOfBase = '\0';
	new_index = (Pad_TextIndex *)marks->Get(string);
	*endOfBase = save_char;
	if (new_index) {
	    index->line = new_index->line;
	    index->chr = new_index->chr;
	    goto gotBase;
	}
    }
    
				// Can't interpret base
    goto error;
    
    //-------------------------------------------------------------------
    // Stage 2: process zero or more modifiers.  Each modifier is either
    // a keyword like "wordend" or "linestart", or it has the form
    // "op count units" where op is + or -, count is a number, and units
    // is "chars" or "lines".
    //-------------------------------------------------------------------
    
  gotBase:
    p = endOfBase;
    while (1) {
	while (isspace(UCHAR(*p))) {
	    p++;
	}
	if (*p == 0) {
	    delete[] string;
	    return PAD_OK;
	}
	
	if ((*p == '+') || (*p == '-')) {
	    p = Forw_back(this, p, index);
	} else {
	    p = Start_end(this, p, index);
	}
	if (p == NULL) {
	    goto error;
	}
    }
    
  error:
    fprintf(stderr, "Pad_Text::Get_index_from_string Bad index %s\n", string);
    delete[] string;
    return PAD_ERROR;
}

void 
Pad_Text::Print_index(Pad_TextIndex &index, char *buf, int type)
{
    int i;
    int char_index = 0;
    
    if (type == 0) {	// Want line.char
	sprintf(buf, "%d.%d", index.line, index.chr);
    } else {			// Want char
	DOTIMES(i, index.line) {
	    char_index += text[i].Length() + 1;
	}
	char_index += index.chr;
	sprintf(buf, "%d", char_index);
    }
}

void 
Pad_Text::Get_index_from_xy(float padx, float pady, Pad_TextIndex *index)
{
    int tabpos;
    char c;
    char *str;
    int len;
    float cursor_x_factor, cursor_y_factor;
    float lineHeight;
    float current_width;
    Pad_BBox bb, lbb;

    Get_bbox(lbb);
    Pad_renderer->Set_font(_font);
    lineHeight = Pad_renderer->Get_font_height();

    if (Get_angle() != 0) {
	float sinTheta, cosTheta;//these variables are necessary only if text is rotated
	float textWidth, textHeight;
	float topLeftX, topLeftY;
	float newX, newY;
	float mappedX, mappedY;
	float scale;

	scale = transform.Get_scale();
	Pad_renderer->Set_font(_font);
	Get_global_bbox(bb);

	textWidth = Compute_width() * scale;
	textHeight = Compute_height() * scale;
	sinTheta = sin(DEG2RAD(Get_angle()));
	cosTheta = cos(DEG2RAD(Get_angle()));
        _Set_angle_data(fmod(Get_angle(), 360.0f));
  
	// Calculate the position of the top left hand corner of the rotated 
	// text when placed into the new bounding box. This is used for the 
	// reverse transformation to determine where to place the text cursor
	if (sinTheta >= 0 && cosTheta >= 0) { // If first quadrant rotation
	    topLeftY = textWidth * sinTheta;
	    topLeftX = 0.0;
	    
	} else if (sinTheta >= 0.0 && cosTheta < 0.0) { // Second quadrant
	    topLeftY= bb.Height();
	    topLeftX = textWidth * (-cosTheta);
		
	} else if (sinTheta < 0.0 && cosTheta < 0.0) { // Third quadrant
	    topLeftY = (textHeight * -cosTheta);
	    topLeftX =  bb.Width();
		
	} else if (sinTheta < 0.0 && cosTheta >= 0.0) { // Forth quadrant
	    topLeftY = 0.0;
	    topLeftX = textHeight * -sinTheta;
	}
	
	// Transform the padx and pady positions to unrotated/translated text frame   
	newX  = (padx - bb.Xmin())  - topLeftX;
	newY  = (bb.Ymax() - pady)  - topLeftY;
	
	mappedX = (newX * cosTheta) - (newY * sinTheta);    
	mappedY = (newY * cosTheta) + (newX * sinTheta);

	//find the line index (which line), and position to place cursor
	index->line = (int)((mappedY /textHeight)  * lines);
	
	cursor_x_factor = mappedX / textWidth;
	
    } else {
	
	Pad_renderer->Set_font(_font);
	Get_global_bbox(bb);

	cursor_y_factor = ((bb.Ymax() - pady) / bb.Height());
	index->line = (int)(cursor_y_factor * lbb.Height() / lineHeight);
 
	cursor_x_factor = (padx - bb.Xmin()) / bb.Width();

    }
    cursor_x_factor -= (_xoffset / lbb.Width());
    index->line += (int)(_yoffset / lineHeight);
    if (index->line < 0) {
	index->line = 0;
    }
    if (index->line >= lines) {
	index->line = lines - 1;
    }

    tabpos = 1;
    index->chr = 0;
    len = text[index->line].Length();
    str = text[index->line].Get();
    if (str[0] == '\t') {
	current_width = 8 * Pad_renderer->Get_char_width(' ');
	tabpos = 0;
    } else {
	current_width = Pad_renderer->Get_char_width(str[0]);
    }

    Get_bbox(bb);
    while (((current_width / bb.Width()) < cursor_x_factor) &&
	   (index->chr < len))
      {
	  c = str[index->chr + 1];
	  if (c == '\t') {
	      current_width += (8 - (tabpos % 8)) * 
		Pad_renderer->Get_char_width(' ');
	      tabpos = 0;
	  } else {
	      current_width += 
		Pad_renderer->Get_char_width(str[index->chr + 1]);
	      tabpos++;
	  }
	  index->chr++;
      }
    if (index->chr > len) {
	index->chr = len;
    }
    if (index->chr < 0) {
	index->chr = 0;
    }
}

//
// Update portion of text that is shown that the cursor is visible.
// (_xoffset, _yoffset) specifies the offset that the text is
// rendered from normal (in pixels in local coordinates).
//
void
Pad_Text::Update_offset(void)
{
    int j;
    int tabpos = 0;
    char c;
    char *str;
    float width;
    float cursorWidth;
    float cursorXpos = 0.0f;
    float lineHeight;
    float descender;
    
    str = text[point.line].Get();
    Pad_renderer->Set_font(_font);
    cursorWidth = Pad_renderer->Get_char_width(str[point.chr]);
    lineHeight = Pad_renderer->Get_font_height();
    descender = Pad_renderer->Get_font_descender();
    
    DOTIMES(j, point.chr) {
	c = str[j];
	if (c == '\t') {
	    width = Pad_renderer->Get_char_width(' ');
	    cursorXpos += (8 - (tabpos % 8)) * width;
	    tabpos = 0;
	} else {
	    width = Pad_renderer->Get_char_width(c);
	    cursorXpos += width;
	    tabpos++;
	}
    }
    
    if ((_xoffset + cursorXpos) < 0) {
	_xoffset += -(_xoffset + cursorXpos);
    } else if ((_xoffset + cursorXpos + cursorWidth + (2*TEXTFIELD_BORDER_WIDTH) + TEXTFIELD_OFFSET) > 
	       _activeArea.width) {
	_xoffset -= ((_xoffset + cursorXpos + cursorWidth + (2*TEXTFIELD_BORDER_WIDTH) + TEXTFIELD_OFFSET) - 
		     _activeArea.width);
    }

    if ((_yoffset - lineHeight * (point.line + 1) - descender) < - _activeArea.height) {
	_yoffset += lineHeight * (point.line + 1) + descender - _activeArea.height;
    } else if ((_yoffset - lineHeight * point.line) > 0) {
	_yoffset -= lineHeight * point.line;
    }
}

//////////////////////////////////////////////
//              other definitions
//////////////////////////////////////////////  

void 
Pad_Text::Compute_bounding_box(void)
{
    float maxWidth, lineHeight;
    float thetaRad;
    float sintextAng, costextAng;
    Pad_BBox bb;

    maxWidth = Compute_width();
    lineHeight = Pad_renderer->Get_font_height();
    
    if (Get_angle() != 0) {
	thetaRad = DEG2RAD(Get_angle());
	sintextAng = sin(thetaRad);
	costextAng = cos(thetaRad);
    }

    
    if (Get_angle() == 0) {
	if (!(optionFlags & PAD_WIDTH_SET)) {
	    Set_bbox_xmin(-0.2);
	    Set_bbox_xmax(maxWidth + 0.1);
	}
	if (!(optionFlags & PAD_HEIGHT_SET)) {
	    Set_bbox_ymin(-1 * lineHeight * (lines + 0.3));
	    Set_bbox_ymax(0.0);
	}
    } else if ((sintextAng >= 0) && (costextAng >= 0)) {  //first quadrant angle
	if (!(optionFlags & PAD_WIDTH_SET)) {
	    Set_bbox_xmin(0.0);
	    Set_bbox_xmax(((maxWidth + 0.1) * costextAng) +
			  ((lineHeight * (lines + 0.3)) * sintextAng));
	}
	if (!(optionFlags & PAD_HEIGHT_SET)) {
	    Set_bbox_ymin((-1 * lineHeight * (lines + 0.3)) * costextAng);
	    Set_bbox_ymax(((maxWidth + 0.1) * sintextAng));
	}
    } else if ((sintextAng >= 0) && (costextAng <= 0)) { //second quadrant angle
	if (!(optionFlags & PAD_WIDTH_SET)) {
	    Set_bbox_xmin((maxWidth + 0.1) * costextAng);
	    Set_bbox_xmax((lineHeight * (lines + 0.3)) * sintextAng);
	}
	if (!(optionFlags & PAD_HEIGHT_SET)) {
	    Set_bbox_ymin(0.0);
	    Set_bbox_ymax((maxWidth + 0.1) * sintextAng + 
			  (-1 * lineHeight * (lines + 0.3)) * costextAng);
	}
    } else if ((sintextAng <= 0) && (costextAng <= 0)) { //third quadrant angle
	if (!(optionFlags & PAD_WIDTH_SET)) {
	    Set_bbox_xmin((maxWidth + 0.1) * costextAng + 
			  (lineHeight * (lines + 0.3)) * sintextAng);
	    Set_bbox_xmax(0.0);
	}
	if (!(optionFlags & PAD_HEIGHT_SET)) {
	    Set_bbox_ymin((maxWidth + 0.1) * sintextAng);
	    Set_bbox_ymax((-1 * lineHeight * (lines + 0.3)) * costextAng);
	}
    } else if ((sintextAng <= 0) && (costextAng >= 0))   { //fourth quadrant angle
	if (!(optionFlags & PAD_WIDTH_SET)) {
	    Set_bbox_xmin((lineHeight * (lines + 0.3)) * sintextAng);
	    Set_bbox_xmax((maxWidth + 0.1) * costextAng);
	}
	if (!(optionFlags & PAD_HEIGHT_SET)) {
	    Set_bbox_ymin((-1 * lineHeight * (lines + 0.3) * costextAng) + 
			  ((maxWidth + 0.1) * sintextAng));
	    Set_bbox_ymax(0.0);
	}
    }

    Get_bbox(bb);
    _activeArea.Set(bb.Xmin() + _hiliteWidth, bb.Ymin() + _hiliteWidth, 
    		    bb.Width() - (2 * _hiliteWidth), bb.Height() - (2 * _hiliteWidth));

				// Need to update this now because otherwise when rotating
				// an text that is a member of a group, the position will
				// be incorrect when computing the groups bbox...
    if (group) {
	Set_position_from_anchor();
    }

    Pad_Object::Compute_bounding_box();
}

//
// Returns width of text object in pixels (local coordinates).
//
float 
Pad_Text::Compute_width(void)
{
    int i;
    Pad_BBox text_bbox, line_bbox;
    float extra_width = 0.0;
    
    Pad_renderer->Set_font(_font);
    if (Pad_focus == this) {	      // Text objects might get wider if cursor is at end of line
	extra_width = Pad_renderer->Get_char_width(' ');
    }
    DOTIMES(i, lines) {
	Pad_renderer->Get_string_bbox(text[i].Get(), &line_bbox);
	text_bbox.Union(line_bbox);
    }
    
    return(text_bbox.Width() + extra_width);
}

//
// Returns width of text object in pixels (local coordinates).
//
float 
Pad_Text::Compute_height(void)
{
    return (Pad_renderer->Get_font_height() * (lines + .3));
}

//
// Returns TRUE if text object contains string.  Note that
// all text objects are considered to contain the empty string.
//
Pad_Bool
Pad_Text::Contains_text(char *string) 
{
    int i;
    Pad_Bool found;
    
    if (string[0] == '\0') {
	found = TRUE;
    } else {
	found = FALSE;
	for (i=0; i<lines; i++) {
	    if (casestrstr(text[i].Get(), string)) {
		found = TRUE;
		break;
	    }
	}
    }
    
    return(found);
}


//////////////////////////////////////////////
//              Text File definitions
//////////////////////////////////////////////  

Pad_TextFile::~Pad_TextFile()    
{
    Generate_delete();

    if (name) 
      {
	  delete name;
	  name = NULL;
      }
}

Pad_TextFile::Pad_TextFile(Pad *pad) :
Pad_Text(pad)
{
    Init();
}

Pad_TextFile::Pad_TextFile(Pad *pad, char *file_name) :
Pad_Text(pad)
{
    Init();
    
    name = new Pad_String(file_name);
    fileLoaded = Load_file();
}

void 
Pad_TextFile::Init(void) 
{
    _type = PAD_TEXTFILE;
    name = NULL;
    fileLoaded = FALSE;
    Set_writeformat_default();
}

//
// Setters and getters for text
//
Pad_Bool
Pad_TextFile::Set_text(char *)
{
    Pad_errorString = "Error: Can't set -text for textfile items";
    return(FALSE);
}

//
// Setters and getters for file
//
Pad_Bool
Pad_TextFile::Set_file(char *file_name) 
{
    Pad_Bool rc;

    textFlags |= TEXT_FILE_SET;
    Damage();
    if (name) {
	delete name;
	name = NULL;
    }
    
    name = new Pad_String(file_name);
    if (name->Length() > 0) {
	fileLoaded = Load_file();
	rc = fileLoaded;
    } else {
	Pad_Text::Set_text("");
	fileLoaded = FALSE;
	delete name;
	name = NULL;
	rc = TRUE;
    }
    
    return(rc);
}

void 
Pad_TextFile::Set_file_default(void)
{
    Set_file(TEXT_DEFAULT_FILE);
    textFlags &= ~TEXT_FILE_SET;
}

char *
Pad_TextFile::Get_file(void)
{
    if (name) {
	return(name->Get());
    } else {
	return("");
    }
}

Pad_Bool
Pad_TextFile::Get_name(Pad_String &result) 
{
    
    result = "";
    result = name;
    return(TRUE);
}

//
// Setters and Getters for the writeformat option
//
Pad_Bool
Pad_TextFile::Set_writeformat(int new_format)
{
    int rc = TRUE;
    textFlags |= TEXT_WRITEFORMAT_SET;
    switch (new_format) {
      case PAD_WRITE_COPY:
      case PAD_WRITE_REFERENCE:
        writeFormat = new_format;
	break;
      default:
        rc = FALSE;
	Pad_errorString = "invalid writeformat. Try \"PAD_WRITE_COPY\" or \"PAD_WRITE_REFERENCE\"";
    }

    return rc;
}


void
Pad_TextFile::Set_writeformat_default(void)
{
    Set_writeformat(TEXT_DEFAULT_WRITEFORMAT);
    textFlags &= ~TEXT_WRITEFORMAT_SET;
}

int
Pad_TextFile::Get_writeformat(void)
{
    return writeFormat;
}


//
// Return true to continue rendering other objects, or false if rendering
// was interrupted.
//
Pad_Bool
Pad_TextFile::Render(void)
{
    Pad_Bool rc;
    float globalWidth, globalHeight;
    
    if (!fileLoaded) 
      {
	  fileLoaded = Load_file();
	  Compute_dimensions(globalWidth, globalHeight);
      }
    
    rc = Pad_Text::Render();
    
    return(rc);
}


Pad_Bool
Pad_TextFile::Load_file(void)
{
    int i;
    float maxWidth, w;;
    char c1, c2;
    int bufsize = 256;
    char buf[256];
    FILE *fp;
    Pad_List new_text;
    Pad_BBox line_bbox;
    Pad_Bool rc = TRUE;
    Pad_String expandedName;

    if (!name) {
	return(FALSE);
    }

    Pad_renderer->Set_font(_font);
    char *dot_ptr = strrchr(name->Get(), '.');
    if (dot_ptr) 
      {
	  if (strcmp(dot_ptr, ".ras") == 0) // Don't load in Sun raster files
	    rc = FALSE;
	  if (strcmp(dot_ptr, ".a") == 0)   // Don't load in library raster files
	    rc = FALSE;
	  if (strcmp(dot_ptr, ".o") == 0)   // Don't load in object raster files
	    rc = FALSE;
	  if (strcmp(dot_ptr, ".ps") == 0)  // Don't load in postscript raster files
	    rc = FALSE;
	  if (strcmp(dot_ptr, ".tar") == 0) // Don't load in tar files
	    rc = FALSE;
	  if (strcmp(dot_ptr, ".gz") == 0)  // Don't load in gnu compressed files
	    rc = FALSE;
	  if (strcmp(dot_ptr, ".Z") == 0)   // Don't load in compressed files
	    rc = FALSE;
	  if (rc == FALSE) {
	      Pad_errorString = "Error: Can't create textfile with binary data";
	      return(FALSE);
	  }
      }

    rc = Pad_Expand_pathname(name->Get(), expandedName);
    if (rc) {
	fp = fopen(expandedName.Get(), "r");
    } else {
	return(FALSE);
    }
    
    if (!fp)		// open failed
      {
	  Pad_errorString = "Error: Can't open file: ";
	  Pad_errorString += name;
	  return(FALSE);
      }
    
    // Check for ASCII file
    
    fread(buf, 1, 2, fp);
    c1 = buf[0];
    c2 = buf[1];
    
    if (((c1 < 32) ||
	 (c1 > 126) ||
	 (c2 < 32) ||
	 (c2 > 126)) &&
	((c1 != '\r') && (c1 != '\n') && (c1 != '\t')) &&
	((c2 != '\r') && (c2 != '\n') && (c2 != '\t')))
      {
	  fclose(fp);
	  Pad_errorString = "Error: Can't create textfile with binary data";
	  return(FALSE);
      }
    else if ((c1 == '%') &&	// Don't load in postscript files
	     (c2 == '!')) 
      {
	  fclose(fp);
	  Pad_errorString = "Error: Can't create textfile with binary data";
	  return(FALSE);
      }
    rewind(fp);
				// File is ok, first delete existing text
    if (text) {
	delete [] text;
	text = NULL;
    }    
				// Read in file line at a time creating list
				// of lines.  Then when all read in, create
				// an array of pointers of the right size.
    lines = 0;
    Pad_String *string_ptr;
    int len;
    
    maxWidth = 0.0;
    while (fgets(buf, bufsize, fp))
      {
	  string_ptr = new Pad_String(buf);
	  len = string_ptr->Length();
				// Get rid of trailing newline
	  if (*string_ptr->Get(len - 1) == '\n') {
	      string_ptr->Insert('\0', len - 1);
	  }
	  Pad_renderer->Get_string_bbox(string_ptr->Get(), &line_bbox);
	  w = line_bbox.Width();
	  if (w > maxWidth) {
	      maxWidth = w;
	  }
	  new_text.Push_last(string_ptr);
	  
	  lines++;
      }
    
    text = new Pad_String[MAX(1, lines)];
    
    DOTIMES(i, lines) 
      {
	  string_ptr = (Pad_String *)new_text.Pop();
	  text[i].Set(string_ptr);
	  delete string_ptr;
      }

    Update();
    
    fclose(fp);
    return(TRUE);
}

//////////////////////////////////////////////////////////////
//
// Pad_TextField class
//
//////////////////////////////////////////////////////////////

Pad_TextField::Pad_TextField(Pad *pad) :
Pad_Text(pad)
{
				// Initialize default event handlers for textfield
    //_Initialize_events(pad->view->win);

    _type = PAD_TEXTFIELD;
    _textfieldFlags = PAD_NO_MASK;

    Set_font_default();
    Set_fill_default();
    Set_pen_default();
    Set_editable_default();
    Update();
    _componentFlags |= COMPONENT_RESHAPE_SET;   // Don't let textfields shrink wrap around their contents
    _hiliteWidth = TEXTFIELD_DEFAULT_HILITEWIDTH;

    Delete_all_tags();
    Add_tag("Textfield");
}

Pad_TextField::~Pad_TextField()
{
}

//
// Render a textfield
//
Pad_Bool
Pad_TextField::Render(void)
{
    Pad_Point min, max;
    float bb[4];

				// First draw background
    min.Set(_activeArea.x, _activeArea.y);
    max.Set(_activeArea.x + _activeArea.width, _activeArea.y + _activeArea.height);

    if (_fillColor.Is_set()) {
	Pad_renderer->Set_color(_fillColor);
	Pad_renderer->Draw_filled_box(min.x, min.y, max.x, max.y);
    }
				// Then draw text.  Offset text properly
				// Compute clipping region so text stays within where it should
    bb[XMIN] = _activeArea.x + TEXTFIELD_BORDER_WIDTH;
    bb[YMIN] = _activeArea.y + TEXTFIELD_BORDER_WIDTH;
    bb[XMAX] = _activeArea.x + _activeArea.width - TEXTFIELD_BORDER_WIDTH;
    bb[YMAX] = _activeArea.y + _activeArea.height - TEXTFIELD_BORDER_WIDTH;
    Pad_prc->win->activeRestorer->Push_clip(bb);

    float xoffset = _xoffset;
    float yoffset = _yoffset;

    _xoffset += TEXTFIELD_BORDER_WIDTH + TEXTFIELD_OFFSET;
    _yoffset += - (TEXTFIELD_BORDER_WIDTH + TEXTFIELD_OFFSET);

    Pad_Text::Render();

    _xoffset = xoffset;
    _yoffset = yoffset;

    Pad_prc->win->activeRestorer->Pop_clip();

				// Finally, draw border
    if (_border.Is_set()) {
	Pad_renderer->Set_border(_border);
	Pad_renderer->Draw_3d_rectangle(min, max, TEXTFIELD_BORDER_WIDTH, PAD_RELIEF_SUNKEN);
    }

    Pad_Component::Render();

    return(TRUE);
}

//
// Return the minimum dimensions necessary for the component to look normal
//
void
Pad_TextField::Minimum_size(Pad_Dimension &dimension)
{
    if (_font.Is_set()) {
	Pad_renderer->Set_font(_font);
	if ((lines >= 0) && (text[0] != "")) {
	    Pad_BBox bb;
	    Pad_renderer->Get_string_bbox(text[0].Get(), &bb);
	    dimension.width = bb.Width() + (2 * TEXTFIELD_BORDER_WIDTH) + (2 * TEXTFIELD_OFFSET);
	} else {
	    dimension.width = TEXTFIELD_DEFAULT_WIDTH;
	}
	dimension.height = (Pad_renderer->Get_font_height() * 1.3) +
	    (2 * TEXTFIELD_BORDER_WIDTH) + (2 * TEXTFIELD_OFFSET);
    } else {
	Pad_Component::Minimum_size(dimension);
    }
}

//
// Return the minimum size for the specified # of columns
//
void
Pad_TextField::Minimum_size(Pad_Dimension &dimension, int cols)
{
    if (_font.Is_set()) {
	Pad_renderer->Set_font(_font);
	dimension.width = (cols * Pad_renderer->Get_font_height()) +
	    (2 * TEXTFIELD_BORDER_WIDTH) + (2 * TEXTFIELD_OFFSET);
	dimension.height = (Pad_renderer->Get_font_height() * 1.3) +
	    (2 * TEXTFIELD_BORDER_WIDTH) + (2 * TEXTFIELD_OFFSET);
    } else {
	Pad_Component::Minimum_size(dimension);
    }
}

//
// Return the preferred (relaxed) dimension of the component 
//
void
Pad_TextField::Preferred_size(Pad_Dimension &dimension)
{
    Minimum_size(dimension);
}

//
// Inherit from component to interact with reshape properly
//
void 
Pad_TextField::Compute_bounding_box(void)
{
    Pad_Component::Compute_bounding_box();
}

//
// Set dims of textfield item.  Redefine to call Pad_Component
// because Pad_Text redefines it.
//
Pad_Bool
Pad_TextField::Set_width(float width)
{
    Pad_Component::Set_width(width);

    return(TRUE);
}

Pad_Bool
Pad_TextField::Set_height(float height)
{
    Pad_Component::Set_height(height);

    return(TRUE);
}

//
// This object no longer the focus object.
// Override so we don't get Pad_Text's definition
//
void
Pad_TextField::Unset_focus(void)
{
    Damage();
}

//
// Create a single text object from a string.
// Carriage returns are ignored.
//
Pad_Bool
Pad_TextField::Set_text(char *str)
{
    char *p;
    char *newStr;

    newStr = new char[strlen(str) + 1];
    p = newStr;
    while (*str) {
	if (*str == '\n') {
	    str++;
	} else {
	    *p++ = *str++;
	}
    }
    *p = '\0';
    Pad_Text::Set_text(newStr);
    delete [] newStr;

    return(TRUE);
}

//
// Insert <str> at <index>, removing carriage returns
//
void
Pad_TextField::Insert_chars(char *str, Pad_TextIndex &index, Pad_Bool reshape)
{
    char *p;
    char *newStr;

    newStr = new char[strlen(str) + 1];
    p = newStr;
    while (*str) {
	if (*str == '\n') {
	    str++;
	} else {
	    *p++ = *str++;
	}
    }
    *p = '\0';
    Pad_Text::Insert_chars(newStr, index, reshape);
    delete [] newStr;
}

//
// Delete text.  
//
void
Pad_TextField::Delete_chars(Pad_TextIndex &index1, Pad_TextIndex &index2, Pad_Bool)
{
    Pad_Text::Delete_chars(index1, index2, FALSE);  // Never reshape when inserting chars to a textarea
}

//
// TextFields default editable flag not necessarily the same as text
//
void 
Pad_TextField::Set_editable_default(void)
{
    Set_editable(TEXTFIELD_DEFAULT_EDITABLE);
    textFlags &= ~TEXT_EDITABLE_SET;
}

//
// Use a different default font size than regular text
//
void
Pad_TextField::Set_font_default(void)
{
    _font.Set(COMPONENT_DEFAULT_FONT);
    _componentFlags &= ~COMPONENT_FONT_SET;
}

//
// Gets called when textfield background is changed to 
// change the border color appropriately.
//
void
Pad_TextField::Fill_changed(void)
{
    int red, green, blue;

    Pad_Text::Fill_changed();

    _fillColor.Get(red, green, blue);
    _border.Set(red, green, blue);  // JM - was _border.Set(COMPONENT_DEFAULT_FILL);
}

void
Pad_TextField::Set_fill_default(void)
{
    Set_fill(TEXTAREA_DEFAULT_FILL);
    _componentFlags &= ~COMPONENT_FILL_SET;
}


//////////////////////////////////////////////////////////////
//
// Pad_TextArea class
//
// A Pad_TextArea is actually container that contains the
// actual text area (a _TextArea) along with two scrollbars.
//
//////////////////////////////////////////////////////////////

Pad_TextArea::Pad_TextArea(Pad *pad) :
Pad_Container(pad),
_textArea(pad),
_horScrollbar(pad),
_verScrollbar(pad)
{
				// Initialize default event handlers for textarea
    //_Initialize_events(pad->view->win);

    _type = PAD_TEXTAREA;

    Set_divisible(FALSE);
    Add(&_horScrollbar, FALSE);
    Add(&_verScrollbar, FALSE);
    Add(&_textArea, FALSE);
    _textArea._container = this;

    _verCallback = new Pad_Callback(Ver_scrollbar_callback, this);
    _verScrollbar.Set_command_callback(_verCallback);
    _verScrollbar.Set_from(0.0f);
    _verScrollbar.Set_line_size(1.0f);
    _verScrollbar.Set_orientation(Pad_Scrollbar::VERTICAL);
    _verScrollbar.Set_findable(FALSE);

    _horCallback = new Pad_Callback(Hor_scrollbar_callback, this);
    _horScrollbar.Set_command_callback(_horCallback);
    _horScrollbar.Set_from(0.0f);
    _horScrollbar.Set_line_size(1.0f);
    _horScrollbar.Set_findable(FALSE);

    _scrollbarsNeedUpdating = TRUE;

    _hiliteWidth = 0;		// The container doesn't get a hilite
    _textArea.Set_findable(FALSE);

    Update();

    Delete_all_tags();
    Add_tag("Textarea");
}

Pad_TextArea::~Pad_TextArea()
{
    Remove(&_horScrollbar, FALSE);
    Remove(&_verScrollbar, FALSE);
    Remove(&_textArea, FALSE);

				// Don't delete callback - it gets deleted by scrollbar
    _verCallback = NULL;
}

//
// Render a Pad_TextArea
//
Pad_Bool
Pad_TextArea::Render(void)
{
    Pad_BBox bb;
    Pad_Point min, max;

    if (_scrollbarsNeedUpdating) {
	_Update_scrollbars();
    }

    if (_textArea._border.Is_set()) {
	Get_bbox(bb);
	min.Set(bb.Xmin() + _hiliteWidth, bb.Ymin() + _hiliteWidth);
	max.Set(bb.Xmax() - _hiliteWidth, bb.Ymax() - _hiliteWidth);
	Pad_renderer->Set_border(_textArea._border);
	Pad_renderer->Draw_filled_3d_rectangle(min, max, 0, PAD_RELIEF_FLAT);
    }

    return(Pad_Container::Render());
}

//
// Returns text object associated with this object
//
Pad_Text *
Pad_TextArea::Get_text_obj(void)
{
    return(&_textArea);
}

// 
// Return the component under the specified point
// (in local coordinates)
//
Pad_TextArea::Component
Pad_TextArea::Pick_component(Pad_Point &pt)
{
    Pad_BBox bb;
    Component component = NONE;

    _textArea.Get_global_bbox(bb);
    if (bb.Contains(pt)) {
	component = TEXTAREA;
    } else {
	_horScrollbar.Get_global_bbox(bb);
	if (bb.Contains(pt)) {
	    component = HOR_SCROLLBAR;
	} else {
	    _verScrollbar.Get_global_bbox(bb);
	    if (bb.Contains(pt)) {
		component = VER_SCROLLBAR;
	    }
	}
    }

    return(component);
}

//
// Calculate new scrollbar positions and lengths
//
void
Pad_TextArea::_Update_scrollbars(void)
{
    float height;
    float bbHeight;
    Pad_BBox bb;

    _scrollbarsNeedUpdating = FALSE;

    Pad_renderer->Set_font(_font);
    height = Pad_renderer->Get_font_height();
    Get_bbox(bb);
    bbHeight = bb.Height();

    _verScrollbar.Set_to(_textArea.lines - 1);
    _verScrollbar.Set_page_size(bbHeight / height);
    _verScrollbar.Set_value(_textArea.lines - _textArea.point.line);

    _horScrollbar.Set_to(1.0);
    _horScrollbar.Set_page_size(1.0);
    _horScrollbar.Set_value(0.0);
}

//
// Callback to handle vertical scrollbar movement.
// Update the text to match the scrollbar
//
int
Pad_TextArea::Ver_scrollbar_callback(Pad_Object *, ClientData clientData, Pad_Event *)
{
    int type;
    Pad_TextArea *textArea;

    type = ((Pad_Object *)clientData)->Type();
    if (type == PAD_TEXTAREA) {
	textArea = (Pad_TextArea *)clientData;

	textArea->_textArea.point.line = (int)(textArea->_textArea.lines - textArea->_verScrollbar.Get_value() - 1);
	textArea->_textArea.Round_index(&textArea->_textArea.point);
	textArea->_textArea.Update_offset();
	textArea->_textArea.Damage();
    }

    return(PAD_OK);
}

//
// Callback to handle horizontal scrollbar movement.
// Update the text to match the scrollbar
//
int
Pad_TextArea::Hor_scrollbar_callback(Pad_Object *, ClientData clientData, Pad_Event *)
{
    int type;
    Pad_TextArea *textArea;

    type = ((Pad_Object *)clientData)->Type();
    if (type == PAD_TEXTAREA) {
	textArea = (Pad_TextArea *)clientData;

	textArea->_textArea.point.chr = (int)(textArea->_horScrollbar.Get_value() - 1);
	textArea->_textArea.Round_index(&textArea->_textArea.point);
	textArea->_textArea.Update_offset();
	textArea->_textArea.Damage();
    }

    return(PAD_OK);
}

//
// Functions to access the underlying _TextArea component
//
void
Pad_TextArea::Select(int selStart, int selEnd)
{
    _textArea.Select(selStart, selEnd);
}

int
Pad_TextArea::Get_selection_end(void)
{
    return(_textArea.Get_selection_end());
}

int
Pad_TextArea::Get_selection_start(void)
{
    return(_textArea.Get_selection_start());
}

void
Pad_TextArea::Set_editable(Pad_Bool editable)
{
    _textArea.Set_editable(editable);
}

Pad_Bool
Pad_TextArea::Get_text(Pad_String &text)
{
    _textArea.Get_text(text);
    return(TRUE);
}

Pad_Bool
Pad_TextArea::Set_text(char *buf)
{
    Pad_Bool rc;

    rc = _textArea.Set_text(buf);
    _scrollbarsNeedUpdating = TRUE; // Update scrollbars to match text

    return(rc);
}

void
Pad_TextArea::Insert_chars(char *str, Pad_TextIndex &index, Pad_Bool)
{
    _textArea.Insert_chars(str, index, FALSE);  // Never reshape when inserting chars to a textarea
    _scrollbarsNeedUpdating = TRUE; // Update scrollbars to match text
}

void
Pad_TextArea::Delete_chars(Pad_TextIndex &index1, Pad_TextIndex &index2, Pad_Bool)
{
    _textArea.Delete_chars(index1, index2, FALSE);  // Never reshape when inserting chars to a textarea
    _scrollbarsNeedUpdating = TRUE; // Update scrollbars to match text
}

//
// Set bounding box of component to specification
//
void
Pad_TextArea::Reshape(float x, float y, float width, float height)
{
    float sb = TEXTAREA_SCROLLBAR_THICKNESS;
    float hl = _hiliteWidth;
    float compX, compY;

    Pad_Container::Reshape(x + hl, y + hl, width - (2 * hl), height - (2 * hl));

    compX = hl;
    compY = hl + sb;
    _textArea.Set_rel_position_xy(compX, compY);
    _textArea.Reshape(compX, compY, width - sb - (2 * hl), height - sb - (2 * hl));

    compX = hl;
    compY = hl;
    _horScrollbar.Set_rel_position_xy(compX, compY);
    _horScrollbar.Reshape(compX, compY, width - sb - (2 * hl), sb);

    compX = width - sb - (2 * hl);
    compY = sb + hl;
    _verScrollbar.Set_rel_position_xy(compX, compY);
    _verScrollbar.Reshape(compX, compY, sb, height - sb - (2 * hl));

    _scrollbarsNeedUpdating = TRUE; // Update scrollbars to match text
}

//
// Return the minimum dimensions necessary for the component to look normal
//
void
Pad_TextArea::Minimum_size(Pad_Dimension &dimension)
{
    float sb = TEXTAREA_SCROLLBAR_THICKNESS;

    _textArea.Minimum_size(dimension);
    dimension.width += sb;
    dimension.height += sb;
}

//
// Return the minimum size for the specified # of columns
//
void
Pad_TextArea::Minimum_size(Pad_Dimension &dimension, int rows, int cols)
{
    float sb = TEXTAREA_SCROLLBAR_THICKNESS;

    _textArea.Minimum_size(dimension, rows, cols);
    dimension.width += sb;
    dimension.height += sb;
}

//
// Return the preferred (relaxed) dimension of the component 
//
void
Pad_TextArea::Preferred_size(Pad_Dimension &dimension)
{
    Minimum_size(dimension);
}

//////////////////////////////////////////////////////////////
//
// _TextArea class
//
// This is the "meat" of the text area, and is an embedded
// component of the Pad_TextArea class which is a container
// that contains one of these as well as two scrollbars.
//
//////////////////////////////////////////////////////////////

_TextArea::_TextArea(Pad *pad) :
Pad_TextField(pad)
{
				// Initialize default event handlers for textareacomp
    //_Initialize_events(pad->view->win);

    _type = PAD_TEXTAREACOMP;
    _textareaFlags = PAD_NO_MASK;
    _container = NULL;

    Update();

    Delete_all_tags();
    Add_tag("Textarea");
}

_TextArea::~_TextArea()
{
}

//
// Render a _TextArea
//
Pad_Bool
_TextArea::Render(void)
{
    Pad_TextField::Render();

    return(TRUE);
}

//
// Set the text.  Bypass TextField since that removes carriage returns
//
Pad_Bool
_TextArea::Set_text(char *buf)
{
    return(Pad_Text::Set_text(buf));
}

//
// Insert text.  Bypass TextField since that removes carriage returns
//
void
_TextArea::Insert_chars(char *str, Pad_TextIndex &index, Pad_Bool)
{
    Pad_Text::Insert_chars(str, index, FALSE);   // Never reshape when inserting chars to a textarea
}

//
// Return the minimum dimensions necessary for the component to look normal
//
void
_TextArea::Minimum_size(Pad_Dimension &dimension)
{
    if (_font.Is_set()) {
	Pad_renderer->Set_font(_font);
	dimension.width = TEXTAREA_DEFAULT_WIDTH;
	dimension.height = TEXTAREA_DEFAULT_ROWS * 
	    ((Pad_renderer->Get_font_height() * 1.3) +
	     (2 * TEXTFIELD_BORDER_WIDTH) + (2 * TEXTFIELD_OFFSET));
    } else {
	Pad_Component::Minimum_size(dimension);
    }
}

//
// Return the minimum size for the specified # of columns (and rows)
//
void
_TextArea::Minimum_size(Pad_Dimension &dimension, int cols)
{
    Minimum_size(dimension, 1, cols);
}

void
_TextArea::Minimum_size(Pad_Dimension &dimension, int rows, int cols)
{
    if (_font.Is_set()) {
	Pad_renderer->Set_font(_font);
	dimension.width = (cols * Pad_renderer->Get_font_height()) +
	    (2 * TEXTAREA_BORDER_WIDTH) + (2 * TEXTAREA_OFFSET);
	dimension.height = rows *
	    ((Pad_renderer->Get_font_height() * 1.3) +
	     (2 * TEXTFIELD_BORDER_WIDTH) + (2 * TEXTFIELD_OFFSET));
    } else {
	Pad_Component::Minimum_size(dimension);
    }
    Minimum_size(dimension);
}

//
// Return the preferred (relaxed) dimension of the component 
//
void
_TextArea::Preferred_size(Pad_Dimension &dimension)
{
    Minimum_size(dimension);
}

//////////////////////////////////////////////////////////////
// Definitions of Text default event handlers
//////////////////////////////////////////////////////////////

//
// Call this once to define event handlers for all Texts
//
void
Pad_Text::_Initialize_events(Pad_Win *win)
{
    Pad_Callback *callback;
    static Pad_List initializedWindows;

				// If already initialized for this window, then return.
				// Else, add to list of initialized windows and initialize.
    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Button Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Text"), "<Run-ButtonPress-1>", callback);

				// Button Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Text"), "<Run-B1-Motion>", callback);

				// Button Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Text"), "<Run-ButtonRelease-1>", callback);

				// Button-2 Paste
    callback = new Pad_Callback(Press2, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Text"), "<Run-ButtonPress-2>", callback);

				// Button-2 Drag
    callback = new Pad_Callback(Drag2, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Text"), "<Run-B2-Motion>", callback);

				// Button-2 Release
    callback = new Pad_Callback(Release2, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Text"), "<Run-ButtonRelease-2>", callback);

				// Key Press
    callback = new Pad_Callback(Key_press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Text"), "<Run-KeyPress>", callback);
}

static Pad_TextIndex *scrollSelStart = NULL;
static Pad_TimerToken textTimer = NULL;	  // Timer for auto-scrolling
static Pad_Point textPoint;               // Point to base auto-scrolling on

//
// Timer callback to auto-scroll text
//
static void
Text_scroll_callback(void *clientData) 
{
    Pad_BBox bb;
    Pad_Text *text = (Pad_Text *)clientData;
    Pad_TextIndex index = text->point;

    if (scrollSelStart) {
	text->Get_global_bbox(bb);

	if (textPoint.y < bb.Ymin()) {
				// Scroll down
	    if (index.line < (text->lines - 1)) {
		index.line++;
	    }
	} else if (textPoint.y > bb.Ymax()) {
				// Scroll up
	    if (index.line > 0) {
		index.line--;
	    }
	} else if (textPoint.x < bb.Xmin()) {
				// Scroll left
	    if (index.chr > 0) {
		index.chr--;
	    }
	} else if (textPoint.x > bb.Xmax()) {
				// Scroll right
	    if (index.chr < (text->text[index.line].Length() - 1)) {
		index.chr++;
	    }
	}
				// Make sure new point isn't past end of line
	if (index.chr > text->text[index.line].Length()) {
	    index.chr = text->text[index.line].Length();
	}

	text->point = index;	// Update point

	text->Select(*scrollSelStart, index);
	
	text->Update_offset();
	text->Damage();
    }

    textTimer = Pad_CreateTimerHandler(100, Text_scroll_callback, text);
}
	
//
// Click on Text to set focus
//
int
Pad_Text::Press(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Text *text;
    Pad_Text *selection;

    if (obj->Is_text()) {
	text = (Pad_Text *)obj;

	if (!text->Get_editable()) {
	    return(PAD_BREAK);
	}

	text->pad->view->Update_focus(obj);
	text->Get_index_from_xy(padEvent->pt.x, padEvent->pt.y, &text->point);

				// Create timer for auto-scrolling
	textTimer = Pad_CreateTimerHandler(100, Text_scroll_callback, text);
	textPoint = padEvent->pt;	// Update auto-scroll point

				// If clicked on object that has selection,
				// then reset selection.
	if (Pad_selection) {
	    if (Pad_selection->Is_text()) {
		selection = (Pad_Text *)Pad_selection;
		if (selection == text) {
		    selection->Select(text->point, text->point);
		}
	    }
	}

	text->Damage();
    }

    return(PAD_BREAK);
}

int
Pad_Text::Drag(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Text *text;

    if (obj->Is_text()) {
	text = (Pad_Text *)obj;

	if (!text->Get_editable()) {
	    return(PAD_BREAK);
	}

	Pad_TextIndex index(text);
	text->Get_index_from_xy(padEvent->pt.x, padEvent->pt.y, &index);
	text->point = index;

				 // Update auto-scroll point
				 // The auto-scroll code will get called by a timer
				 // started by the button-press.
	textPoint = padEvent->pt;

				// Remember selection start for dragging
	if (!scrollSelStart) {
	    scrollSelStart = new Pad_TextIndex(index);
	    text->Select(*scrollSelStart, *scrollSelStart);
	} else {
	    text->Select(*scrollSelStart, index);
	}
    }

    return(PAD_BREAK);
}

int
Pad_Text::Release(Pad_Object *, ClientData, Pad_Event *)
{
    if (scrollSelStart) {
	delete scrollSelStart;
	scrollSelStart = NULL;
    }
    if (textTimer) {
	Pad_DeleteTimerHandler(textTimer);
	textTimer = NULL;
    }

    return(PAD_BREAK);
}

//
// Click with button-2 to paste
// Define motion and release to stop other event handlers
// from being called
//
static Pad_Bool pasting = FALSE;

int
Pad_Text::Press2(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
    Pad_Text *text;
    Pad_Text *selection = NULL;

    if (obj->Is_text()) {
	text = (Pad_Text *)obj;
	if (!text->Get_editable()) {
	    return(PAD_OK);
	}

	if (Pad_selection) {
	    if (Pad_selection->Is_text()) {
		selection = (Pad_Text *)Pad_selection;
	    }
	}
	if (!selection || (selection->selStart == selection->selEnd)) {
				// Nothing to paste - continue along
	    return(PAD_OK);
	} else {
				// Paste selection at pointer
	    Pad_String str;
	    Pad_TextIndex index(text);
	    text->Get_index_from_xy(padEvent->pt.x, padEvent->pt.y, &index);
	    selection->Get_selection(str);
	    text->Insert_chars(str.Get(), index);
	    pasting = TRUE;
	    return(PAD_BREAK);
	}
    }
    return(PAD_OK);
}

int
Pad_Text::Drag2(Pad_Object *, ClientData, Pad_Event *)
{
    if (pasting) {
	return(PAD_BREAK);
    } else {
	return(PAD_OK);
    }
}

int
Pad_Text::Release2(Pad_Object *, ClientData, Pad_Event *)
{
    if (pasting) {
	pasting = FALSE;
	return(PAD_BREAK);
    } else {
	return(PAD_OK);
    }
}

//
// Key bindings for modifying text
//
int
Pad_Text::Key_press(Pad_Object *obj, ClientData, Pad_Event *padEvent)
{
#define KEYSTRINGLEN 20
    KeySym keysym;

    char keystring[KEYSTRINGLEN];
    Pad_TextIndex *point;
    Pad_Text *text;
    Pad_Bool reshape = TRUE;

    if (obj->Is_text()) {
	text = (Pad_Text *)obj;

	if (text->_componentFlags & COMPONENT_RESHAPE_SET) {
	    reshape = FALSE;
	}

	if (!text->Get_editable()) {
	    return(PAD_BREAK);
	}

	point = &text->point;
	Pad_TextIndex index(text), index2(text);

	XLookupString(&padEvent->eventPtr->xkey, keystring, KEYSTRINGLEN,
		      &keysym, (XComposeStatus *)NULL);

	if ((padEvent->eventPtr->xkey.state == 0) ||
	    (padEvent->eventPtr->xkey.state & ShiftMask)) {
	    switch (keysym) {
	    case XK_Return:
		text->Get_index_from_string("point", &index);
		text->Insert_chars("\n", index, reshape);
		text->Select(0, 0);
		text->Update_offset();
		break;
	    case XK_BackSpace:
	    case XK_Delete:
		if (text->selStart == text->selEnd) {
				// Nothing selected
		    if ((point->line > 0) || (point->chr > 0)) {
			text->Get_index_from_string("point - 1 char", &index);
			index2.line = index.line;
			index2.chr = index.chr + 1;
			text->Round_index(&index2);
			text->Delete_chars(index, index2);
		    }
		} else {
				// Something selected, delete it
		    text->Delete_chars(text->selStart, text->selEnd);
		    text->Select(0, 0);
		}
		break;
	    case XK_Left:
		if (padEvent->eventPtr->xkey.state & ShiftMask) {
				// Shift key pressed
		    if (text->selStart == text->selEnd) {
			text->selStart = point;
			text->selEnd = point;
		    }
		    point->chr--;
		    text->Round_index(point);
		    text->Select(text->selStart, *point);
		} else {
				// No shift key
		    text->Select(0, 0);
		    text->Get_index_from_string("point - 1 char", &index);
		    *point = index;
		}
		break;
	    case XK_Right:
		if (padEvent->eventPtr->xkey.state & ShiftMask) {
				// Shift key pressed
		    if (text->selStart == text->selEnd) {
			text->selStart = point;
			text->selEnd = point;
		    }
		    point->chr++;
		    text->Round_index(point);
		    text->Select(text->selStart, *point);
		} else {
				// No shift key
		    text->Select(0, 0);
		    text->Get_index_from_string("point + 1 char", &index);
		    *point = index;
		}
		break;
	    case XK_Up:
		if (padEvent->eventPtr->xkey.state & ShiftMask) {
				// Shift key pressed
		    if (text->selStart == text->selEnd) {
			text->selStart = point;
			text->selEnd = point;
		    }
		    point->line--;
		    text->Round_index(point);
		    text->Select(text->selStart, *point);
		} else {
				// No shift key
		    text->Select(0, 0);
		    text->Get_index_from_string("point - 1 line", &index);
		    *point = index;
		}
		break;
	    case XK_Down:
		if (padEvent->eventPtr->xkey.state & ShiftMask) {
				// Shift key pressed
		    if (text->selStart == text->selEnd) {
			text->selStart = point;
			text->selEnd = point;
		    }
		    point->line++;
		    text->Round_index(point);
		    text->Select(text->selStart, *point);
		} else {
				// No shift key
		    text->Select(0, 0);
		    text->Get_index_from_string("point + 1 line", &index);
		    *point = index;
		}
		break;
	    case XK_a: case XK_b: case XK_c: case XK_d: case XK_e: case XK_f: 
	    case XK_g: case XK_h: case XK_i: case XK_j: case XK_k: case XK_l: 
	    case XK_m: case XK_n: case XK_o: case XK_p: case XK_q: case XK_r: 
	    case XK_s: case XK_t: case XK_u: case XK_v: case XK_w: case XK_x: 
	    case XK_y: case XK_z:
	    case XK_A: case XK_B: case XK_C: case XK_D: case XK_E: case XK_F: 
	    case XK_G: case XK_H: case XK_I: case XK_J: case XK_K: case XK_L: 
	    case XK_M: case XK_N: case XK_O: case XK_P: case XK_Q: case XK_R: 
	    case XK_S: case XK_T: case XK_U: case XK_V: case XK_W: case XK_X: 
	    case XK_Y: case XK_Z:
	    case XK_0: case XK_1: case XK_2: case XK_3: case XK_4: case XK_5: 
	    case XK_6: case XK_7: case XK_8: case XK_9:
	    case XK_space: case XK_exclam: case XK_quotedbl: case XK_numbersign:
	    case XK_dollar: case XK_percent: case XK_ampersand: case XK_apostrophe:
	    case XK_parenleft: case XK_parenright: case XK_asterisk:
	    case XK_plus: case XK_comma: case XK_minus: case XK_period: case XK_slash:
	    case XK_colon: case XK_semicolon: case XK_less: case XK_equal: case XK_greater:
	    case XK_question: case XK_at: case XK_bracketleft: case XK_backslash: 
	    case XK_bracketright: case XK_asciicircum: case XK_underscore: case XK_grave:
	    case XK_braceleft: case XK_bar: case XK_braceright: case XK_asciitilde:
		if (text->selStart != text->selEnd) {
				// Something selected, delete it
		    text->Delete_chars(text->selStart, text->selEnd);
		    text->Select(0, 0);
		}

		keystring[1] = '\0';
		text->Get_index_from_string("point", &index);
		text->Insert_chars(keystring, index, reshape);
		text->Select(0, 0);
		break;
	    }
	} else if (padEvent->eventPtr->xkey.state & ControlMask) {
				// 
				// Control keys
				// 
	    switch (keysym) {
	    case XK_a:		// Beginning of line
		text->Get_index_from_string("point linestart", &index);
		*point = index;
		break;
	    case XK_b:		// Back one char
		text->Get_index_from_string("point - 1 char", &index);
		*point = index;
		break;
	    case XK_d:		// Delete current character
		text->Get_index_from_string("point", &index);
		index2.line = index.line;
		index2.chr = index.chr + 1;
		text->Round_index(&index2);
		text->Delete_chars(index, index2);
		break;
	    case XK_e:		// End of line
		text->Get_index_from_string("point lineend", &index);
		*point = index;
		break;
	    case XK_f:		// Forward one char
		text->Get_index_from_string("point + 1 char", &index);
		*point = index;
		break;
	    case XK_k:		// Delete from cursor to end of line
		if (text->point.chr == text->text[text->point.line].Length()) {
				// At end of line, delete carriage return
		    text->Get_index_from_string("point lineend + 1 char", &index);
		} else {
				// Not at end of line, delete to end of line
		    text->Get_index_from_string("point lineend", &index);
		}
		text->Delete_chars(text->point, index);
		break;
	    case XK_n:		// Down one line
		text->Get_index_from_string("point + 1 line", &index);
		*point = index;
		break;
	    case XK_p:		// up one line
		text->Get_index_from_string("point - 1 line", &index);
		*point = index;
		break;
	    }
	} else if (padEvent->eventPtr->xkey.state & Mod1Mask) {
				// 
				// Meta keys
    			// 

	    switch (keysym) {
	    case XK_f:		// Next word
		text->Get_index_from_string("point + 1 char wordend", &index);
		*point = index;
		break;
	    case XK_b:		// Previous word
		text->Get_index_from_string("point - 1 char wordstart", &index);
		*point = index;
		break;
	    }
	}

	text->Update_offset();

	text->Damage();
    }

    return(PAD_BREAK);
}

//////////////////////////////////////////////////////////////
// Definitions of Pad_TextField default event handlers
//////////////////////////////////////////////////////////////

//
// Call this once to define event handlers for all Pad_TextField's
//
void
Pad_TextField::_Initialize_events(Pad_Win *win)
{
    Pad_Callback *callback;
    static Pad_List initializedWindows;

				// If already initialized for this window, then return.
				// Else, add to list of initialized windows and initialize.
    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Button Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textfield"), "<Run-ButtonPress-1>", callback);

				// Button Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textfield"), "<Run-B1-Motion>", callback);

				// Button Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textfield"), "<Run-ButtonRelease-1>", callback);

				// Button-2 Paste
    callback = new Pad_Callback(Press2, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textfield"), "<Run-ButtonPress-2>", callback);

				// Button-2 Drag
    callback = new Pad_Callback(Drag2, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textfield"), "<Run-B2-Motion>", callback);

				// Button-2 Release
    callback = new Pad_Callback(Release2, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textfield"), "<Run-ButtonRelease-2>", callback);

				// Key Press
    callback = new Pad_Callback(Key_press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textfield"), "<Run-KeyPress>", callback);
}

//
// Bindings for modifying textfields
//
int
Pad_TextField::Press(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    int rc = PAD_BREAK;
    Pad_TextField *textfield;

    if (obj->Type() == PAD_TEXTFIELD) {
	textfield = (Pad_TextField *)obj;

	rc = Pad_Text::Press(obj, clientData, padEvent);
    }

    return(rc);
}

int
Pad_TextField::Drag(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    int rc = PAD_BREAK;
    Pad_TextField *textfield;

    if (obj->Type() == PAD_TEXTFIELD) {
	textfield = (Pad_TextField *)obj;

	rc = Pad_Text::Drag(obj, clientData, padEvent);
    }

    return(rc);
}

int
Pad_TextField::Release(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    int rc = PAD_BREAK;
    Pad_TextField *textfield;

    if (obj->Type() == PAD_TEXTFIELD) {
	textfield = (Pad_TextField *)obj;

	rc = Pad_Text::Release(obj, clientData, padEvent);
    }

    return(rc);
}

int
Pad_TextField::Press2(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    int rc = PAD_BREAK;
    Pad_TextField *textfield;

    if (obj->Type() == PAD_TEXTFIELD) {
	textfield = (Pad_TextField *)obj;

	rc = Pad_Text::Press2(obj, clientData, padEvent);
    }

    return(rc);
}

int
Pad_TextField::Drag2(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    int rc = PAD_BREAK;
    Pad_TextField *textfield;

    if (obj->Type() == PAD_TEXTFIELD) {
	textfield = (Pad_TextField *)obj;

	rc = Pad_Text::Drag2(obj, clientData, padEvent);
    }

    return(rc);
}

int
Pad_TextField::Release2(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    int rc = PAD_BREAK;
    Pad_TextField *textfield;

    if (obj->Type() == PAD_TEXTFIELD) {
	textfield = (Pad_TextField *)obj;

	rc = Pad_Text::Release2(obj, clientData, padEvent);
    }

    return(rc);
}

int
Pad_TextField::Key_press(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
#define KEYSTRINGLEN 20
    int rc = PAD_OK;
    KeySym keysym;
    char keystring[KEYSTRINGLEN];
    Pad_TextField *textfield;

    if (obj->Type() == PAD_TEXTFIELD) {
	textfield = (Pad_TextField *)obj;

	if (!textfield->Get_editable()) {
	    return(PAD_BREAK);
	}

	XLookupString(&padEvent->eventPtr->xkey, keystring, KEYSTRINGLEN,
		      &keysym, (XComposeStatus *)NULL);


	switch (keysym) {
	  case XK_Return:
	    rc = PAD_BREAK;
	    break;
	  default:
	    rc = Pad_Text::Key_press(obj, clientData, padEvent);
	    break;
	}
    }
    return(rc);
}

//////////////////////////////////////////////////////////////
// Definitions of Pad_TextArea default event handlers
//////////////////////////////////////////////////////////////

//
// Call this once to define event handlers for all Pad_TextArea's
//
void
Pad_TextArea::_Initialize_events(Pad_Win *win)
{
    Pad_Callback *callback;
    static Pad_List initializedWindows;

				// If already initialized for this window, then return.
				// Else, add to list of initialized windows and initialize.
    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Button Press
    callback = new Pad_Callback(Press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textarea"), "<Run-ButtonPress-1>", callback);

				// Button Drag
    callback = new Pad_Callback(Drag, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textarea"), "<Run-B1-Motion>", callback);

				// Button Release
    callback = new Pad_Callback(Release, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textarea"), "<Run-ButtonRelease-1>", callback);

				// Button-2 Paste
    callback = new Pad_Callback(Press2, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textarea"), "<Run-ButtonPress-2>", callback);

				// Button-2 Drag
    callback = new Pad_Callback(Drag2, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textarea"), "<Run-B2-Motion>", callback);

				// Button-2 Release
    callback = new Pad_Callback(Release2, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textarea"), "<Run-ButtonRelease-2>", callback);

				// Key Press
    callback = new Pad_Callback(Key_press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textarea"), "<Run-KeyPress>", callback);
}

static Pad_TextArea::Component textAreaComponent = Pad_TextArea::NONE;

//
// Helper function to apply the object's transformation
// to the event, putting the result in the events object coords.
//
static void
_Transform_event(Pad_Object *obj, Pad_Event *padEvent)
{
    Pad_Point pt;
    float mag;

    pt = padEvent->pt;
    mag = padEvent->mag;
    padEvent->pt = padEvent->objPt;
    padEvent->mag = padEvent->objMag;
    obj->transform.Invert(padEvent);
    padEvent->objPt = padEvent->pt;
    padEvent->objMag = padEvent->mag;
    padEvent->pt = pt;
    padEvent->mag = mag;
}

//
// Textarea event bindings.  Send them on to the appropriate sub-component
//
int
Pad_TextArea::Press(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    Pad_Bool rc = PAD_BREAK;
    Pad_TextArea *textArea;

    if (obj->Type() == PAD_TEXTAREA) {
	textArea = (Pad_TextArea *)obj;
	textAreaComponent = textArea->Pick_component(padEvent->pt);

	switch (textAreaComponent) {
	  case TEXTAREA:
	    rc = Pad_Text::Press(&textArea->_textArea, clientData, padEvent);
	    break;
	  case HOR_SCROLLBAR:
	    _Transform_event(&textArea->_horScrollbar, padEvent);
	    rc = Pad_Scrollbar::Press(&textArea->_horScrollbar, clientData, padEvent);
	    break;
 	  case VER_SCROLLBAR:
	    _Transform_event(&textArea->_verScrollbar, padEvent);
	    rc = Pad_Scrollbar::Press(&textArea->_verScrollbar, clientData, padEvent);
	    break;
	  default:
	    rc = PAD_BREAK;
	    break;
	}
    }

    return(rc);
}

int
Pad_TextArea::Drag(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    Pad_Bool rc = PAD_BREAK;
    Pad_TextArea *textArea;

    if (obj->Type() == PAD_TEXTAREA) {
	textArea = (Pad_TextArea *)obj;

	switch (textAreaComponent) {
	  case TEXTAREA:
	    rc = Pad_Text::Drag(&textArea->_textArea, clientData, padEvent);
	    break;
	  case HOR_SCROLLBAR:
	    _Transform_event(&textArea->_horScrollbar, padEvent);
	    rc = Pad_Scrollbar::Drag(&textArea->_horScrollbar, clientData, padEvent);
	    break;
 	  case VER_SCROLLBAR:
	    _Transform_event(&textArea->_verScrollbar, padEvent);
	    rc = Pad_Scrollbar::Drag(&textArea->_verScrollbar, clientData, padEvent);
	    break;
	  default:
	    rc = PAD_BREAK;
	    break;
	}
    }

    return(rc);
}

int
Pad_TextArea::Release(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    Pad_Bool rc = PAD_BREAK;
    Pad_TextArea *textArea;

    if (obj->Type() == PAD_TEXTAREA) {
	textArea = (Pad_TextArea *)obj;

	switch (textAreaComponent) {
	  case TEXTAREA:
	    rc = Pad_Text::Release(&textArea->_textArea, clientData, padEvent);
	    break;
	  case HOR_SCROLLBAR:
	    _Transform_event(&textArea->_horScrollbar, padEvent);
	    rc = Pad_Scrollbar::Release(&textArea->_horScrollbar, clientData, padEvent);
	    break;
 	  case VER_SCROLLBAR:
	    _Transform_event(&textArea->_verScrollbar, padEvent);
	    rc = Pad_Scrollbar::Release(&textArea->_verScrollbar, clientData, padEvent);
	    break;
	  default:
	    rc = PAD_BREAK;
	    break;
	}
    }

    return(rc);
}

int
Pad_TextArea::Press2(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    Pad_Bool rc = PAD_BREAK;
    Pad_TextArea *textArea;

    if (obj->Type() == PAD_TEXTAREA) {
	textArea = (Pad_TextArea *)obj;
	textAreaComponent = textArea->Pick_component(padEvent->pt);

	switch (textAreaComponent) {
	  case TEXTAREA:
	    rc = Pad_Text::Press2(&textArea->_textArea, clientData, padEvent);
	    textArea->_scrollbarsNeedUpdating = TRUE; // Update scrollbars to match text
	    break;
	  case HOR_SCROLLBAR:
 	  case VER_SCROLLBAR:
	  default:
	    rc = PAD_OK;
	    break;
	}
    }

    return(rc);
}

int
Pad_TextArea::Drag2(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    Pad_Bool rc = PAD_BREAK;
    Pad_TextArea *textArea;

    if (obj->Type() == PAD_TEXTAREA) {
	textArea = (Pad_TextArea *)obj;

	switch (textAreaComponent) {
	  case TEXTAREA:
	    rc = Pad_Text::Drag2(&textArea->_textArea, clientData, padEvent);
	    break;
	  case HOR_SCROLLBAR:
 	  case VER_SCROLLBAR:
	  default:
	    rc = PAD_OK;
	    break;
	}
    }

    return(rc);
}

int
Pad_TextArea::Release2(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    Pad_Bool rc = PAD_BREAK;
    Pad_TextArea *textArea;

    if (obj->Type() == PAD_TEXTAREA) {
	textArea = (Pad_TextArea *)obj;

	switch (textAreaComponent) {
	  case TEXTAREA:
	    rc = Pad_Text::Release2(&textArea->_textArea, clientData, padEvent);
	    break;
	  case HOR_SCROLLBAR:
 	  case VER_SCROLLBAR:
	  default:
	    rc = PAD_OK;
	    break;
	}
    }

    return(rc);
}

int
Pad_TextArea::Key_press(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    int type;
    Pad_Bool rc = PAD_BREAK;
    Pad_TextArea *textArea;

    type = obj->Type();
    if (type == PAD_TEXTAREA) {
	textArea = (Pad_TextArea *)obj;
	rc = _TextArea::Key_press(&textArea->_textArea, clientData, padEvent);
    } else if (type == PAD_TEXTAREACOMP) {
	rc = _TextArea::Key_press(obj, clientData, padEvent);
    }

    return(rc);
}

//////////////////////////////////////////////////////////////
// Definitions of _TextArea default event handlers
//////////////////////////////////////////////////////////////

//
// Call this once to define event handlers for all _TextArea's
//
void
_TextArea::_Initialize_events(Pad_Win *win)
{
    Pad_Callback *callback;
    static Pad_List initializedWindows;

				// If already initialized for this window, then return.
				// Else, add to list of initialized windows and initialize.
    if (initializedWindows.Member(win)) {
        return;
    }
    initializedWindows.Push_last(win);

				// Key Press
    callback = new Pad_Callback(Key_press, win);
    Pad_CreateBinding(win->bindingTable, Pad_GetUid("Textarea"), "<Run-KeyPress>", callback);
}

//
// Key bindings for modifying textareas
//
int
_TextArea::Key_press(Pad_Object *obj, ClientData clientData, Pad_Event *padEvent)
{
    int rc = PAD_BREAK;
    _TextArea *textareacomp;

    if (obj->Type() == PAD_TEXTAREACOMP) {
	textareacomp = (_TextArea *)obj;
	textareacomp->_container->_scrollbarsNeedUpdating = TRUE; // Update scrollbars to match text
	rc = Pad_Text::Key_press(textareacomp, clientData, padEvent);
    }

    return(rc);
}
