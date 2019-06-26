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

#ifndef INCL_TEXT
#define INCL_TEXT

#include "defs.h"
#include "object.h"
#include "component.h"
#include "container.h"
#include "scrollbar.h"

#include <string.h>
#define TEXT_LEVELS 2		// Maximum refinement level for text

#define TEXT_LOD0_SIZE   3	// Size below which to display at in lowest resolution (in pixels)
#define TEXT_LOD1_SIZE   6	// Size below which to display at in middle resolution (in pixels)

#define TEXT_FAST_SIZE 20	// Size below which text can be rendered at lower resolution
				// if system is getting slow

				     // Bits for textFlags
#define TEXT_FILE_SET        (1<<0)  // Set if -file has been configured
#define TEXT_TEXT_SET        (1<<1)  // Set if -text has been configured
#define TEXT_WRITEFORMAT_SET (1<<2)  // Set if -writeformat has been configured
#define TEXT_EDITABLE_SET    (1<<3)  // Set if -editable has been configured
#define TEXT_EDITABLE        (1<<4)  // Set if text is editable

class Pad_String;
class Pad_TextArea;

class Pad_TextIndex
{
    friend ostream &operator<<(ostream &, Pad_TextIndex &);
  public:
    int line;			// Lines numbers starting at 0
    int chr;			// Char index numbers starting at 0
    Pad_String name;		// Name of index
    Pad_Text *text;		// Text object this index is associated with

    int  Get(void);		// Convert to single integer index from start of text

    Pad_TextIndex& operator=(int intIndex);
    Pad_TextIndex& operator=(Pad_TextIndex *index);
    Pad_TextIndex& operator=(Pad_TextIndex &index);
    Pad_Bool       operator<(Pad_TextIndex &index);
    Pad_Bool       operator<=(Pad_TextIndex &index);
    Pad_Bool       operator>(Pad_TextIndex &index);
    Pad_Bool       operator>=(Pad_TextIndex &index);
    Pad_Bool       operator==(Pad_TextIndex &index);
    Pad_Bool       operator!=(Pad_TextIndex &index);

    Pad_TextIndex(Pad_Text *newText);
    Pad_TextIndex(Pad_Text &newText);
    Pad_TextIndex(Pad_TextArea *newTextArea);
    Pad_TextIndex(Pad_TextIndex &index);
};

//
// A multi-line text object
//  
class Pad_Text : public Pad_Component
{
 public:
    unsigned char  textFlags;	// OR'd combination of flags
    int            lines;	// Number of lines of text
    Pad_String *   text;	// Array of strings
    Pad_TextIndex  point;	// Insertion point
    int            level;	// Render level (0-low res, 2-max res)
    Pad_HashTable *marks;	// Store marks, each containing an index
    Pad_ColorRef   cursorColor;	// Cursor color
    Pad_TextIndex  selStart;	// First point of selection
    Pad_TextIndex  selEnd;	// Last point of selection
    char           echoChar;    // Char to display instead of actual text ('\0' to display regular text)
    Pad_Bool       refineNeeded;// True if further refinement is needed after a render 
    float          renderedFontSize;  // Size of font as rendered for current render

    virtual void     Compute_bounding_box(void);
    virtual float    Compute_dimensions(float &global_width, float &global_height);
    virtual float    Compute_dimensions(Pad_Event *event, float &global_width, float &global_height);
    virtual float    Compute_dimensions(Pad_List &views, float &global_width, float &global_height);
            float    Compute_height(void);
    virtual float    Compute_scale_within_frame(float *frame);
            float    Compute_width(void);
    virtual Pad_Bool Contains_text(char *);
    virtual Pad_Bool Continue_refinement(void);
    virtual void     Pen_changed(void);
    virtual Pad_Bool Get_editable(void);
            void     Get_selection(Pad_String &text);  // Return current selection
            int      Get_selection_end(void);          // Return position of last character in selection
            int      Get_selection_start(void);        // Return position of first character in selection
    virtual Pad_Bool Get_text(Pad_String &text);
            float    Get_text_angle(void);
    virtual Pad_Text *Get_text_obj(void);
    virtual Pad_Bool Is_rotatable(void);
    virtual Pad_Bool Is_text(void);         // True if a type of text object (derives from Pad_Text)
            void     Render_cursor(void);
    virtual Pad_Bool Render(void);
    virtual Pad_Bool Render_medium(void);
    virtual Pad_Bool Render_fast(void);
    virtual Pad_Bool Rotate(float theta);
    virtual Pad_Bool Rotate(float theta, Pad_Point &);
            void     Select(int selStart, int selEnd); // Set selection (start=end means no selection)
            void     Select(Pad_TextIndex &selStart, Pad_TextIndex &selEnd);
            void     Set_echo_char(char c);            // Char to display instead of actual text
    virtual void     Set_editable(Pad_Bool editable);  // True if this object is editable
    virtual void     Set_editable_default(void);
    virtual void     Set_focus(void);                  // This object is now the focus object
    virtual void     Set_font_default(void);
    virtual Pad_Bool Set_height(float height);
    virtual Pad_Bool Set_text(char *new_text);
    virtual Pad_Bool Set_text_default(void);
    virtual Pad_Bool Set_width(float width);
    virtual void     Unset_focus(void);	                // This object is no longer the focus object
    virtual void     Update_offset(void);
        
				// Implementation of Tcl commands
            int Bbox_cmd(int argc, char **argv);
            int Compare(int argc, char **argv);
            int Del(int argc, char **argv);
            int Get(int argc, char **argv);
            int Index(int argc, char **argv);
            int Insert(int argc, char **argv);
            int Mark(int argc, char **argv);

				// Index commands
            int  Get_index_from_string(const char *string, Pad_TextIndex *index);
            void Get_index_from_xy(float padx, float pady, Pad_TextIndex *index);
            void Print_index(Pad_TextIndex &index, char *buf, int type);
            void Forward_chars(Pad_TextIndex *index, int count);
            void Backward_chars(Pad_TextIndex *index, int count);
            void Round_index(Pad_TextIndex *index);
            void Update_index(Pad_TextIndex &start_index, Pad_TextIndex &end_index);
            void Bbox_of_index(Pad_TextIndex &index, float *bb);
    virtual void Delete_chars(Pad_TextIndex &index1, Pad_TextIndex &index2, Pad_Bool reshape=TRUE);
    virtual void Insert_chars(char *str, Pad_TextIndex &index, Pad_Bool reshape=TRUE);
            void Get_chars(Pad_String &result, Pad_TextIndex &index1, Pad_TextIndex &index2);

				// Event handlers
    static Pad_EventCallbackProc(Press);
    static Pad_EventCallbackProc(Drag);
    static Pad_EventCallbackProc(Release);
    static Pad_EventCallbackProc(Press2);
    static Pad_EventCallbackProc(Drag2);
    static Pad_EventCallbackProc(Release2);
    static Pad_EventCallbackProc(Key_press);

    virtual ~Pad_Text();
    
    void Init(void);
    Pad_Text(Pad *pad);
    Pad_Text(Pad *pad, int, char *, float width, float char_pos);

    PAD_TYPE("text");

  protected:
    float _xoffset;
    float _yoffset;

  private:
    void  _Compute_dimensions(float &global_width, float &global_height);

    static void     _Initialize_events(Pad_Win *win);
};

class Pad_TextFile : public Pad_Text
{
  public:
    
    Pad_String *name;
    Pad_Bool    fileLoaded;
    int         writeFormat;

            char *   Get_file(void);
    virtual Pad_Bool Get_name(Pad_String &string);
    virtual int      Get_writeformat(void);
            void     Init(void);
            Pad_Bool Load_file(void);
    virtual Pad_Bool Render(void);
            Pad_Bool Set_file(char *filename);
            void     Set_file_default(void);
            Pad_Bool Set_text(char *new_text);
            Pad_Bool Set_writeformat(int);
            void     Set_writeformat_default(void);

    virtual ~Pad_TextFile();
    Pad_TextFile(Pad *pad);
    Pad_TextFile(Pad *pad, char *file_name);

    PAD_TYPE("textfile");
};

class Pad_TextField : public Pad_Text
{
    friend class Pad_TextArea;

  public:
    virtual Pad_Bool   Render(void);
    virtual void       Compute_bounding_box(void);
				// Nice dimensions
    virtual void       Minimum_size(Pad_Dimension &dimension);
    virtual void       Minimum_size(Pad_Dimension &dimension, int cols);
    virtual void       Preferred_size(Pad_Dimension &dimension);

    virtual Pad_Bool   Set_text(char *new_text);
    virtual void       Insert_chars(char *str, Pad_TextIndex &index, Pad_Bool reshape=TRUE);
    virtual void       Delete_chars(Pad_TextIndex &index1, Pad_TextIndex &index2, Pad_Bool reshape=TRUE);
    virtual void       Set_editable_default(void);
    virtual Pad_Bool   Set_width(float width);
    virtual Pad_Bool   Set_height(float height);
				// Special setters and getters for fill color
    virtual void       Fill_changed(void);
    virtual void       Set_fill_default(void);

    virtual void       Set_font_default(void);
				// Get and set options

    virtual void       Unset_focus(void); // This object is no longer the focus object

				// Event bindings
    static Pad_EventCallbackProc(Press);
    static Pad_EventCallbackProc(Drag);
    static Pad_EventCallbackProc(Release);
    static Pad_EventCallbackProc(Press2);
    static Pad_EventCallbackProc(Drag2);
    static Pad_EventCallbackProc(Release2);
    static Pad_EventCallbackProc(Key_press);

    virtual ~Pad_TextField();
    Pad_TextField(Pad *pad);

    PAD_TYPE("textfield");

  protected:
    unsigned char _textfieldFlags;
    Pad_BorderRef _border;              // 3D Bevel

  private:
    static void     _Initialize_events(Pad_Win *win);
};

//
// This is the "meat" of the text area, and is an embedded
// component of the Pad_TextArea class which is a container
// that contains one of these as well as two scrollbars.
//
class _TextArea : public Pad_TextField
{
  public:
    friend class Pad_TextArea;

    virtual Pad_Bool Render(void);
	
    virtual Pad_Bool Set_text(char *new_text);
    virtual void     Insert_chars(char *str, Pad_TextIndex &index, Pad_Bool reshape=TRUE);
				// Nice dimensions
    virtual void     Minimum_size(Pad_Dimension &dimension);
    virtual void     Minimum_size(Pad_Dimension &dimension, int cols);
    virtual void     Minimum_size(Pad_Dimension &dimension, int rows, int cols);
    virtual void     Preferred_size(Pad_Dimension &dimension);
				// Get and set options

				// Event handlers
    static Pad_EventCallbackProc(Key_press);
    
    virtual ~_TextArea();
    _TextArea(Pad *pad);
    
    PAD_TYPE("textareacomp");
    
  private:
    unsigned char   _textareaFlags;
    static void     _Initialize_events(Pad_Win *win);
    Pad_TextArea    *_container;
};

//
// A Pad_TextArea is actually a container that contains the
// actual text area (a _TextArea) along with two scrollbars.
//
class Pad_TextArea : public Pad_Container
{
  public:
    friend class Pad_TextIndex;
    friend class _TextArea;

    virtual Pad_Bool Render(void);

				// Return the component under the specified point
            enum Component {NONE, TEXTAREA, HOR_SCROLLBAR, VER_SCROLLBAR};
            Component  Pick_component(Pad_Point &pt);

				// Type conversion from Pad_TextArea to _TextArea component
            operator _TextArea() {return _textArea;}

				// Accessor functions for the underlying _TextArea
            void     Select(int selStart, int selEnd);
            int      Get_selection_end(void);         
            int      Get_selection_start(void);       
    virtual void     Set_editable(Pad_Bool editable); 
    virtual Pad_Bool Get_text(Pad_String &text);
    virtual Pad_Bool Set_text(char *new_text);
    virtual void     Delete_chars(Pad_TextIndex &index1, Pad_TextIndex &index2, Pad_Bool reshape=TRUE);
    virtual void     Insert_chars(char *str, Pad_TextIndex &index, Pad_Bool reshape=TRUE);
    virtual Pad_Text *Get_text_obj(void);
				// Nice dimensions
    virtual void     Minimum_size(Pad_Dimension &dimension);
    virtual void     Minimum_size(Pad_Dimension &dimension, int rows, int cols);
    virtual void     Preferred_size(Pad_Dimension &dimension);
    virtual void     Reshape(float x, float y, float width, float height);
				// Get and set options

				// Event handlers
    static Pad_EventCallbackProc(Press);
    static Pad_EventCallbackProc(Drag);
    static Pad_EventCallbackProc(Release);
    static Pad_EventCallbackProc(Press2);
    static Pad_EventCallbackProc(Drag2);
    static Pad_EventCallbackProc(Release2);
    static Pad_EventCallbackProc(Key_press);

    static Pad_EventCallbackProc(Hor_scrollbar_callback);
    static Pad_EventCallbackProc(Ver_scrollbar_callback);

    virtual ~Pad_TextArea();
    Pad_TextArea(Pad *pad);
	
    PAD_TYPE("textarea");

  protected:
    _TextArea      _textArea;
    Pad_Scrollbar  _horScrollbar;
    Pad_Scrollbar  _verScrollbar;
    
  private:
    static void    _Initialize_events(Pad_Win *win);
    void           _Update_scrollbars(void);  // Calculate new scrollbar positions

    Pad_Bool       _scrollbarsNeedUpdating;   // True when text has been modified, and scrollbars out of date
    Pad_Callback  *_verCallback;              // Callback when vertical scrollbar is move
    Pad_Callback  *_horCallback;              // Callback when horizontal scrollbar is move
};

#endif

