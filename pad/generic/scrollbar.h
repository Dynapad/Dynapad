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

#ifndef SCROLLBAR_H
#define SCROLLBAR_H 1

#include "defs.h"
#include "component.h"
#include "pad-string.h"
#include "callback.h"

				       // Bits for _scrollbarFlags
#define SCROLLBAR_HORIZONTAL      (1<<0)   // Set if scrollbar is horizontal
#define SCROLLBAR_FROM_SET        (1<<1)   // Set if -from is configured
#define SCROLLBAR_TO_SET          (1<<2)   // Set if -to is configured
#define SCROLLBAR_ORIENTATION_SET (1<<3)   // Set if -orientation is configured
#define SCROLLBAR_PAGESIZE_SET    (1<<4)   // Set if -pagesize is configured
#define SCROLLBAR_LINESIZE_SET    (1<<5)   // Set if -linesize is configured
#define SCROLLBAR_COMMAND_SET     (1<<6)   // Set if -command is configured

//
// A Pad_Scrollbar is a widget for manipulating a value
//
class Pad_Scrollbar : public Pad_Component {
  public:
    Pad_Scrollbar(Pad *pad);
    virtual ~Pad_Scrollbar();

    virtual Pad_Bool   Render(void);

    virtual void       Minimum_size(Pad_Dimension &dimension);
    virtual void       Preferred_size(Pad_Dimension &dimension);
    virtual void       Reshape(float x, float y, float width, float height);
    virtual Pad_Bool   Set_height(float height);
    virtual Pad_Bool   Set_width(float width);

				// Return the component under the specified point
            enum Component {NONE, FROM_ARROW, TO_ARROW, THUMB, TROUGH_FROM, TROUGH_TO};
            Component  Pick_component(Pad_Point &pt);

				// Set 3D background border from color
    virtual void       Fill_changed(void);

				// Orientation can be HORIZONTAL or VERTICAL
            enum {HORIZONTAL, VERTICAL};
            void       Set_orientation(int orientation);
            void       Set_orientation_default(void);
            int        Get_orientation(void);

				// Current value
            void       Set_value(float value);
            void       Set_value(Pad_Point &pt, Pad_Bool drag);
            void       Set_value_default(void);
            float      Get_value(void);

				// From and to values
            void       Set_from(float from);
            void       Set_from_default(void);
            float      Get_from(void);
            void       Set_to(float to);
            void       Set_to_default(void);
            float      Get_to(void);

				// Page size
            void       Set_page_size(float value);
            void       Set_page_size_default(void);
            float      Get_page_size(void);

				// Line size
            void       Set_line_size(float value);
            void       Set_line_size_default(void);
            float      Get_line_size(void);

				// Currently active component
            void       Set_active_component(Component component);
            Component  Get_active_component(void);

				// Interaction functions
            void       Line_up(Pad_Event *event);
            void       Line_down(Pad_Event *event);
            void       Page_up(Pad_Event *event);
            void       Page_down(Pad_Event *event);
            void       Position(Pad_Event *event, Pad_Bool drag);

				// Set and get callbacks
            void           Set_command_callback(Pad_Callback *command);
            void           Set_command_callback_default(void);
            Pad_Callback * Get_command_callback(void);
            void           Set_lineup_callback(Pad_Callback *command);
            Pad_Callback * Get_lineup_callback(void);
            void           Set_linedown_callback(Pad_Callback *command);
            Pad_Callback * Get_linedown_callback(void);
            void           Set_pageup_callback(Pad_Callback *command);
            Pad_Callback * Get_pageup_callback(void);
            void           Set_pagedown_callback(Pad_Callback *command);
            Pad_Callback * Get_pagedown_callback(void);
            void           Set_position_callback(Pad_Callback *command);
            Pad_Callback * Get_position_callback(void);

    static Pad_EventCallbackProc(Press);
    static Pad_EventCallbackProc(Drag);
    static Pad_EventCallbackProc(Release);

				// Get and set options

    PAD_TYPE("scrollbar");

  protected:
    virtual void   _Update(void);        // Call this when any properties of the scrollbar change
    virtual void   _Compute_dimensions(float &thickness, float &length, float &arrowThickness, float &arrowLength,
				       float &thumbLength, float &troughLength);

    int            _scrollbarFlags;      // OR'd combination of flags
    Component      _activeComponent;     // The currently active scrollbar component
    float          _value;               // Current value of scrollbar
    float          _from;	         // Smallest value scrollbar can take
    float          _to;                  // Largest value scrollbar can take
    float          _lineSize;	         // Size of line
    float          _pageSize;	         // Size of page
    Pad_BorderRef  _border;              // 3D Bevel
    Pad_BorderRef  _bgBorder;            // 3D Bevel for background
    Pad_Callback * _command;             // Callback to execute whenever the user interacts with scrollbar
    Pad_Callback * _lineUpCommand;       // Callback to execute to go up one line
    Pad_Callback * _lineDownCommand;     // Callback to execute to go down one line
    Pad_Callback * _pageUpCommand;       // Callback to execute to go up one page
    Pad_Callback * _pageDownCommand;     // Callback to execute to go down one page
    Pad_Callback * _positionCommand;     // Callback to execute to go to an absolute position
				// Some cached data describing how to render the scrollbar
				// This gets computed whenever any properties change, and
				// speeds up rendering.
    Pad_PList      _fromArrow;           // List of points describing 'from' arrow (for rendering)
    Pad_PList      _toArrow;             // List of points describing 'to' arrow (for rendering)
    Pad_Point      _thumbMin;            // Lower left corner of 'thumb' (for rendering)
    Pad_Point      _thumbMax;            // Upper right corner of 'thumb' (for rendering)

  private:
    static void     _Initialize_events(Pad_Win *win);
};

#endif
