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

#ifndef BUTTON_H
#define BUTTON_H 1

#include "defs.h"
#include "component.h"
#include "pad-string.h"
#include "callback.h"

#define LABEL_TEXT_SET      (1<<0)   // Set if label is non-default

//
// A Pad_Label is a simple component with a 'label' string
//
class Pad_Label : public Pad_Component {
  public:
    Pad_Label(Pad *pad);
    virtual ~Pad_Label();

    virtual void     Minimum_size(Pad_Dimension &dimension);
    virtual void     Preferred_size(Pad_Dimension &dimension);
    virtual Pad_Bool Render(void);
    virtual Pad_Bool Continue_refinement(void);

    virtual void     Reshape(float x, float y, float width, float height);
				// Set alignment of label
				// (COMPONENT_ALIGN_LEFT, ..._CENTER, or ..._RIGHT)
            void     Set_alignment(int a);
				// Get and set label
    virtual void     Set_label(const char *label);
            void     Set_label_default(void);
            char *   Get_label(void);
				// Get and set options

    PAD_TYPE("label");

  protected:
    int        _labelFlags;     // OR'd combination of flags
    int        _alignment;      // Alignment style
    Pad_Bool   _refineNeeded;   // records if label needs refinement
    float      _labelOffsetX;   // Offset of label for proper alignment in x direction
    float      _labelOffsetY;   // Offset of label for proper alignment in y direction
    Pad_String _label;		// Text label 
    float      _offset;		// Offset that text is drawn within label
    void       _Align();        // Update label to align itself with widget dimensions
    void       _Set_clipping(); // If label smaller then width, then turn clipping on
};

				       // Bits for _buttonFlags
#define BUTTON_ARMMED      (1<<0)      // Set if button is armmed
#define BUTTON_COMMAND_SET (1<<1)      // Set if command is non-default
#define BUTTON_RELIEF_SET  (1<<2)      // Set if relief is non-default

//
// A Pad_Button is a Pad_Label with a callback
//
class Pad_Button : public Pad_Label {
  public:
    Pad_Button(Pad *pad);
    virtual ~Pad_Button();

				// Buttons look depressed when activated
    virtual void       Arm(void);
    virtual void       Disarm(void);
    virtual Pad_Bool   Is_armmed(void);

    virtual Pad_Bool   Render(void);
    virtual void       Fill_changed(void);

				// Set 3D border relief of button
        void           Set_relief(int relief);
        void           Set_relief_default(void);
        int            Get_relief(void);
				// Callback when button is invoked
        void           Set_callback(Pad_Callback *command);
        void           Set_callback_default(void);
        Pad_Callback * Get_callback(void);

				// Get and set options

				// Event handlers
    static Pad_EventCallbackProc(Press);
    static Pad_EventCallbackProc(Drag);
    static Pad_EventCallbackProc(Release);

    PAD_TYPE("button");

  protected:
    int            _buttonFlags;         // OR'd combination of flags
    int            _relief;              // Relief of button
    Pad_BorderRef  _border;              // 3D Bevel
    Pad_BorderRef  _activeBorder;        // 3D Bevel when active
    Pad_Callback * _command;             // Callback to execute when button is invoked

  private:
    static void     _Initialize_events(Pad_Win *win);
};


//
// A Pad_Checkbox is a Pad_Button which can be toggled.
//
// It has a label along with a checkbox for showing its state.
// This indicator is a raised/sunken rectangle if checkbox
// is not a member of a checkbox group, and a raised/sunken
// diamond otherwise.
//
// The normal bindings for buttons are redefined for toggling
// the checkbox state.
//
// NOTE: The substrate does not provide direct support for
// checkbox groups (assumed to be provided by higher levels,
// like Java's AWT).  We just provide a method for indicating
// if checkbox is member of a group or not, and render accordingly.
//

class Pad_Checkbox : public Pad_Button {
  public:
    Pad_Checkbox(Pad *pad);

                        // Mehtods for the checkbox state, and group
    virtual Pad_Bool Get_state(void);
    virtual void     Set_state(Pad_Bool);
    virtual void     Set_checkboxGroup(Pad_Bool);
    virtual Pad_Bool Get_checkboxGroup(void);

    virtual void     Minimum_size(Pad_Dimension &dimension);
    virtual Pad_Bool Render(void);
    virtual void     Fill_changed(void);

    PAD_TYPE("checkbox");

				// Event handlers
    static Pad_EventCallbackProc(Press);
    static Pad_EventCallbackProc(Drag);
    static Pad_EventCallbackProc(Release);

  protected:
                     // Hide button arm methods (checkbox has its own state semantics).
    void Arm(void);
    void Disarm(void);
    Pad_Bool Is_armmed(void);
    
  private:
    static void     _Initialize_events(Pad_Win *win);
    Pad_Bool        _inCheckboxGroup;
    static Pad_Bool _dragedOut;   // needed for dealing with mouse drags after button press
};

#endif
