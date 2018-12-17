# "(c) Copyright 1993-1997 Pad++ Consortium {University of New Mexico (UNM),
# New York University (NYU)}, All Rights Reserved."
# Licensee can not remove or obscure any of the
# copyright or trademark notices in this software.
#
# IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
# FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
# ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
# DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#
# THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
# IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
# NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
# MODIFICATIONS.
#
# See the file "License" for general information on usage and
# redistribution, and the file "LicenseTerms" for the specific license
# agreement on usage and redistribution of this file, and the Pad++
# software in general.

proc make.selection {PAD} {
    global _pad _color _font
    global geometry menu

		# Set selection defaults if not already defined
    if {![info exists _pad(Selection)]} {
	set _pad(Selection) "show"
    }
    if {![info exists _pad(SelectGroupMembers)]} {
	set _pad(SelectGroupMembers) 0
    }
    if {![info exists _pad(SelectLargeItems)]} {
	set _pad(SelectLargeItems) 0
    }
    if {![info exists _pad(SelectSmallItems)]} {
	set _pad(SelectSmallItems) 1
    }
    if {![info exists _pad(SelectTransparentItems)]} {
	set _pad(SelectTransparentItems) 0
    }
    if {![info exists _pad(SmallObject)]} {
	set _pad(SmallObject) 5
    }

    if {[winfo exists .selection]} {
	raise .selection
	return
    }
    toplevel .selection -bg $_color(toolbg)
    wm resizable .selection 0 0
    wm title .selection "Pad++ Selection"

    label .selection.label1 -text "Control Selection:" -anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg)

    checkbutton .selection.groupmembers -text {Group Members} -variable _pad(SelectGroupMembers) \
	-font $_font(tools) \
	-anchor w \
	-command "global _mode ; selectMode $PAD \$_mode" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    checkbutton .selection.largeitems -text {Large Items} -variable _pad(SelectLargeItems) \
	-font $_font(tools) \
	-anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    frame .selection.small -bg $_color(toolbg)
    checkbutton .selection.small.smallitems -text {Small Items} -variable _pad(SelectSmallItems) \
	-font $_font(tools) \
	-anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    entry .selection.small.smallsize -width 3 -relief sunken -bg $_color(toolbg) \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
    label .selection.small.pixels -text "pixels" -pady 0 -padx 0 -anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg)
					# Fix broken default backspace/delete bindings
    bind .selection.small.smallsize <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
    bind .selection.small.smallsize <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}
    bind .selection.small.smallsize <Enter> {focus %W}
    bind .selection.small.smallsize <Leave> "set _pad(SmallObject) \[%W get\]"
    bind .selection.small.smallsize <Key-Return> "set _pad(SmallObject) \[%W get\]"
    .selection.small.smallsize delete 0 end
    .selection.small.smallsize insert 0 $_pad(SmallObject)

    checkbutton .selection.transparentitems -text {Transparent Items} -variable _pad(SelectTransparentItems) \
	-font $_font(tools) \
	-anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    frame .selection.dummy1 -bg $_color(toolbg)
    label .selection.label2 -text "Selection Visibility:" -pady 0 -padx 0 -anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg)

    radiobutton .selection.showhandles -text "Show" -command "selection_UpdateVisibility $PAD" \
	-font $_font(tools) \
	-variable _pad(Selection) -value "show" -anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    radiobutton .selection.showbounds -text "Show bounds" -command "selection_UpdateVisibility $PAD" \
	-font $_font(tools) \
	-variable _pad(Selection) -value "showbounds" -anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    radiobutton .selection.hidehandles -text "Hide" -command "selection_UpdateVisibility $PAD" \
	-font $_font(tools) \
	-variable _pad(Selection) -value "hide" -anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    frame .selection.dummy2 -bg $_color(toolbg)
    frame .selection.tag -bg $_color(toolbg)
    label .selection.tag.label -text "Tag:" -pady 0 -padx 0 -anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg)
    entry .selection.tag.tag -width 10 -relief sunken -bg $_color(toolbg) \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
					# Fix broken default backspace/delete bindings
    bind .selection.tag.tag <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
    bind .selection.tag.tag <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}
    bind .selection.tag.tag <Enter> {focus %W}
    button .selection.tag.search -text {Search} -command "eval pad_select $PAD \[.selection.tag.tag get\]" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    button .selection.close -text Close -command {destroy .selection} \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    pack .selection.label1 -fill x
    pack .selection.groupmembers -fill x -padx 10
    pack .selection.largeitems -fill x -padx 10
    pack .selection.small -fill x -expand 1
    pack .selection.small.smallitems -padx 10 -side left
    pack .selection.small.smallsize -side left
    pack .selection.small.pixels -side left
    pack .selection.transparentitems -fill x -padx 10

    pack .selection.dummy1 -ipady 5 -fill x
    pack .selection.label2 -fill x
    pack .selection.showhandles -fill x -padx 10
    pack .selection.showbounds -fill x -padx 10
    pack .selection.hidehandles -fill x -padx 10

    pack .selection.dummy2 -ipady 5 -fill x
    pack .selection.tag -fill x -expand 1
    pack .selection.tag.label -side left
    pack .selection.tag.tag -side left -fill x -expand 1
    pack .selection.tag.search -side left

    pack .selection.close -side bottom -fill x

    pad_ManageWindow .selection
}

proc selection_UpdateVisibility {PAD} {
    set selection [$PAD find -groupmembers withtag "selected"]
    pad_unselect $PAD all
    eval pad_select $PAD $selection
}
