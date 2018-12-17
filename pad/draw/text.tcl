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

#####################################################################
#
# This file defines all the functions used for the text widget.
# The event bindings that attach keys to these functions are defined
# in the function bindText in the file events.tcl
#
#####################################################################
#####################################################################
#####################################################################

#
# This updates the text kill buffer display on the main pad surface
#
proc updateBufferText {PAD text} {
    global _pad

    if {![winfo exists $PAD]} {
	return
    }
    if {$_pad(TextBuffer) == 0} {
	$PAD delete bufferText
	return
    }
    set id [$PAD find withtag bufferText]
    if {$id == ""} {
	set id [$PAD create text -anchor ne -place "0 0 1" -tags "bufferText" -text " " \
		-events 0 -sticky 1 -pen gray40 -font "Times-8" -layer "status"]
    }
    $PAD itemconfig $id -text $text
    updateBufferLocation $PAD
}

#
# This updates the kill buffer's location so that it is always
# in the upper right corner of the screen
#
proc updateBufferLocation {PAD} {
    if {![winfo exists $PAD]} {
	return
    }
    set id [$PAD find withtag "bufferText"]
    if {$id != ""} {
	$PAD ic $id -z [expr 1.0 / [$PAD getzoom]]
	$PAD layout position 1 1 -ref 1 1 1 $id
    }
}

#
# Turn the text cursor on
#
proc textCursorOn {PAD} {
    $PAD config -cursor xterm
}

#
# Turn the text cursor off
#
proc textCursorOff {PAD} {
    global _pad
    
    pad_set_cursor $PAD "@$_pad(PadBitmaps)/none.xbm black" "none"
}

#
# Set the focus to the specified text object,
# and highlight that text object
#
proc textSetFocus {PAD textid} {
    set prevFocus [$PAD focus]
    $PAD focus $textid
    focus $PAD
    $PAD delete textbgnd
    set bbox [$PAD bbox $textid]
    set fill [$PAD ic $textid -pen]
    set zoom [$PAD getzoom]
    eval $PAD create rectangle $bbox -pen $fill -penwidth [expr 5.0 / $zoom] \
	-transparency 0.1 -tags textbgnd -joinstyle miter -events 0
    $PAD lower textbgnd $textid
}

#
# If mark has changed position, then move it
#
proc textUpdateMark {PAD textid orig_mark} {
    set new_mark [$PAD text $textid index mark]
    if {$orig_mark != $new_mark} {
	set old_bbox [$PAD bbox mark]
	if {$old_bbox != ""} {
	    set new_bbox [$PAD text $textid bbox mark]
	    set dx [expr [lindex $new_bbox 0] - [lindex $old_bbox 0]]
	    set dy [expr [lindex $new_bbox 1] - [lindex $old_bbox 1]]
	    $PAD slide mark $dx $dy
	}
    }
}

#
# Check to see if point is off screen.
# If so, center text object in x or y.
#
proc textUpdateView {PAD} {
    set textid [$PAD focus]
    eval $PAD coords textbgnd [$PAD bbox $textid]
				# Don't update view for sticky text objects
    if {[$PAD ic $textid -sticky] != 0} {
	return
    }
    set pointbbox [$PAD text $textid bbox point]
    set xy [bbcenter $pointbbox]
    set pointx [lindex $xy 0]
    set pointy [lindex $xy 1]
    set viewbbox [$PAD bbox 1]
    set newx ""
    set newy ""
    if {($pointx < [lindex $viewbbox 0]) || ($pointx > [lindex $viewbbox 2])} {
	set newx $pointx
    }
    if {($pointy < [lindex $viewbbox 1]) || ($pointy > [lindex $viewbbox 3])} {
	set newy $pointy
    }
    if {($newx != "") || ($newy != "")} {
	$PAD update
	$PAD moveto $newx $newy "" 1000
    }
}

#
# Change view so that upper left corner of text object
# is put near the upper left corner of window.
# View is not zoomed.
#
proc textCenter {PAD textid} {
    global _pad

    set view [$PAD getview]
    set viewx [lindex $view 0]
    set viewy [lindex $view 1]
    set zoom  [lindex $view 2]
    set bbox [$PAD bbox $textid]
    set view_bbox [$PAD bbox 1]
    set x [expr $viewx - ([lindex $view_bbox 0] + (1.0 / $zoom) - [lindex $bbox 0])]
    set y [expr $viewy - ([lindex $view_bbox 3] - (1.0 / $zoom) - [lindex $bbox 3])]
    $PAD moveto $x $y "" $_pad(AnimationSpeed)
}
#
# Get user input on status line, and
# execute script when return key is pressed
#
proc textGetData {PAD script} {
    unset-mark $PAD
    set textid [$PAD focus]
    updateStatusText $PAD "" 1
    $PAD focus status
    $PAD addtag text status
    $PAD text status mark set mark 0.0
    $PAD bind status <KeyPress-Return> "
        set result \[$PAD ic status -text\]
        updateStatusText $PAD {} 0
        $PAD deletetag text status
        $PAD focus $textid
        $script \$result
    "
}

#
# Position this object to the right of and aligned on top with
# the current text object.  If there is no current text, position
# it near the center of the screen.
# Then change the view so that the new
# object is at the nw corner of the screen.
#
proc textLayout {PAD textid} {
    set zoom  [$PAD getzoom]
    set old_textid [$PAD focus]
    if {$old_textid != ""} {
	set place [$PAD ic $old_textid -place]
	$PAD ic $textid -place $place
    } else {
	$PAD ic $textid -place center -z [expr 10.0 / $zoom]
    }
    while 1 {
	set bbox [$PAD bbox $textid]
	set overlapIds [eval $PAD find overlapping $bbox]
	set ids ""
	foreach id $overlapIds {
	    if {($id != 1) && ($id != $textid)} {
		lappend ids $id
	    }
	}
	if {$ids == ""} {
	    break
	}
	set obbox [eval $PAD bbox $ids]
	set dx [expr [lindex $obbox 2] - [lindex $bbox 0] + (0.5 / $zoom)]
	set dy [expr [lindex $obbox 3] - [lindex $bbox 3]]
	$PAD slide $textid $dx $dy
    }
				# Put focus on new object
				# Position view so that upper left corner of object
				# is near upper left corner of screen
    $PAD focus $textid
    $PAD update
    textCenter $PAD $textid
}

###################################################################
#
# Generic (emacs-like) text functions
#
###################################################################

#
# Set mark to point
#
proc set-mark {PAD} {
    global _text

    $PAD delete mark
    set textid [$PAD focus]
    $PAD text $textid mark set mark point
    set _text(markedTextID) $textid
    set bbox [$PAD text $textid bbox point]
    set dx [expr 0.05 * ([lindex $bbox 2] - [lindex $bbox 0])]
    set dy [expr 0.05 * ([lindex $bbox 3] - [lindex $bbox 1])]
    set markid [eval $PAD create rectangle $bbox \
	    -fill red -pen none -tags "mark"]
    $PAD slide $markid $dx $dy
    $PAD lower $markid $textid
    set _text(appendBuffer) 0
}

#
# Delete any text mark
#
proc unset-mark {PAD} {
    global _text

    $PAD delete mark
    catch {$PAD text $_text(markedTextID) mark unset mark}
    set _text(markedTextID) ""
}

#
# Copy from region into kill buffer
#
proc kill-ring-save {PAD} {
    global _text

    set textid [$PAD focus]
    if {[lmember [$PAD text $textid mark names] mark]} {
	set _text(buffer) [$PAD text $textid get mark point]
	updateBufferText $PAD $_text(buffer)
	set _text(appendBuffer) 0
    }
}

#
# Yank from kill buffer to point
#
proc yank {PAD} {
    global _text

    if {$_text(buffer) != ""} {
	set textid [$PAD focus]
	set orig_mark [$PAD text $textid index mark]
	$PAD text $textid insert point $_text(buffer)
	textUpdateMark $PAD $textid $orig_mark
	set _text(appendBuffer) 0
    }
}

#
# Delete char before point
#
proc delete-backward-char {PAD} {
    global _text

    set textid [$PAD focus]
    set orig_mark [$PAD text $textid index mark]
    $PAD text $textid delete "point - 1 char"
    textUpdateMark $PAD $textid $orig_mark
    set _text(appendBuffer) 0
}

#
# Delete char at point
#
proc delete-char {PAD} {
    global _text

    set textid [$PAD focus]
    set orig_mark [$PAD text $textid index mark]
    $PAD text $textid delete point
    textUpdateMark $PAD $textid $orig_mark
    set _text(appendBuffer) 0
}

#
# Kill from point to end of line
#
proc kill-line {PAD} {
    global _text

    set textid [$PAD focus]
    set point [$PAD text $textid index point]
    set lineend [$PAD text $textid index "point lineend"]
    set orig_mark [$PAD text $textid index mark]
    if {$point == $lineend} {
				# Delete carriage return
	$PAD text $textid delete point
	set buffer "\n"
    } else {
				# Delete until end of line
	set buffer [$PAD text $textid get point "point lineend"]
	$PAD text $textid delete point "point lineend"
    }
    if {$_text(appendBuffer)} {
	append _text(buffer) $buffer
    } else {
	set _text(buffer) $buffer
    }
    textUpdateMark $PAD $textid $orig_mark
    updateBufferText $PAD $_text(buffer)
    set _text(appendBuffer) 1
}

#
# Delete word at point
#
proc kill-word {PAD} {
    global _text

    set textid [$PAD focus]
    set start_index [$PAD text $textid index point]
    set end_index   [$PAD text $textid index "point + 1 char wordend"]
    if {$_text(appendBuffer)} {
	append _text(buffer) [$PAD text $textid get $start_index $end_index]
    } else {
	set _text(buffer) [$PAD text $textid get $start_index $end_index]
    }
    set orig_mark [$PAD text $textid index mark]
    $PAD text $textid delete $start_index $end_index
    textUpdateMark $PAD $textid $orig_mark
    updateBufferText $PAD $_text(buffer)
    set _text(appendBuffer) 1
}

#
# Delete region into kill buffer
#
proc kill-region {PAD} {
    global _text

    set textid [$PAD focus]
    if {[lmember [$PAD text $textid mark names] mark]} {
	set _text(buffer) [$PAD text $textid get mark point]
	set orig_mark [$PAD text [$PAD focus] index mark]
	$PAD text [$PAD focus] delete mark point
	textUpdateMark $PAD $textid $orig_mark
	updateBufferText $PAD $_text(buffer)
	set _text(appendBuffer) 0
    }
}

proc switch-to-buffer {PAD buffer} {
    global _text _pad

    if {[info exists _text(file$buffer)]} {
	$PAD focus $_text(file$buffer)
	textCenter $PAD $_text(file$buffer)
    } else {
	updateStatusMsg $PAD "(New Buffer)"
	set textid [$PAD create text -tags text -anchor nw -pen $_pad(PenColor) -font $_pad(Font)]
	textLayout $PAD $textid
	set _text(file$buffer) $textid
	$PAD text $textid mark set mark 0.0
    }
}

#
# Load in the specified file
# Put it near the center of the screen, so that
# it doesn't overlap anything else.
#
proc find-file {PAD file} {
    global _text _pad

				# First check if file already loaded
    if {[info exists _text(file$file)]} {
	$PAD focus $_text(file$file)
	textCenter $PAD $_text(file$file)
	return
    }
				# Else, load it
    if {[catch {set textid [$PAD create textfile -file $file -font $_pad(Font)]}]} {
				# File doesn't exist, make new one
	updateStatusMsg $PAD "(New File)"
	set textid [$PAD create text -font $_pad(Font)]
    }
    $PAD ic $textid -tags text -anchor nw -pen $_pad(PenColor)
    textLayout $PAD $textid
    set _text(file$file) $textid
    $PAD text $textid mark set mark 0.0
}

#
# Put the mark where point is now, 
# and point where the mark is now.
#
proc exchange-point-and-mark {PAD} {
    global _text
    
    if {$_text(markedTextID) != ""} {
	set textid [$PAD focus]
	set orig_mark [$PAD text $textid index mark]
	$PAD text $textid mark set mark [$PAD text $textid index point]
	$PAD text $textid mark set point $orig_mark
	textUpdateMark $PAD $textid $orig_mark
    }
}

