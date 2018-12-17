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



#
# Routine for making the line properties popup
#
proc make.lineprops {PAD} {
    global _pad menu _color _font

    set lp ".lineprops"

    # create a toplevel popup
    if { [winfo exists $lp] } {
        raise $lp
       	return
    }

    # make a popup
    toplevel $lp -bg $_color(toolbg) 
    wm title $lp "Pad++ Line Preferences"

    # create a pad surface on the popup for displaying a generic line
    pad $lp.pad -width 200 -height 200
    $lp.pad moveto 0 0 1

    # Add user-defined options
    pad_InitOptions $lp.pad

    # create a horizontal pad line with arrows
    set lineid [eval $lp.pad create line -100 0 0 0 100 0 \
                -arrowshape \"$_pad(ArrowA) $_pad(ArrowB) $_pad(ArrowC)\" \
                -arrow $_pad(LineArrow) -penwidth $_pad(PenWidth) -tags arrow]

    # Make a label for the preferences popup
    label $lp.label -text "Properties for newly created lines" \
	-font $_font(tools) \
	-bg $_color(toolbg)
    frame $lp.labelspace -height 20 \
	-bg $_color(toolbg)
    pack $lp.label
    pack $lp.labelspace -fill x

    # setup the gui for setting the line options
    make.lineopts $lp $lp.pad $lineid

    # create buttons for applying to selected lines and closing
    frame $lp.f4 -bg $_color(toolbg) \
	-bg $_color(toolbg)

    button $lp.f4.apply -text Apply -command "applyLineProps $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    button $lp.f4.close -text Close -command "destroy $lp" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    pack $lp.pad
    pack $lp.f4 -fill x
    pack $lp.f4.apply -side left -expand true -fill x
    pack $lp.f4.close -side right -expand true -fill x

    pad_ManageWindow $lp
}

#
# Make another toplevel for rest of line properties
#
proc prop_MakeLinePropWindow {PAD} {
    global _color _prop

    set items [pad_sel $PAD]

    foreach item $items {
	if {[$PAD type $item] != "line"} {
	    continue
	}
	if {[winfo exists .lineprops$item]} {
	    raise .lineprops$item
	    return
	}

	set lineprops ".lineprops$item"
	toplevel $lineprops -bg $_color(toolbg) 
    
	set type [$PAD type $item]
	frame $lineprops.f0 -bg $_color(toolbg) 
	label $lineprops.f0.type1 -text "Line Id: " -bg $_color(toolbg) 
	set font [lindex [$lineprops.f0.type1 config -font] 4]
	set mediumIndex [string first medium $font]
	if {$mediumIndex != -1} {
	    set bfFont "[string range $font 0 [expr $mediumIndex - 1]]Bold[string range $font [expr $mediumIndex + 6] end]"
	} else {
	    set bfFont $font
	}
	label $lineprops.f0.type2 -text "$item" -font $bfFont -bg $_color(toolbg) 
	pack $lineprops.f0 
	pack $lineprops.f0.type1 $lineprops.f0.type2 -side left

	set padline "$lineprops.padline"
	frame $padline -bg $_color(toolbg) 

	make.lineopts $padline $PAD $item
	pack $padline -fill x -expand 1
	button $lineprops.close -text Close -command "propClose $lineprops" \
		-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
		-highlightbackground $_color(toolbg)
	pack $lineprops.close -side top -fill x

	pad_ManageWindow $lineprops 0 0
	wm title $lineprops "Pad++ line Properties"

	$PAD bind $item <Delete> "destroy $lineprops"
	bind $lineprops <Destroy> "prop_LineWindowDestroy $PAD $item"
    }
}

proc prop_LineWindowDestroy {PAD item} {
    global _pad

    $PAD bind $item <Delete> {}
					
			# Unset global variables associated with line properties box.
			# Note: Don't unset $item.Undulate since that is used by
			#       PadDraw to maintain the state about undulation
    catch {unset _pad($item.ArrowA)}
    catch {unset _pad($item.ArrowB)}
    catch {unset _pad($item.ArrowC)}
    catch {unset _pad($item.LineArrow)}
    catch {unset _pad($item.Roughness)}
    catch {unset _pad($item.PenWidth)}
    catch {unset _pad($item.CapStyle)}
    catch {unset _pad($item.JoinStyle)}
}

#
# Close property window
#
proc propClose {window} {
    destroy $window
}

#
# Routine called when arrow shape is changed.
# 
proc updateLineArrowShape {PAD item type val} {
    set zoom [$PAD getzoom]
    set shape [$PAD ic $item -arrowshape]
    set a [lindex $shape 0]
    set b [lindex $shape 1]
    set c [lindex $shape 2]

    switch $type {
	A {set a [expr $val / $zoom]}
	B {set b [expr $val / $zoom]}
	C {set c [expr $val / $zoom]}
    }

    $PAD ic $item -arrowshape "$a $b $c"

    if {[$PAD hastag "selected" $item]} {
	pad_unselect $PAD $item
	pad_select $PAD $item
    }
}

#
# Routine called when arrow head existence changes (none, first, last, or both)
# 
proc updateLineArrowWhere {PAD item index} {
    global _pad

    $PAD ic $item -arrow $_pad($index)

    if {[$PAD hastag "selected" $item]} {
	pad_unselect $PAD $item
	pad_select $PAD $item
    }
}

#
# Routines to set arrow of properties window
#
proc setLineArrowShape {new_shape} {
    if {[winfo exists .lineprops]} {
	.lineprops.pad ic arrow -arrowshape $new_shape
    }
}

proc setLineArrowWhere {new_where} {
    if {[winfo exists .lineprops]} {
	.lineprops.pad ic arrow -arrow $new_where
    }
}

#
# Routine called when cap style is changed
# 
proc updateCapStyle {PAD item index} {
    global _pad

    $PAD ic $item -capstyle $_pad($index)

    if {[$PAD hastag "selected" $item]} {
	pad_unselect $PAD $item
	pad_select $PAD $item
    }
}

#
# Routine called when join style is changed
# 
proc updateJoinStyle {PAD item index} {
    global _pad

    $PAD ic $item -joinstyle $_pad($index)

    if {[$PAD hastag "selected" $item]} {
	pad_unselect $PAD $item
	pad_select $PAD $item
    }
}

#
# Routine called when penwidth is changed.
# 
proc updateLineWidth {PAD item index val} {
    set zoom [$PAD getzoom]
    set w [expr $val / $zoom]

    $PAD ic $item -penwidth $w
				# Update Tool palette if visible
    if {$index == "PenWidth"} {
	setDrawingWidth $PAD $w
    }

    if {[$PAD hastag "selected" $item]} {
	pad_unselect $PAD $item
	pad_select $PAD $item
    }
}

#
# Routine to line width of properties window
#
proc setLineWidth {new_width} {
    if {[winfo exists .lineprops]} {
	.lineprops.pad ic arrow -penwidth $new_width
    }
}

#
# Routine called when noise parameters are changed.
# 
proc updateLineRoughness {PAD item index val} {
    $PAD ic $item -roughness $val

    if {[$PAD ic $item -undulate]} {
	if {$val > 0.0} {
	    $PAD ic $item -undulate 1
	} else {
	    $PAD ic $item -undulate 0
	}
    }

    if {[$PAD hastag "selected" $item]} {
	pad_unselect $PAD $item
	pad_select $PAD $item
    }
}

#
# Routine called when undulation is changed.
#
proc updateLineUndulation {PAD item index} {
    global _pad

    $PAD ic $item -undulate $_pad($index)

    if {[$PAD hastag "selected" $item]} {
	pad_unselect $PAD $item
	pad_select $PAD $item
    }
}

#
# Routine called for the Apply button on line preferences popup.
# Set the properties of all selected lines to those specified
# by the preferences.
#
proc applyLineProps {PAD} {
    global _pad

    set lp .lineprops
    set zoom [$PAD getzoom]

    foreach item [pad_sel $PAD] {
	set a [expr $_pad(ArrowA) / $zoom]
	set b [expr $_pad(ArrowB) / $zoom]
	set c [expr $_pad(ArrowC) / $zoom]
	$PAD ic $item \
            -capstyle $_pad(CapStyle) \
            -joinstyle $_pad(JoinStyle) \
            -arrow $_pad(LineArrow) \
	    -arrowshape "$a $b $c"
	$PAD ic $item -roughness $_pad(Roughness)
	$PAD ic $item -undulate $_pad(Undulate)
        updateLineWidth $PAD $item PenWidth [$lp.f0.w get]
    }
}

proc make.lineopts {inframe PAD item} {
    global _pad _color _font

    set zoom [$PAD getzoom]
    if {$inframe == ".lineprops"} {
	set index ""
	set preferences 1
    } else {
	set index "$item."
	set preferences 0
    }

    frame $inframe.f0 -bg $_color(toolbg) 
    scale $inframe.f0.w -label "Line width:" -from 0 -to 100 -length 100 \
	-font $_font(tools) \
	-variable _pad(${index}PenWidth) \
	-command "updateLineWidth $PAD $item ${index}PenWidth" \
	-orient horizontal -showvalue true -bg $_color(toolbg)  \
	-activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    if {$preferences} {
	set penwidth $_pad(PenWidth)
    } else {
	set penwidth [$PAD ic $item -penwidth]
    }
    $inframe.f0.w set [expr $penwidth * $zoom]

    frame $inframe.f0.rf -bg $_color(toolbg)
    scale $inframe.f0.rf.r -label "Roughness:" \
	-font $_font(tools) \
	-from 0.0 -to 1.0 -resolution 0.01 \
	-variable _pad(${index}Roughness) \
        -command "updateLineRoughness $PAD $item ${index}Roughness" \
	-orient horizontal -showvalue true -bg $_color(toolbg)  \
	-activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    if {$preferences} {
	set roughness $_pad(Roughness)
    } else {
	set roughness [$PAD ic $item -roughness]
    }
    $inframe.f0.rf.r set $roughness

    checkbutton $inframe.f0.rf.u -text "Undulate" \
	-font $_font(tools) \
	-variable _pad(${index}Undulate) \
	-command "updateLineUndulation $PAD $item ${index}Undulate" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    if {$preferences} {
	set undulate $_pad(Undulate)
    } else {
	set undulate [$PAD ic $item -undulate]
    }
    set _pad(${index}Undulate) $undulate

    # setup scales for arrowshape parameters
    set shape [$PAD ic $item -arrowshape]
    label $inframe.f0.l -text "Arrow shape parameters:" \
	-font $_font(tools) \
	-bg $_color(toolbg)
    scale $inframe.f0.a -label "Tip to center:" -from 0 -to 100 -length 100 \
	-font $_font(tools) \
	-command "updateLineArrowShape $PAD $item A" \
	-variable _pad(${index}ArrowA) \
	-orient horizontal -showvalue true -bg $_color(toolbg)  \
	-activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    if {$preferences} {
	set a $_pad(ArrowA)
    } else {
	set a [lindex $shape 0]
    }
    $inframe.f0.a set [expr $a * $zoom]

    scale $inframe.f0.b -label "Tip to trail:" -from 0 -to 100 -length 100 \
	-font $_font(tools) \
	-variable _pad(${index}ArrowB) \
	-command "updateLineArrowShape $PAD $item B" \
	-orient horizontal -showvalue true -bg $_color(toolbg) \
	-activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    if {$preferences} {
	set b $_pad(ArrowB)
    } else {
	set b [lindex $shape 1]
    }
    $inframe.f0.b set [expr $b * $zoom]

    scale $inframe.f0.c -label "Trail to shaft:" -from 0 -to 100 -length 100 \
	-font $_font(tools) \
	-variable _pad(${index}ArrowC) \
	-command "updateLineArrowShape $PAD $item C" \
	-orient horizontal -showvalue true -bg $_color(toolbg) \
	-activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    if {$preferences} {
	set c $_pad(ArrowC)
    } else {
	set c [lindex $shape 2]
    }
    $inframe.f0.c set [expr $c * $zoom]

    # setup radiobuttons for line capstyle
    set labelwidth 10
    frame $inframe.f1 -bg $_color(toolbg) 
    label $inframe.f1.w0 -text "Cap Style: " -anchor w -width $labelwidth \
	-font $_font(tools) \
	-bg $_color(toolbg) 
    radiobutton $inframe.f1.w1 -text round -value round \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-variable _pad(${index}CapStyle) \
	-command "updateCapStyle $PAD $item ${index}CapStyle" \
        -activebackground $_color(toolbg) -highlightbackground $_color(toolbg)

    radiobutton $inframe.f1.w2 -text butt -value butt \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-variable _pad(${index}CapStyle) \
	-command "updateCapStyle $PAD $item ${index}CapStyle" \
        -activebackground $_color(toolbg) -highlightbackground $_color(toolbg)

    radiobutton $inframe.f1.w3 -text projecting -value projecting \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-variable _pad(${index}CapStyle) \
	-command "updateCapStyle $PAD $item ${index}CapStyle" \
        -activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    if {!$preferences} {
	set _pad(${index}CapStyle) [$PAD ic $item -capstyle]
    }

    # setup radiobuttons for line joinstyle
    frame $inframe.f2 -bg $_color(toolbg) 
    label $inframe.f2.w0 -text "Join Style: " -anchor w -width $labelwidth \
	-font $_font(tools) \
	-bg $_color(toolbg) 
    radiobutton $inframe.f2.w1 -text round -value round \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-variable _pad(${index}JoinStyle) \
	-command "updateJoinStyle $PAD $item ${index}JoinStyle" \
        -activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    radiobutton $inframe.f2.w2 -text bevel -value bevel \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-variable _pad(${index}JoinStyle) \
	-command "updateJoinStyle $PAD $item ${index}JoinStyle" \
        -activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    radiobutton $inframe.f2.w3 -text miter -value miter \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-variable _pad(${index}JoinStyle) \
	-command "updateJoinStyle $PAD $item ${index}JoinStyle" \
        -activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    if {!$preferences} {
	set _pad(${index}JoinStyle) [$PAD ic $item -joinstyle]
    }

    # setup radiobuttons for line arrow style
    frame $inframe.f3 -bg $_color(toolbg) 
    label $inframe.f3.w0 -text "Arrow Style: " -anchor w -width $labelwidth \
	-font $_font(tools) \
	-bg $_color(toolbg) 
    radiobutton $inframe.f3.w1 -text none -value none \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-variable _pad(${index}LineArrow) \
	-command "updateLineArrowWhere $PAD $item ${index}LineArrow" \
        -activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    radiobutton $inframe.f3.w2 -text first -value first \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-variable _pad(${index}LineArrow) \
	-command "updateLineArrowWhere $PAD $item ${index}LineArrow" \
        -activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    radiobutton $inframe.f3.w3 -text last -value last \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-variable _pad(${index}LineArrow) \
	-command "updateLineArrowWhere $PAD $item ${index}LineArrow" \
        -activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    radiobutton $inframe.f3.w4 -text both -value both \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-variable _pad(${index}LineArrow) \
	-command "updateLineArrowWhere $PAD $item ${index}LineArrow" \
        -activebackground $_color(toolbg) -highlightbackground $_color(toolbg)
    if {!$preferences} {
	set _pad(${index}LineArrow) [$PAD ic $item -arrow]
    }
    
    pack $inframe.f1 -anchor w -fill x -expand 1
    pack $inframe.f2 -anchor w -fill x -expand 1
    pack $inframe.f3 -anchor w -fill x -expand 1
    pack $inframe.f0 -anchor w -fill x -expand 1

    pack $inframe.f0.w -fill x -anchor w
    pack $inframe.f0.rf -fill x -expand t
    pack $inframe.f0.rf.r -side left -expand t -fill x
    pack $inframe.f0.rf.u -side left -fill x
    pack $inframe.f0.l -fill x -anchor w
    pack $inframe.f0.a $inframe.f0.b $inframe.f0.c -side left
    pack $inframe.f1.w0 $inframe.f1.w1 $inframe.f1.w2 $inframe.f1.w3 -side left
    pack $inframe.f2.w0 $inframe.f2.w1 $inframe.f2.w2 $inframe.f2.w3 -side left
    pack $inframe.f3.w0 $inframe.f3.w1 $inframe.f3.w2 $inframe.f3.w3 \
         $inframe.f3.w4 -side left
}
