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

################################################################################
#
# Definitions for alignment
#
################################################################################

#
# Make align tool
#
proc make.align {PAD} {
    global _color _layout _font

    if {[winfo exists .align]} {
        raise .align
	return
    }
    toplevel .align -bg $_color(toolbg)
    wm resizable .align 0 0

    set _layout(align_type) "-left"

		# Make pad widget for show and selecting alignment type
    set p ".align.pad"
    pad $p -width 102 -height 102 -bg $_color(toolbg) -defaultEventHandlers 0
    $p moveto 50 50 1
    $p create polygon 0 0 25 25 25 75 0 100 -fill $_color(toolselbg) -pen "black" -tags "-left type" 
    $p create polygon 100 0 75 25 75 75 100 100 -fill $_color(toolbg) -pen "black" -tags "-right type" 
    $p create polygon 0 100 25 75 75 75 100 100 -fill $_color(toolbg) -pen "black" -tags "-top type" 
    $p create polygon 0 0 25 25 75 25 100 0 -fill $_color(toolbg) -pen "black" -tags "-bottom type" 
    $p create rectangle 25 25 75 75 -fill $_color(toolbg) -tags "-stackz type" 

    $p bind type <Enter> {
	%W raise %O
	%W ic %O -fill $_color(toolactivebg)
    }
    $p bind type <Leave> {
	if {[%W hastag $_layout(align_type) %O]} {
	    %W ic %O -fill $_color(toolselbg)
	} else {
	    %W ic %O -fill $_color(toolbg)
	}
    }
    $p bind type <ButtonPress-1> {
	set item [%W find withtag $_layout(align_type)]
	%W ic $item -fill $_color(toolbg)
	%W ic %O -fill $_color(toolselbg)
	set _layout(align_type) [lindex [%W gettags %O] 0]
    }

		# Make rest of GUI
    button .align.apply -text "Align Selected Items" -command "layout_AlignApply $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) -anchor w \
	-highlightbackground $_color(toolbg)

    button .align.new -text "New Alignment Tool" -command "layout_AlignCreateTool $PAD [lindex [pad_sel $PAD] 0]" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) -anchor w \
	-highlightbackground $_color(toolbg)

    button .align.create -text "Create Tool From Selected" -command "
             layout_AlignMakeItemTool $PAD \[lindex \[pad_sel $PAD\] 0\];
             pad_unselect $PAD all
        " \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) -anchor w \
	-highlightbackground $_color(toolbg)

    button .align.close -text "Close" -command "layout_AlignClose $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    pack .align.pad
    pack .align.apply -fill x
    pack .align.new -fill x
    pack .align.create -fill x
    pack .align.close -fill x

    pad_ManageWindow .align
    wm title .align "Pad++ Alignment"

    layout_AlignBind $PAD
}

#
# Define event handlers for the alignment tool
#
proc layout_AlignBind {PAD} {
    $PAD bind aligntool <Select-ButtonPress-1> {
	layout_AlignPress %P %O %i %j
	break
    }
    $PAD bind aligntool <Select-B1-Motion> {
	layout_AlignDrag %P %O %i %j
	break
    }
    $PAD bind aligntool <Select-B1-ButtonRelease> {
	layout_AlignRelease %P %O %i %j
	break
    }
}

#
# Event handlers for alignment
#
proc layout_AlignPress {PAD item i j} {
    global _layout _pad

    set _layout(toolcolor) [$PAD ic $item -pen]
    $PAD ic $item -pen red
    $PAD delete modifier
    set _layout(x) $i
    set _layout(y) $j
}

proc layout_AlignDrag {PAD item i j} {
    global _layout _pad
    
    set dx [expr $i - $_layout(x)]
    set dy [expr $j - $_layout(y)]
    $PAD slide $item $dx $dy	
    $PAD slide aligndec$item $dx $dy	
    set _layout(x) $i
    set _layout(y) $j
    $PAD layout align [$PAD ic $item -info] -coords [$PAD coords $item] -overlaponly selected
}

proc layout_AlignRelease {PAD item i j} {
    global _layout _pad

    $PAD ic $item -pen $_layout(toolcolor)
}

#
# Close the alignment tool window.
# Delete all local alignment tools
#
proc layout_AlignClose {PAD} {
    pad_fade_delete $PAD aligntool
    pad_fade_delete $PAD aligndec

    destroy .align
}

#
# Make the specified item an alignment tool
#
proc layout_AlignMakeItemTool {PAD {item ""}} {
    global _layout

    if {[$PAD type $item] != "line"} {
	pad_bgnd_error "Can only make an alignment tool out of lines"
	return
    }

    $PAD addtag aligntool $item
    set zoom [$PAD getzoom]
    set len [expr 7.0 / $zoom]
    set width [expr 2.0 / $zoom]
    
    switch -- $_layout(align_type) {
	"-left" {
	    set dx [expr -$len]
	    set dy [expr -$len]
	}
	"-right" {
	    set dx [expr $len]
	    set dy [expr $len]
	}
	"-top" {
	    set dx [expr -$len]
	    set dy [expr $len]
	}
	"-bottom" {
	    set dx [expr $len]
	    set dy [expr -$len]
	}
	"-stackz" {
	    pad_bgnd_error "Cannot make tool for scale alignment"
	    return
	}
    }

    $PAD ic $item -info "$_layout(align_type)"
    foreach {x y} [$PAD coords $item] {
	$PAD create line $x $y [expr $x + $dx] [expr $y + $dy] \
	    -pen red -penwidth $width -tags "aligndec aligndec$item hide" -events 0
    }

		# When alignment tools is deleted, remove associated decoration 
    $PAD bind $item <Delete> {
	%W delete aligndec%O
    }
		# Don't write out alignment tools
    $PAD bind $item <Write> {
	set Pad_Write 0
	return ""
    }
}

#
# Align the selected items based on the 
# parameters specified in _layout.
#
proc layout_AlignCreateTool {PAD} {
    global _layout

    if {$_layout(align_type) == "-stackz"} {
	pad_bgnd_error "Cannot make tool for scale alignment"
	return
    }
	
    set bbox [$PAD bbox 1]
    if {($_layout(align_type) == "-left") || ($_layout(align_type) == "-right")} {
	set aligntool [$PAD create line 0 0 0 [lindex [$PAD config -height] 4] -pen red4 \
			   -tags "aligntool item"]
	$PAD scale $aligntool [expr 1.0 / [$PAD getzoom]]
	if {$_layout(align_type) == "-left"} {
	    $PAD layout position 0.1 0 -ref 1 0 0 $aligntool
	} else {
	    $PAD layout position 0.9 0 -ref 1 0 0 $aligntool
	}
    } else {
	set aligntool [$PAD create line 0 0 [lindex [$PAD config -width] 4] 0 -pen red4 \
			   -tags "aligntool item"]
	$PAD scale $aligntool [expr 1.0 / [$PAD getzoom]]
	if {$_layout(align_type) == "-top"} {
	    $PAD layout position 0 0.9 -ref 1 0 0 $aligntool
	} else {
	    $PAD layout position 0 0.1 -ref 1 0 0 $aligntool
	}
    }
    layout_AlignMakeItemTool $PAD $aligntool

    if {[$PAD modifier get] != "Select"} {
	selectMode $PAD "Select"
    }
}

#
# Align the selected items based on the 
# parameters specified in _layout.
#
proc layout_AlignApply {PAD} {
    global _layout

    if {$_layout(align_type) != "-stackz"} {
	eval $PAD layout align $_layout(align_type) selected
	
	$PAD addtag alignitems selected
	pad_unselect $PAD all
	eval pad_select $PAD alignitems
	$PAD deletetag alignitems alignitems

    } else {
        # Stack, scale and group selected objects.
        # Objects are ordered by their display order:(.
	set objs [pad_sel $PAD]
	if {[llength $objs] != 0} {
	    set topObj [lindex $objs 0]
	    set otherObjs [lrange $objs 1 [expr [llength $objs] - 1]]
	    set grp [layout_stackObjs $PAD $topObj $otherObjs]
	    
	    # unselect objs
	    eval pad_unselect $PAD $objs
	    
	    # select the new group
	    pad_select $PAD $grp
	}
    }
}

################################################################################
#
# Definitions for distribution
#
################################################################################

#
# Make distribute tool
#
proc make.distribute {PAD} {
    global _color _layout _font

    if {[winfo exists .distribute]} {
        raise .distribute
	return
    }
    toplevel .distribute -bg $_color(toolbg)
    wm resizable .distribute 0 0

    set _layout(distribute_type) "-horizontal"

		# Make pad widget for show and selecting distribution type
    set p ".distribute.pad"
    pad $p -width 102 -height 102 -bg $_color(toolbg) -defaultEventHandlers 0
    $p moveto 50 50 1
    $p create rectangle 35 0 65 100 -fill $_color(toolbg) -pen "black" -tags "-vertical type" 
    $p create rectangle 0 35 100 65 -fill $_color(toolselbg) -pen "black" -tags "-horizontal type" 

    $p bind type <Enter> {
	%W ic %O -fill $_color(toolactivebg)
    }
    $p bind type <Leave> {
	if {[%W hastag $_layout(distribute_type) %O]} {
	    %W ic %O -fill $_color(toolselbg)
	} else {
	    %W ic %O -fill $_color(toolbg)
	}
    }
    $p bind type <ButtonPress-1> {
	%W raise %O
	set item [%W find withtag $_layout(distribute_type)]
	%W ic $item -fill $_color(toolbg)
	%W ic %O -fill $_color(toolselbg)
	set _layout(distribute_type) [lindex [%W gettags %O] 0]
    }

		# Make rest of GUI
    button .distribute.apply -text "Distribute Selected Items" -command "layout_DistributeApply $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) -anchor w \
	-highlightbackground $_color(toolbg)

    frame .distribute.s -bg $_color(toolbg)
    checkbutton .distribute.s.spacing -text "Spacing" -variable _layout(distribute_spacing) \
	-command "layout_DistributeSetSpacing $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) -anchor w \
	-highlightbackground $_color(toolbg)
    entry .distribute.s.space -width 7 \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
    layout_DistributeSetSpacing $PAD
    bind .distribute.s.space <BackSpace> {
	%W delete [expr [%W index insert] -1]
    }
    bind .distribute.s.space <Delete> {
	%W delete [expr [%W index insert] -1]
    }
    bind .distribute.s.space <Key-Return> "
	layout_DistributeApply $PAD
    "

    button .distribute.create -text "Create Tool From Selected" \
	-command "layout_DistributeMakeItemTool $PAD \[lindex \[pad_sel $PAD\] 0\]; pad_unselect $PAD all" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) -anchor w \
	-highlightbackground $_color(toolbg)

    button .distribute.close -text "Close" -command "layout_DistributeClose $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    pack .distribute.pad
    pack .distribute.apply -fill x
    pack .distribute.s -expand t -fill both
    pack .distribute.s.spacing -side left
    pack .distribute.s.space -side left -expand 1 -fill x
    pack .distribute.create -fill x
    pack .distribute.close -fill x

    pad_ManageWindow .distribute
    wm title .distribute "Pad++ Distribution"

    layout_DistributeBind $PAD
}

#
# Set the spacing between items to be distributed
#
proc layout_DistributeSetSpacing {PAD} {
    global _layout

    if {$_layout(distribute_spacing)} {
	.distribute.s.space config -state normal -fg black -relief sunken
    } else {
	.distribute.s.space config -state disabled -fg gray40 -relief flat
    }
}

#
# Define event handlers for the distribution tool
#
proc layout_DistributeBind {PAD} {
    $PAD bind distributetool <Select-ButtonPress-1> {
	layout_DistributePress %P %O
    }
    $PAD bind distributetool <Select-B1-Motion> {
	layout_DistributeDrag %P %O
    }
    $PAD bind distributetool <Select-B1-ButtonRelease> {
	layout_DistributeRelease %P %O
    }
}

#
# Event handlers for distribution
#
proc layout_DistributePress {PAD tool} {
    global _layout _pad

    set _layout(toolcolor) [$PAD ic $tool -pen]
    $PAD ic $tool -pen red
}

proc layout_DistributeDrag {PAD tool} {
    layout_DistributeApply $PAD $tool
}

proc layout_DistributeRelease {PAD tool} {
    global _layout _pad

    $PAD ic $tool -pen $_layout(toolcolor)
}

#
# Close the distribution tool window.
# Delete all local distribution tools
#
proc layout_DistributeClose {PAD} {
    pad_fade_delete $PAD distributetool

    destroy .distribute
}

#
# Make the specified item an distribution tool
#
proc layout_DistributeMakeItemTool {PAD {item ""}} {
    global _layout

    set type [$PAD type $item]
    if {($type != "line") && ($type != "polygon")} {
	pad_bgnd_error "Can only make an distribution tool out of lines and polygons"
	return
    }

    $PAD addtag distributetool $item

		# Create button to attach selected objects to distribute tool
		# Need to make our own event bindings so button will operate
		# in Select mode.
    set zoom [$PAD getzoom]
    set id [$PAD create button -text "Attach" -command "layout_DistributeAttachItems $PAD $item" \
		-transparency 0.5 -anchor center -z [expr 1.0 / $zoom]]
    $PAD addtag "distbutton$item" $id
#    set button "_button$id"
#    $PAD bind $id <Select-Enter>          "pad_button_enter $button %l; break"
#    $PAD bind $id <Select-Leave>          "pad_button_leave $button; break"
#    $PAD bind $id <Select-ButtonPress-1>  "pad_button_arm $button; break"
#    $PAD bind $id <Select-B1-Motion>      "pad_button_motion $button %U %V; break"
#    $PAD bind $id <Select-ButtonRelease-1> "pad_button_activate $button; break"
    layout_DistributeApply $PAD $item

		# Make event bindings for button in "Select" mode
    $PAD bind distbutton$item <Select-ButtonPress-1> {
	%P ic %O -relief sunken
	break
    }
    $PAD bind distbutton$item <Select-B1-Motion> {
	break
    }
    $PAD bind distbutton$item <Select-B1-ButtonRelease> "
	%P ic %O -relief raised
	layout_DistributeAttachItems %P $item
	break
    "

		# Whenever tool is modified, re-distribute items
    $PAD bind $item <Modify> {
	layout_DistributeApply %W %O
    }
		# When tool is deleted, delete associated items
    $PAD bind $item <Delete> {
	%W delete distbutton%O
    }

		# Don't write out distribution tools
    $PAD bind $item <Write> {
	set Pad_Write 0
	return ""
    }
}

#
# Attach the selected items to the specified tool
# so that they will be distributed by the tool.
#
proc layout_DistributeAttachItems {PAD tool} {
    set items [pad_sel $PAD]
    set items [lremove $items $tool]
    set items [lremove $items [$PAD find withtag distbutton$tool]]
    $PAD ic $tool -info $items
    pad_unselect $PAD all
    pad_select $PAD $tool
}

#
# Distribute the selected items based on the 
# parameters specified in _layout.
#
proc layout_DistributeApply {PAD {tool ""}} {
    global _layout

    if {![winfo exists .distribute]} {
	return
    }

    set space [.distribute.s.space get]
    if {$_layout(distribute_spacing) && ($space != "")} {
	set space_option "-space $space"
    } else {
	set space_option ""
    }
    if {$tool == ""} {
	eval $PAD layout distribute $_layout(distribute_type) $space_option selected

	$PAD addtag distitems selected
	pad_unselect $PAD all
	eval pad_select $PAD distitems
	$PAD deletetag distitems distitems
    } else {
	set coords [$PAD coords $tool]

			# Position the button with the line
        set zoom [$PAD getzoom]
        $PAD ic distbutton$tool -place "[lrange $coords 0 1] [expr 1.0 / $zoom]"

	set items [$PAD ic $tool -info]
	if {$items == ""} {
	    return
	}
	eval $PAD layout distribute -coords \$coords $space_option $items
    }
}

#
# Stack, scale and group objects with respect to a top object.
# All objects are moved to the x-y place of the top object.
# Their minsize is set to current size of the top object.
# They are also scaled (default 1/2) consecutively.
#
proc layout_stackObjs {PAD topObj otherObjs {makeGroup 1} {scale 0.5}} {
    # Place objects with respect to the first one.
    # We could do this consecutively (place 2nd wrt 1st, 3rd wrt 2nd, etc
    # but there's a problem with objects not being updated and so we have
    # to use the first one.
    set i 1
    foreach obj $otherObjs {
	layout_stackObj $PAD $topObj $obj $scale $i 
	incr i
    }

    if {$makeGroup == 1} {
	return [$PAD create group -members [linsert $otherObjs 0 $topObj]]
    } else {
	return ""
    }
}


proc layout_stackObj {PAD topObj botObj scale counter} {

    # make botObj same size as topObj
    set x [expr 1.0*[$PAD getsize $topObj]/[$PAD getsize $botObj]]
    $PAD scale $botObj $x
    
    # have botObj fade in when at this size
    pad_fadein $PAD $botObj
    
    # place botObj at topObj and scale it accordingly
    $PAD ic $botObj -x [$PAD ic $topObj -x] -y [$PAD ic $topObj -y]
    $PAD scale $botObj [expr $scale/$counter]
}
