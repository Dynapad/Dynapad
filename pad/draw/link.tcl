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
# Hyperlinks are supported through application-specific options on pad items.
# The options are:
#   -linkto      Where the item is linked to.  May be a tagOrId or a bounding box.
#   -linkstyle   The style of the link: "box", "underline", "destbox", or "none".
#   -linkcolor   The color the link is rendered with.
#
# Links to items are stored via tags as well as global variables.
# When a link is made between item "A" and "B", the following tags are added:
#   A: link linkto<num>
#   B: linkdest link<num>
# where <num> is a unique integer.
# When a link is made to a place, then that node just gets the tag "link".
# If a link is made from the pad surface, a transparent hidden link is
# created with the tag "linkhidden".
# These tags are used for event bindings and finding the source of a link.
# Temporary objects (both while creating links and while following them) 
# have the tag "linkconnect"
#
# The data for the hyperlinked items are stored in the global array "_link"
# where each item has the following properties stored as:
#   _link($id.destobj)   Tag of destination (can't have destbbox if have destobj)
#   _link($id.destbbox) Place of destination (can't have destobj if have destbbox)
#   _link($id.style)     Style of link (default is "box")
#   _link($id.color)     Color of link if non-default (which is red)
#
# In addition, the following global array entries are used
#   _link(cursor)        The cursor just before pointer goes over a link
#   _link(current)       The id the current link source being defined, or "" if none
#                        Or, the id of the current link source the pointer is over
#   _link(counter)       Current link number in system
#   _link(dest)          Current destination of link
#   _link(source)        Current source of link
#
# There is a function to convert from the old hyperlink format.
# After a file with the old format is read in, call link_ConverFormat
#

proc make.link {PAD} {
    global _color _link _font

    if {[winfo exists .link]} {
        raise .link
	return
    }

    toplevel .link
    wm resizable .link 0 0

    set ids [pad_sel $PAD]
    set linkids ""
    foreach id $ids {
				# If group is selected, then operate on its members
	set objs [pad_get_members $PAD $id]
	foreach obj $objs {
	    if {[$PAD hastag "link" $obj]} {
		lappend linkids $obj
	    }
	}
    }
    if {[llength $linkids] == 0} {
	label .link.error -text "No Hyperlinks selected" \
	    -font $_font(tools) \
	    -bg $_color(toolbg) 
	pack .link.error -fill x

	set x [winfo x .]
	set y [winfo y .]
	wm geometry .link +[expr $x + 100]+[expr $y + 100]
    } else {
	set _link(selection) $linkids

	label .link.ids -text "Link(s): $linkids" -pady 0 -padx 0 -anchor w \
	    -font $_font(tools) \
	    -bg $_color(toolbg) 
	button .link.style -text {Style} -anchor w \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	    -highlightbackground $_color(toolbg)
	bind .link.style <ButtonPress-1> "link_StyleMenu $PAD \[winfo pointerx .\] \[winfo pointery .\]; break"

	frame .link.color -bg $_color(toolbg) 
	label .link.color.label -text "Color:" -pady 0 -padx 0 -anchor w \
	    -font $_font(tools) \
	    -bg $_color(toolbg) 
	entry .link.color.color -width 10 -relief sunken \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -highlightbackground $_color(toolbg)
					# Fix broken default backspace/delete bindings
	bind .link.color.color <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
	bind .link.color.color <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}
	bind .link.color.color <Enter> {focus %W}
	bind .link.color.color <Leave> "link_SetColor $PAD \"$linkids\" \[%W get\]"
	bind .link.color.color <Key-Return> "link_SetColor $PAD \"$linkids\" \[%W get\]"
	.link.color.color delete 0 end
	.link.color.color insert 0 [$PAD ic [lindex $linkids 0] -linkcolor]

	button .link.unlink -text {Remove link(s)} -anchor w \
	    -font $_font(tools) \
	    -command "link_Remove $PAD \"$linkids\"" \
	    -bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	    -highlightbackground $_color(toolbg)

	pack .link.ids -fill x
	pack .link.style -fill x
	pack .link.color -fill x -expand 1
	pack .link.color.label -side left
	pack .link.color.color -side left
	pack .link.unlink -fill x

	pad_ManageWindow .link
    }

    button .link.close -text "Close" -command {destroy .link} -anchor w\
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    pack .link.close -fill x

    wm title .link "Pad++ Hyperlink Tools"
}

#
# Popup the style menu
#
proc link_StyleMenu {PAD x y} {
    global _color _link _font

    if {![winfo exists .linkstyle]} {
	menu .linkstyle \
	    -font $_font(tools) \
	    -bg $_color(menubg) -tearoff 0 \
	    -activebackground $_color(menuactivebg)
	.linkstyle add command \
	    -label {Box} \
	    -command "link_SetStyle $PAD \"\$_link(selection)\" box"
	.linkstyle add command \
	    -label {Box & Destination} \
	    -command "link_SetStyle $PAD \"\$_link(selection)\" destbox"
	.linkstyle add command \
	    -label {Underline} \
	    -command "link_SetStyle $PAD \"\$_link(selection)\" underline"
	.linkstyle add command \
	    -label {None} \
	    -command "link_SetStyle $PAD \"\$_link(selection)\" none"
    }

    tk_popup .linkstyle $x $y
}

#
# Set the link style of each of the specified items to the specified style
#
proc link_SetStyle {PAD objs style} {
    if {[llength $objs] > 1} {
	foreach linkid $objs {
	    link_SetStyle $PAD $linkid $style
	}
	return
    }

    $PAD ic $objs -linkstyle $style
}

#
# Set the link color of each of the specified items to the specified color
#
proc link_SetColor {PAD objs color} {
    if {[llength $objs] > 1} {
	foreach linkid $objs {
	    link_SetColor $PAD $linkid $color
	}
	return
    }

    $PAD ic $objs -linkcolor $color
}

#
# bindLink
#
#   Define event bindings for hyperlinks.
#   First, define bindings for hyperlinked objects (Link mode)
#   Then, define bindings for creation of hyperlinks (LinkCreate mode)
#
proc bindLink {PAD} {
    global _pan _link

    set _link(current) ""
    set _link(source) ""
    set _link(dest) ""
    if {![info exists _link(counter)]} {
	set _link(counter) 1
    }

    ######
    # Events to follow hyperlinks
    ######

    $PAD bind link <Run-B2-Enter> {break}
    $PAD bind link <Run-B2-Leave> {break}
    $PAD bind link <Run-B3-Enter> {break}
    $PAD bind link <Run-B3-Leave> {break}

    $PAD bind link <Run-Enter> {
	link_Enter %P %O "%l"
	break
    }

    $PAD bind link <Run-Leave> {
	link_Leave %P
	break
    }

    $PAD bind link <Run-ButtonPress-1> {
	panEvent %P %W "press" %O %s %x %y %i %j %l
	break
    }

    $PAD bind link <Run-B1-Motion> {
	panEvent %P %W "motion" %O %s %x %y %i %j %l
	break
    }

    $PAD bind link <Run-ButtonRelease-1> {
	panEvent %P %W "release" %O %s %x %y %i %j %l
	if {$_pan(started) == 0} {
	    link_Follow %P $_link(current) %s "%l"
	}
	break
    }

    ######
    # Events to define hyperlinks
    ######

    $PAD bind all <LinkCreate-Motion> {
	link_Modify %P %i %j %O
    }

    $PAD bind all <LinkCreate-ButtonPress-1> {
	panEvent %P %W "press" %O %s %x %y %i %j %l
	break
    }

    $PAD bind all <LinkCreate-B1-Motion> {
	panEvent %P %W "motion" %O %s %x %y %i %j %l
	break
    }

    $PAD bind all <LinkCreate-ButtonRelease-1> {
	panEvent %P %W "release" %O %s %x %y %i %j %l
	if {$_pan(started) == 0} {
	    if {$_link(current) == ""} {
		if {$_pad(SelectGroupMembers)} {
		    link_StartDefinition %P %O %i %j "%l"
		} else {
		    link_StartDefinition %P [pad_get_group %P %O] %i %j "%l"
		}
	    } else {
		if {%O != 1} {
		    %P delete "linkconnect.dest"
		    if {$_pad(SelectGroupMembers)} {
			%P ic $_link(source) -linkto %O
		    } else {
			%P ic $_link(source) -linkto [pad_get_group %P %O]
		    }
		    set _link(current) ""
		}
	    }
	}
	break
    }

    $PAD bind all <LinkCreate-KeyPress-space> {
	%P delete "linkconnect.dest"
	eval link_Modify %P [lrange [%P getview] 0 1] 1
	%P ic $_link(current) -linkto [%P bbox 1]
	set _link(current) ""
	break
    }

    ######
    # Events to clean up hyperlinks when a source or destination is deleted
    ######

    $PAD bind link <Delete> {link_Destroy %P %O}
    $PAD bind linkdest <Delete> {link_Destroy %P %O}

    ######
    # Events to make sure things get written out properly
    ######

    $PAD bind linkconnect <Write> {set Pad_Write 0; return ""}
}

#
# Get the ID (or list of IDs) of the item this object is linked to,
# or return an empty string if not to linked to an item.
#
proc link_GetID {PAD obj} {
    set link [$PAD ic $obj -linkto]
    if {[llength $link] == 1} {
	set id [$PAD find -groupmembers withtag $link]
    } else {
	set id ""
    }

    return $id
}

#
# Pointer has moved over a hyperlink, so indicate hyperlink
#
proc link_Enter {PAD obj portals} {
    global _link

    set _link(cursor) [getconfig $PAD -cursor]
    if {[$PAD ic $obj -linkto] != ""} {

			# If item is a group member, then normally,
			# indicate hyperlink around member.  However,
			# if all the members of that group link to
			# the same place, then indicate hyperlink around group.
	set source $obj
	if {[$PAD getgroup $source] != ""} {
	    set group [$PAD getgroup $source]
	    set members [$PAD ic $group -members]
	    set same 1
	    set link [$PAD ic $source -linkto]
	    foreach member [lrange $members 0 end] {
		if {[$PAD ic $member -linkto] != $link} {
		    set same 0
		    break
		}
	    }
	    if {$same} {
		set source $group
	    }
	}

	set _link(current) $obj
        global _pad
	
	    pad_set_cursor $PAD "@$_pad(PadBitmaps)/press.xbm $_pad(PadBitmaps)/pressmask.xbm black white" "press"
		

	set style [$PAD ic $obj -linkstyle]
	switch -exact $style {
	    "underline" {
		link_ShowUnderline $PAD $source $portals
	    }
	    "destbox" {
		link_ShowDestbox $PAD $source $portals
	    }
	    "box" {
		link_ShowBox $PAD $source $portals
	    }
	    "none" {
		# Do nothing
	    }
	}
    } else {
	link_Remove $PAD $obj
    }
}

#
# Pointer has left hyperlink, so stop indicating link
#
proc link_Leave {PAD} {
    global _link

    if {$_link(current) != ""} {
	$PAD config -cursor $_link(cursor)
	$PAD delete linkconnect
	set _link(current) ""
    }
}

#
# Start defining a hyperlink
# If one is currently being defined, then stop that and start another
#
proc link_StartDefinition {PAD obj i j portals} {
    global _link

			# Delete indicator showing source
    $PAD delete "linkconnect.source"

			# If linking from the pad surface, then make a transparent source to link from
    if {$obj == 1} {
	set width [lindex [$PAD config -width] 4]
	set height [lindex [$PAD config -height] 4]
	set zoom [$PAD getzoom]
	set w2 [expr 0.05 * $width / $zoom]
	set h2 [expr 0.05 * $height / $zoom]
	set obj [$PAD create rectangle [expr $i - $w2] [expr $j - $h2] [expr $i + $w2] [expr $j + $h2] \
		     -fill black -pen none -transparency 0 -tags "item linkhidden"]
    }

    set _link(current) $obj

			# Delete existing connection line from this source
    $PAD delete "linkconnect.$obj"  

    set zoom [eval $PAD getzoom $portals]
    set id [$PAD create line $i $j $i $j -pen yellow -events 0 \
		-penwidth 0 -tags "linkconnect linkconnect.$obj" \
		-arrow last]
    set arrowshape [$PAD ic $id -arrowshape]
    set arrowA [lindex $arrowshape 0]
    set arrowB [lindex $arrowshape 1]
    set arrowC [lindex $arrowshape 2]
    set scale [expr 1.5 / $zoom]
    $PAD ic $id -arrowshape "[expr $arrowA * $scale] [expr $arrowB * $scale] [expr $arrowC * $scale]"
}

#
# Modify definition of current hyperlink.
# Update indicator to follow pointer.
# Also give feedback to show what destination object is active
#
proc link_Modify {PAD i j dest} {
    global _link _pad

    if {$_link(current) == ""} {
			# Haven't started creating link yet
	if {$_pad(SelectGroupMembers) == 0} {
	    set dest [pad_get_group $PAD $dest]
	}
	if {$dest != $_link(source)} {
	    $PAD delete linkconnect.source
	    set _link(source) $dest
	    if {$dest != 1} {
		eval $PAD create rectangle [$PAD bbox $dest] -fill none -pen yellow -penwidth 0 \
		    -tags \"linkconnect linkconnect.source\" -events 0
	    }
	}	
    } else {
			# Looking for destination
	set obj $_link(current)
	if {$obj != ""} {
	    set connector [$PAD find -groupmembers withtag "linkconnect.$obj"]
	    if {$connector != ""} {
		set coords [$PAD coords $connector]
		eval $PAD coords $connector [lrange $coords 0 1] $i $j
	    }
	    
	    if {$_pad(SelectGroupMembers) == 0} {
		set dest [pad_get_group $PAD $dest]
	    }
	    if {$dest != $_link(dest)} {
		$PAD delete linkconnect.dest
		set _link(dest) $dest
		if {$dest != 1} {
		    eval $PAD create rectangle [$PAD bbox $dest] -fill none -pen yellow -penwidth 0 \
			-tags \"linkconnect linkconnect.dest\" -events 0
		}
	    }	
	}
    }
}

#
# Create indicators showing connections of each 
# linked object to its destination
#
proc link_ShowConnections {PAD} {
    foreach obj [$PAD find -groupmembers withtag link] {
	set source [bbcenter [$PAD bbox $obj]]
	set link [$PAD ic $obj -linkto]
	if {[llength $link] == 4} {
	    set dest [bbcenter $link]
	} else {
	    set dest [bbcenter [$PAD bbox $link]]
	}
	set id [eval $PAD create line $source $dest -pen yellow -events 0 \
		    -penwidth 0 -tags \"linkconnect linkconnect.$obj\" \
		    -arrow last]
	set arrowshape [$PAD ic $id -arrowshape]
	set arrowA [lindex $arrowshape 0]
	set arrowB [lindex $arrowshape 1]
	set arrowC [lindex $arrowshape 2]
	set scale [expr [maxdim [$PAD bbox $id]] / 100.0]
	$PAD ic $id -arrowshape "[expr $arrowA * $scale] [expr $arrowB * $scale] [expr $arrowC * $scale]"
    }
}

#
# Define link specified by a tagOrId, or a place triplet
#
proc link_Define args {
    global _link _pad

    set PAD [lindex $args 0]
    set obj [lindex $args 1]
    if {[llength $args] == 3} {
	set value [lindex $args 2]
    }

    
				# Set new value if specified
    if {[info exists value]} {
				# If linking from a group, then actually make links
				# from each group member
	set objs [pad_get_members $PAD $obj]
	foreach obj $objs {
	    if {[llength $value] == 1} {
		if {![info exists _pad(importing)] || ($_pad(importing) == 0)} {
		    set dests [pad_get_members $PAD $value]
		    if {[info exists _pad(pasting)] && $_pad(pasting)} {
				# If currently pasting, then we need to create this
				# link from scratch, so first remove it, and then continue on
				# dealing with item id instead of tag.
			link_Remove $PAD $obj
			set value [$PAD find -groupmembers withtag $value]
		    }
		    
		    if {[string range $value 0 3] == "link"} {
				# Link being specified by "link" tag - probably the result
				# of reading in a file, so allow links to nonexistant tags
				# so files can be read in properly. 
				# Also, update counter to be bigger than the current link
			set _link($obj.destobj) $value
			set count [string range $value 4 end]
			if {$_link(counter) <= $count} {
			    set _link(counter) [expr $count + 1]
			}
		    } elseif {$dests != ""} {
				# Specify link to another item
			link_Remove $PAD $obj
			set tag "link$_link(counter)"
			foreach dest $dests {
			    $PAD addtag $tag $dest
			    $PAD addtag "linkdest" $dest
			}
			$PAD addtag "link" $obj
			$PAD addtag "linkto$_link(counter)" $obj
					# If item has "drag" tag, then make it lower priority
			if {[$PAD hastag "drag" $obj]} {
			    $PAD deletetag "drag" $obj
			    $PAD addtag "drag" $obj
			}
			set _link($obj.destobj) $tag
			incr _link(counter)
		    } else {
			return -code error "Invalid link: $value"
		    }
		}
	    } elseif {[llength $value] == 4} {
				# Specify link to a bounding box
		link_Remove $PAD $obj
		$PAD addtag "link" $obj
					# If item has "drag" tag, then make it lower priority
		if {[$PAD hastag "drag" $obj]} {
		    $PAD deletetag "drag" $obj
		    $PAD addtag "drag" $obj
		}
		set _link($obj.destbbox) $value
	    } elseif {$value == ""} {
		link_Remove $PAD $obj
	    } else {
		return -code error "Invalid link: $value"
	    }
	}
    }
				# Return current value
    if {[info exists _link($obj.destobj)]} {
	if {[$PAD hastag "link" $obj] && ([$PAD find -groupmembers withtag $_link($obj.destobj)] != "")} {
	    set link $_link($obj.destobj)
	} else {			
	    set link ""
	}
    } elseif {[info exists _link($obj.destbbox)]} {
	if {[$PAD hastag "link" $obj]} {
	    set link $_link($obj.destbbox)
	} else {
	    set link ""
	}
    } else {
	set link ""
    }

    return $link
}

#
# Define link style to be "box", "destbox", "underline", or "none"
#
proc link_Style args {
    global _link

    set PAD [lindex $args 0]
    set obj [lindex $args 1]
    if {[llength $args] == 3} {
	set value [lindex $args 2]
    }

				# Set new value if specified
    if {[info exists value]} {
	switch -exact $value {
	    "none" -
	    "box" -
	    "destbox" -
	    "underline" {
		set _link($obj.style) $value
	    }
	    default {
		return -code error "Bad style, must be: box, destbox, underline, or none"
	    }
	}
    }

				# Return current value
    if {[info exists _link($obj.style)]} {
	set style $_link($obj.style)
    } else {
	set style "box"
    }

    return $style
}

#
# Define link color
#
proc link_Color args {
    global _link

    set PAD [lindex $args 0]
    set obj [lindex $args 1]
    if {[llength $args] == 3} {
	set value [lindex $args 2]
    }

				# Set new value if specified
    if {[info exists value]} {
	set _link($obj.color) $value
    }

				# Return current value
    if {[info exists _link($obj.color)]} {
	set color $_link($obj.color)
    } else {
	set color "red"
    }

    return $color
}

#
# Remove the link from the specified object.
#
proc link_Remove {PAD tagOrIds} {
    global _link

    if {[llength $tagOrIds] > 1} {
	foreach tagOrId $tagOrIds {
	    link_Remove $PAD $tagOrId
	}
	return
    }
    set obj [$PAD find -groupmembers withtag $tagOrIds]

    $PAD deletetag "link" $obj
    if {[info exists _link($obj.destobj)]} {
	set tag $_link($obj.destobj)
	set count [string range $tag 4 end]
	$PAD deletetag "linkto$count" $obj
	if {[$PAD hastag "linkhidden" $obj]} {
	    $PAD delete "linkhidden"
	}
	set dest [$PAD find -groupmembers withtag $tag]
	$PAD deletetag $tag $tag
			# Determine if this was the last link terminating at dest.
			# If so, then remove "linkdest" tag
	foreach item $dest {
	    set count 0
	    foreach tag [$PAD gettags $item] {
		if {([string range $tag 0 3] == "link") && ([string range $tag 0 5] != "linkto") && ($tag != "link")} {
		    incr count
		}
	    }
	    if {$count == 1} {
		$PAD deletetag "linkdest" $item
	    }
	}
	unset _link($obj.destobj)
    }
    if {[info exists _link($obj.destbbox)]} {
	unset _link($obj.destbbox)
    }
    if {[info exists _link($obj.style)]} {
	unset _link($obj.style)
    }
    if {[info exists _link($obj.color)]} {
	unset _link($obj.color)
    }
}

#
# The object is getting destroyed, so remove
# all links to and from it.
#
proc link_Destroy {PAD obj} {
    if {![winfo exists $PAD]} {
	return
    }

    link_Remove $PAD $obj

    foreach tag [$PAD gettags $obj] {
	if {[string range $tag 0 3] == "link"} {
	    set count [string range $tag 4 end]
	    set item [$PAD find -groupmembers withtag "linkto$count"]
	    if {$item != ""} {
		link_Remove $PAD $item
	    }
	}
    }
}

#
# Follow the link from the specified object.
# If within a portal, then follow the link within the portal,
# unless the shift key is down in which case the link is
# followed at the top-level view.
#
proc link_Follow {PAD obj state portals} {
    global _modifier _pad

    set link [$PAD ic $obj -linkto]
    if {$link != ""} {
				# Holding the shift key should follow link
				# in top-level view
	if {$state & $_modifier(Shift)} {
	    set portals ""
	}

	if {[llength $link] == 1} {
				# Follow link to another item
	    eval $PAD center -twostep $link $_pad(AnimationSpeed) 0.5 0.5 0.9 $portals
	} else {
				# Follow link to a place
	    eval $PAD centerbbox -twostep $link $_pad(AnimationSpeed) 0.5 0.5 1.0 $portals
	}
    }
}

#
# Show indicator of this hyperlink source.
# This is a box around the source.
#
proc link_ShowBox {PAD obj portals} {
    set bbox [$PAD bbox $obj]

					# If source is a group, then show link that
					# first member goes to
    if {[$PAD type $obj] == "group"} {
	set obj [lindex [$PAD ic $obj -members] 0]
    }

    eval $PAD create rectangle $bbox -pen [$PAD ic $obj -linkcolor] -penwidth 0 \
	    -tags "linkconnect" -events 0
}

#
# Show indicator of this hyperlink source.
# This is a box around the source with a line
# to the destination.
#
proc link_ShowDestbox {PAD obj portals} {
    set zoom [eval $PAD getzoom $portals]
    set bbox [$PAD bbox $obj]
    set bbcenter [bbcenter $bbox]    

					# If source is a group, then show link that
					# first member goes to
    if {[$PAD type $obj] == "group"} {
	set obj [lindex [$PAD ic $obj -members] 0]
    }

    eval $PAD create rectangle $bbox -fill [$PAD ic $obj -linkcolor] -pen none \
	    -transparency 0.5 -tags "linkconnect" -events 0

    set link [$PAD ic $obj -linkto]
    if {[llength $link] == 4} {
	set dest [bbcenter $link]
    } elseif {[llength $link] == 1} {
	set dest [bbcenter [$PAD bbox $link]]
    }
    if {$dest != ""} {
	eval $PAD create line $bbcenter $dest -pen [$PAD ic $obj -linkcolor] -penwidth [expr 5.0 / $zoom] \
	    -transparency 0.5 -tags linkconnect -events 0 -capstyle "butt"
    }
}

#
# Show indicator of this hyperlink source.
# This is an underline directly below the object.
#
proc link_ShowUnderline {PAD obj portals} {
    set bbox [$PAD bbox $obj]
    set x0 [lindex $bbox 0]
    set y0 [lindex $bbox 1]
    set x1 [lindex $bbox 2]
    set y1 [lindex $bbox 3]

					# If source is a group, then show link that
					# first member goes to
    if {[$PAD type $obj] == "group"} {
	set obj [lindex [$PAD ic $obj -members] 0]
    }

    $PAD create line $x0 $y0 $x1 $y0 -pen [$PAD ic $obj -linkcolor] -penwidth 0 \
	    -tags linkconnect -events 0
}

#
# Convert old-style hyperlink format to the current format.
# This goes through all objects that are the old format,
# and converts them.
#
proc link_ConvertFormat {PAD} {
    global _hyper

    set objs [$PAD find -groupmembers withtag "Hyper"]
    foreach obj $objs {
	if {[info exists _hyper($obj.obj)]} {
			# Link to an item
	    $PAD ic $obj -linkto $_hyper($obj.obj)
	} elseif {[info exists _hyper($obj)]} {
			# Link to a place
	    $PAD ic $obj -linkto $_hyper($obj)
	}
    }
    
    $PAD deletetag "Hyper" all
    catch {unset _hyper}
}

#
# When an item is pasted, we don't want it to be the destination
# of any links, so remove related tags.  The paste code calls this routine.
#
proc link_CleanupPaste {PAD obj} {
    $PAD deletetag "linkdest" $obj
    foreach tag [$PAD gettags $obj] {
	if {[regexp {link[0-9]+} $tag]} {
	    $PAD deletetag $tag $obj
	}
    }
}

#
# The specified list of files have just been read in, and need to be cleaned up.
# The links to places are fine, but the links to other items might
# conflict with existing links.  The tags must be changed to be larger
# than any existing link tags, and then the link must be made.
#
proc link_CleanupImport {PAD objs} {
    global _link

    set delta [expr $_link(counter) - 1]
    set maxlink $_link(counter)

    foreach obj $objs {
	set tags [$PAD gettags $obj]
	foreach tag $tags {
	    if {[regexp {linkto[0-9]+} $tag]} {
		set num [expr $delta + [string range $tag 6 end]]
		$PAD deletetag $tag $obj
		$PAD addtag "linkto$num" $obj
		set _link($obj.destobj) "link$num"
		if {$num > $maxlink} {
		    set maxlink $num
		}
	    }
	    if {[regexp {link[0-9]+} $tag]} {
		set num [expr $delta + [string range $tag 4 end]]
		$PAD deletetag $tag $obj
		$PAD addtag "link$num" $obj
	    }
	}
    }

    incr _link(counter) [expr $maxlink + 1]
}

#
# This hyperlink source has moved.  So, if it is a link to a view bbox,
# then change the view bbox accordingly.
#
proc link_SourceMoved {PAD obj obbox nbbox} {
    set linkto [$PAD ic $obj -linkto]
    if {[llength $linkto] == 4} {
	set newview [pad_calculate_transformed_view_bbox $linkto $obbox $nbbox]
	$PAD ic $obj -linkto $newview
    }
}
