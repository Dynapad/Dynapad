# "(c) Copyright 1993-1997 Pad++ Consortium University of New Mexico (UNM),
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

#############################################################################
#
#  This file sets the bindings that are needed by pad animation objects 
#  and implements most of the inner workings of animation.
#
############################################################################
#
#  How Animations Are Stored
#  Animation paths and other attributes are stored in the _animate array this
#  way: _animate(ObjPath.<id#>.<animationAttribute>)
#  The animation attributes that we keep track of are:
#
#             pathF - the forward path of the animation. This is a list
#                     of dx, dy values.  AnimPlay goes thru this list and
#                     slides the object according to the dx and dy of each
#                     list element.
#
#             pathR - the reverse path of the animation.  This is just a 
#                     reversed pathF.
#
#             xF -  xcoord of the animation endpoint
#             yF -  ycoord of the animation endpoint
#             sF -  scale of the animation endpoint, although animation
#                   does not do anything with scale at this time.
#
#             xR -  xcoord of the animation startpoint
#             yR -  ycoord of the animation startpoint
#             sR -  scale of the animation startpoint, although animation
#                   does not do anything with scale at this time.
#
#             pathLine - id of the line that graphically
#                          represents the animation path of that object.
#
#             N - Number of elements (dx, dy pairs) in the animation path 
#             time - list of time values (in ms) for animPlay to wait 
#                    between slides. Each element in this list
#                    corresponds to a dx, dy pair in the animation path.
#
#             dx, dy - This is the x and y distance of the startpoint 
#                      to the endpoint.  This is used when importing 
#                      animations (see menu.tcl:import).
#
############################################################################
#  
#   Important Procedures
#   animInit - Must be called _first_ to set up bindings and 
#              initialize some variables.
#  
#   animSetObjPath, animRecordMotion, animEndRecordMotion - These procs
#     are used in recording animations.  They create animation paths and
#     set up all other variables that are needed. Tied to AnimRecord events.
#
#   animDelObject - When animation object is deleted, this procedure 
#                   is called and unsets all tcl info that pertains to that
#                   object.
#
#   animWrite - write out animations to pad files.  Fired by write events.
#
#   animPlay - Play an anim-object's animation.  
#              To play forward:
#                    animPlay R F $PAD
#
#              To play backward:  
#                    animPlay F R $PAD
#
#############################################################################
#
#   How Animations are Saved Out
#   There are a few things that are done first:
#   
#   Get the coordinates, color, place, and arrowshape of the 
#   anim-object's pathLine
#   
#   Dx and dy are the distances between start and endpoints.  They
#   are scaled according to zoom (creating sort of a unit distance).
#
#   Next, go thru the path lists and scale each element according to zoom.
#
#   Now its time to actually write stuff out. Here's whats most important:  
#    1 - Get the zoom.  The path will have to be scaled to this zoom if 
#        importing.
#    2 - Check if this pad file is being imported.  This _animate(import)
#        gets set within proc import in menu.tcl       
#    3 - If we are importing, then go thru the path list and scale it.
#        The code that determines the endpoint is in menu.tcl at the end
#        of proc import. 
#    4 - Otherwise, set the object's path to the original path.  We dont
#         need to scale it if we are doing a pad open file.
#
#    5 - Then set the rest of the tcl vars that are needed by animation.
#
##########################################################################

####################################################
#
# proc animInit {PAD}
#
# Initializing routine. Set needed event bindings.
#
####################################################
proc animInit {PAD} {

    global _animate
    global _color

    set _animate(REL) OFF

    set _animate(appendMode) OFF

    # Get ready to do stuff
    set _animate(mode) ready

    # Set the animation play speed 
    # to normal speed. 
    set _animate(speed) 0

    set _animate(maxSpeedValue) 0
    
    set _animate(minSpeedValue) -10

    set _animate(groupCount) 0

    set _animate(options) OFF

    $PAD modifier create AnimRecord

    $PAD modifier create AnimMAdd

    $PAD modifier create AnimMRemove

    # Define record bindings
    
    # Record zooming
    $PAD bind all <AnimRecord-ButtonPress-2> \
	"zoomEvent $PAD %P %W press %x %y %i %j %l in; animRecordZoom $PAD %O in"

    $PAD bind all <AnimRecord-ButtonPress-3> \
	"zoomEvent $PAD %P %W press %x %y %i %j %l out; animRecordZoom $PAD %O out"
    
    $PAD bind all <AnimRecord-ButtonRelease-3> \
	"zoomEvent $PAD %P %W release"

    $PAD bind all <AnimRecord-ButtonRelease-3> \
	"zoomEvent $PAD %P %W release"

    $PAD bind all <AnimRecord-ButtonPress-1> \
	"animSetObjPath %i %j %O %W $PAD; break"

    $PAD bind all <AnimRecord-B1-Motion> \
	"animRecordMotion %i %j %W $PAD; break"

    $PAD bind all <AnimRecord-ButtonRelease-1> \
	"animEndRecordMotion $PAD"
    
    # Define selectmode bindings
    $PAD bind animate <Select-Enter> \
	"animSelectEvent $PAD <Select-Enter> %O; break"

    $PAD bind animate <Select-Leave> \
	"animSelectEvent $PAD <Select-Leave> %O; break"

    $PAD bind animate <Select-ButtonPress-1> \
	"animSelectEvent $PAD <Select-ButtonPress-1> %O "

    $PAD bind animate <Select-B1-Motion> \
	"animSelectEvent $PAD <Select-B1-Motion> %O"

    $PAD bind animate <Select-ButtonRelease-1> \
	"animSelectEvent $PAD <Select-B1-ButtonRelease> %O"

    # This binding allows you to play an animation in run mode 
    # by clicking on the object

    # Play Forward
    $PAD bind animate <Run-Button-1> \
	"animRunEvent $PAD <Run-Button-1> %O; break"
    
    $PAD bind animate <Run-B1-Motion> "break"

    # Play Backward when hitting the shift key also
    # This also does not work
    $PAD bind animate <Run-KeyPress-Shift_L> \
	"animRunEvent $PAD <Run-KeyPress-Shift> %O"

    $PAD bind animate <Run-KeyPress-Shift_R> \
	"animRunEvent $PAD <Run-KeyPress-Shift> %O"

    ######

    $PAD bind animate <Run-ButtonRelease-1> \
	"animRunEvent $PAD <Run-ButtonRelease-1> %O; break"

    # This indicates that the object has an animation
    $PAD bind animate <Run-Enter> \
	"animRunEvent $PAD <Run-Enter> %O; break"

    $PAD bind animate <Run-Leave>  \
	"animRunEvent $PAD <Run-Leave> %O; break"

    # Delete the path line of an object 
    # whenever we delete it
    $PAD bind animate <Delete> \
	"animDelObject $PAD %O"

    # Bindings for writing out animation objects
    $PAD bind animate <Write> {return [animWrite %P %O]}

    $PAD bind pathLine <Write> {set Pad_Write 0; return ""}

    $PAD bind hilite_sel <Write> {set Pad_Write 0; return ""}

    # Colors
    set _color(animHilite.0) "\#ff0000"
    set _color(animHilite.1) "\#c33b3b"  
    set _color(animHilite.2) "\#8b7373"
    set _color(animHilite.3) "\#ea6060"

    # Group colors are shades of blue
    set _color(animMulti.selected) "DeepSkyBlue3"
    set _color(animMulti.entered) "DeepSkyBlue1"

    set _animate(animMultiCount) 0 
}

#################################################
#
# proc animRecordZoom {PAD objectID dir}
#
# Make a zoom path.  
# Using the anim tools interface:
#        Select an object
#        Click record button
#        Zoom in or out.
#        Hit stop when done.
#
# This procedure creates a zoom path:
#     _animate(OnjPath.<id#>.zoomf)  <- Forward path
#     _animate(OnjPath.<id#>.zoomr)  <- Reverse path
#
# Still need an animPlayZoom though...
#################################################
proc animRecordZoom {PAD objectID dir} {
    global _animate
    global _zoom
    global Pad_Error
    
    set gID [$PAD getgroup $objectID]
    if {$gID != ""} {
	set objectID $gID	
    }
    
    if {[info exists _animate(ObjPath.$objectID.zoomf)]} {
	unset _animate(ObjPath.$objectID.zoomf)
	unset _animate(ObjPath.$objectID.zoomr)
    }
    
    set _animate(ObjPath.$objectID.zoomr) ""

    _animRecordZoom $PAD $objectID $dir

}

#################################################
#
# proc _animRecordZoom {PAD objectID dir}
#
# Helper function for animRecordZoom.
#
#################################################
proc _animRecordZoom {PAD objectID dir} {
    global _animate
    global _zoom
    global Pad_Error

    set gID [$PAD getgroup $objectID]
    if {$gID != ""} {
	set objectID $gID	
    }
    
    if {($_zoom(more) == 0) || ($Pad_Error == 1)} {
	pad_unselect $PAD $objectID
	return
    }

    pad_select $PAD $objectID
    $PAD delete modifier

    set place [$PAD ic $objectID -place]

    lappend _animate(ObjPath.$objectID.zoomf) $place

    set _animate(ObjPath.$objectID.zoomr) \
	[linsert $_animate(ObjPath.$objectID.zoomr) 0 $place]

    after idle "_animRecordZoom $PAD $objectID $dir"
}
  
#################################################
#
# proc animSetObjPath {x y object PAD}
#
# Get the selected id of the item and set 
# its startpoint. 
#
# Function is fired by:
#        $PAD bind all <ButtonPress-1>
#
#################################################    
proc animSetObjPath {x y object w PAD} {

    global _animate
    global _color

    # Check if we have selected anything.  If we haven't, and there is no
    # default, then return without doing anything.
    set itemCount [llength $_animate(selectedObject)]

    if {$_animate(selectedObject) == ""} {
	if {$object != 1} {
	    set gid [$PAD getgroup $object]
	    if {$gid == ""} {	    
		set _animate(recordID) $object
		set _animate(selectedObject) $object
		pad_hilite $PAD $_animate(recordID) \
		    $_color(animHilite.$_animate(hilite.color.count))
	    } else {
		set _animate(recordID) $gid
		set _animate(selectedObject) $gid
		foreach id [$PAD ic $gid -members] {
		    $PAD addtag "animate" $id
		}
		pad_hilite $PAD $gid \
		    $_color(animHilite.$_animate(hilite.color.count))
	    }
	} else {
	    set _animate(recordID) ""
	    return	
	}
    } elseif {$itemCount > 1} {
	# If more than one object is selected then group it.
	set _animate(recordID) \
	    [$PAD create group -members $_animate(selectedObject) -divisible 0]
	foreach id $_animate(selectedObject) {
	    # Un hilite each item within the group
	    pad_unhilite $PAD $id

	    # Hilite entire group
	    pad_hilite $PAD $_animate(recordID) \
		    $_color(animHilite.$_animate(hilite.color.count))
	}
    } else {
	# Otherwise don't group it
	set _animate(recordID) $_animate(selectedObject)

	if {[$PAD hastag "group" $_animate(recordID)]} {
	    foreach id [$PAD ic $_animate(recordID) -members] {
		$PAD addtag "animate" $id
	    }
 	}   
    }
    set zoom [$PAD getzoom]
    set penwidth [expr 1.0 / $zoom]
    set arrowshape "[expr 8.0 / $zoom] [expr 10.0 / $zoom] [expr 3.0 / $zoom]"

    foreach memberID $_animate(selectedObject) {
	if {$_animate(appendMode) == "OFF"} {
	    if {[info exists _animate(ObjPath.$memberID)]} {
		unset _animate(ObjPath.$memberID)
	    }
	    if {[info exists _animate(ObjPath.$memberID.pathF)]} {
		unset _animate(ObjPath.$memberID.pathF)
	    }
	    if {[info exists _animate(ObjPath.$memberID.pathR)]} {
		    unset _animate(ObjPath.$memberID.pathR)
	    }
	    if {[info exists _animate(ObjPath.$memberID.pathLine)]} {
		$PAD ic $_animate(ObjPath.$memberID.pathLine) -lock 0
		$PAD delete $_animate(ObjPath.$memberID.pathLine)
	    }
	    if {[info exists _animate(ObjPath.$memberID.time)]} {
		    unset _animate(ObjPath.$memberID.time)
	    }
	    # Get the initial absolute coordinates of each 
	    # of the selected objects.
	    set place [$PAD ic $memberID -place]
	    set _animate(ObjPath.$memberID.xR) [lindex $place 0]
	    set _animate(ObjPath.$memberID.yR) [lindex $place 1]
	    set _animate(ObjPath.$memberID.sR) [lindex $place 2]

	    set _animate(ObjPath.$memberID.pathLine) \
	    [$PAD create line $x $y \
		 -pen $_color(animHilite.$_animate(hilite.color.count)) \
		 -penwidth $penwidth \
		 -arrowshape $arrowshape \
		 -tags "pathLine" \
		 -events 0]
	} else {
	    if {![info exists _animate(ObjPath.$memberID.xR)]} {
		set place [$PAD ic $memberID -place]
		set _animate(ObjPath.$memberID.xR) [lindex $place 0]
		set _animate(ObjPath.$memberID.yR) [lindex $place 1]
		set _animate(ObjPath.$memberID.sR) [lindex $place 2]
	    } else {
		set place [$PAD ic $memberID -place]		
		set dx [expr [lindex $place 0] - $_animate(ObjPath.$memberID.xF)]
		set dy [expr [lindex $place 1] - $_animate(ObjPath.$memberID.yF)]
		set ds [expr [lindex $place 2] - $_animate(ObjPath.$memberID.sF)]

		lappend _animate(ObjPath.$memberID.pathF) [list $dx $dy]

		if {$itemCount == 1} {
		    lappend _animate(ObjPath.$memberID) [list $dx $dy]
		}
	    }
	}
	$PAD addtag "animate" $memberID
    }
    $PAD addtag "hilite" hilite$_animate(recordID)
	
    # Get scale or zoom factor of object
    set s [lindex [$PAD ic $_animate(recordID) -place] 2]
   
    if {![bbenclosed $x $y [$PAD bbox $_animate(recordID)]]} {	
	set _animate(recordID) ""	
	return	
    }

    # Get the recordID of the selected item.
    set _animate(defaultPlayID) $_animate(selectedObject)

    # Apply an animation tag to the object
    $PAD addtag "animate" $_animate(recordID)

    # Do time stuff
    set _animate(timer) [$w clock]
    set _animate(tLast) [$w clock $_animate(timer)]

    # Create a path line
    if {$_animate(appendMode) == "OFF" || \
	    ![info exists _animate(ObjPath.$_animate(recordID).pathLine)]  } {

	if {$itemCount == 1} {
	    
	    if {[info exists _animate(ObjPath.$_animate(recordID).pathLine)]} {
		$PAD delete $_animate(ObjPath.$_animate(recordID).pathLine)
	    }
	} else {
	    set id [lindex $_animate(selectedObject) 0]
	    if {[info exists _animate(ObjPath.$id.pathLine)] \
		    && $_animate(appendMode) == "ON"} {
		$PAD ic $_animate(ObjPath.$id.pathLine) -lock 0 -transparency 1
	    }
	}
		
	set _animate(ObjPath.$_animate(recordID).pathLine) \
	    [$PAD create line $x $y \
		 -pen $_color(animHilite.$_animate(hilite.color.count)) \
		 -penwidth $penwidth \
		 -arrowshape $arrowshape \
		 -tags "pathLine" \
		 -events 0]
    } else {
	$PAD ic $_animate(ObjPath.$_animate(recordID).pathLine) -lock 0 

	$PAD ic $_animate(ObjPath.$_animate(recordID).pathLine) -transparency 1
    }
	
	
    # Unlock the pathLine
    $PAD ic $_animate(ObjPath.$_animate(recordID).pathLine) -lock 0

    # show the path line
    $PAD ic $_animate(ObjPath.$_animate(recordID).pathLine) -transparency .5 

    set _animate(lastX) $x
    set _animate(lastY) $y
    set _animate(lastS) $s

    incr _animate(hilite.color.count)
    if {$_animate(hilite.color.count) > 3} {
	set _animate(hilite.color.count) 0
    }
}

#######################################################
#
# proc animRecordMotion {x y PAD}
#
# Record the drag motion of an object.  Creates a list
# of dx and dy movements.
#
# Function is fired by:
#        $PAD bind all <B1-Motion>
#
#######################################################
proc animRecordMotion {x y w PAD} {

    global _animate
    global _color

    if { $_animate(recordID) == ""  } {
	return
    }
    set s [lindex [$PAD ic $_animate(recordID) -place] 2]

    # Draw a line for our path
    $PAD coords -append \
	$_animate(ObjPath.$_animate(recordID).pathLine) $x $y

    $PAD ic $_animate(ObjPath.$_animate(recordID).pathLine) \
	-arrow last 

    # Get the distance that the mouse moved
    set dx [expr $x - $_animate(lastX)]
    set dy [expr $y - $_animate(lastY)]
    set ds [expr $s - $_animate(lastS)]

    # Slide the object to simulate dragging
    $PAD slide $_animate(recordID) $dx $dy

    # Slide the hilite box
    $PAD slide hilite$_animate(recordID) $dx $dy

    # Store the coords.
    lappend _animate(ObjPath.$_animate(recordID)) [list $dx $dy]

    # keep track of the last x,y coords
    # (we use them to compute dx, dy)
    set _animate(lastX) $x
    set _animate(lastY) $y
    set _animate(lastS) $s

    # Keep track of time
    set _animate(tPres) [$w clock $_animate(timer)]
    set dt [expr $_animate(tPres) - $_animate(tLast)]
    set _animate(tLast) [$w clock $_animate(timer)]

    # Append the time differential
    lappend _animate(ObjPath.$_animate(recordID).time) $dt

}
	
##########################################################
#
# proc animEndRecordMotion {PAD}
#
# Stop drawing an animation path.  Set the endpoint.  Assign
# The recorded object(s) animation data.
#
# Function is fired by:
#        $PAD bind all <B1-ButtonRelease>
#
##########################################################
proc animEndRecordMotion {PAD} {

    global _animate

    if {$_animate(recordID) == "" || \
	    ![info exists _animate(ObjPath.$_animate(recordID))]} {
	return
    }

    if {[llength $_animate(selectedObject)] > 1} {
	set coords [$PAD coords $_animate(ObjPath.$_animate(recordID).pathLine)]
	set color [$PAD ic $_animate(ObjPath.$_animate(recordID).pathLine) -pen]
	set place [$PAD ic $_animate(ObjPath.$_animate(recordID).pathLine) -place]

 	$PAD delete $_animate(ObjPath.$_animate(recordID).pathLine)
# 	unset _animate(ObjPath.$_animate(recordID).pathLine)

	pad_unhilite $PAD $_animate(recordID)
	pad_ungroup $PAD $_animate(recordID)

	# re-hilite the group members
	foreach id $_animate(selectedObject) {
	    pad_hilite $PAD $id red
	    $PAD addtag "hilite" hilite$id

	    if {$_animate(appendMode) == "OFF"} {
		set t [eval $PAD coords -append $_animate(ObjPath.$id.pathLine) $coords]
		$PAD ic $_animate(ObjPath.$id.pathLine) -arrow last
		set newpath $_animate(ObjPath.$_animate(recordID))
		set _animate(ObjPath.$id.time) \
		    $_animate(ObjPath.$_animate(recordID).time)
	    } else {
		if {[info exists _animate(ObjPath.$id.pathLine)]} {
		    set oldcoords [$PAD coords $_animate(ObjPath.$id.pathLine)]
		    set newcoords [concat $oldcoords $coords]
		} else {
		    set newcoords $coords
		}
		    set _animate(ObjPath.$id.pathLine) \
			[eval $PAD create line $newcoords \
			     -events 0 \
			     -tags pathLine \
			     -transparency 0 \
			     -arrow last \
			     -pen $color \
			     -lock 1]
		if {[info exists _animate(ObjPath.$id.pathF)]} {
		    set newpath $_animate(ObjPath.$id.pathF)
		} else {
		    set newpath ""
		}
		set newpath [append newpath " $_animate(ObjPath.$_animate(recordID))"]
	    }
	    set _animate(ObjPath.$id.pathF) $newpath
	    
	    set place [$PAD ic $id -place]
		
	    set _animate(ObjPath.$id.xF) [lindex $place 0]
	    set _animate(ObjPath.$id.yF) [lindex $place 1]
	    set _animate(ObjPath.$id.sF) [lindex $place 2]
		    
	    animMakeRevPath $id

	    set _animate(ObjPath.$id.N) \
		[llength $_animate(ObjPath.$id.pathF)]
	}
    } else {   
	if {[info exists _animate(ObjPath.$_animate(recordID).pathF)]} {
	    unset _animate(ObjPath.$_animate(recordID).pathF)
	}
	set _animate(ObjPath.$_animate(recordID).pathF) \
	    $_animate(ObjPath.$_animate(recordID))
	
	set place [$PAD ic $_animate(recordID) -place]
	
	set _animate(ObjPath.$_animate(recordID).xF) [lindex $place 0]
	set _animate(ObjPath.$_animate(recordID).yF) [lindex $place 1]
	set _animate(ObjPath.$_animate(recordID).sF) [lindex $place 2]
	
	animMakeRevPath $_animate(recordID)
	
	set _animate(ObjPath.$_animate(recordID).N) \
	    [llength $_animate(ObjPath.$_animate(recordID).pathF)]
    }	
    set _animate(selectedObject) "" 
}


##########################################################################
#
# proc animSelectEvent {PAD objectID} 
#
# Catch animation select events 
#
##########################################################################
proc animSelectEvent {PAD  event objectID} {
    global _animate 
    global _color

    if {$_animate(mode) == "forwardPlay" || \
	    $_animate(mode) == "backwardPlay" } {
	return
    }

    set gID [$PAD getgroup $objectID]
    if {$gID != ""} {
	set objectID $gID
    }

    set tags [$PAD gettag $objectID]
    set index [lsearch -regexp $tags "multi_*"]

    switch -exact -- $event {
	<Select-Enter> {
	    set zoom [$PAD getzoom]
	    set penwidth [expr 1.0/$zoom]
	    set arrowshape "[expr 8.0 / $zoom] [expr 10.0 / $zoom] [expr 3.0 / $zoom]"
	    
	    $PAD ic $_animate(ObjPath.$objectID.pathLine) -lock 0
	    $PAD ic $_animate(ObjPath.$objectID.pathLine) \
		-transparency .5 \
		-penwidth $penwidth \
		-arrowshape $arrowshape \
		-lock 1 
	    
	    if {$index != -1} {
		set ids [$PAD find withtag [lindex $tags $index]]
		
		foreach id $ids {
		    pad_hilite $PAD $id $_color(animMulti.entered) .5
		}
	    } else {
		pad_hilite $PAD $objectID\
		    [$PAD ic $_animate(ObjPath.$objectID.pathLine) -pen] .3
		$PAD ic $_animate(ObjPath.$objectID.pathLine) -lock 0
		$PAD ic $_animate(ObjPath.$objectID.pathLine) \
		    -transparency .5 -lock 1
	    }
	}

	<Select-Leave> {
	    if {$index != -1} {
		set ids [$PAD find withtag [lindex $tags $index]]
		foreach id $ids {
		    pad_unhilite $PAD $id 
		    $PAD ic $_animate(ObjPath.$id.pathLine) -lock 0
		    $PAD ic $_animate(ObjPath.$id.pathLine) \
			-transparency 0 -lock 1
		}
	    } else {
		pad_unhilite $PAD $objectID
		$PAD ic $_animate(ObjPath.$objectID.pathLine) -lock 0    
		$PAD ic $_animate(ObjPath.$objectID.pathLine) \
		    -transparency 0 -lock 1
	    }
	}

	<Select-ButtonPress-1> {
	    
	}

	<Select-B1-Motion> {
	    
	}

	<Select-B1-ButtonRelease> {
	    if {$index != -1} {
		$PAD ic hilite_sel -lock 0
		$PAD delete hilite_sel

		set group [lindex $tags $index]
		set _animate(selectedGroup) $group
		set ids [$PAD find withtag $group]
		foreach id $ids {
		    pad_hilite $PAD $id $_color(animMulti.selected) .8
		    $PAD addtag "hilite_sel" hilite$id 
		    $PAD ic hilite$id -lock 1
		}
	    }

	}
    }
}

##########################################################################
#
# proc animRunEvent {PAD objectID}
#
# Do stuff in run mode
#
##########################################################################
proc animRunEvent {PAD event objectID} {
    global _animate
    global _color

    if {$_animate(mode) == "forwardPlay" || \
	    $_animate(mode) == "backwardPlay"} {
	if {$event != "<Run-ButtonRelease-1>" } {
	    return
	}
    }

    set gID [$PAD getgroup $objectID]
    if {$gID != ""} {
	set objectID $gID
    }

    set tags [$PAD gettag $objectID]
    set index [lsearch -regexp $tags "multi_*"] 

    switch -exact -- $event {
	
	<Run-Button-1> {
	    if {$index != -1} {
		set family [$PAD find withtag [lindex $tags $index]]
 	    } 

	    set _animate(defaultPlayID) $objectID

	    if {$index != -1} {
	      foreach id $family {
		 $PAD ic hilite$id -lock 0
		 pad_unhilite $PAD $id
	      }
	    }


	    set _animate(mode) forwardPlay
	    animPlay R F $PAD
	    after idle pad_unselect $PAD $objectID
	    set _animate(mode) ready
	}

	<Run-KeyPress-Shift> {
	    pad_unhilite $PAD $objectID

	    set _animate(defaultPlayID) $objectID

	    animPlay F R $PAD	    
	}

	<Run-ButtonRelease-1> {
	}

	<Run-Enter> {
	    if {$index != -1} {
		# hilite the group members
		set ids [$PAD find withtag [lindex $tags $index]]
	
		foreach id $ids {
		    pad_hilite $PAD $id $_color(animMulti.entered) .3
		}
	    } else {
		pad_hilite $PAD $objectID $_color(animMulti.entered) .3
	    }
	}

	<Run-Leave> {
	    pad_unhilite $PAD $objectID

	    set tags [$PAD gettag $objectID]
	    set index [lsearch -regexp $tags "multi_*"]

	    if {$index != -1} {
		set ids [$PAD find withtag [lindex $tags $index]]

		foreach id $ids {
		    pad_unhilite $PAD $id 
		}
	    }
	}
    }
}

##########################################################################
# proc animDelObject {obj}
#
# This procedure removes all references to an animate object once it is
# deleted.
#
##########################################################################
proc animDelObject {PAD obj} {
    global _animate

    if {$_animate(mode) == "rec"} {
	return
    }

    set gID [$PAD getgroup $obj]
    if {$gID != ""} {
	set obj $gID
    }

    $PAD ic $_animate(ObjPath.$obj.pathLine) -lock 0
    $PAD delete $_animate(ObjPath.$obj.pathLine)
    $PAD ic hilite$obj -lock 0
    $PAD delete hilite$obj

    if {[info exists _animate(ObjPath.$obj.pathF)]} {
	unset _animate(ObjPath.$obj.pathF)
    }
    if {[info exists animate(ObjPath.$obj.pathR)]} {
	unset _animate(ObjPath.$obj.pathR)
    }
    if {[info exists animate(ObjPath.$obj.pathLine)]} {
	unset _animate(ObjPath.$obj.pathLine)
    }
    if {[info exists _animate(ObjPath.$obj.xR)]} {
	unset _animate(ObjPath.$obj.xR)
    }
    if {[info exists _animate(ObjPath.$obj.yR)]} {
	unset _animate(ObjPath.$obj.yR)
    }
    if {[info exists _animate(ObjPath.$obj.sR)]} {
	unset _animate(ObjPath.$obj.sR)
    }
    if {[info exists _animate(ObjPath.$obj.xF)]} {
	unset _animate(ObjPath.$obj.xF)
    }
    if {[info exists _animate(ObjPath.$obj.yF)]} {
	unset _animate(ObjPath.$obj.yF)
    }
    if {[info exists _animate(ObjPath.$obj.sF)]} {
	unset _animate(ObjPath.$obj.sF)
    }
    if {[info exists _animate(ObjPath.$obj.N)]} {
	unset _animate(ObjPath.$obj.N)
    }
    if {[info exists _animate(ObjPath.$obj.time)]} {
	unset _animate(ObjPath.$obj.time)      
    }
}
  
##########################################################################
# proc animWrite {PAD %O}
#
# write out animation objects
#
##########################################################################
proc animWrite {PAD obj} {
    global _animate
   
    set gID [$PAD getgroup $obj]
    if {$gID != ""} {
	#set obj $gID
	set Pad_Write 0
	return ""
    }

    # These vars are used for writing out path lines
    set coords [$PAD coords $_animate(ObjPath.$obj.pathLine)]
    set color [$PAD ic $_animate(ObjPath.$obj.pathLine) -pen]
    set place [$PAD ic $_animate(ObjPath.$obj.pathLine) -place]
    set penwidth [$PAD ic $_animate(ObjPath.$obj.pathLine) -penwidth]
    set arrowshape [$PAD ic $_animate(ObjPath.$obj.pathLine) -arrowshape]

    set zoom [$PAD getzoom]

    set dx [expr [expr $_animate(ObjPath.$obj.xF) - \
		      $_animate(ObjPath.$obj.xR)] * $zoom]
    set dy [expr [expr $_animate(ObjPath.$obj.yF) - \
		      $_animate(ObjPath.$obj.yR)] * $zoom]

    set pathLine_F ""

    set pathLine_R ""

    foreach point $_animate(ObjPath.$obj.pathF) {
	eval lappend pathLine_F {[list [expr [lindex $point 0]*$zoom] \
				      [expr [lindex $point 1]*$zoom]]}
    }

    foreach point $_animate(ObjPath.$obj.pathR) {
	eval lappend pathLine_R {[list [expr [lindex $point 0]*$zoom] \
				      [expr [lindex $point 1]*$zoom]]}
    }
	
    set result "global _animate
set zoom \[\$PAD getzoom\]
if {\[info exists _animate(import)\]} {
set _animate(ObjPath.\$Pad_ID.pathF) \"\"
set _animate(ObjPath.\$Pad_ID.pathR) \"\"
foreach point \[list $pathLine_F\] {
    eval lappend _animate(ObjPath.\$Pad_ID.pathF) {\[list \[expr \[lindex \$point 0\]/\$zoom\] \[expr \[lindex \$point 1\]/\$zoom\]\]}
}
foreach point \[list $pathLine_R\] {
    eval lappend _animate(ObjPath.\$Pad_ID.pathR) {\[list \[expr \[lindex \$point 0\]/\$zoom\] \[expr \[lindex \$point 1\]/\$zoom\]\]}
}
} else {
set _animate(ObjPath.\$Pad_ID.pathF) {$_animate(ObjPath.$obj.pathF)}
set _animate(ObjPath.\$Pad_ID.pathR) {$_animate(ObjPath.$obj.pathR)}
}
set _animate(ObjPath.\$Pad_ID.dx) $dx
set _animate(ObjPath.\$Pad_ID.dy) $dy
set _animate(ObjPath.\$Pad_ID.xR) $_animate(ObjPath.$obj.xR)
set _animate(ObjPath.\$Pad_ID.yR) $_animate(ObjPath.$obj.yR)
set _animate(ObjPath.\$Pad_ID.sR) $_animate(ObjPath.$obj.sR)
set _animate(ObjPath.\$Pad_ID.xF) $_animate(ObjPath.$obj.xF) 
set _animate(ObjPath.\$Pad_ID.yF) $_animate(ObjPath.$obj.yF)
set _animate(ObjPath.\$Pad_ID.sF) $_animate(ObjPath.$obj.sF)
set _animate(ObjPath.\$Pad_ID.pathLine) \[\$PADLOAD create line $coords -events 0 -place {$place} -tags pathLine -penwidth $penwidth -transparency 0 -arrow last -arrowshape {$arrowshape} -pen $color -lock 1\] 
set _animate(ObjPath.\$Pad_ID.N) $_animate(ObjPath.$obj.N)
set _animate(ObjPath.\$Pad_ID.time) {$_animate(ObjPath.$obj.time)}
"

return $result
}
    
##########################################################################
# proc animMAdd {PAD} 
#
# Add an animate object to a multi group
#
##########################################################################
proc animMAdd {PAD} {
    global _animate
    global _color

    if {![info exists _animate(selectedGroup)]} {
	return
    }

    set ids [pad_sel $PAD]

    foreach id $ids {
	$PAD addtag $_animate(selectedGroup) $id
	pad_hilite $PAD $id $_color(animMulti.entered) .5
    }
}
    
##########################################################################
# proc animMRemove {PAD} 
#
# Remove an animation object from a multi group
#
##########################################################################
proc animMRemove {PAD} {
    global _animate

    if {![info exists _animate(selectedGroup)]} {
	return
    }

    set ids [pad_sel .pad]

    foreach id $ids {
	$PAD deletetag $_animate(selectedGroup) $id
	$PAD ic hilite$id -lock 0
	$PAD delete hilite$id
    }
}


##########################################################################
#
# proc animReset {PAD} 
#
# This procedure returns the selected objects to the beginning or end of
# their animation path, which depends upon the variable ForR.
#
##########################################################################
proc animReset {RorF ForR PAD} {

    global _animate
    
    set ids [pad_sel $PAD]

    set selectids $ids
    if {$ids == ""} {
	set ids $_animate(defaultPlayID)
	foreach id $ids {
	    pad_select $PAD $id 
	}
	set selectids ""
    } 
# Loop thru and see if objects belong to multi-anim-groups
    foreach id $ids {
	set tags [$PAD gettag $id]
	set index [lsearch -regexp $tags "multi_*"]
	
	if {$index != -1} {
	    foreach newID [$PAD find withtag [lindex $tags $index]] {
		lnewappend ids $newID
	    }
	}
    }

    pad_unselect $PAD all
 
    foreach id $ids {
	if {[info exists _animate(ObjPath.$id.pathF)]} { 
	    set x0 $_animate(ObjPath.$id.x$RorF)
	    set y0 $_animate(ObjPath.$id.y$RorF)
	    set xN $_animate(ObjPath.$id.x$ForR)
	    set yN $_animate(ObjPath.$id.y$ForR)
	} else {
	    set x0 [lindex [$PAD ic $id -place] 0]
	    set y0 [lindex [$PAD ic $id -place] 1]
	    set xN [lindex [$PAD ic $id -place] 0]
	    set yN [lindex [$PAD ic $id -place] 1]
	}    
	
	if {$_animate(REL) == "ON"} {
	    $PAD slide $id [expr $xN-$x0] [expr $yN-$y0]
	} else {
	    $PAD ic $id -anchor c -place \
		    [list $xN $yN [lindex [$PAD ic $id -place] 2]]
	}
    }
    
    foreach id $selectids {
	pad_select $PAD $id
    }
}

###########################################################################    
#
# proc animPlay 
#
# Play an animation.
#
############################################################################
proc animPlay {RorF ForR PAD}  {

    global _animate _color

    set ids [pad_sel $PAD]

    # If no objects are selected, then default to
    # the last object to record an animation.
    if { $ids == ""  } {
	if {![info exists _animate(defaultPlayID)]} {
	    set _animate(isLooping) false
	    return
	} else {
	    set ids $_animate(defaultPlayID)
	}
    }  else {
 	set _animate(defaultPlayID) $ids
    }

    # Unselect everything
    pad_unselect $PAD all

    # Loop thru and see if objects belong to multi-anim-groups
    foreach id $ids {
	set tags [$PAD gettag $id]
	set index [lsearch -regexp $tags "multi_*"]

	if {$index != -1} {
	    foreach newID [$PAD find withtag [lindex $tags $index]] {
		lnewappend ids $newID
	    }
	    $PAD ic hilite_sel -lock 0
	    $PAD delete hilite_sel
	}
    }

    #set _animate(defaultPlayID) $ids

    set maxID ""
    set max   0
    set i     0

    foreach id $ids {	
	if {[info exists _animate(ObjPath.$id.pathF)]} {
	    if {$_animate(REL) == "OFF"} {
		$PAD ic $id -place \
		    [list $_animate(ObjPath.$id.x$RorF) \
			 $_animate(ObjPath.$id.y$RorF) \
			 [lindex [$PAD ic $id -place] 2]]
	    }
	    if { $_animate(ObjPath.$id.N) > $max } {
		set max $_animate(ObjPath.$id.N)
		set maxID $id	
	    }
	} else {
	    set ids [lremove $ids $id]
	}
    }

    if {$maxID == ""} {
	if {[winfo exists .animate] && [$PAD getmodifier] != "Run"} {
		animPlayStop $PAD
	}
	
	return
    }

    foreach Point $_animate(ObjPath.$maxID.pathF) {
	    # if {[info exists _animate(whileAnimLoop)]} {
	    # 	    if {$_animate(whileAnimLoop) == "OFF" } {
	    # 		animPlayStop $PAD
	    # 		return
	    # 	    }
	    # 	}

	foreach id $ids {
	    # If i is greater than the size of the path associated
	    # with id, then id will not be moved. 
	    if {[lindex $_animate(ObjPath.$id.path$ForR) $i] != ""} {
		set place [lindex $_animate(ObjPath.$id.path$ForR) $i]    
		$PAD slide $id [lindex $place 0] [lindex $place 1]
	    }
	}
	if { [lindex $_animate(ObjPath.$id.time) $i] != "" && $i != 0} {
	    after [expr [lindex $_animate(ObjPath.$id.time) $i] * abs($_animate(speed))]
	}

	$PAD update
	update
	incr i
    }

    if {$_animate(REL) == "OFF"} {
	foreach id $ids {
	    $PAD ic $id -place \
		[list $_animate(ObjPath.$id.x$ForR) \
		     $_animate(ObjPath.$id.y$ForR) \
		     [lindex [$PAD ic $id -place] 2]]
	}
	$PAD update
    }
    
    # Check if there is a selected multi-group.
    # If so hilite it accordingly.  Also check if we are in 
    # Run mode since we dont want the animation hilited when clicking
    # on it in run mode.  An alternative to this method would be to unset
    # _animate(selectedGroup) when switching to run mode.
    if {[info exists _animate(selectedGroup)] && \
	    [$PAD getmodifier] != "Run" } {
	foreach id [$PAD find withtag $_animate(selectedGroup)] {
	    pad_hilite $PAD $id $_color(animMulti.selected) .8
	    $PAD addtag "hilite_sel" hilite$id 
	    $PAD ic hilite$id -lock 1
	}
    }

    # Check if animation tools toplevel is showing
    if {[winfo exists .animate] && [$PAD getmodifier] != "Run"} {
	if {$_animate(loopSwitch) == "OFF" \
		|| $_animate(isLooping) == "false"} {   
	    animPlayStop $PAD
	}
    }
}


########################################################################
#
# proc animMakeRevPath {Id}
#
# This procedure creates and stores a reverse of _animate(ObjPath.$Id.pathF).
#
########################################################################
proc animMakeRevPath {Id} {

    global _animate

    set _animate(ObjPath.$Id.pathR) ""

    foreach Point $_animate(ObjPath.$Id.pathF) {
	set x [expr -1*[lindex $Point 0]]
	set y [expr -1*[lindex $Point 1]]
	set _animate(ObjPath.$Id.pathR) \
	    [linsert $_animate(ObjPath.$Id.pathR) 0 [list $x $y]]
    }
}

########################################################################
#
# proc animPlayZoom {PAD id} 
#
# Play animation with a recorded zoom.  Takes an id and a direction to
# zoom.  
#   To play forward:
#       animPlayZoom $PAD $id f
#
#   To Play backward:
#       animPlayZoom $PAD $id r
#
########################################################################
proc animPlayZoom {PAD id {dir f}} {
    global _animate

    set i 0
    
    foreach point $_animate(ObjPath.$id.zoom$dir) {
	if {[lindex $_animate(ObjPath.$id.zoom$dir) $i] != ""} {
	    set place [lindex $_animate(ObjPath.$id.zoom$dir) $i]
	    $PAD ic $id -place "$place"	    
	}

	$PAD update
	incr i
    }
}

########################################################################
#
# proc animEventsOff {PAD} 
#
# This procedure turns off the following animation events:
#        Run-Enter
#        Run-Leave
#        
#        Select-Enter
#        Select-Leave
#
########################################################################
proc animEventsOff {PAD} {
    $PAD bind animate <Run-Enter> ""
    $PAD bind animate <Run-Leave> ""
    $PAD bind animate <Run-Button-1> ""
    $PAD bind animate <Run-ButtonRelease-1> ""
    $PAD bind animate <Select-Enter> ""
    $PAD bind animate <Select-Leave> ""
    $PAD bind animate <Select-ButtonPress-1> ""

}






