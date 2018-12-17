# "(c) Copyright 1993-1996 Pad++ Consortium {University of New Mexico (UNM),
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

########################################################################
########################################################################

###             snoop.tcl (c) 1996 by George Hartogensis             ###

########################################################################
########################################################################

#
# Known Bugs:
#  - Items should get updated automatically when selection leaves
#  - Should be able to edit group members with left and right arrows
#  - After item is created, mode ignores lock status
#  - Should have some help
#  - Not all properites are there (pen for lines for instance)
#

#
# proc make.snoopLens {PAD}
#
# Make a property snooper lens and bind it to the movement of the mouse.
#
proc make.snoopLens {PAD} {
    global _prop
    global _snoop
    global _mode
    global _pad

    #
    # Create the snooper lens.
    #
    set _snoop(Font) "Helvetica-1.25"
    set _snoop(Font.2) "Helvetica-10"

    set _snoop(panelId) [$PAD create rectangle -130 -90 130 90 \
			     -fill grey \
			     -transparency 0.5]
    set _snoop(rectId) [$PAD create rectangle -131 -91 131 91 \
			    -pen navy]
    set _snoop(centerSpotId) [$PAD create line 0 0 0 0 \
				  -pen lightgrey \
				  -penwidth 32 \
				  -transparency 0.5]
    set _snoop(line1Id) [$PAD create line -16 0 16 0 \
	                     -pen navy]
    set _snoop(line2Id) [$PAD create line 0 -16 0 16 \
	                     -pen navy]
    set _snoop(idRectId) [$PAD create rectangle -122 66 122 88 \
	                      -fill lightgrey \
			      -pen navy \
			      -transparency 0.0]
    set _snoop(idTextId) [$PAD create text \
                              -font $_snoop(Font) \
			      -text "" \
			      -pen red3 \
			      -place "-122 67 6"]
    set _snoop(typeTextId) [$PAD create text \
				-text "" \
				-font $_snoop(Font) \
				-pen red3 \
				-place "-63 67 6"]
    set _snoop(depthTextId) [$PAD create text \
				-text "" \
				-font $_snoop(Font) \
				-pen red3 \
				-place "58 67 6"]
    set _snoop(xyzRectId) [$PAD create rectangle -126 -86 126 -64 \
			       -fill lightgrey \
			       -pen navy]
    set _snoop(xTextId) [$PAD create text \
			     -text "" \
			     -font $_snoop(Font) \
			     -pen red3 \
			     -place "-120 -83 6"]
    set _snoop(yTextId) [$PAD create text \
			     -text "" \
			     -font $_snoop(Font) \
			     -pen red3 \
			     -place "-30 -83 6"]
    set _snoop(zTextId) [$PAD create text \
			     -text "" \
			     -font $_snoop(Font) \
			     -pen red3 \
			     -place "60 -83 6"]
    set _snoop(infoRectId1) [$PAD create rectangle -126 -61 -29 61 \
				 -fill lightgrey \
				 -pen navy \
				 -transparency 0.0]
    set _snoop(infoRectId2) [$PAD create rectangle 29 -61 126 61 \
				 -fill lightgrey \
				 -pen navy \
				 -transparency 0.0]
    set _snoop(padSnooperTextId) [$PAD create text \
				      -text "Pad++ Properties Viewer" \
				      -font $_snoop(Font) \
				      -pen navy \
				      -place "-125 55 8"]
    set _snoop(padSnooperTextBgId) [$PAD create text \
					-text "Pad++ Properties Viewer" \
					-font $_snoop(Font) \
					-pen white \
					-place "-126 56 8"]
    set _snoop(item1Id) [$PAD create text \
			     -pen red3 \
			     -font $_snoop(Font) \
			     -anchor nw \
			     -place "-123 57 6"]
    set _snoop(item2Id) [$PAD create text \
			     -pen red3 \
			     -font $_snoop(Font) \
			     -anchor nw \
			     -place "32 57 6"]
    set lensMembers "$_snoop(panelId) $_snoop(rectId) $_snoop(centerSpotId) \
                     $_snoop(line1Id) $_snoop(line2Id) $_snoop(idRectId) \
                     $_snoop(idTextId) $_snoop(typeTextId)  \
		     $_snoop(depthTextId) \
                     $_snoop(xyzRectId) $_snoop(xTextId) $_snoop(yTextId) \
                     $_snoop(zTextId) $_snoop(infoRectId1) \
                     $_snoop(infoRectId2) $_snoop(padSnooperTextBgId) \
                     $_snoop(padSnooperTextId) $_snoop(item1Id) \
                     $_snoop(item2Id)"
    set _snoop(lensId) [$PAD create group \
			    -members "$lensMembers" \
			    -transparency 0 \
			    -layer status \
			    -sticky 1 \
			    -tags snoop \
			    -z [expr 1.0/[lindex [$PAD getview] 2]]]
    #
    # Set the justStarted flag, set the entered flag and init oldId
    # and which.
    #
    set _snoop(justStarted) True
    set _snoop(entered) False
    set _snoop(oldId) ""
    set _snoop(which) 2
    set _snoop(groupWhich) 2
    #
    # Get rid of the cursor
    #
    pad_set_cursor $PAD "@$_pad(PadBitmaps)/none.xbm black" "none"
    #
    # Set the modifier to Snoop and save the old mode.
    #
    $PAD modifier create Snoop
    
    set _snoop(oldState) $_mode
    
    $PAD modifier set Snoop

    snoopBindings $PAD
    #
    # Calculate the offsets from the center (because of shadow):
    #
    set _snoop(offsetX) 3
    set _snoop(offsetY) -3
} 

#
# proc snoopBindings {PAD}
#
# This makes a snooper for the currently selected items
#
proc snoopSelected {PAD} {
    set items [pad_sel $PAD]
    set x 0
    set y 0
    foreach item $items {
	set ctr [bbcenter [$PAD bbox $item]]
	make.snoopLens $PAD
	eval snoopDragREnter $PAD $ctr $item
	set snoop [eval snoopButtonPress $PAD $ctr $item]
	$PAD layout position $x $y -ref 1 0 0 $snoop
	set x [expr $x + 0.05]
	set y [expr $y - 0.05]
	$PAD update
    }
}

#
# proc snoopBindings {PAD}
#
# These are the bindings for Snoop mode
#
proc snoopBindings {PAD} {
    global _pad
    global _snoop tcl_platform

    $PAD bind all <Snoop-Motion> "
	snoopDragREnter $PAD %i %j 
	if { {$tcl_platform(platform)} == {unix} } {
          $PAD config -cursor {@$_pad(PadBitmaps)/none.xbm black}
        } else {
	  $PAD config -cursor {none}
        }
        set _snoop(which) 2
       	break
    "
    $PAD bind all <Snoop-ButtonPress-1> "
        snoopButtonPress $PAD %i %j
        break 
    "
    $PAD bind all <Snoop-Enter> "
        snoopDragREnter $PAD %i %j
        break
    " 
    $PAD bind all <Snoop-Leave> "
        set _snoop(justStarted) True
        $PAD ic $_snoop(lensId) -transparency 0
        break 
    "

    $PAD bind all <Snoop-Down> "
        incr _snoop(which)
        snoopDragREnter $PAD %i %j
        break
    "
    $PAD bind all <Snoop-Up> "
        incr _snoop(which) -1 
        snoopDragREnter $PAD %i %j
        break
     "
    $PAD bind all <Snoop-Right> "
        incr _snoop(groupWhich) -1 
        snoopDragREnter $PAD %i %j
        break
     "
    $PAD bind all <Snoop-Left> "
        incr _snoop(groupWhich) 1 
        snoopDragREnter $PAD %i %j
        break
     "
}

#
# proc snoopDragREnter {PAD x y {foundIdList ""}}
#
# This is the workhorse.  It continuously updates the snooper lens with
# information about the objects that the snooper lens passes over.
# If foundIdList is specified, then that item is used, else, the ones at
# (x, y) are used.
# 
proc snoopDragREnter {PAD x y {foundIdList ""}} {
    global _snoop

    set z [lindex [$PAD getview] 2]
    
    $PAD ic $_snoop(lensId) -x [expr $x + $_snoop(offsetX)/$z] \
	                    -y [expr $y + $_snoop(offsetY)/$z]
    if {$_snoop(justStarted) == "True"} {
	$PAD ic $_snoop(lensId) -transparency 1
	set _snoop(justStarted) False
    }
    if {$foundIdList == ""} {
	set foundIdList [$PAD find closest $x $y]
    }
    if {$_snoop(which) > [llength $foundIdList]} {
	set _snoop(which) [llength $foundIdList]
    }
    set foundId [lindex $foundIdList \
		     [expr [llength $foundIdList]-$_snoop(which)]]
    if {$_snoop(which) < 1} {
	set _snoop(which) 1 
    }
    $PAD ic $_snoop(depthTextId) \
	    -text \
	    "Depth [expr $_snoop(which)-1]/[expr [llength $foundIdList]-1]"

    if {$foundId != "" && $foundId != $_snoop(lensId)} {
	if {$_snoop(oldId) != $foundId || $_snoop(entered) == "False"} {
	    $PAD ic $_snoop(centerSpotId) -pen gold
	    $PAD ic $_snoop(idRectId) -transparency 1.0
	    $PAD ic $_snoop(idTextId) -text "Id $foundId"
	    $PAD ic $_snoop(typeTextId) -text "Type [$PAD type $foundId]"

	    $PAD ic $_snoop(depthTextId) -transparency 1.0

	    $PAD ic $_snoop(infoRectId1) -transparency 1.0
	    $PAD ic $_snoop(infoRectId2) -transparency 1.0

	    $PAD ic $_snoop(padSnooperTextBgId) -transparency 0.0
	    $PAD ic $_snoop(padSnooperTextId) -transparency 0.0

	    $PAD ic $_snoop(item1Id) -text ""
	    $PAD ic $_snoop(item2Id) -text ""

	    catch {snoop_[$PAD type $foundId]_Display $PAD $foundId}
	    snoopDisplay $PAD $foundId
	    
	    set _snoop(entered) True
	    set _snoop(oldId) $foundId
	}
    } elseif {$foundId == "" || $foundId == $_snoop(lensId)} {
	if {$_snoop(oldId) != $foundId} {
	    $PAD ic $_snoop(centerSpotId) -pen lightgrey
	    $PAD ic $_snoop(idRectId) -transparency 0.0
	    $PAD ic $_snoop(idTextId) -text ""
	    $PAD ic $_snoop(typeTextId) -text ""

	    $PAD ic $_snoop(depthTextId) -transparency 0.0
	    
	    $PAD ic $_snoop(infoRectId1) -transparency 0.0
	    $PAD ic $_snoop(infoRectId2) -transparency 0.0
	    $PAD ic $_snoop(padSnooperTextBgId) -transparency 1.0
	    $PAD ic $_snoop(padSnooperTextId) -transparency 1.0

	    $PAD ic $_snoop(item1Id) -text ""
	    $PAD ic $_snoop(item2Id) -text ""
	    
	    set _snoop(entered) False
	}
    }
    set z [lindex [$PAD getview] 2]
    if {$z > 0.1} {
	$PAD ic $_snoop(xTextId) -text "X [format %.2f $x]"
	$PAD ic $_snoop(yTextId) -text "Y [format %.2f $y]"
	$PAD ic $_snoop(zTextId) -text "Z [format %.2f $z]"
    } elseif {$z > 0.01} {
	$PAD ic $_snoop(xTextId) -text "X [format %.1f $x]"
	$PAD ic $_snoop(yTextId) -text "Y [format %.1f $y]"
	$PAD ic $_snoop(zTextId) -text "Z [format %.3f $z]"
    } else {
	$PAD ic $_snoop(xTextId) -text "X [format %.0f $x]"
	$PAD ic $_snoop(yTextId) -text "Y [format %.0f $y]"
	$PAD ic $_snoop(zTextId) -text "Z [format %.4f $z]"
    }
}


#
# proc snoopButtonPress {PAD x y foundId}
# If foundId is specified, then make a snoop for that item,
# else make one for the item at (x, y).
#
proc snoopButtonPress {PAD x y {foundId ""}} {
    global _snoop _pad

    if {$foundId == ""} {
	set foundIdList [$PAD find closest $x $y]
	set foundId [lindex $foundIdList \
			 [expr [llength $foundIdList]-$_snoop(which)]]
    }
    if {$foundId != ""} {
        $PAD deletetag snoop $_snoop(lensId)
	$PAD removeg $_snoop(centerSpotId) $_snoop(lensId)
	$PAD delete $_snoop(centerSpotId)
	$PAD removeg $_snoop(line1Id) $_snoop(lensId)
	$PAD delete $_snoop(line1Id)
	$PAD removeg $_snoop(line2Id) $_snoop(lensId)
	$PAD delete $_snoop(line2Id)
	$PAD delete $_snoop(padSnooperTextId)
	$PAD delete $_snoop(padSnooperTextBgId)
        $PAD removeg $_snoop(item1Id)
        $PAD delete $_snoop(item1Id)
	$PAD removeg $_snoop(item2Id)
        $PAD delete $_snoop(item2Id)
	$PAD removeg $_snoop(infoRectId1)
        $PAD delete $_snoop(infoRectId1)
	$PAD removeg $_snoop(infoRectId2)
        $PAD delete $_snoop(infoRectId2)
	
	$PAD ic $_snoop(idRectId) -tags "raise drag"
	$PAD ic $_snoop(idTextId) -tags "raise drag"
	$PAD ic $_snoop(typeTextId) -tags "raise drag"
	$PAD ic $_snoop(depthTextId) -tags "raise drag"
					# Set variable so when dragging element, entire snooper gets dragged
	set _pad($_snoop(idRectId).upselect) -1
	set _pad($_snoop(idTextId).upselect) -1
	set _pad($_snoop(typeTextId).upselect) -1
	set _pad($_snoop(depthTextId).upselect) -1

	$PAD bind raise <Run-ButtonPress> {%P raise [pad_get_group %P %O]}

	set _snoop(idRectId.$foundId) $_snoop(idRectId)
	set _snoop(idTextId.$foundId) $_snoop(idTextId)
	set _snoop(typeTextId.$foundId) $_snoop(typeTextId)
	set _snoop(depthTextId.$foundId) $_snoop(depthTextId)
	set _snoop(xTextId.$foundId) $_snoop(xTextId)
	set _snoop(yTextId.$foundId) $_snoop(yTextId)
	set _snoop(zTextId.$foundId) $_snoop(zTextId)

	set z [lindex [$PAD getview] 2]
        if {[$PAD ic $foundId -height] > [$PAD ic $foundId -width]} {
	    set aliasz [expr $z * [$PAD ic $foundId -height]/70.0]
	} else {
	    set aliasz [expr $z * [$PAD ic $foundId -width]/70.0]
	}

	set place [$PAD ic $_snoop(lensId) -place]

	set _snoop(frameId.$foundId) [$PAD create rectangle 0 73 80 175 \
		-fill lightgrey \
		-pen navy \
                -x [expr $x - 85/$z] \
		-y [expr $y + 10/$z] \
		-z [expr 1.0/$z]]
        if {[$PAD type $foundId] != "alias"} {
	    set _snoop(aliasId.$foundId) \
		[$PAD create alias -reference $foundId \
		    -x [expr $x - 85/$z] \
		    -y [expr $y + 13/$z] \
		    -z [expr 1.0/$aliasz]]
	} else {
	    set _snoop(aliasId.$foundId) [$PAD create alias \
		    -reference  [$PAD ic $foundId -reference] \
		    -x [expr $x - 85/$z] \
		    -y [expr $y + 13/$z] \
		    -z [expr 1.0/$aliasz]]
	}	    
	set _snoop(pickUpButton.$foundId) [$PAD create button \
		-text "Grab " \
		-font $_snoop(Font.2) \
		-pen navy \
		-command "snoopPickUpCommand $PAD $foundId $_snoop(lensId)"  \
	        -x [expr $x - 125/$z] \
		-y [expr $y - 63/$z] \
		-z [expr 1.0/(1.3*$z)]]
	set _snoop(newButton.$foundId) [$PAD create button \
		-text "New" \
		-pen navy \
	        -font $_snoop(Font.2) \
		-command "make.snoopLens $PAD"  \
	        -x [expr $x - 75/$z] \
		-y [expr $y - 63/$z] \
		-z [expr 1.0/(1.3*$z)]]


	snoopAllField $PAD [$PAD type $foundId] $foundId 
        make.snoop_fields $PAD $foundId $x $y $z



	set _snoop(helpButton.$foundId) [$PAD create button \
		-text "?" \
		-font $_snoop(Font.2) \
		-pen navy \
		-x [expr $x + 72/$z] \
		-y [expr $y - 63/$z] \
		-z [expr 1.0/(1.3*$z)]]
	$PAD ic $_snoop(helpButton.$foundId) -command "snoopHelp $PAD $_snoop(helpButton.$foundId)"
	set _snoop(quitButton.$foundId) [$PAD create button \
		-text "Close " \
		-font $_snoop(Font.2) \
		-pen navy \
		-command "snoopQuit $PAD $_snoop(lensId)" \
		-x [expr $x + 88/$z] \
		-y [expr $y - 63/$z] \
		-z [expr 1.0/(1.3*$z)]]
	$PAD addg $_snoop(frameId.$foundId) $_snoop(lensId)
	$PAD addg $_snoop(aliasId.$foundId) $_snoop(lensId)
	$PAD addg $_snoop(textRowId.$foundId) $_snoop(lensId)
	$PAD addg $_snoop(textRowSBId.$foundId) $_snoop(lensId)
	$PAD addg $_snoop(newButton.$foundId) $_snoop(lensId)
	$PAD addg $_snoop(pickUpButton.$foundId) $_snoop(lensId)
	$PAD addg $_snoop(helpButton.$foundId) $_snoop(lensId)
	$PAD addg $_snoop(quitButton.$foundId) $_snoop(lensId)

	$PAD ic $_snoop(panelId) -fill grey2
	$PAD ic $_snoop(lensId) -z [expr 1.1*[$PAD ic $_snoop(lensId) -z]] 
	$PAD ic $_snoop(item1Id) -z 8
	$PAD ic $_snoop(item2Id) -z 8

	#snoopDroppedBindings $PAD $foundId

    } else {
	$PAD delete $_snoop(lensId)
    }
    selectMode $PAD $_snoop(oldState)

    return $_snoop(lensId)
}


#
# This gets called whenever the propsfield scrollbar gets modified
#
proc textRow_ScrollCallback {PAD sb textrow value} {
     set i [expr floor( ($value-160)/190.0+0.5)]
     set f [expr ($value-160)/190.0 - $i ]
     if { [expr abs($f)] < 0.4 } { set f 0.0 }
     set g [expr ($f*$f*$f)*4]
     set j [expr ($g+$i)*190+160]
 
     $PAD ic $textrow -x [expr 160 - $j]
}


#
# snoopHelp
#
#   Create some simple help for the snooper
#
proc snoopHelp {PAD buttonId} {
    global _snoop _pad
					
					# First, create help
    set bgnd [$PAD create rectangle 0 0 250 300 \
		  -fill grey2 -pen none -transparency 0.5]
    set bgndoutline [$PAD create rectangle 0 0 250 300 \
		  -pen navy]
    set fgnd [$PAD create rectangle 5 35 245 295 \
		  -fill lightgray -pen navy]
    set text [$PAD create text \
		  -font $_snoop(Font.2) \
		  -pen navy \
		  -anchor nw -place "10 290 1" \
		  -text \
"Properties Help

Use slider to find object property.

Enter new value in field and then
    press return to change value.

Bring up Properties Viewer for an
    object by selecting it and then
    selecting the Object/Properties
    menu (Control-P).

Or, bring up the Properties lens from
    the Tools/Properties menu."
	     ]
    set close [$PAD create button \
		   -text "Close " \
		   -font $_snoop(Font.2) \
		   -pen navy \
		   -anchor s -place "125 5 1" \
		   -command "pad_fade_delete $PAD snoophelp"]
    set grp [$PAD create group -members "$bgnd $bgndoutline $fgnd $text $close" \
		-tags "snoophelp" \
		-anchor sw \
		-layer status]

					# Now, animate it to starting point
    set zoom [$PAD getzoom]
    set ctr [bbcenter [$PAD bbox $buttonId]]
    set x [lindex $ctr 0]
    set y [lindex $ctr 1]
    $PAD ic $grp -place "$x $y [expr 0.01 / $zoom]"
    $PAD scale $grp 100 $x $y $_pad(AnimationSpeed)
}

proc snoopQuit {PAD lensId} {
    pad_fade_delete $PAD $lensId
}

    
#
# proc snoopPickUpCommand {PAD Id}
#
# This procedure deletes the dropped lens and makes a new floating lens. 
# It also clears the <Modify> bindings.
#
proc snoopPickUpCommand {PAD foundId lensId} {
    global _snoop

    $PAD delete $lensId
    $PAD bind $foundId <Modify> "break"
    make.snoopLens $PAD
}


proc snoopField {PAD foundId Type Label j i} {
    global _snoop
    set Id $j.$i.Id

    set _snoop(inputText$j.$i.Id.$foundId) [$PAD create text \
	-pen red3 \
	-font $_snoop(Font) \
	-anchor w \
	-place "[expr  - 35 + $j*190] [expr  + (67 - $i*19) ] [expr 6]"]

    set _snoop(inputField$j.$i.Id.$foundId) [$PAD create textfield \
	-fill grey \
	-pen navy \
	-font $_snoop(Font.2) \
	-width 120 \
	-height 25 \
	-anchor w \
	-x [expr (20 + $j*190)] \
	-y [expr (67 - $i*19)] \
	-z [expr 1.0/(1.25)]]

    $PAD ic $_snoop(inputText$Id.$foundId) -text $Label
    $PAD ic $_snoop(inputField$Id.$foundId) -text [eval "$PAD ic $foundId -$Type"]
    $PAD bind $_snoop(inputField$Id.$foundId) <Key-Return> "\[
         snoopTextUpdates $PAD $foundId $Type $Id
         break\]"

    $PAD addg $_snoop(inputText$j.$i.Id.$foundId)   $_snoop(textRowGrpId.$foundId)
    $PAD addg $_snoop(inputField$j.$i.Id.$foundId)  $_snoop(textRowGrpId.$foundId)
}

proc snoopAllField {P type id} {
    global _snoop

    set _snoop(textRowGrpId.$id) [$P create group]

    snoopField $P $id  anchor        "Anchor"    1 1
    snoopField $P $id  sticky        "Sticky"    1 2
    snoopField $P $id  events        "Events"    1 3
    snoopField $P $id  lock          "Lock"      1 4
    snoopField $P $id  tags          "Tags"      1 5
       labelpage $P $id 1

    snoopField $P $id  minsize       "MinSize"   2 1
    snoopField $P $id  maxsize       "MaxSize"   2 2
    snoopField $P $id  transparency  "Trans."    2 3
    snoopField $P $id  faderange     "FdRange"   2 4
    snoopField $P $id  alwaysrender  "AlwyRend"  2 5
       labelpage $P $id 2

    snoopField $P $id  clipping      "Clipping"  3 1
    snoopField $P $id  layer         "Layer"     3 2
    snoopField $P $id  aliases       "Aliases"   3 3
    snoopField $P $id  info          "Info"      3 4
    snoopField $P $id  timerrate     "TmrRate"   3 5
       labelpage $P $id 3

       labelpage $P $id 4
       labelpage $P $id 5
    
    if {$type == "text"} {
        snoopField $P $id  pen           "Pen"       4 1
        snoopField $P $id  text          "Text"      4 2
        snoopField $P $id  font          "Font"      4 3

    } elseif {$type == "textfield"} {
        snoopField $P $id  pen           "Pen"       4 1
        snoopField $P $id  text          "Text"      4 2
        snoopField $P $id  font          "Font"      4 3
	snoopField $P $id  fill          "Fill"      4 4

    } elseif {$type == "textfile"} {
        snoopField $P $id  pen           "Pen"       4 1
        snoopField $P $id  file          "File"      4 2
        snoopField $P $id  font          "Font"      4 3

    } elseif {$type == "alias"} {
        snoopField $P $id  reference     "Ref."      4 1

    } elseif {($type == "rectangle") || ($type == "polygon") || ($type == "oval")} {
        snoopField $P $id  pen           "Pen"       4 1
	snoopField $P $id  fill          "Fill"      4 2
	snoopField $P $id  joinstyle     "JnStyle"   4 3
	snoopField $P $id  penwidth      "PenWidth"  4 4

    } elseif {($type == "line")} {
        snoopField $P $id  pen           "Pen"       4 1
	snoopField $P $id  penwidth      "PenWidth"  4 2

    } elseif {$type == "portal"} {
        snoopField $P $id  pen           "Pen"       4 1
	snoopField $P $id  fill          "Fill"      4 2
        snoopField $P $id  font          "Font"      4 3
	snoopField $P $id  lookon        "Lookon"    4 4
	snoopField $P $id  view          "View"      4 5

        snoopField $P $id  relief        "Relief"    5 1
	snoopField $P $id  border        "Border"    5 2
        snoopField $P $id  borderwidth   "BdrWidth"  5 3
	snoopField $P $id  visiblelayers "VisLyrs"   5 4
	snoopField $P $id  title         "Title"     5 5

    } elseif {$type == "group"} {
        snoopField $P $id  members       "Members"   4 1
	snoopField $P $id  divisible     "Div."      4 2

    } elseif {$type == "html"} {
        snoopField $P $id  htmlanchors   "Anchors"   4 1
	snoopField $P $id  url           "URL"       4 2
        snoopField $P $id  width         "Width"     4 3
	snoopField $P $id  border        "Border"    4 4
	snoopField $P $id  borderwidth   "BdrWidth"  4 5

        snoopField $P $id  divisible     "Div."      5 1
	snoopField $P $id  fill          "Fill"      5 2

    } elseif {$type == "htmlanchor"} {
        snoopField $P $id  html          "HTML"      4 1
	snoopField $P $id  url           "URL"       4 2
        snoopField $P $id  state         "State"     4 3
	snoopField $P $id  ismap         "Map"       4 4

    } elseif {$type == "graph"} {
        snoopField $P $id  pen           "Pen"       4 1
	snoopField $P $id  fill          "Fill"      4 2
        snoopField $P $id  lpen          "Lpen"      4 3
	snoopField $P $id  gpen          "Gpen"      4 4
	snoopField $P $id  font          "Font"      4 5

    }


}

proc make.snoop_fields { PAD foundId x y z} {
    global _snoop 

    set group  $_snoop(textRowGrpId.$foundId)
    set gbbox [$PAD bbox $group]
    $PAD removeg -notransform $group
    $PAD ic $group -x 160
    $PAD ic $group -y 0
    $PAD ic $group -anchor sw
    set bbox [$PAD bbox $group]
    set width [bbwidth $bbox]
    set height [bbheight $bbox]

    set sb [$PAD create scrollbar -orientation horizontal  \
		-height [expr 140/$z] \
		    -width [expr 850/$z] \
		    -from  160 \
		    -to $width \
		    -value 160  \
		    -anchor sw \
		    -linesize 25 \
		    -pagesize [expr ($width - 160) * 160 / $width] \
		    -x [expr $x - 39/$z] \
		    -y [expr $y - 63/$z] \
		    -z 0.125
	       ]

    set _snoop(textRowId.$foundId) [$PAD create panel 0 83 166 185 \
	    -fill lightgrey \
	    -anchor w \
	    -x [expr $x - 40/$z] \
	    -y [expr $y + 10/$z] \
 	    -z [expr 1.0/$z]]

    set panel $_snoop(textRowId.$foundId)

    $PAD ic $sb -command "textRow_ScrollCallback $PAD $sb $group"
    #$PAD layout position 0 0 -ref $panel 0 1 $sb
    $PAD addg $group $panel
    #$PAD addg -notransform $panel $group
    #$PAD addg -notransform $sb $group

    $PAD layout position 0 0 -ref $panel 0 0 $group
    set _snoop(textRowSBId.$foundId) $sb
}

proc labelpage { PAD foundId j } {
    global _snoop 

    set _snoop(page$j.Id.$foundId) [$PAD create text \
	-pen black \
	-font $_snoop(Font) \
	-anchor w \
	-text $j \
	-place "[expr  122 + $j*190] [expr  48 ] [expr 10]"]

    $PAD addg $_snoop(page$j.Id.$foundId) $_snoop(textRowGrpId.$foundId)
}


    
    
proc snoopDroppedBindings {PAD foundId} {
    global _snoop

    $PAD bind $foundId <Modify> "
       snoopUpdateXYZ $PAD $foundId
       break"      
    $PAD bind $_snoop(inputField1.1.Id.$foundId) <Key-Return> "
       snoopTextUpdates $PAD $foundId
       break"
    $PAD bind $_snoop(lensId) <Enter> "puts HI break"
}

proc snoopTextUpdates {PAD foundId what Id} {
    global _snoop

    $PAD ic $foundId -$what [$PAD ic $_snoop(inputField$Id.$foundId) -text] 
}
proc snoopUpdateXYZ {PAD foundId} {
    global _snoop

    set x [$PAD ic $foundId -x]
    set y [$PAD ic $foundId -y]
    set z [$PAD ic $foundId -z]

    if {$z > 0.1} {
 	$PAD ic $_snoop(xTextId.$foundId) -text "X [format %.2f $x]"
	$PAD ic $_snoop(yTextId.$foundId) -text "Y [format %.2f $y]"
	$PAD ic $_snoop(zTextId.$foundId) -text "Z [format %.2f $z]"
      } elseif {$z > 0.01} {
	$PAD ic $_snoop(xTextId.$foundId) -text "X [format %.1f $x]"
	$PAD ic $_snoop(yTextId.$foundId) -text "Y [format %.1f $y]"
	$PAD ic $_snoop(zTextId.$foundId) -text "Z [format %.3f $z]"
      } else {
	$PAD ic $_snoop(xTextId.$foundId) -text "X [format %.0f $x]"
	$PAD ic $_snoop(yTextId.$foundId) -text "Y [format %.0f $y]"
	$PAD ic $_snoop(zTextId.$foundId) -text "Z [format %.4f $z]"
      }

    if {[$PAD ic $foundId -height] > [$PAD ic $foundId -width]} {
	set aliasz [expr [$PAD ic $foundId -height]/70.0]
    } else {
	set aliasz [expr [$PAD ic $foundId -width]/70.0]
    }
    $PAD ic $_snoop(aliasId.$foundId) -z [expr 1.0/$aliasz]
}
#
# The display procedures:
#
# These procedure display properties that are unique to $Type.  They are
# of the form proc snoop_$Type_Display {PAD}.
#
proc snoop_rectangle_Display {PAD foundId} {
    global _snoop

    $PAD ic $_snoop(item1Id) -text "Pen:\
                                    \n [$PAD ic $foundId -pen]\
                                    \n\nFill:\
                                    \n [$PAD ic $foundId -fill]\
                                    \n\nPen Width:\
                                    \n [$PAD ic $foundId -penwidth]"
}

proc snoop_oval_Display {PAD foundId} {
    global _snoop

    $PAD ic $_snoop(item1Id) -text "Pen:\
                                    \n [$PAD ic $foundId -pen]\
                                    \n\nFill:\
                                    \n [$PAD ic $foundId -fill]\
                                    \n\nPen Width:\
                                    \n [$PAD ic $foundId -penwidth]"
}

proc snoop_text_Display {PAD foundId} {
    global _snoop

    set font " [$PAD ic $foundId -font]"
    if {[string length $font] > 10} {
	set font " ..[string range $font [expr [string length $font]-9] end]"
    }
	
    $PAD ic $_snoop(item1Id) -text "Pen:\
                                    \n [$PAD ic $foundId -pen]\
                                    \n\nFont:\
                                    \n$font\
                                    \n\nZ:[$PAD ic $foundId -z]"
}

proc snoop_group_Display {PAD foundId} {
    global _snoop

    set pair 0
    foreach i [$PAD ic $foundId -members] {
	if {$pair == 13} {
	    append memberList " ..."
	    break
	}
	if {[expr $pair%2]== 0} {
	    append memberList "\n"
	}
	if {$i < 100} {
	    append memberList " $i  "
	} elseif {$i < 1000} {
	    append memberList " $i "
	} else {
	    append memberList " $i"
	}
	incr pair
    }
    $PAD ic $_snoop(item1Id) -text "Divisible: [$PAD ic $foundId -divisible]\
                                    \n\nMembers:$memberList"
}

proc snoop_scale_Display {PAD foundId} {
    global _snoop

}

proc snoop_portal_Display {PAD foundId} {
    global _snoop

}

proc snoop_checkbutton_Display {PAD foundId} {
    global _snoop

}

proc snoop_html_Display {PAD foundId} {
    global _snoop

}

proc snoop_polygon_Display {PAD foundId} {
    global _snoop

    $PAD ic $_snoop(item1Id) -text "Pen:\
                                    \n [$PAD ic $foundId -pen]\
                                    \n\nFill:\
                                    \n [$PAD ic $foundId -fill]\
                                    \n\nPen Width:\
                                    \n [$PAD ic $foundId -penwidth]"
}

proc snoop_button_Display {PAD foundId} {
    global _snoop

}

proc snoop_canvas_Display {PAD foundId} {
    global _snoop

}

proc snoop_panel_Display {PAD foundId} {
    global _snoop

}

proc snoop_spline_Display {PAD foundId} {
    global _snoop

}

proc snoop_frame_Display {PAD foundId} {
    global _snoop

}

proc snoop_grid_Display {PAD foundId} {
    global _snoop

}

proc snoop_tcl_Display {PAD foundId} {
    global _snoop

}

proc snoop_scrollbar_Display {PAD foundId} {
    global _snoop

}

proc snoop_textfield_Display {PAD foundId} {
    global _snoop

}

proc snoop_line_Display {PAD foundId} {
    global _snoop

    $PAD ic $_snoop(item1Id) -text "Pen:\
                                    \n [$PAD ic $foundId -pen]\
                                    \n\nPen Width:\
                                    \n [$PAD ic $foundId -penwidth]\
                                    \n\nCapstyle:\
                                    \n [$PAD ic $foundId -capstyle]"
}

proc snoop_graph_Display {PAD foundId} {
    global _snoop

}

proc snoop_htmlanchor_Display {PAD foundId} {
    global _snoop

}

proc snoop_label_Display {PAD foundId} {
    global _snoop

}

proc snoop_textarea_Display {PAD foundId} {
    global _snoop

}

proc snoop_alias_Display {PAD foundId} {
    global _snoop

}

proc snoop_kpl_Display {PAD foundId} {
    global _snoop

}


proc snoop_window_Display {PAD foundId} {
    global _snoop

}

proc snoop_textfile_Display {PAD foundId} {
    global _snoop

}

proc snoop_image_Display {PAD foundId} {
    global _snoop

}

#
# proc snoopDisplay {PAD foundId}
#
# This procedure displays properties that are posessed by all $Types.
#
proc snoopDisplay {PAD foundId} {
    global _snoop

    set counter 0
    set tagList ""

    foreach i [$PAD ic $foundId -tags] {
	if {$counter == 8} {
	    append tagList "\n ..." 
	    break
	}
	if {[string length $i] > 10} {
	    set i [string range $i 0 8]
	    append i ".."
	}
	append tagList "\n"
	append tagList " $i"
	incr counter
    }
    $PAD ic $_snoop(item2Id) -text "Tags:$tagList"
}











































