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

proc startPadDraw {PAD} {
    global env _pad _mode
    global tcl_platform

    set _mode "Run"
    processArgs
    makePad $PAD
    initializeLayers $PAD
    $PAD bindtags $PAD specific ;# Force all new objects to get specific events first
    makeTop $PAD
    makeMenuBar $PAD
    update			;# Get some feedback quickly
    

    bindPadDraw $PAD            ;# Set up event bindings

    setDrawingWidth $PAD $_pad(PenWidth)
    update
    #
    # Initialize option extensions
    #
    pad_InitOptions $PAD

    #
    # Check if running in 8 bit mode
    #
    pad_Check8Bits $PAD

    #
    # Offer startup tips
    #
    pad_StartupTips $PAD

    #
    # Load in startup screen
    # 
    if {$_pad(SplashScreen) && 
        ($tcl_platform(platform) == "unix")} {
	startupScreen $PAD
    }
    updateStatusText $PAD "" 0

    #
    # Open tools windows
    #
    controlToolWindows $PAD
    if {$tcl_platform(platform) == "unix"} {
	bindIconification $PAD
    }

    selectMode $PAD "Pan"
    #
    # Initialize widget bindings
    #
    widget_init $PAD
    #
    # Activate auto-focus
    # (Only for unix.  On windows, this code
    #  has the effect of auto-raising the window.)
    #
    if {$tcl_platform(platform) == "unix"} {
      bind Entry <Enter> "+focus %W"
      bind Scale <Enter> "+focus %W"
      bind Button <Enter> "+focus %W"
      bind Checkbutton <Enter> "+focus %W"
      bind Radiobutton <Enter> "+focus %W"
      bind Listbox <Enter> "+focus %W"
      bind Scrollbar <Enter> "+focus %W"
   }
}

#
# Process command line args.  Current supported args are:
#   -nosplash     Run without startup "splash" screen
#
proc processArgs {} {
    global argc argv _pad

    foreach arg $argv {
	if {$arg == "-nosplash"} {
	    set _pad(SplashScreen) 0
	}
    }
}

#
# If warning hasn't been turned off, and we are
# running with anything other than a 8 bit display,
# then make a warning for the user.
#
proc pad_Check8Bits {PAD} {
    global _pad _font _color

    if {!$_pad(8bitWarning) || ([winfo depth .] == 8)} {
	return
    }

    toplevel .message
    wm title .message "Pad++ Warning"
    frame .message.f -bg $_color(toolbg)
    checkbutton .message.f.button -variable _pad(8bitWarning) \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	    -highlightbackground $_color(toolbg)
    label .message.f.label -text "Show this warning at startup" \
	    -font $_font(tools) -bg $_color(toolbg)
    pack .message.f.button -side left
    pack .message.f.label -side left

    set button [pad_frame_message .message .message.f \
"Your display is set for [winfo depth .] bits. \
While Pad++ will work, it will be much faster \
if you set your display to 8 bits, and re-run Pad++." \
Continue Quit]

    if {$button == "Quit" } {
	exit
    }
}

#
# If startup tips are requested, then
# give the next one.
#
proc pad_StartupTips {PAD} {
    global _pad _font _color

    if {$_pad(StartupTips) == 0} {
	return
    }

    toplevel .message
    wm title .message "Pad++ Tip"
    frame .message.f -bg $_color(toolbg)
    checkbutton .message.f.button -variable _pad(WantTips) \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	    -highlightbackground $_color(toolbg)
    .message.f.button select
    label .message.f.label -text "Show tips at startup" \
	    -font $_font(tools) -bg $_color(toolbg)
    pack .message.f.button -side left
    pack .message.f.label -side left

    set tip [pad_GetNextTip]
    pad_frame_message .message .message.f $tip OK

    if {$_pad(WantTips) == 0} {
	set _pad(StartupTips) 0
    }
}

#
# Return the next tip from the tips.txt file,
# update the _pad(StartupTips) variable.
#
proc pad_GetNextTip {} {
    global env _pad

    set tipnum 0
    set tip ""
    set file [open $env(PADHOME)/draw/tips.txt "r"]
    while {![eof $file]} {
	set line [gets $file]
	if {[string trim $line " "] == ""} {
	    incr tipnum
	    if {$tipnum == $_pad(StartupTips)} {
		break
	    }
	    set tip ""
	}
	append tip $line
    }
    
    incr _pad(StartupTips)
    if {[eof $file]} {
	set _pad(StartupTips) 1
    }

    return $tip
}

#
# Create "main" and "status" layers, and order them properly
#
proc initializeLayers {PAD} {
    $PAD layer create "main"
    $PAD layer create "status"
    $PAD raise -layer status
}

proc startupScreen {PAD} {
    global env 
    global portalid

    $PAD delete all
    updateLogoPosition $PAD
    update
    $PAD read "$env(PADHOME)/draw/startup.pad"
    $PAD damage
    $PAD update all
    $PAD update all
    $PAD moveto -twostep 111.621 237.277 900 2000 $portalid
    $PAD moveto -twostep 111.621 237.277 2000 2000
    $PAD delete all
    $PAD moveto 0 0 1
}

proc makePad {PAD} {
    global _pad _color
    global pad env
    global Pad_Version
    global VersionString
    global menu geometry
    global tcl_platform

    pad $PAD -width $_pad(Width) -height $_pad(Height) -bg $_color(padbg) \
	     -gamma $_pad(Gamma) -defaultEventHandlers 0
    $PAD addtag pad 1
    if {$tcl_platform(platform) != "unix"} {
	$PAD cache config -size 0 
    }
    set _pad(PadTitle$PAD) "Pad++ v$Pad_Version"
    set _pad(PadFileName$PAD) $_pad(PadFileName)
    set _pad(ChangesMade$PAD)    false
    lappend _pad(Pads) $PAD

    pack $PAD -side left -expand true -fill both
    focus $PAD

				# Add the paddraw fonts
    $PAD font path +$env(PADHOME)/draw/fonts

                                # Have cache manager use the default dir and size
    $PAD cache config -dir $_pad(cacheDir)
    $PAD cache config -size $_pad(cacheSize)
    $PAD cache config -delay $_pad(cacheDelay)

				# Configure pad widget according to current values
    $PAD config \
	-gamma $_pad(Gamma) \
	-desiredFrameRate $_pad(DesiredFrameRate) \
	-smallObjSize $_pad(SmallObjSize) \
	-mediumObjSize $_pad(MediumObjSize)

				# Don't want status line to get written out
    $PAD bind status <Write> {set Pad_Write 0; return ""}
    $PAD bind statusbg <Write> {set Pad_Write 0; return ""}
				# Don't want handles written out
    $PAD bind modifier <Write> {set Pad_Write 0; return ""}
				# Append result of all procedures
				# that start with Pad_preWrite
				# for the beginning of the save file
    $PAD bind preWrite <Write> {
	set _pad(selected) [pad_sel %P]
	pad_unselect %P all     # Unselect items before writing out
	set result ""
	foreach cmd [info command Pad_preWrite*] {
	    append result [$cmd %P]
	    append result "\n"
	}
	return $result
    }
				# Append result of all procedures
				# that start with Pad_postWrite
				# for the beginning of the save file
    $PAD bind postWrite <Write> {
	global _pad

	eval pad_select %P $_pad(selected)
	unset _pad(selected)
	set result ""
	foreach cmd [info command Pad_postWrite*] {
	    append result [$cmd %P]
	    append result "\n"
	}

	if {[info exists _pad(StartView)]} {
	    append result "set _pad(StartView) \"$_pad(StartView)\"\n"
	}
	return $result
    }

				# When an image object that was allocated by PadDraw
				# is deleted, free up associated image memory.
    $PAD bind image_item <Delete> {
	set token [%P ic %O -image]
	catch {%P image free $token}
    }

				# When an item is deleted, let's make sure that any
				# modifiers get deleted along with it.
    $PAD bind item <Delete> {
	if {[%W hastag "selected" %O]} {
	    %W delete "handle%O"
	}
    }
				# When the window is resized, update location of certain items
				# Call every Tcl procedure starting with "Pad_Configure..."
    bind $PAD <Configure> {
	foreach cmd [info command Pad_Configure*] {
	    $cmd %W
	}
				# Must delay creation of status line because logo gets created
				# just after this and we don't want them to overlap
	after 0 updateStatusLocation %W
	updateBufferLocation %W
	pad_update_debug_status_location %W
    }
    bind $PAD <Destroy> {padExitCleanup %W}

				# Make sure each geometry element is ok
    foreach window [array name geometry] {
	set geo $geometry($window)
	if {$geo != 0} {
	    regsub {\+.*} $geo "" dim
	    if {$dim != ""} {
		regexp {(.*)x(.*)} $dim match x y
		if {($x <= 1) || ($y <= 1)} {
		    unset geometry($window)
		}
	    }
	}
    }
}

#
# Show coordinates in status line
#
proc setCoords {PAD visible} {
    if {$visible} {
	$PAD bind all <Motion> {
	    set coords "%i %j [%W getzoom]"
	    updateStatusText %W $coords 1
	}
    } else {
	$PAD bind all <Motion> {}
	updateStatusText $PAD "" 0
    }
}

#
# Update location of status line when window size changes
#
proc updateStatusLocation {PAD} {
    global _pad

    if {![winfo exists $PAD]} {
	return
    }
    if {![info exists _pad(StatusLine$PAD)]} {
	set _pad(StatusLine$PAD) $_pad(StatusLine)
    }

    if {($_pad(StatusLine$PAD) == 0)} {
	$PAD ic status -lock 0
	$PAD ic statusbg -lock 0
	$PAD delete status statusbg
	updateLogoPosition $PAD
	return
    }

			# Update status background
    set bgid [$PAD find withtag statusbg]
    if {$bgid == ""} {
	set bgid [$PAD create rectangle 0 0 1 1 -tags statusbg -fill white -pen black \
		-penwidth 0 -transparency 0.5 -events 0 -sticky 1 -layer status]
	$PAD lower $bgid
    }
    set xy [$PAD padxy 2 [expr [lindex [$PAD config -height] 4] - 1]]
    set x1 [lindex $xy 0]
    set y1 [lindex $xy 1]
    set xy [$PAD padxy [expr [lindex [$PAD config -width] 4] - 2] \
	    [expr [lindex [$PAD config -height] 4] - 20]]
    set x2 [lindex $xy 0]
    set y2 [lindex $xy 1]

    $PAD ic $bgid -lock 0
    $PAD coords $bgid $x1 $y1 $x2 $y2
    $PAD ic $bgid -lock 1

			# Update status text
    set id [$PAD find withtag status]
    if {$id != ""} {
	$PAD ic $id -lock 0
	$PAD layout position 0 0 -ref $bgid 0 0 $id
	$PAD ic $id -lock 1
	$PAD lower $bgid $id
    }

			# Update logo position
    updateLogoPosition $PAD $bgid
}

#
# This updates the status line on the main pad surface
# If <emphasizebg> is 1, then make the background solid
# If <emphasizebg> is 0, then make the background transparent
#
proc updateStatusText {PAD text {emphasizebg 1}} {
    global _pad

    if {![winfo exists $PAD]} {
	return
    }
    if {![info exists _pad(StatusLine$PAD)]} {
	set _pad(StatusLine$PAD) $_pad(StatusLine)
    }

    if {($_pad(StatusLine$PAD) == 0)} {
	$PAD ic status -lock 0
	$PAD ic statusbg -lock 0
	$PAD delete status statusbg
	updateLogoPosition $PAD
	return
    }
    set id [$PAD find withtag status]
    if {$id == ""} {
	set id [$PAD create text -anchor sw -tags status -text " " \
		-events 0 -sticky 1 -layer status -font "Times-10"]
	updateStatusLocation $PAD
    }
    $PAD itemconfig $id -lock 0
    $PAD itemconfig $id -text $text
    $PAD itemconfig $id -lock 1

    set bgid [$PAD find withtag statusbg]
    if {$bgid != ""} {
	$PAD ic $bgid -lock 0
	if {$emphasizebg} {
	    $PAD ic $bgid -transparency 1.0
	} else {
	    $PAD ic $bgid -transparency 0.5
	}
	$PAD ic $bgid -lock 1
    }
}

proc updateStatusMsg {PAD text {time 1}} {
    updateStatusText $PAD $text 1
    after [expr 1000 * $time] "updateStatusText $PAD {} 0"
}

#
# Update location of debug status line when window size changes
#
proc pad_update_debug_status_location {PAD} {
    global _pad

    if {![winfo exists $PAD]} {
	return
    }
    if {![info exists _pad(DebugStatusLine)] || ($_pad(DebugStatusLine) == 0)} {
	$PAD ic debugstatus -lock 0
	$PAD delete debugstatus
	return
    }
    set id [$PAD find withtag debugstatus]
    if {$id != ""} {
	$PAD ic $id -lock 0
	set xy [$PAD padxy 5 [expr [lindex [$PAD config -height] 4] - 20]]

	set zoom [$PAD getzoom]
	$PAD ic $id -place "$xy [expr 1.0 / $zoom]"
	$PAD ic $id -lock 1
    }
}

#
# This updates the debug status line on the main pad surface
#
proc pad_update_debug_status {PAD} {
    global _pad

    if {![winfo exists $PAD]} {
	return
    }
    if {![info exists _pad(DebugStatusLine)] || $_pad(DebugStatusLine) == 0} {
	$PAD ic debugstatus -lock 0
	$PAD delete debugstatus
	$PAD ic 1 -renderscript {}
	return
    }
    if {[$PAD ic 1 -renderscript] == ""} {
	$PAD ic 1 -renderscript "$PAD renderitem; pad_update_debug_status $PAD"
    }

    set id [$PAD find withtag debugstatus]
    if {$id == ""} {
	set id [$PAD create text -anchor sw -tags debugstatus -text " " \
		-events 0 -sticky 1 -layer status -font "Times-10"]
				# Don't want debug status line to get written out
	$PAD bind debugstatus <Write> {set Pad_Write 0; return ""}
	pad_update_debug_status_location $PAD
    }
    $PAD itemconfig $id -lock 0
    set text [$PAD info status render]
    $PAD itemconfig $id -text $text
    $PAD itemconfig $id -lock 1
}

#
# Define mechanism for (un)iconifying PadDraw window.
#
proc bindIconification {PAD} {
    global env

		# Define window to get mapped when main pad window is iconified
    set toplevel [winfo toplevel $PAD]
    set lastdot [string last . $toplevel]
    set icon ".icon[string range $toplevel [expr $lastdot + 1] end]"
    if { ![winfo exists $icon] } {
	set image [image create photo -file $env(PADHOME)/images/icon.gif]
	toplevel $icon
	button $icon.image -image $image
	pack $icon.image
	wm iconwindow [winfo toplevel $PAD] $icon
    }
    bind $PAD <Unmap> {iconifyPadWindow %W}
    bind $PAD <Map> {uniconifyPadWindow %W}
}

#
# Make top-level window
#
proc makeTop {PAD} {
    global _pad
    global geometry

    set toplevel [winfo toplevel $PAD]
    wm minsize $toplevel 100 100
    wm title $toplevel $_pad(PadTitle$PAD)
    wm iconname $toplevel Pad++
    if {[winfo toplevel $PAD] != "."} {
	set x [winfo x .]
	set y [winfo y .]
	wm geometry [winfo toplevel $PAD] "+[expr $x + 100]+[expr $y + 100]"
    } elseif {[info exists geometry(.pad)]} {
				# Need to specify widget size instead of window geometry
	                        # because, otherwise the packer keeps the top-level geometry
	                        # fixed and won't let the -width and -height configure
                                # options change it.
	set plusindex [string first + $geometry(.pad)]
	set xindex [string first x $geometry(.pad)]
	set position [string range $geometry(.pad) $plusindex end]
	set width [string range $geometry(.pad) 0 [expr $xindex - 1]]
	set height [string range $geometry(.pad) [expr $xindex + 1] [expr $plusindex - 1]]
	wm geometry $toplevel $position
	$PAD config -width $width
	$PAD config -height $height
    } else {
	wm geometry $toplevel +$_pad(X)+$_pad(Y)
    }
}

proc makeIndex {} {
    auto_mkindex . draw/*.tcl
}

#
# Set the anchor of an item, and set the item's place
# in such a way that after the anchor has been set,
# the item doesn't move.
#
proc pad_set_anchor {PAD item newanchor} {
    set bbox [$PAD bbox $item]
    $PAD ic $item -anchor $newanchor
    $PAD layout position 0 0 -bbox $bbox 0 0 $item
}

#
# Make a file dialog box to let the user choose a file to open,
# and then open it.
#
proc pad_open {PAD} {
    global _pad

    set filename [make.file "Select a file to read" $_pad(PadFileName$PAD)]
    if {$filename != ""} {
	pad_open_file $PAD $filename
    }
}

proc pad_open_file {PAD filename} {
    global _pad _file Pad_Version

    if {[file exists $filename]} {
	updateStatusText $PAD "Loading $filename ..." 1
	update
	$PAD delete all
	set _pad(PadFileName$PAD) $filename
	if {[file isdirectory $filename]} {
	    set _pad(PadDirName$PAD) $filename
	} else {
	    set _pad(PadDirName$PAD) [file dirname $filename]
	}
	catch {unset _pad(StartView)}
	$PAD read $_pad(PadFileName$PAD)
	if {$_file(RGB) == 0} {
				# No RGB requested, so throw out RGB
	    global Pad_ObjectList
	    foreach obj $Pad_ObjectList {
		if {[$PAD type $obj] == "image"} {
		    $PAD image config [$PAD ic $obj -image] -rgb 0
		}
	    }
	}
	link_ConvertFormat $PAD
	set _pad(PadTitle$PAD) "Pad++ v$Pad_Version ($_pad(PadFileName$PAD))"
	wm title [winfo toplevel $PAD] $_pad(PadTitle$PAD)
	changesMade $PAD false
	updateStatusText $PAD {} 0
				# If there is a StartView then use that.  Else, center items
	if {[info exists _pad(StartView)]} {
	    eval $PAD centerbbox $_pad(StartView) 0 .5 .5 1
	} else {
	    set objs [$PAD find all && !withtag statusbg && !withtag status && !withtag logo]
	    if {$objs != ""} {
		set bbox [eval $PAD bbox $objs]
		eval $PAD centerbbox $bbox 0 .5 .5 1
	    }
	}
    }
	
}

#
# Make a file dialog box to let the user choose a file to import,
# and then import it.  This will read all kinds of files, and inserts
# it into the existing pad dataspace.
#
proc pad_import {PAD} {
    global _pad _file

    if {![info exists _pad(PadDirName$PAD)]} {
	set _pad(PadDirName$PAD) ""
    }
    if {$_pad(PadDirName$PAD) != ""} {
	set file $_pad(PadDirName$PAD)
    } else {
	set file $_pad(PadFileName$PAD)
    }
    set filename [make.file "Select a file (or directory) to import" $file "import"]
    if {$filename != ""} {
	import $PAD $filename $_file(SelectItems)
	if {[file isdirectory $filename]} {
	    set _pad(PadDirName$PAD) $filename
	} else {
	    set _pad(PadDirName$PAD) [file dirname $filename]
	}
    }
}

#
# If there is a view associated with some objects, and the objects move,
# then this function calculates how the view should change in order to
# keep the same relative position to the objects.
#
proc pad_calculate_transformed_view_bbox {oview obbox nbbox} {
    set width [bbwidth $nbbox]
    set height [bbheight $nbbox]
    set ctr [bbcenter $nbbox]
    set xctr [lindex $ctr 0]
    set yctr [lindex $ctr 1]
    set octr [bbcenter $obbox]
    set oxctr [lindex $octr 0]
    set oyctr [lindex $octr 1]
    set newx1 [expr $xctr - ($width * 0.5) * (([lindex $oview 0] - $oxctr) / ([lindex $obbox 0] - $oxctr))]
    set newy1 [expr $yctr - ($height * 0.5) * (([lindex $oview 1] - $oyctr) / ([lindex $obbox 1] - $oyctr))]
    set newx2 [expr $xctr + ($width * 0.5) * (([lindex $oview 2] - $oxctr) / ([lindex $obbox 2] - $oxctr))]
    set newy2 [expr $yctr + ($height * 0.5) * (([lindex $oview 3] - $oyctr) / ([lindex $obbox 3] - $oyctr))]

    return "$newx1 $newy1 $newx2 $newy2"
}

#
# Turn the specified list of polygons into portals
# of the same shape
#
proc pad_PolygonToPortal {PAD items} {
    set zoom [eval $PAD getzoom]
    set width [expr 5.0 / $zoom]
    foreach item $items {
	if {![catch {set coords [$PAD coords $item]}]} {
	    set fc [$PAD ic $item -fill]
	    pad_unselect $PAD $item
	    $PAD delete $item
	    set id [eval $PAD create portal $coords -fill $fc -borderwidth $width -tags \"item drag\" \
			-visiblelayers \"all -status\"]
	    pad_select $PAD $id
	    changesMade $PAD true
	}
    }
}

#
# Set the specified option to the given value for
# all members of the group, recursively.
#
proc pad_set_group_option {PAD group option value} {
    foreach member [$PAD ic $group -members] {
	set type [$PAD type $member]
	if {($type == "group") || ($type == "frame") || ($type == "panel")} {
	    pad_set_group_option $PAD $member $option $value
	} else {
	    catch {$PAD ic $member $option $value}
	}
    }
}

#
# Return true if there is a draganddrop method
# defined for any of <container>'s tags.
#
proc pad_supports_draganddrop {PAD container state} {
    global _modifier

    set result 0

    foreach tag [$PAD ic $container -tags] {
	if {[info command "pad_draganddrop_$tag"] != ""} {
	    set result 1
	    break
	}
    }

    return $result
}

#
# Mark container for drag and drop, if
# container supports it.
#
proc pad_mark_draganddrop_container {PAD x y state} {
    global _select

    set container [$PAD pick -indivisible $x $y]
    if {![info exists _select(container)]} {
	set _select(container) ""
    }
    if {$_select(container) != $container} {
	pad_unhilite $PAD $_select(container)
	if {[pad_supports_draganddrop $PAD $container $state]} {
	    pad_hilite $PAD $container
	}
	set _select(container) $container
    }
}

#
# Highlight the specified item by ...
#
proc pad_hilite {PAD item {color yellow} {trans 1}} {
    global _hilite

    set _hilite($item) 1

    set zoom [$PAD getzoom]
    eval $PAD create rectangle [$PAD bbox $item] \
	-pen $color \
	-penwidth [expr 3 / $zoom] \
	-tags "hilite$item" -events 0 \
	-transparency $trans    
}

#
# Restore the specified item to it's un-highlighted form
#
proc pad_unhilite {PAD item} {
    global _hilite

    if {[info exists _hilite($item)]} {
	$PAD delete hilite$item
	unset _hilite($item)
    }
}

#
# Flash the pen or fill or tagor id (depending on type) briefly
#
proc pad_flash {PAD tagorid type} {
    if {$type == "pen"} {
	set option "-pen"
    } elseif {$type == "fill"} {
	set option "-fill"
    } else {
	return
    }

    set color [$PAD ic $tagorid $option]
    if {($color == "white") || ($color == "#ffffff")} {
	set contrasting_color "black"
    } else {
	set contrasting_color "white"
    }

    $PAD ic $tagorid $option $contrasting_color
    $PAD update
    after 100
    $PAD ic $tagorid $option $color
    $PAD update
    after 100
    $PAD ic $tagorid $option $contrasting_color
    $PAD update
    after 100
    $PAD ic $tagorid $option $color
    $PAD update
}

#
# Animate the coords of the specified item from
# its current coordinates to the specified ones.
#
proc pad_anim_coords {PAD item coords} {
    global _pad

    set totalTime $_pad(AnimationSpeed)
    set clock [$PAD clock]
    set time 0
    set origCoords [$PAD coords $item]
    set destCoords $coords
    set len [llength $origCoords]
    while {$time < $totalTime} {
	set coords ""
	set t [expr 1.0 * $time / $totalTime]
	for {set i 0} {$i < $len} {incr i} {
	    set coord1 [lindex $origCoords $i]
	    set coord2 [lindex $destCoords $i]
	    lappend coords [lerp $t $coord1 $coord2]
	}
	eval $PAD coords $item $coords
	$PAD update
	set time [$PAD clock $clock]
    }
    eval $PAD coords $item $destCoords
    $PAD clock $clock delete
}

#
# For each item (if it is a line), turn it into a spline.
# Save the original coordinates in _pad($item.line_coords)
# to be used later in the same session to unsmooth the resulting spline.
#
proc pad_smooth {PAD items} {
    global _pad

    foreach item $items {
	if {[$PAD type $item] == "line"} {
	    set _pad($item.line_coords) [$PAD coords -objectcoords $item]
	    set maxdim [maxdim [$PAD bbox $item]]
	    set error [expr $maxdim * 0.01]
	    set properties [$PAD ic -nondefaults $item]

	    set spline_coords [eval $PAD line2spline $error $_pad($item.line_coords)]
	    set spline_item [eval $PAD create spline $spline_coords]
	    foreach property $properties {
		set option [lindex $property 0]
		set value [lindex $property 1]
					# Lines could have user-defined types that aren't
					# defined for splines, so catch option setting
		catch {$PAD ic $spline_item $option $value}
	    }
	    pad_unselect $PAD $item
	    $PAD delete $item
	    $PAD setid $spline_item $item
	    pad_unselect $PAD $item
	    pad_select $PAD $item
	}
    }
}

#
# For each item (if it is a spline that was smoothed from a line this session), 
# turn it back into a line.
#
proc pad_unsmooth {PAD items} {
    global _pad

    foreach item $items {
	if {[$PAD type $item] == "spline"} {
	    if {[info exists _pad($item.line_coords)]} {
		set properties [$PAD ic -nondefaults $item]
		set line_item [eval $PAD create line $_pad($item.line_coords)]
		unset _pad($item.line_coords)
		foreach property $properties {
		    set option [lindex $property 0]
		    set value [lindex $property 1]
					# Splines could have user-defined types that aren't
					# defined for lines, so catch option setting
		    catch {$PAD ic $line_item $option $value}
		}
		pad_unselect $PAD $item
		$PAD delete $item
		$PAD setid $line_item $item
		pad_unselect $PAD $item
		pad_select $PAD $item
	    }
	}
    }
}

#
# Make the specified item never fade
#
proc pad_fadenone {PAD item {factor 1.0}} {
			# Fade group members, not group item
    if {[$PAD type $item] == "group"} {
	foreach member [$PAD ic $item -members] {
	    pad_fadenone $PAD $member $factor
	}
    }

    $PAD ic $item -minsize 0 -maxsize -1
}

#
# Set the -minsize on <item> to fade in at the default size.
#
proc pad_fadedefault {PAD item {factor 1.0}} {
			# Fade group members, not group item
    if {[$PAD type $item] == "group"} {
	foreach member [$PAD ic $item -members] {
	    pad_fadedefault $PAD $member $factor
	}
    }

    $PAD ic $item -minsize {} -maxsize {}
}

#
# Set the -minsize on <item> to fade in at the
# current size (relative to the window size).
#
proc pad_fadein {PAD item {factor 1.0}} {
			# Fade group members, not group item
    if {[$PAD type $item] == "group"} {
	foreach member [$PAD ic $item -members] {
	    pad_fadein $PAD $member $factor
	}
    }

    set obj_size [expr $factor * [$PAD getsize $item]]
    set bbox [$PAD bbox $item]
    if {[bbwidth $bbox] >= [bbheight $bbox]} {
	set window_dim [winfo width $PAD]
    } else {
	set window_dim [winfo height $PAD]
    }
    set min_percent [expr (100.0 * $obj_size) / $window_dim + 0.5]
    $PAD ic $item -minsize $min_percent%
}

#
# Set the -maxsize on <item> to fade out at the
# current size (relative to the window size).
#
proc pad_fadeout {PAD item {factor 1.0}} {
			# Fade group members, not group item
    if {[$PAD type $item] == "group"} {
	foreach member [$PAD ic $item -members] {
	    pad_fadeout $PAD $member $factor
	}
    }

    set obj_size [expr $factor * [$PAD getsize $item]]
    set bbox [$PAD bbox $item]
    if {[bbwidth $bbox] >= [bbheight $bbox]} {
	set window_dim [winfo width $PAD]
    } else {
	set window_dim [winfo height $PAD]
    }
    set max_percent [expr (100.0 * $obj_size) / $window_dim - 0.5]
    $PAD ic $item -maxsize $max_percent%
}

#
# Set up fading so that one object fades into another.
# The objects are specified by selection where the
# first (back) object selected fades into the second (front)
# object selected as you zoom in.
#
proc pad_cross_fade {PAD} {
    set sel [pad_sel $PAD]
    if {[llength $sel] != 2} {
	pad_bgnd_error \
"Cross fades require exactly two objects 
to be selected.  The one in back will fade
into the one in front when zoomed in."
	return
    }
    set obj1 [lindex $sel 1]
    set obj2 [lindex $sel 0]
    pad_fadein $PAD $obj1 0.8
    pad_fadeout $PAD $obj2 1.2
}

#
# Generate a modal pop-up window with the
# specified error message.  The user must
# acknowledge it before proceeding.
#
proc pad_bgnd_error {msg} {
    pad_message $msg OK
}

#
# Generate a modal pop-up window with the
# specified error message.  <args> specifies
# a list of button labels that will be created.
# This returns the button that was pressed.
#
proc pad_message {msg args} {
    toplevel .message

    return [eval pad_frame_message .message \"\" [list $msg] $args]
}

#
# Like pad_message, but packs the message within
# the specified toplevel window, and packs the 
# given frame after the message, but before the buttons.
#
proc pad_frame_message {toplevel frame msg args} {
    global _color _pad _font

    wm resizable $toplevel 0 0
    message $toplevel.text -text $msg -aspect 500 \
	-font $_font(tools) \
	-bg $_color(toolbg)
    frame $toplevel.btns -bg $_color(toolbg)
    pack $toplevel.text -expand 1 -fill x
    if {$frame != ""} {
        pack $frame -expand 1 -fill x
    }
    pack $toplevel.btns -expand 1 -fill x
    set i 0
    foreach button $args {
	button $toplevel.btns.button$i -text $button \
	    -font $_font(tools) \
	    -command "set _pad(message) $button; destroy $toplevel" \
	    -bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	    -highlightbackground $_color(toolbg)
	pack $toplevel.btns.button$i -expand 1 -ipadx 10 -side left
	set i [incr i]
    }

				# Global modal lock on the message window
    focus "$toplevel.btns.button0"
    pad_ManageWindow $toplevel 0 0
    update idletask
    grab $toplevel
    tkwait window $toplevel

    return $_pad(message)
}

#
# Return true if this item has a fixed aspect ratio
#
proc pad_fixed_aspect_ratio {PAD id} {
    set type [$PAD type $id]
    if {!(($type == "line") || 
         ($type == "spline") || 
         ($type == "rectangle") || 
         ($type == "polygon") || 
         ($type == "oval") || 
         ($type == "portal"))} {
	set fixed 1
    } else {
	set fixed 0
    }

    return $fixed
}

#
# Return true if this item is rotatable
#
proc pad_is_rotatable {PAD id} {
    set type [$PAD type $id]
    if {(($type == "line") || 
         ($type == "spline") || 
         ($type == "text") || 
         ($type == "textfile") || 
         ($type == "image") || 
         ($type == "portal") || 
         ($type == "spline") || 
         ($type == "rectangle") || 
         ($type == "polygon") || 
         ($type == "oval") || 
         ($type == "group"))} {
	set rotatable 1
    } else {
	set rotatable 0
    }

    return $rotatable
}

#
# pad_select PAD tagOrId ?tagOrID ...?
#
#   Select items specified by the list of tagOrIds
#   Don't select items with any of the following tags:
#     modifier
#     htmlanchor
#     hide
#
proc pad_select {args} {
    global _select _pad

    if {[llength $args] < 1} {
	return -code error "usage: pad_select PAD tagOrId ..."
    }
    set PAD [lindex $args 0]

				# First get the items specified
    set ids ""
    foreach item [lrange $args 1 end] {
	if {$item == ""} {
	    continue
	}
	if {$item == "all"} {
	    set new_ids "[$PAD find withtag $item] "
	} else {
	    set new_ids "[$PAD find -groupmembers withtag $item] "
	}
	foreach id $new_ids {
	    if {![$PAD hastag "modifier" $id] && 
                ![$PAD hastag "htmlanchor" $id] && 
                ![$PAD hastag "hide" $id] && 
		![$PAD ic $id -lock]} {
		lappend ids $id
	    }
	}
    }
    set ids [lremove $ids 1]	;# Can't select pad surface

    if {$ids != ""} {
				# For each selected item
	foreach id $ids {
	    if {[$PAD hastag "selected" $id]} {
				# Item already selected
		continue
	    }
				# Add 'selected' tag to them
	    $PAD addtag selected $id
	    makeBBoxHandles $PAD $id
	}
	setDefaults $PAD [lindex $ids 0]
    }
}

#
# pad_unselect PAD tagOrId ?tagOrID ...?
#
#   Unselect items specified by the list of tagOrIds
#
proc pad_unselect {args} {
    global _select _pad

    if {[llength $args] < 1} {
	return -code error "usage: pad_unselect PAD tagOrId ..."
    }
				# Start by unselecting requested items
    set PAD [lindex $args 0]
    set ids ""
				# Build up list of requested items
    if {[lindex $args 1] == "all"} {
	set ids [pad_sel $PAD]
    } else {
	foreach item [lrange $args 1 end] {
	    append ids "[$PAD find -groupmembers withtag $item] "
	}
	set ids [lremove $ids 1]	;# Can't unselect pad surface
    }

    if {$ids != ""} {
	foreach id $ids {
				# Delete 'selected' tag from item
	    $PAD deletetag selected $id
				# Delete handles associated with item
	    $PAD delete "handle$id"
	}
    }
}

#
# pad_group
#
# Create a group with the specified items as members
# Put the group (and all the members) on the layer that
# the top-most member was on.
#
proc pad_group {PAD members} {

    set group ""
    if {[llength $members] >= 1} {
	set last_member [lindex $members [expr [llength $members] - 1]]
	set layer [$PAD ic $last_member -layer]
	set group [$PAD create group -layer $layer -members "$members" -tags "group" -maxsize -1]
    }

    return $group
}

#
# pad_ungroup
#
# Ungroup the specified groups (i.e., release their members)
#
proc pad_ungroup {PAD gids} {
    set members ""
    foreach gid $gids {
	if {[$PAD type $gid] == "group"} {
	    append members [$PAD ic $gid -members]
	    append members " "
	    $PAD ic $gid -members ""
	    $PAD delete $gid
	}
    }

    return $members
}

#
# pad_make_alias
#
#   Make an alias for each of the selected objects
#
proc pad_make_alias {PAD} {
    global _pad

    set items ""
    foreach item [pad_sel $PAD] {
	set z [$PAD ic $item -z]
	lappend items [$PAD create alias -reference $item -tags "item" -z $z]
    }
    if {$items == ""} {
	return
    }
    pad_unselect $PAD all
    set view [$PAD getview]
    set _pad(clipboard_zoom) [lindex $view 2]
    set _pad(clipboard_pastecnt) 0
    pad_position_pasted_items $PAD $items
    pad_select $PAD $items
}

#
# pad_cut
#
#   Cut the selected objects out of the pad surface onto the clipboard
#
proc pad_cut {PAD} {
    pad_copy $PAD
    pad_fade_delete $PAD [pad_sel $PAD]
}

#
# pad_copy
#
#   Copy the selected objects onto the clipboard.
#
proc pad_copy {PAD} {
    global _pad

    set ids [pad_sel $PAD]
    set _pad(clipboard) [eval $PAD write \"\" $ids]
    set view [$PAD getview]
    set _pad(clipboard_zoom) [lindex $view 2]
    set _pad(clipboard_pastecnt) 0
    changesMade $PAD true
}

#
# pad_paste
#
#   Copy the objects out of the clipboard onto the specified pad surface.
#   Select the copied objects, and unselect everything else.
#
proc pad_paste {PAD} {
    global _pad _layer Pad_ObjectList PADLOAD

    set _pad(pasting) 1
    updateStatusText $PAD "Pasting..." 1
    update
    pad_unselect $PAD all
    if {[info exists _pad(clipboard)]} {
	set PADLOAD $PAD
	set Pad_ObjectList ""
	uplevel #0 "$_pad(clipboard)"
	if {$Pad_ObjectList != ""} {
	    pad_position_pasted_items $PAD $Pad_ObjectList
	}
	foreach item $Pad_ObjectList {
					# Hyperlinks code needs to clean up pasted items
	    link_CleanupPaste $PAD $item
	    $PAD ic $item -layer $_layer(current)
	}
	changesMade $PAD true
    }
    updateStatusText $PAD "" 0
    set _pad(pasting) 0
}

#
# pad_fade_delete
#
#   Delete the selected objects off of the pad surface.
#
proc pad_delete {PAD} {
    pad_fade_delete $PAD [pad_sel $PAD]
}

#
# pad_fade_delete
#
#   Delete the specified objects off of the pad surface.
#   Do so by fading them out first.  Take _pad(FadeTime) milliseconds
#   to do the fade.
#
proc pad_fade_delete {PAD tagorids} {
    global _pad

    foreach tagorid $tagorids {
	$PAD addtag delete_item $tagorid
    }
    if {![info exists _pad(fade_clock)]} {
	set _pad(fade_clock) [$PAD clock]
    }
    after 0 _pad_fade_delete $PAD 15
}

# helper function to pad_fade_delete
proc _pad_fade_delete {PAD level} {
    global _pad

    if {![info exists _pad(fade_clock)]} {
	return
    }

    $PAD ic delete_item -transparency [expr $level / 16.0]
    set dt [$PAD clock $_pad(fade_clock)]
    set level [expr int(15.0 * ($_pad(FadeTime) - $dt) / $_pad(FadeTime))]
    if {$level <= 0} {
	$PAD delete delete_item
	$PAD delete modifier
	$PAD deletetag delete_item all       ;# Some locked items may not get deleted
	$PAD clock $_pad(fade_clock) delete  ;# Delete fade clock
	unset _pad(fade_clock)
    } else {
	after 0 _pad_fade_delete $PAD $level
    }
}

#
# pad_position_pasted_items
#
#   Position the specified list of items so that they
#   appear in the middle of the screen at the size they
#   were copied.
#
proc pad_position_pasted_items {PAD items} {
    global _pad

    set idsbb [eval $PAD bbox $items]
    set idcenter [bbcenter $idsbb]
    set xctr [lindex $idcenter 0]
    set yctr [lindex $idcenter 1]
    set view [$PAD getview]
    set xloc [lindex $view 0]
    set yloc [lindex $view 1]
    set zoom [lindex $view 2]
    set dx [expr $xloc - $xctr + (25 * $_pad(clipboard_pastecnt) / $zoom)]
    set dy [expr $yloc - $yctr - (25 * $_pad(clipboard_pastecnt) / $zoom)]
    set dz [expr $_pad(clipboard_zoom) / $zoom]
    incr _pad(clipboard_pastecnt)
    
    foreach id $items {
	$PAD slide $id $dx $dy
	$PAD scale $id $dz $xloc $yloc
	$PAD deletetag selected $id
	pad_select $PAD $id
    }
}

#
# pad_center_object
#
#   Positions the object so that it is centered on the screen, and 
#   fills up <value> percentage of the screen.
#
proc pad_center_object {PAD id {value 0.9}} {
    set view [$PAD getview]
    set viewbbox [$PAD bbox 1]
    set viewwidth [bbwidth $viewbbox]
    set viewheight [bbheight $viewbbox]

    set bbox [$PAD bbox $id]
    set width [bbwidth $bbox]
    set height [bbheight $bbox]
    if {[expr ($width - $viewwidth) > ($height - $viewheight)]} {
	set scale_factor [expr $value * $viewwidth / $width]
    } else {
	set scale_factor [expr $value * $viewheight / $height]
    }
    $PAD scale $id $scale_factor

    set bbox [$PAD bbox $id]
    set bbcenter [bbcenter $bbox]
    set dx [expr [lindex $view 0] - [lindex $bbcenter 0]]
    set dy [expr [lindex $view 1] - [lindex $bbcenter 1]]
    $PAD slide $id $dx $dy

    if {[$PAD hastag "selected" $id]} {
	pad_unselect $PAD $id
	pad_select $PAD $id
    }
}

#
# pad_same_size
#
#   Resizes the objects so they are all the same size.
#   The definition of size for the purposes of this routine
#   is the height of the object (or the font size for text
#   objects).  All objects are made the same size as the
#   largest one selected.
#
proc pad_same_size {PAD ids} {
    if {$ids == ""} {
	return
    }

    set biggestid ""
    set biggestsize 0
    foreach id $ids {
	set lines [pad_get_lines $PAD $id]
	set size [expr [bbheight [$PAD bbox $id]] / $lines]
	if {$size > $biggestsize} {
	    set biggestid $id
	    set biggestsize $size
	}
    }

    foreach id $ids {
	set lines [pad_get_lines $PAD $id]
	set size [expr [bbheight [$PAD bbox $id]] / $lines]
	$PAD scale $id [expr $biggestsize / $size]
    }
    
    pad_unselect $PAD all
    eval pad_select $PAD $ids
}

#
# pad_get_lines
#
#   Returns the number of lines of text in this object.
#   If it is not a text object, this returns 1.
#
proc pad_get_lines {PAD id} {
    set type [$PAD type $id]
    if {($type == "text") || ($type == "textfile")} {
	set text [$PAD ic $id -text]
	set lines [expr 1 + [regsub -all "\n" $text "" junk]]
    } else {
	set lines 1
    }

    return $lines
}

#
# pad_get_font_size
#
#   Given the name of a font (such as Times-bold-12),
#   this returns the size of that font (12 in this case).
#
proc pad_get_font_size {font} {
    set index [string last "-" $font]
    if {$index == -1} {
	    set size 12   ;# Default size if none specified
    } else {
	set size [string range $font [expr $index + 1] end]
	if {[regexp {[0-9]+} $size] == 0} {
	    set size 12   ;# Default size if none specified
	}
    }
    return $size
}


#
# pad_get_group
#
#   Returns the top-most group member containing item,
#   or item if item is not a group.
#
proc pad_get_group {PAD id} {
    set group [$PAD getgroup $id]
    if {$group == ""} {
	return $id
    } else {
	return [pad_get_group $PAD $group]
    }
}

#
# pad_get_members
#
#   Returns all the members, recursively, of the specified item.
#
proc pad_get_members {PAD id} {
    if {[$PAD type $id] == "group"} {
	set objs [$PAD ic $id -members]
	set members ""
	foreach obj $objs {
	    set temp [pad_get_members $PAD $obj]
	    foreach member $temp {
		lappend members $member
	    }
	}
	set objs $members
    } else {
	set objs $id
    }

    return $objs
}

#
# Methods for dealing with the starting view of a Pad++ file.
# When a file is loaded (through open or import), the view
# changed to that associated with the file.
#
proc pad_set_start_view {PAD} {
    global _pad

    set _pad(StartView) [$PAD bbox 1]
    $PAD deletetag "start" all
}

proc pad_goto_start_view {PAD} {
    global _pad

    if {[info exists _pad(StartView)]} {
	eval $PAD centerbbox $_pad(StartView) $_pad(AnimationSpeed) .5 .5 1
    } else {
	pad_bgnd_error "There is no current starting view"
    }
}

proc pad_clear_start_view {PAD} {
    global _pad

    catch {unset _pad(StartView)}
}

#
# pad_position_obj_to_bbox
#
#   Positions the object so that it fills the specified bounding box.
#
proc pad_position_obj_in_bbox {PAD id xmin ymin xmax ymax} {
    set viewwidth [expr $xmax - $xmin]
    set viewheight [expr $ymax - $ymin]

    set bbox [$PAD bbox $id]
    set width [bbwidth $bbox]
    set height [bbheight $bbox]
    if {[expr ($width - $viewwidth) > ($height - $viewheight)]} {
	$PAD scale $id [expr $viewwidth / $width]
    } else {
	$PAD scale $id [expr $viewheight / $height]
    }

    set bbox [$PAD bbox $id]
    set dx [expr $xmin - [lindex $bbox 0]]
    set dy [expr $ymax - [lindex $bbox 3]]
    $PAD slide $id $dx $dy

    if {[$PAD hastag "selected" $id]} {
	pad_unselect $PAD $id
	pad_select $PAD $id
    }
}

#
# pad_toggle_full_screen
#
#   This toggles Pad++ into 'fullscreen' mode (taking up the whole screen) 
#   and back into normal mode again. In fullscreen mode, there is no menubar, 
#   the status line is turned off, and Pad++ appears with no window manager 
#   decorations.
#
proc pad_toggle_full_screen {PAD} {

    # pause to let the window manager catch up
    proc kluge_pause {} {
	for {set i 0} {$i < 2500} {incr i} {}
    }

    if {[pad_test_prop _padsize full] && [pad_get_prop _padsize full] == 1} {
	pad_normal_screen $PAD
    } else {
	pad_full_screen $PAD
    }
}

#
# pad_full_screen
#
#   Go to full screen mode (see pad_toggle_full_screen).
#
proc pad_full_screen {PAD} {
    global _pad
    
    if {[pad_test_prop _padsize full] && [pad_get_prop _padsize full] == 1} {
	# already full size
	return
    }
    
    # setup the logo so that clicking on it returns
    # you to normal size
    $PAD ic logo -lock 0 -events 1 -text "UCSD UNM NYU *"
    $PAD bind logo <ButtonRelease-1> "pad_toggle_full_screen %W"
    
    # hide the status line
#    set orig_status $_pad(StatusLine$PAD)
#    set _pad(StatusLine$PAD) 0
#    updateStatusText $PAD {} 0
    
    # remember the current size
    set geometry [wm geometry .]
    
    # set the width/height of the window to the
    # width/height of the screen, make the window an
    # override-redirect window so that it doesn't have
    # any WM decorations. To make this work you need to
    # withdraw and then map the window again. We stick in
    # a small pause before mapping the window since this seems
    # to make things work well.
    
    set width [winfo screenwidth .]
    set height [winfo screenheight .]
    
    # create a new toplevel - this is then given the focus
    toplevel .tmp
    wm geometry .tmp 1x1+2000+2000
    wm deiconify .tmp
    kluge_pause
    
    # withdraw the window
    wm withdraw .
    
    # hide the menubar
    pack forget .menubar
    
    # change the window size, set override-redirect
    wm overrideredirect . 1
    wm geometry . $width\x$height\+0+0
    
    # pause and then map
    kluge_pause
    wm deiconify .
    
    # take the focus back from the temporary toplevel
    
    wm overrideredirect . 0
    focus -force .tmp
    focus -force .
    wm overrideredirect . 1
    after 1000 "destroy .tmp"
    
    # remember the old state
    pad_set_prop _padsize full 1
    pad_set_prop _padsize geometry $geometry
#    pad_set_prop _padsize status $orig_status
}

#
# pad_normal_screen
#
#   Return to normal screen mode (see pad_toggle_full_screen).
#
proc pad_normal_screen {PAD} {
    global _pad

    if {[pad_test_prop _padsize full] && [pad_get_prop _padsize full] == 1} {
	# this is essentially the reverse of fullsize
	
	# withdraw the window
	wm withdraw .
	
	# repack the menubar
	pack .menubar -side top -before $PAD -fill x
	
	# set it to a normal window, at its original size
	wm overrideredirect . 0
	wm geometry . [pad_get_prop _padsize geometry]
	
	# pause, then map the window
	kluge_pause
	wm deiconify .
	
	# restore the logo
	$PAD ic logo -lock 0 -events 0 -text "UCSD UNM NYU" -lock 1
	
	# restore the status line
#	set _pad(StatusLine$PAD) [pad_get_prop _padsize status]
#	updateStatusText $PAD {} 0
	
	# note the new state
	pad_set_prop _padsize full 0
    }
}

#
# lmember: Returns true if 'element' is in 'list', false otherwise
#
proc lmember args {
    if {[llength $args] != 2} {
	puts "wrong # args: should be \"lmember list element\""
	return
    }
    set list [lindex $args 0]
    set element [lindex $args 1]
    set ind [lsearch -exact $list $element]
    if {$ind == -1} {
	return 0
    } else {
	return 1
    }
}

#
# lremove: Removes 'element' from 'list', or nothing if not if a member.
#          Returns the modified list.
#
proc lremove args {
    if {[llength $args] != 2} {
	puts "wrong # args: should be \"lremove list element\""
	return
    }
    set list [lindex $args 0]
    set element [lindex $args 1]
    
    set indx [lsearch -exact $list $element]
    if {$indx != -1} {
	set list [lreplace $list $indx $indx]
    }
    
    return $list
}

#
# lnewappend: Appends each value to variable varName as a list element
# if doesn't already exist, and returns the new value of the variable.
# Creates the variable if it doesn't already exist. 
#
proc lnewappend args {
    if {[llength $args] == 0} {
	puts "wrong # args: should be \"lnewappend varName value ?value ...?\""
	return
    }
    if {[llength $args] == 1} {
	return
    }

    set listname [lindex $args 0]
    upvar $listname list
    if {![info exists list]} {
	set list ""
    }
    foreach arg [lrange $args 1 end] {
	if {![lmember $list $arg]} {
	    lappend list $arg
	}
    }
    return $list
}

#
# lrev: Returns the reverse of the specified list
#

proc lrev {list} {
    set newlist ""
    foreach item $list {
	set newlist "$item $newlist"
    }
    return $newlist
}

#
# 9 functions to return points on the bbox
#
proc bbsw {bbox} {
    return "[lindex $bbox 0] [lindex $bbox 1]"
}

proc bbw {bbox} {
    set ctry [expr 0.5 * ([lindex $bbox 1] + [lindex $bbox 3])]
    return "[lindex $bbox 0] $ctry"
}

proc bbnw {bbox} {
    return "[lindex $bbox 0] [lindex $bbox 3]"
}

proc bbn {bbox} {
    set ctrx [expr 0.5 * ([lindex $bbox 0] + [lindex $bbox 2])]
    return "$ctrx [lindex $bbox 3]"
}

proc bbne {bbox} {
    return "[lindex $bbox 2] [lindex $bbox 3]"
}

proc bbe {bbox} {
    set ctry [expr 0.5 * ([lindex $bbox 1] + [lindex $bbox 3])]
    return "[lindex $bbox 2] $ctry"
}

proc bbse {bbox} {
    return "[lindex $bbox 2] [lindex $bbox 1]"
}

proc bbs {bbox} {
    set ctrx [expr 0.5 * ([lindex $bbox 0] + [lindex $bbox 2])]
    return "$ctrx [lindex $bbox 1]"
}
proc bbcenter {bbox} {
    set ctrx [expr 0.5 * ([lindex $bbox 0] + [lindex $bbox 2])]
    set ctry [expr 0.5 * ([lindex $bbox 1] + [lindex $bbox 3])]
    return "$ctrx $ctry"
}

#
# bbheight: Returns the height of the specified bounding box
#           bbox is specified as a single 4 element list
#

proc bbheight {bbox} {
    set lly [lindex $bbox 1]
    set ury [lindex $bbox 3]
    set height [expr $ury - $lly]
    
    return $height
}

#
# bbwidth: Returns the width of the specified bounding box
#          bbox is specified as a single 4 element list
#

proc bbwidth {bbox} {
    set llx [lindex $bbox 0]
    set urx [lindex $bbox 2]
    set width [expr $urx - $llx]
    
    return $width
}

#
# bbcenter: Returns the center (x, y) of the specified bounding box
#           bbox is specified as a single 4 element list
#

proc bbcenter {bbox} {
    set centerx [expr 0.5 * ([lindex $bbox 0] + [lindex $bbox 2])]
    set centery [expr 0.5 * ([lindex $bbox 1] + [lindex $bbox 3])]
    
    return "$centerx $centery"
}

#
# bbenclosed: Decides if the point (x, y) is enclosed by the
#             specified bounding box.  Everything in global coords
#

proc bbenclosed {x y bbox} {
    if {($x > [lindex $bbox 0]) &&
	($y > [lindex $bbox 1]) &&
	($x < [lindex $bbox 2]) &&
	($y < [lindex $bbox 3])} {
	return 1
    } else {
	return 0
    }
}

#
# bbenclosedoron: Decides if the point (x, y) is enclosed by or on the
#             specified bounding box.  Everything in global coords
#

proc bbenclosedoron {x y bbox} {
    if {($x >= [lindex $bbox 0]) &&
	($y >= [lindex $bbox 1]) &&
	($x <= [lindex $bbox 2]) &&
	($y <= [lindex $bbox 3])} {
	return 1
    } else {
	return 0
    }
}

#
# bbunion: Returns the bbox containing bbox1 and bbox2
#
proc bbunion {bbox1 bbox2} {
    set x1 [lindex $bbox1 0]
    set y1 [lindex $bbox1 1]
    set x2 [lindex $bbox1 2]
    set y2 [lindex $bbox1 3]
    set u1 [lindex $bbox2 0]
    set v1 [lindex $bbox2 1]
    set u2 [lindex $bbox2 2]
    set v2 [lindex $bbox2 3]
    
    if {$x1 < $u1} {set q1 $x1} else {set q1 $u1}
    if {$y1 < $v1} {set r1 $y1} else {set r1 $v1}
    if {$x2 > $u2} {set q2 $x2} else {set q2 $u2}
    if {$y2 > $v2} {set r2 $y2} else {set r2 $v2}
    
    return "$q1 $r1 $q2 $r2"
}

#
# maxdim: Returns the maximum dimension of the specified bounding box
#         bbox is specified as a single 4 element list
#

proc maxdim {bbox} {
    set width [bbwidth $bbox]
    set height [bbheight $bbox]
    
    return [max $width $height]
}

#
# stickyviewbbox: Returns the bounding box of the specified pad surface,
#                 ignoring the current view (appropriate for sticky objects).
#

proc stickyviewbbox {PAD} {
    set w [winfo width $PAD]
    set h [winfo height $PAD]
    set w2 [expr (0.5 * $w)]
    set h2 [expr (0.5 * $h)]
    return "[expr - $w2] [expr - $h2] $w2 $h2"
}

#
# min: Returns the minimum value of its n args
#

proc min args {
    set min [lindex $args 0]
    
    foreach i $args {
	if {$i < $min} {set min $i}
    }
    
    return $min
}

#
# max: Returns the maximum value of its n args
#

proc max args {
    set max [lindex $args 0]
    
    foreach i $args {
	if {$i > $max} {set max $i}
    }
    
    return $max
}

#
# sign: Returns the sign of the argument
#

proc sign {num} {
    if {$num >= 0} {
	return 1
    } else {
	return -1
    }
}

#
# lerp: Linearly interpolates from low (when t=0) to high (when t=1)
#

proc lerp {t low high} {
    return [expr ($t * $high) + ((1 - $t) * $low)]
}

#
# odd: Returns true if the number is odd, false otherwise
#
proc odd {number} {
    return [expr $number - (2 * ($number / 2))]
}

#
# even: Returns true if the number is even, false otherwise
#
proc even {number} {
    return [expr ! [odd $number]]
}

proc getconfig {PAD option} {
    return [lindex [$PAD config $option] 4]
}

#
# convertUnits
#
# Convert the specified value from one unit to another.
# If the fromUnits isn't specified, it defaults to the
# current system value.
#
proc convertUnits {PAD value toUnits {fromUnits ""}} {
    if {$fromUnits == ""} {
	set fromUnits [lindex [$PAD config -units] 4]
    }
    set configscreenmmwidth [lindex [$PAD config -widthmmofscreen] 4]
    if {$configscreenmmwidth == 0} {
	set screenmmwidth [winfo screenmmwidth $PAD]
    } else {
	set screenmmwidth $configscreenmmwidth
    }
    set mmppixel [expr [winfo screenwidth $PAD].0 / $screenmmwidth]
    set width [lindex [$PAD config -width] 4]
    set height [lindex [$PAD config -height] 4]
    set maxdim [max $width $height]
    case $fromUnits {
	inches {set pixels [expr $mmppixel * 25.4 * $value]}
	mm     {set pixels [expr $mmppixel * $value]}
	points {set pixels [expr $mmppixel * 0.353 * $value]}
	pixels {set pixels $value}
	default {puts "convertUnits: unknown unit type: $toUnits"}
    }
    case $toUnits {
	inches {set rc [expr $pixels / ($mmppixel * 25.4)]}
	mm     {set rc [expr $pixels / ($mmppixel)]}
	points {set rc [expr $pixels / ($mmppixel * 0.353)]}
	pixels {set rc $pixels}
    }
    
    return $rc
}

#
# stickycoords converts window pixels coordinates (x, y) to the equivalent sticky (i, j)
# coordinate.  Used to create a sticky object at a location given by (%x, %y).
#

proc stickycoords {PAD x y} {
    set width [winfo width $PAD]
    set height [winfo height $PAD]
    set sip [expr $x - ($width * 0.5)]
    set sjp [expr ($height * 0.5) - $y]
    return "$sip $sjp"
}


#
# Font name manipulation
#
proc pad_font_name {font} {
    set i [string first - $font]
    if {$i == -1} {
	return $font
    } else {
	return [string range $font 0 [expr $i - 1]]
    }

}

proc pad_select_cursor {PAD} {
    global _pad
	global tcl_platform

	if {$tcl_platform(platform) == "unix" } {

      if {[info exists _pad(SelectGroupMembers)] && $_pad(SelectGroupMembers)} {
        return "@$_pad(PadBitmaps)/hollowselect.xbm $_pad(PadBitmaps)/hollowselectmask.xbm black white"
      } else {
        return "@$_pad(PadBitmaps)/select.xbm $_pad(PadBitmaps)/selectmask.xbm black white"
      }
	} else {
	  if {[info exists _pad(SelectGroupMembers)] && $_pad(SelectGroupMembers)} {
        return "hollowselect"
      } else {
        return "select"
      }
    }

}

proc pad_pointer_cursor {PAD} {
    global _pad
	global tcl_platform

    if { $tcl_platform(platform) == "unix" } {

      if {[info exists _pad(SelectGroupMembers)] && $_pad(SelectGroupMembers)} {
        return "@$_pad(PadBitmaps)/hollowpointer.xbm $_pad(PadBitmaps)/hollowpointermask.xbm black white"
      } else {
        return "@$_pad(PadBitmaps)/pointer.xbm $_pad(PadBitmaps)/pointermask.xbm black white"
      }
	} else {
	  if {[info exists _pad(SelectGroupMembers)] && $_pad(SelectGroupMembers)} {
        return "hollowpointer"
      } else {
        return "pointer"
      }
	}

}

proc pad_font_bold {font} {
    return [expr [string first bold $font] != -1]
}

proc pad_font_italic {font} {
    return [expr [string first italic $font] != -1]
}


proc pad_set_cursor {Window unixArg winArg} {
    global tcl_platform

    if {$tcl_platform(platform) == "unix"} {
	$Window config -cursor $unixArg
    } else {
	$Window config -cursor $winArg
    }
}

