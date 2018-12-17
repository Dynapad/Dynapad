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
# This file defines the basic navigation interaction on a Pad.
# It is designed to be able to be used in a stand-alone mode, i.e.,
# without the rest of the PadDraw application - for imbedding in other
# applications.  In order to be used this way:
#
#   * Source this file
#   * Call bindPan with the name of the pad widget
#   * Call bindZoom with the name of the pad widget
#
# You can modify these global variables to control how these events work:
# 
#   * _pad(ZoomSpeed) controls the zoom speed (defaults to 5)
#   * _pad(ZoomInEvent) and _pad(ZoomOutEvent) specify on what event zooming occurs.  
#     The format is standard Tcl event sequence format.  Reasonable examples are:
#       <ButtonPress-2>
#       <Alt-ButtonPress-2>
#       <KeyPress-z>
#
#####################################################################
#####################################################################
#####################################################################

#
# Set some defaults for variables if they are not already set
# This way, bindPan and bindZoom can be used outside of PadDraw.
#
if {![info exists _pad(ZoomSpeed)]} {
    set _pad(ZoomSpeed) 5
}
if {![info exists _pad(ZoomStyle)]} {
    set _pad(ZoomStyle) "FollowCursor"
}
if {![info exists _pad(ZoomInEvent)]} {
    set _pad(ZoomInEvent)  <ButtonPress-2>
}
if {![info exists _pad(ZoomOutEvent)]} {
    set _pad(ZoomOutEvent) <ButtonPress-3>
}
if {![info exists _pad(EventModes)]} {
    set _pad(EventModes) {}
}
if {![info exists _pad(Click)]} {
    set _pad(Click) 10
}
if {![info exists _pad(PadBitmaps)]} {
    set _pad(PadBitmaps) $env(PADHOME)/bitmaps
}
if {![info exists _layer]} {
    set _layer(current) "main"
}
if {[info command "pad_set_cursor"] == ""} {
    proc pad_set_cursor {Window unixArg winArg} {
	global tcl_platform
	
	if {$tcl_platform(platform) == "unix"} {
	    $Window config -cursor $unixArg
	} else {
	    $Window config -cursor $winArg
	}
    }
}


#
# Define X event numbers
#
set _event(KeyPress)           2
set _event(KeyRelease)         3
set _event(ButtonPress)        4
set _event(ButtonRelease)      5
set _event(MotionNotify)       6
set _event(EnterNotify)        7
set _event(LeaveNotify)        8
set _event(FocusIn)            9
set _event(FocusOut)          10
set _event(KeymapNotify)      11
set _event(Expose)            12
set _event(GraphicsExpose)    13
set _event(NoExpose)          14
set _event(VisibilityNotify)  15
set _event(CreateNotify)      16
set _event(DestroyNotify)     17
set _event(UnmapNotify)       18
set _event(MapNotify)         19
set _event(MapRequest)        20
set _event(ReparentNotify)    21
set _event(ConfigureNotify)   22
set _event(ConfigureRequest)  23
set _event(GravityNotify)     24
set _event(ResizeRequest)     25
set _event(CirculateNotify)   26
set _event(CirculateRequest)  27
set _event(PropertyNotify)    28
set _event(SelectionClear)    29
set _event(SelectionRequest)  30
set _event(SelectionNotify)   31
set _event(ColormapNotify)    32
set _event(ClientMessage)     33
set _event(MappingNotify)     34
set _event(PortalIntercept)   35

set _modifier(Shift)           1
set _modifier(CapsLock)        2
set _modifier(Control)         4
set _modifier(Mod1)            8
set _modifier(Mod2)            16
set _modifier(Mod3)            32
set _modifier(Mod4)            64
set _modifier(Mod5)            128

#
# pad_sel: Convenience function to return current selected objects
#
proc pad_sel {{PAD .pad}} {
    set ids [$PAD find -groupmembers withtag selected]
    return $ids
}

#
# pad_lock_mode: Lock the current mode.  This means that one can use the
#                mode to create an object, and stay in that mode to continue
#                creating objects.  Used in conjunction with pad_try_select
#
proc pad_lock_mode {{btn ""}} {
    global _padlockedmode

    set _padlockedmode 1
    if {$btn != ""} {
	$btn configure -foreground blue
    }
}

#
# pad_unlock_mode: Unlock the current mode.  See pad_lock_mode.
#
proc pad_unlock_mode {} {
    global _padlockedmode

    set _padlockedmode 0
}

#
# pad_try_select: Enters Select mode and selects the item, unless
#                 _padlockedmode is set to 1.
#
proc pad_try_select {PAD id} {
    global _padlockedmode

    if {($_padlockedmode == 0) && ($id != "")} {
	selectMode $PAD Select
	pad_select $PAD $id 
    }
}

#
# Given a list of portals, return the last one.
#
proc eventPortal {{portalList ""}} {
    if {$portalList == ""} {
	set portal ""
    } else {
	set portal [lindex $portalList [expr [llength $portalList] - 1]]
    }
    return $portal
}

#
# change the portals view to the space directly underneath it.
#
proc trackView {PAD portal} {
    set bbox [$PAD bbox $portal]
    set xview [expr 0.5 * ([lindex $bbox 0] + [lindex $bbox 2])]
    set yview [expr 0.5 * ([lindex $bbox 1] + [lindex $bbox 3])]
    set zoom [expr 1.0 / [$PAD scale $portal]]
    if {[$PAD hastag "magnify" $portal]} {
	$PAD moveto $xview $yview "" 0 $portal
    } else {
	$PAD moveto $xview $yview $zoom 0 $portal
    }
}

#
# Define event bindings for all of PadDraw
#
proc bindPadDraw {PAD} {
    global _pad

    set _pad(EventModes) {Run Text Select Pick Draw Text Line MultiLine Mag Rectangle Oval Polygon Portal Select Link LinkCreate}
    foreach mode $_pad(EventModes) {
	$PAD modifier create $mode
    }

    bindPan $PAD
    bindMag $PAD
    bindZoom $PAD
    bindDrag $PAD
    bindMenu $PAD
    bindKeys $PAD
    bindSelect $PAD
    bindPick $PAD
    bindDraw $PAD
    bindHTML $PAD
    bindText $PAD
    bindLine $PAD
    bindMultiLine $PAD
    bindOval $PAD
    bindRectangle $PAD
    bindPolygon $PAD
    bindPortal $PAD
    bindLink $PAD
    bindHandle $PAD
    bindReshaper $PAD
    bindRotate $PAD
    bindDesktop $PAD

    #
    # Object bindings
    #
    bindOutline $PAD
    animInit $PAD
    #
    # Bind accelerators 
    # (Must be last)
    #
    bindAccelerators $PAD
}

#
# Accelerators
#

proc bindAccelerators {PAD} {    
    $PAD bind all <Control-KeyPress-a> {.menubar.edit.m    invoke "Select All"}
    $PAD bind all <Control-KeyPress-b> {.menubar.arrange.m invoke "Send to Back"}
    $PAD bind all <Control-KeyPress-c> {.menubar.edit.m    invoke "Copy"}
    $PAD bind all <Control-KeyPress-d> {.menubar.edit.m    invoke "Delete"}
    $PAD bind all <Control-KeyPress-f> {.menubar.arrange.m invoke "Bring to Front   "}
    $PAD bind all <Control-KeyPress-g> {.menubar.arrange.m invoke "Group"}
    $PAD bind all <Control-KeyPress-l> {.menubar.tools.m   invoke "Colors..."}
    $PAD bind all <Control-KeyPress-o> {.menubar.tools.m   invoke "Drawing..."}
    $PAD bind all <Control-KeyPress-p> {.menubar.object.m  invoke "Properties..."}
    $PAD bind all <Control-KeyPress-q> {.menubar.file.m    invoke "Exit"}
    $PAD bind all <Control-KeyPress-r> {.menubar.object.m  invoke "Reshape"}
    $PAD bind all <Control-KeyPress-s> {.menubar.tools.m   invoke "Search..."}
    $PAD bind all <Control-KeyPress-t> {pad_toggle_full_screen %W}
    $PAD bind all <Control-KeyPress-u> {.menubar.arrange.m invoke "UnGroup"}
    $PAD bind all <Control-KeyPress-v> {.menubar.edit.m    invoke "Paste"}
    $PAD bind all <Control-KeyPress-x> {.menubar.edit.m    invoke "Cut"}

				#
				# Control button click Pans in any mode
				#
    $PAD bind all <KeyPress-Control_L> {
	updateStatusText %P "Pan view" 1
	set _pan(cursor) [lindex [%W config -cursor] 4]
	pad_set_cursor %W "@$_pad(PadBitmaps)/hand.xbm $_pad(PadBitmaps)/handmask.xbm black white" "hand"
	
	event_PanEnter %P
	break
    }
    $PAD bind all <KeyPress-Control_R> {
	updateStatusText %P "Pan view" 1
	set _pan(cursor) [lindex [%W config -cursor] 4]
	
	pad_set_cursor %W "@$_pad(PadBitmaps)/hand.xbm $_pad(PadBitmaps)/handmask.xbm black white"	"hand"
	
	event_PanEnter %W
	break
    }
    $PAD bind all <KeyRelease-Control_L>    {catch {%W config -cursor $_pan(cursor)}; updateStatusText %P "" 0; break}
    $PAD bind all <KeyRelease-Control_R>    {catch {%W config -cursor $_pan(cursor)}; updateStatusText %P "" 0; break}
    $PAD bind all <Control-ButtonPress-1>   {panEvent %P %W "press"   %O %s %x %y %i %j %l; break}
    $PAD bind all <Control-B1-Motion>       {panEvent %P %W "motion"  %O %s %x %y %i %j %l; break}
    $PAD bind all <Control-ButtonRelease-1> {panEvent %P %W "release" %O %s %x %y %i %j %l; break}

				#
				# Alt button click Zooms in any mode
				#
    $PAD bind all <KeyPress-Alt_L>         {
	updateStatusText %P "Magnify view" 1
	set _mag(cursor) [lindex [%P config -cursor] 4]
	pad_set_cursor %W "@$_pad(PadBitmaps)/in.xbm $_pad(PadBitmaps)/inmask.xbm black white" "in"
	
	event_MagIn %P
	break
    }
    $PAD bind all <KeyPress-Alt_R>         {
	updateStatusText %P "Magnify view" 1
	set _mag(cursor) [lindex [%P config -cursor] 4]
	pad_set_cursor %W "@$_pad(PadBitmaps)/in.xbm $_pad(PadBitmaps)/inmask.xbm black white" "in"
	event_MagIn %P
	break
    }
    $PAD bind all <KeyRelease-Alt_L>       {catch {%P config -cursor $_mag(cursor)}; updateStatusText %P "" 0; break}
    $PAD bind all <KeyRelease-Alt_R>       {catch {%P config -cursor $_mag(cursor)}; updateStatusText %P "" 0; break}
    $PAD bind all <Shift-KeyRelease-Alt_L> {catch {%P config -cursor $_mag(cursor)}; updateStatusText %P "" 0; break}
    $PAD bind all <Shift-KeyRelease-Alt_R> {catch {%P config -cursor $_mag(cursor)}; updateStatusText %P "" 0; break}
					# Bug fix because some people have their keys mapped
					# in such a way that the shift release generates a Meta_L instead of Alt_L
    $PAD bind all <Shift-KeyRelease-Meta_L> {catch {%P config -cursor $_mag(cursor)}; updateStatusText %P "" 0; break}
    $PAD bind all <Shift-KeyRelease-Meta_R> {catch {%P config -cursor $_mag(cursor)}; updateStatusText %P "" 0; break}
    $PAD bind all <Alt-ButtonPress-1>      {event_MagPress %P %i %j %x %y %O %s; break}
    $PAD bind all <Alt-B1-Motion>          {event_MagDrag %P %i %j %x %y %s; break}
    $PAD bind all <Alt-ButtonRelease-1>    {event_MagRelease %P %x %y %s; break}
    $PAD bind all <Alt-KeyPress-Shift_R>   {
	updateStatusText %P "Shrink view" 1
	pad_set_cursor %W "@$_pad(PadBitmaps)/out.xbm $_pad(PadBitmaps)/outmask.xbm black white" "out"
	event_MagOut %P
	break
    }
    $PAD bind all <Alt-KeyPress-Shift_L>   {
	updateStatusText %P "Shrink view" 1

	pad_set_cursor %W "@$_pad(PadBitmaps)/out.xbm $_pad(PadBitmaps)/outmask.xbm black white" "out"
	
	event_MagOut %P
	break
    }
    $PAD bind all <Alt-KeyRelease-Shift_R> {
	updateStatusText %P "Magnify view" 1
	pad_set_cursor  %W "@$_pad(PadBitmaps)/in.xbm $_pad(PadBitmaps)/inmask.xbm black white" "in"
	event_MagIn %P
	break
    }
    $PAD bind all <Alt-KeyRelease-Shift_L> {
	updateStatusText %P "Magnify view" 1
	pad_set_cursor %W "@$_pad(PadBitmaps)/in.xbm $_pad(PadBitmaps)/inmask.xbm black white"	"in"
	event_MagIn %P
	break
    }
}

proc bindKeys {PAD} {
    $PAD bind all <KeyPress-Delete> {
	pad_delete %P
    }

    $PAD bind all <KeyPress-Left> {
	set pos [eval %W getview %l]
	set xview [lindex $pos 0]
	set yview [lindex $pos 1]
	set zoom [lindex $pos 2]
	set ids [pad_sel %W]
	if {$ids == ""} {
	    set new_xview [expr $xview - 100 / $zoom]
	    eval %W moveto $new_xview $yview $zoom 0 %l
	} else {
	    set dx [expr -1.0 / $zoom]
	    foreach id $ids {
		%W delete handle$id    ;# Turn off selection handles when moving item with arrows
		%W slide $id $dx 0
	    }
	    pad_select %W $ids
	}
    }

    $PAD bind all <KeyPress-Right> {
	set pos [eval %W getview %l]
	set xview [lindex $pos 0]
	set yview [lindex $pos 1]
	set zoom [lindex $pos 2]
	set ids [pad_sel %W]
	if {$ids == ""} {
	    set new_xview [expr $xview + 100 / $zoom]
	    eval %W moveto $new_xview $yview $zoom 0 %l
	} else {
	    set dx [expr 1.0 / $zoom]
	    foreach id $ids {
		%W delete handle$id    ;# Turn off selection handles when moving item with arrows
		%W slide $id $dx 0
	    }
	    pad_select %W $ids
	}
    }

    $PAD bind all <KeyPress-Up> {
	set pos [eval %W getview %l]
	set xview [lindex $pos 0]
	set yview [lindex $pos 1]
	set zoom [lindex $pos 2]
	set ids [pad_sel %W]
	if {$ids == ""} {
	    set new_yview [expr $yview + 100 / $zoom]
	    eval %W moveto $xview $new_yview $zoom 0 %l
	} else {
	    set dy [expr 1.0 / $zoom]
	    foreach id $ids {
		%W delete handle$id    ;# Turn off selection handles when moving item with arrows
		%W slide $id 0 $dy
	    }
	    pad_select %W $ids
	}
    }

    $PAD bind all <KeyPress-Down> {
	set pos [eval %W getview %l]
	set xview [lindex $pos 0]
	set yview [lindex $pos 1]
	set zoom [lindex $pos 2]
	set ids [pad_sel %W]
	if {$ids == ""} {
	    set new_yview [expr $yview - 100 / $zoom]
	    eval %W moveto $xview $new_yview $zoom 0 %l
	} else {
	    set dy [expr -1.0 / $zoom]
	    foreach id $ids {
		%W delete handle$id    ;# Turn off selection handles when moving item with arrows
		%W slide $id 0 $dy
	    }
	    pad_select %W $ids
	}
    }

			# Page Down zooms out
    $PAD bind all <KeyPress-Next> {
	%W zoom [expr 1.0 - ($_pad(ZoomSpeed) / 200.0)] %i %j
    }

			# Page Up zooms in
    $PAD bind all <KeyPress-Prior> {
	%W zoom [expr 1.0 + ($_pad(ZoomSpeed) / 200.0)] %i %j
    }
}

#
# bindPan
#
# Sets up event bindings for the 'Pan' mode so that the left button
# drags the view on the current surface - this can be either the main
# view, or the surface within a portal.
#
# After the buttonrelease event, the global variable _pan(started) is
# equal to true if there was panning, and false if there was no (or
# very little) panning.
#

proc bindPan {PAD} {
    global $PAD _pan

    $PAD modifier set "Run"

    set _pan(started) 0
    set _pan(origx) 0
    set _pan(origy) 0
    set _pan(i) 0
    set _pan(j) 0

    $PAD bind all <Run-ButtonPress-1>   {panEvent %P %W "press"   %O %s %x %y %i %j %l}
    $PAD bind all <Run-B1-Motion>       {panEvent %P %W "motion"  %O %s %x %y %i %j %l}
    $PAD bind all <Run-ButtonRelease-1> {panEvent %P %W "release" %O %s %x %y %i %j %l}
}

#
# event_PanEnter gets called to set the cursor to Pan mode
#
proc event_PanEnter {PAD} {
    global _pad

    pad_set_cursor $PAD "@$_pad(PadBitmaps)/hand.xbm $_pad(PadBitmaps)/handmask.xbm black white" "hand"   
}

#
# panEvent gets called by event bindings to do the actual panning
#
proc panEvent {PAD Window type obj state x y i j l} {
    global $PAD _pan _pad

#    puts "panEvent: $PAD, $Window, $type, obj=$obj, xy=($x, $y), ij=($i, $j), portals=$l"
    switch -exact $type {
	"press" {
	    set portals "$l"
	    set portal [eventPortal $portals]

	    set trackView 0
	    if {($portal != "") &&
	        ([$PAD hastag "trackView" $portal])} {
	        set trackView 1
	    }
	    set _pan(origx) $x
	    set _pan(origy) $y
	    if {!$trackView} {
		set view [eval $Window getview $l]
		set _pan(i) $i
		set _pan(j) $j
	    } else {
		set view [$Window getview]
		set coords [$Window padxy $x $y]
		set _pan(i) [lindex $coords 0]
		set _pan(j) [lindex $coords 1]
	    }
	    set _pan(started) 0
	}
	"motion" {
	    set x $x
	    set y $y
					# Don't pan until the cursor has moved at least a reasonable # of pixels
	    if {!$_pan(started)} {
		if {([expr abs($_pan(origx) - $x) >= $_pad(Click)] ||
		     [expr abs($_pan(origy) - $y) >= $_pad(Click)])} {
		    set _pan(started) 1
		} else {
		    return
		}
	    }
					# If tracking view, pan at top-level window level
	    set portals "$l"
	    set portal [eventPortal $portals]
	    if {($portal != "") &&
	        ([$PAD hastag "trackView" $portal])} {
		set view [$Window getview]
		set coords [$Window padxy $x $y]
		set i [lindex $coords 0]
		set j [lindex $coords 1]
		set portals ""
	    }

	    set dx [expr ($i - $_pan(i))]
	    set dy [expr ($j - $_pan(j))]
	    set view [eval $Window getview $portals]
	    set xview [lindex $view 0]
	    set yview [lindex $view 1]
#	    puts "ij:($i, $j) _pan:($_pan(i) $_pan(j)) view:($xview, $yview) d:($dx, $dy) [expr $xview - $dx] [expr $yview - $dy]"
	    eval $Window moveto [expr $xview - $dx] [expr $yview - $dy] \"\" 0 $portals
	}
    }
}

#
# bindZoom
#
# Sets up event bindings for the 'Zoom' mode so that the middle and right buttons
# zoom the view on the current surface - this can be either the main
# view, or the surface within a portal.
#

proc bindZoom {PAD} {
    global _pad _zoom
    global env 
    global $PAD

    #
    # Zoom - hold
    #
    set _zoom(started) 0

    set sequence [parseSequence $_pad(ZoomInEvent)]
    set inModifier [lindex $sequence 0]
    set inEvent    [lindex $sequence 1]
    set inDetail   [lindex $sequence 2]

    set sequence [parseSequence $_pad(ZoomOutEvent)]
    set outModifier [lindex $sequence 0]
    set outEvent    [lindex $sequence 1]
    set outDetail   [lindex $sequence 2]

    if {($inEvent == "ButtonPress") || ($inEvent == "Button")} {
        set inButton 1
    } else {
        set inButton 0
    }
    if {($outEvent == "ButtonPress") || ($outEvent == "Button")} {
        set outButton 1
    } else {
        set outButton 0
    }
    if {($inEvent == "KeyPress") || ($inEvent == "Key")} {
        set inKey 1
    } else {
        set inKey 0
    }
    if {($outEvent == "KeyPress") || ($outEvent == "Key")} {
        set outKey 1
    } else {
        set outKey 0
    }

    $PAD bind all $_pad(ZoomInEvent)  "zoomEvent $PAD %P %W press %x %y %i %j %l %O %s in "
    $PAD bind all $_pad(ZoomOutEvent) "zoomEvent $PAD %P %W press %x %y %i %j %l %O %s out"

    if {$inButton} {
	if {$inDetail != ""} {
	    set inButtonModifier "B$inDetail"
	    if {$inModifier != ""} {
		append inButtonModifier "-$inModifier"
	    }
	}
					# Must define zooming event bindings for specific modes,
					# to make sure they get high priority for zooming buttons
	foreach mode $_pad(EventModes) {
	    $PAD bind all <$mode-$inButtonModifier-Motion> "zoomEvent $PAD %P %W motion %x %y %i %j %l %O %s {}"
	}
    }
    if {$outButton} {
	if {$outDetail != ""} {
	    set outButtonModifier "B$outDetail"
	    if {$outModifier != ""} {
		append outButtonModifier "-$outModifier"
	    }
	}
					# Must define zooming event bindings for specific modes,
					# to make sure they get high priority for zooming buttons
	foreach mode $_pad(EventModes) {
	    $PAD bind all <$mode-$outButtonModifier-Motion> "zoomEvent $PAD %P %W motion %x %y %i %j %l %O %s {}"
	}
    }

    if {$inButton} {
	$PAD bind all <ButtonRelease-$inDetail> "zoomEvent $PAD %P %W release %x %y %i %j %l %O %s {}"
    }
    if {$outButton} {
	$PAD bind all <ButtonRelease-$outDetail> "zoomEvent $PAD %P %W release %x %y %i %j %l %O %s {}"
    }
    if {$inKey} {
	$PAD bind all <KeyRelease> "zoomEvent $PAD %P %W release %x %y %i %j %l %O %s {}"
    }
}

proc zoomEvent {BindingPAD PAD Window type x y i j l obj state dir} {
    global _pad _zoom

#    puts -nonewline "zoomEvent: type=$type, BindingPAD=$BindingPAD, PAD=$PAD, Window=$Window ij=($i, $j), portals=$l"

    switch -exact $type {
	"press" {
	    set zoom_in_cursor "@$_pad(PadBitmaps)/in.xbm $_pad(PadBitmaps)/inmask.xbm black white"
	    set zoom_out_cursor "@$_pad(PadBitmaps)/out.xbm $_pad(PadBitmaps)/outmask.xbm black white"
	    set current_cursor [lindex [$PAD config -cursor] 4]
	    if {($current_cursor != $zoom_in_cursor) && ($current_cursor != $zoom_out_cursor)} {
		set _zoom(cursor) $current_cursor
	    }
	    
	    if {$dir == "in"} {
		  set _zoom(dir) 1
		  pad_set_cursor $PAD "@$_pad(PadBitmaps)/in.xbm $_pad(PadBitmaps)/inmask.xbm black white" "in"		
	    } else {
		  pad_set_cursor $PAD "@$_pad(PadBitmaps)/out.xbm $_pad(PadBitmaps)/outmask.xbm black white" "out"
		set _zoom(dir) -1
	    }
	    set _zoom(more) 1
	    set _zoom(clock) [$Window clock]
	    set _zoom(iterations) 1
	    set _zoom(modified) 0
	    set _zoom(started) 1
	    panEvent $PAD $Window "press" $obj $state $x $y $i $j $l
	    holdZoom $Window "press" $Window $obj $state $x $y $i $j $l
	}
	"motion" {
	    holdZoom $Window "motion" $Window $obj $state $x $y $i $j $l
	}
	"release" {
	    if {$_zoom(started)} {
		panEvent $PAD $Window "release" $obj $state $x $y $i $j $l
		catch {$PAD config -cursor $_zoom(cursor)}
		set _zoom(more) 0
		if {$_zoom(modified) == 1} {
		    changesMade $PAD true true
		}
		if {$_zoom(clock) != ""} {
		    $Window clock $_zoom(clock) delete
		    set _zoom(clock) ""
		}
		set _zoom(started) 0
	    }
	}
    }
}

#
# Zoom when button is held down.
#
proc holdZoom {PAD type Window obj state x y i j portals} {
    global _zoom _pad Pad_Error 

# puts "holdZoom: type=$type, xy=($x, $y), ij=($i, $j)"

    if {($_zoom(more) == 0) || ($Pad_Error == 1)} {
	return
    }

    if {$x == ""} {
	set x $_zoom(x)
	set y $_zoom(y)
	set i $_zoom(i)
	set j $_zoom(j)
    } else {
	set _zoom(x) $x
	set _zoom(y) $y
	set _zoom(i) $i
	set _zoom(j) $j
    }

    if {$type == "press"} {
	set _zoom(previ) $i
	set _zoom(prevj) $j
    }
    
    set portal [eventPortal $portals]
    set ids [pad_sel $PAD]

    set actualTime [$PAD clock $_zoom(clock)]
    $PAD clock $_zoom(clock) reset
				# Calculate zooming iterations needed to keep constant
				# perceptual flow - independent of differing scene complexity
				# and render times.
				# Also, use a simple filter to change the rendering rate
				# based on recent history - try to avoid jerkiness.
    set requested_iterations [expr $actualTime.0 / 50]  ;# Tuned for 20 frames per second
    set iterations [expr 0.5 * ($_zoom(iterations) + $requested_iterations)]
    set _zoom(iterations) $iterations
    if {$type == "press"} {set iterations 1}
    if {$iterations > 5} {set iterations 5}
    
    set zoomFactor [expr $_pad(ZoomSpeed) * 0.01]
    set zmult [expr 1 + ($iterations * $zoomFactor * $_zoom(dir))]
    
    if {$ids == ""} {
				# If trackView and not magnify, then zoom top-level
				# window.  Else, zoom within portal
	if {[$PAD hastag "trackView" $portal] && ![$PAD hastag "magnify" $portal]} {
	    $PAD zoom $zmult $i $j 0
	} else {
	    if {$_pad(ZoomStyle) == "FollowCursor"} {
				# Need to turn off fastpanning here because otherwise
				# the call to panEvent will generate a pan which will
				# get rendered before the zoom.  Better to combine them
				# or it will be jerky.
		set fastpan [lindex [$Window config -fastPan] 4]
		$Window config -fastPan 0
		panEvent $PAD $Window "motion" $obj $state $x $y $i $j $portals
		$Window config -fastPan $fastpan
	    }
	    eval $PAD zoom $zmult $i $j 0 $portals
	}

				# This is a bit confusing.  When zooming the view, we need
				# pass in the same coords so that if a motion event caused
				# a pan, the next timer will set the zooming point to what
				# it started out as.  See the comment below for zooming objects.
				#    For the old style zooming (zoom around cursor), we want to
				# always use the latest coords.
	if {$type != "motion"} {
	    if {$_pad(ZoomStyle) == "FollowCursor"} {
		after idle "holdZoom $PAD timer $Window $obj $state $x $y $i $j \"$portals\""
	    } else {
		after idle "holdZoom $PAD timer $Window $obj $state {} {} {} {} \"$portals\""
	    }
	}
    } else {
        if {$_zoom(modified) == 0} {
            pad_prepare_undo $PAD "Scale" [pad_sel $PAD] -place
            set _zoom(modified) 1
        }
	set dx [expr $i - $_zoom(previ)]
	set dy [expr $j - $_zoom(prevj)]
	foreach id $ids {
	    if {$_pad(ZoomStyle) == "FollowCursor"} {
		$PAD slide $id $dx $dy
	    }
	    $PAD scale $id $zmult $i $j
	    if {[$PAD type $id] == "portal"} {
		if {[$PAD hastag "trackView" $id]} {
		    trackView $PAD $id
		}
	    }
	}
	if {$_pad(ZoomStyle) == "FollowCursor"} {
	    $PAD slide modifier $dx $dy
	}
	set _zoom(previ) $i
	set _zoom(prevj) $j

        $PAD scale modifier $zmult $i $j
        $PAD scale handle [expr 1.0 / $zmult]
				# This is a bit confusing.  When zooming objects, we need to
				# continuously update the coordinates we are zooming around.
				# We don't pass in the coords so that the previous motion event
				# coords will get used instead. See the comment above for zooming views.
	if {$type != "motion"} {
	    after idle "holdZoom $PAD timer $Window $obj $state {} {} {} {} \"$portals\""
	}
    }
}

#
# Given a Tcl event sequence, returns a list of
# three elements: modifiers, event, and detail.
#
proc parseSequence {sequence} {
    set len [string length $sequence]
    set ltindex [string last "<" $sequence]
    if {$ltindex != 0} {
	return -code error "Illegal sequence, first character must be \"<\""
    }
    set gtindex [string last ">" $sequence]
    if {$gtindex != [expr $len - 1]} {
	return -code error "Illegal sequence, last character must be \">\""
    }
    set dash1index [string last "-" $sequence]
    if {$dash1index == -1} {
	set dash1index [expr $len - 1]
    }
    set dash2index [string last "-" [string range $sequence 0 [expr $dash1index - 1]]]
    if {$dash2index == -1} {
	set dash2index 0
    }
    set modifier [string range $sequence 1 [expr $dash2index - 1]]
    set event  [string range $sequence [expr $dash2index + 1] [expr $dash1index - 1]]
    set detail [string range $sequence [expr $dash1index + 1] [expr $len - 2]]

    return [list $modifier $event $detail]
}

#
# bindDrag
#
# sets up event bindings for dragging objects that have the word
# drag in their tag field. this happens in the run mode.
#
proc bindDrag {PAD} {
    global _drag
    
    $PAD bind drag <Run-ButtonPress-1> {event_DragPress %P %i %j %x %y %O %s;break}
    $PAD bind drag <Run-B1-Motion> {event_DragDrag %P %i %j %x %y %s;break}
    $PAD bind drag <Run-ButtonRelease-1> {event_DragRelease  %P %x %y %s;break}
  
    $PAD bind drag <Run-Enter> {
	pad_set_cursor %P "@$_pad(PadBitmaps)/drag.xbm $_pad(PadBitmaps)/dragmask.xbm black white" "drag"
    }
    
    $PAD bind drag <Run-Leave> {
	pad_set_cursor %P "@$_pad(PadBitmaps)/hand.xbm $_pad(PadBitmaps)/handmask.xbm black white" "hand"
    }
}


proc event_DragPress {PAD i j x y obj state} {
    global _drag _pad

					# Find appropriate group containing this object.
					# and drag that instead.  If n=_pad($obj.upselect)
					# exists, then drag n groups up from the object.
					# If n = -1, then drag the topmost object.
    
    if {[info exists _pad($obj.upselect)]} {
	    set upselect $_pad($obj.upselect)
    } else {
	    set upselect 0
    }
    set group [$PAD getgroup $obj]
    
    while {($group != "") && (($upselect < 0) || ($upselect > 0))} {
	    set obj $group
	    set group [$PAD getgroup $obj]
	    incr upselect -1
    }
  
    set _drag(x) $x
    set _drag(y) $y
    set _drag(padx) $i
    set _drag(pady) $j
    set _drag(origpadx) $i
    set _drag(origpady) $j
    set _drag(mode) "point"
    set _drag(id) ""
    
    
    # Don't drag object if it is bigger than current view
	
    set bbox [$PAD bbox 1]
    set objbbox [$PAD bbox $obj]
    if {([lindex $objbbox 0] < [lindex $bbox 0]) &&
	([lindex $objbbox 1] < [lindex $bbox 1]) &&
	([lindex $objbbox 2] > [lindex $bbox 2]) &&
	([lindex $objbbox 3] > [lindex $bbox 3])} {
	set $obj 1
    }
    if {$obj != 1} {
        set _drag(id) $obj
    }
}

proc event_DragDrag {PAD i j x y state} {
    global _drag

    set id $_drag(id)
    
    if {($id != "")} {
	set zoom [$PAD getzoom]
	
	set dx [expr $i - $_drag(padx)]
	set dy [expr $j - $_drag(pady)]
	
	$PAD slide $id $dx $dy
	if {([$PAD type $id] == "portal") && [$PAD hastag "trackView" $id]} {
	    trackView $PAD $id
	}
	$PAD slide modifier $dx $dy
	set _drag(padx) $i
	set _drag(pady) $j
    }
}

proc event_DragRelease {PAD x y state} {
    global _drag
    
    changesMade $PAD true true
}

#
# bindSelect sets up Select mode event bindings to select objects
# with the left mouse button (shift adds new objects).
#
proc bindSelect {PAD} {
    $PAD bind item <Select-Enter> {
	event_SelectEnter %P %O "%l"
    }
    $PAD bind item <Select-Leave> {
	event_SelectLeave %P %O "%l"
    }

    $PAD bind all <Select-ButtonPress-1> {
	event_SelectPress %P %i %j %x %y %O %s "%l"
    }
    $PAD bind all <Select-B1-Motion> {
	event_SelectDrag %P %i %j %x %y %s
    }
    $PAD bind all <Select-ButtonRelease-1> {
	event_SelectRelease %P %x %y %s "%l"
    }
}

#
# Bind enter and leave over items
# to change cursor
#
proc event_SelectEnter {PAD obj portals} {
	 global _pad

				# If small items should not be selected,
				# then don't indicate them
    if {$_pad(SelectSmallItems) == 0} {
	set size [eval $PAD getsize $obj $portals]
	if {$size < $_pad(SmallObject)} {
	    return
	}
    }
					
				# Similarly, don't indicate transparent items
				# if requested not to.
    if {$_pad(SelectTransparentItems) == 0} {
	set transparency [$PAD ic $obj -transparency]
	if {$transparency == 0} {
	    return
	}
    }

    if {[$PAD itemconfigure $obj -lock] == 0} {
	  $PAD config -cursor [pad_select_cursor $PAD]	
    }
}

proc event_SelectLeave {PAD obj portals} {
	 global _pad

				# If small items should not be selected,
				# then don't indicate them
    if {$_pad(SelectSmallItems) == 0} {
	set size [eval $PAD getsize $obj $portals]
	if {$size < $_pad(SmallObject)} {
	    return
	}
    }
    
				# Similarly, don't indicate transparent items
				# if requested not to.
    if {$_pad(SelectTransparentItems) == 0} {
	set transparency [$PAD ic $obj -transparency]
	if {$transparency == 0} {
	    return
	}
    }

	$PAD config -cursor [pad_pointer_cursor $PAD]
   
}

#
# Find appropriate group containing this object.
# and select that instead.  If n=_pad($obj.upselect)
# exists, then select n groups up from the object.
# If n = -1, then select the topmost object.
#
proc event_GetGroupSelection {PAD obj} {
    global _pad

    if {$_pad(SelectGroupMembers) == 0} {
	set group [$PAD getgroup $obj]
	if {[info exists _pad($obj.upselect)]} {
	    set upselect $_pad($obj.upselect)
	} else {
	    set upselect -1
	}
	while {($group != "") && (($upselect < 0) || ($upselect > 0))} {
	    set obj $group
	    set group [$PAD getgroup $obj]
	    incr upselect -1
	}
    }

    return $obj
}

#
# Return true if item is not too large to be selected
#
proc event_ItemNotTooLarge {PAD item} {
    global _pad

    set notTooLarge 1
    if {!$_pad(SelectLargeItems)} {
	set viewbbox [$PAD bbox 1]
	set itembbox [$PAD bbox $item]
	if {([lindex $itembbox 0] < [lindex $viewbbox 0]) &&
	    ([lindex $itembbox 1] < [lindex $viewbbox 1]) &&
	    ([lindex $itembbox 2] > [lindex $viewbbox 2]) &&
	    ([lindex $itembbox 3] > [lindex $viewbbox 3])} {
	    set notTooLarge 0
	}
    }

    return $notTooLarge
}

proc event_SelectPress {PAD i j x y obj state portals} {
    global _select _modifier _handle _pad

#    puts "Select Press: P=$PAD, obj=$obj, ij=($i, $j), xy=($x, $y)"
    set _select(started) 0

				# If transparent items should be selected,
				# then make sure we find them.  Tricky because
				# objects that are faded out don't get events
				# while objects that have transparency=0 do.
    if {$_pad(SelectTransparentItems) == 1} {
	set objs [$PAD find -groupmembers overlapping $i $j $i $j&& !withtype group && !withtag modifier]
	if {$objs == ""} {
	    set obj 1
	} else {
	    set obj [lindex $objs end]
	    set obj [event_GetGroupSelection $PAD $obj]
	    while {![event_ItemNotTooLarge $PAD $obj]} {
		set len [llength $objs]
		set objs [lrange $objs 0 [expr $len - 2]]
		set obj [lindex $objs end]
		set obj [event_GetGroupSelection $PAD $obj]
		if {$obj == ""} {
		    set obj 1
		}
	    }
	}
    } else {
	set transparency [$PAD ic $obj -transparency]
	if {$transparency == 0} {
	    set objs [$PAD find -groupmembers overlapping $i $j $i $j&& !withtype group && !withtag modifier]
	    set done 0
	    while {!$done} {
		set obj [lindex $objs end]
		if {($obj == "") || ($obj == 1)} {
		    set done 1
		    set obj 1
		} else {
		    set transparency [$PAD ic $obj -transparency]
		    if {$transparency > 0} {
			set done 1
		    } else {
			set objs [lrange $objs 0 [expr [llength $objs] - 2]]
		    }
		}
	    }
	}
    }
				# If small items should not be selected,
				# then don't select them
    if {$_pad(SelectSmallItems) == 0} {
	set size [eval $PAD getsize $obj $portals]
	if {$size < $_pad(SmallObject)} {
	    set obj 1
	}
    }
				# Find appropriate group containing this object.
    set obj [event_GetGroupSelection $PAD $obj]

    pad_StopReshaping $PAD
    pad_StopRotating $PAD

    if {[$PAD hastag "selected" $obj]} {
	set _select(selected) 1    ;# Current object selected
    } else {
	set _select(selected) 0    ;# Current object not selected
    }

    set _select(x) $x
    set _select(y) $y
    set _select(padi) $i
    set _select(padj) $j
    set _select(origpadi) $i
    set _select(origpadj) $j
    set _select(mode) "point"
    set _select(id) ""
    if {!($state & $_modifier(Shift)) && $_select(selected)} {    ;# Shift key not pressed and current obj selected
        # Do nothing
    } else {
	if {$_select(selected)} {     ;# Shift key pressed and current obj selected
            pad_unselect $PAD $obj
            set _select(selected) 0
        } else {                   ;# Current obj not selected
	    if {!($state & $_modifier(Shift))} {   ;# shift key not pressed
	        pad_unselect $PAD all
            }
				# Don't select object if it is bigger than current view (unless asked for)
	    set select [event_ItemNotTooLarge $PAD $obj]
	    if {$select} {
		pad_select $PAD $obj
		if {$obj != 1} {
		    set _select(selected) 1
		}
	    }
	}
    }
				# Don't want selection handles or selected objects
				# to be identified as possible draganddrop destinations
    $PAD ic selected -events 0
    $PAD ic modifier -events 0
    pad_mark_draganddrop_container $PAD $x $y $state

				# Autoraise objects if requested
    if {[info exists _pad($obj.autoraise)] && $_pad($obj.autoraise)} {
	$PAD raise $obj
    }
}

proc event_SelectDrag {PAD i j x y state} {
    global _select

    set zoom [$PAD getzoom]

    if {$_select(selected)} {
	if {$_select(started) == 0} {
	    pad_prepare_undo $PAD "Move" [pad_sel $PAD] -place
	    set _select(started) 1
	}
	set dx [expr $i - $_select(padi)]
	set dy [expr $j - $_select(padj)]
	foreach id [pad_sel $PAD] {
	    $PAD slide $id $dx $dy
	    if {([$PAD type $id] == "portal") &&
	    [$PAD hastag "trackView" $id]} {
		trackView $PAD $id
	    }
	}
	$PAD slide modifier $dx $dy
	set _select(padi) $i
	set _select(padj) $j
    } else {
	if {$_select(mode) == "point"} {
	    if {[expr (abs($_select(x) - $x)) >= 5] ||
	        [expr (abs($_select(y) - $y)) >= 5]} {
		    set _select(mode) "rect"
		    set _select(id) [$PAD create rectangle $_select(origpadi) $_select(origpadj) $i $j \
			    -minsize 0 -alwaysrender 1 -transparency 0.5 \
			    -penwidth [expr 1.0 / (90 * $zoom)] -pen "black"]
	    }
	} else {
	    set coords [$PAD coords $_select(id)]
	    set newcoords "[lrange $coords 0 1] $i $j"
	    eval $PAD coords $_select(id) $newcoords
	}
    }

    if {$_select(id) == ""} {
	pad_mark_draganddrop_container $PAD $x $y $state
    }
}

proc event_SelectRelease {PAD x y state portals} {
    global _pad _select _modifier

    #    puts "Select Release: P=$PAD, id=$_select(id)"
    set selected_items [$PAD find -groupmembers withtag selected]
    if {$_select(id) == ""} {
				# Drag and drop
	set container [$PAD pick -indivisible $x $y]

	foreach tag [$PAD ic $container -tags] {
	    if {[info command "pad_draganddrop_$tag"] != ""} {
		foreach item [pad_sel $PAD] {
		    pad_draganddrop_$tag $PAD $container $item
		}
	    }
	}
    } else {
				# Select objects in marquee
	set bbox [$PAD bbox $_select(id)]
	$PAD delete $_select(id)
	set _select(id) ""
	if {$_pad(SelectGroupMembers)} {
	    set ids [eval $PAD find -groupmembers enclosed $bbox && !withtype "group"]
	} else {
	    set ids [eval $PAD find enclosed $bbox]
	}
	if {$ids != ""} {
				# If small or transparent items should not be selected,
				# then go through items one by one to check them
	    if {($_pad(SelectSmallItems) == 0) || ($_pad(SelectTransparentItems) == 0)} {
		foreach id $ids {
		    set select 1
		    set size [eval $PAD getsize $id $portals]
		    if {$_pad(SelectSmallItems) == 0} {
			if {$size < $_pad(SmallObject)} {
			    set select 0
			}
		    }
		    if {$_pad(SelectTransparentItems) == 0} {
			if {[eval $PAD isvisible $id $portals] == 0} {
			    set select 0
			}
		    }
		    if {$select} {
			pad_select $PAD $id
		    }
		}
	    } else {
		eval pad_select $PAD $ids
	    }
	}
    }

				# Item might get unselected by draganddrop methods,
				# so operate on original set.
    foreach item $selected_items {
	$PAD ic $item -events 1
    }
    $PAD ic modifier -events 1
    $PAD ic bbox -events 0	;# Don't ever want dotted rectangles selecting objects to get events

    if {$_select(container) != ""} {
				# Unhilite container (even though it already may be done)
	pad_unhilite $PAD $_select(container)
	set _select(container) ""
    }
    changesMade $PAD true true
}

#
# Set properties to match specified object
# (size, font, width, color, roughness, cap and join styles, and roughness)
#
proc setDefaults {PAD id} {
    global _pad

    set type [$PAD type $id]
    set zoom [$PAD getzoom]
				# Set line width and font size
    switch -exact $type {
	rectangle -
	polygon -
	portal -
        spline -
	line {
	    set scale [$PAD ic $id -scale]
	    if {$type == "portal"} {
		set width [$PAD ic $id -borderwidth]
	    } else {
		set width [$PAD ic $id -penwidth]
		set _pad(JoinStyle) [$PAD ic $id -joinstyle]
	    }
	    if {($type == "line")} {
		set arrow_shape [$PAD ic $id -arrowshape]
		set _pad(ArrowA) [expr [lindex $arrow_shape 0] * $zoom]
		set _pad(ArrowB) [expr [lindex $arrow_shape 1] * $zoom]
		set _pad(ArrowC) [expr [lindex $arrow_shape 2] * $zoom]
		set _pad(LineArrow) [$PAD ic $id -arrow]
		set _pad(Roughness) [$PAD ic $id -roughness]
		set _pad(Undulate) [$PAD ic $id -undulate]
		set _pad(CapStyle) [$PAD ic $id -capstyle]
		setLineArrowShape "$_pad(ArrowA) $_pad(ArrowB) $_pad(ArrowC)"
		setLineArrowWhere $_pad(LineArrow)
		setLineWidth [expr $width * $zoom]
	    }
	    setDrawingWidth $PAD [expr $width * $zoom * $scale]
	}
        textarea -
        textfield -
	textfile -
	text {
	    set _pad(Font) [$PAD ic $id -font]
	    set size [$PAD ic $id -z]
	    set group [$PAD getgroup $id]
	    while {$group != ""} {
		set size [expr $size * [$PAD ic $group -z]]
		set group [$PAD getgroup $group]
	    }
	    set _pad(TextSize) [expr $zoom * $size]
	}
    }
}

#
# bindPick sets up Pick mode event bindings to pick the color of the
# object under the mouse with the left button.
#
proc bindPick {PAD} {
    $PAD bind all <Pick-ButtonPress-1> {
	set type [%W type %O]

				# Pick pen color
	switch -exact $type {
	    line -
	    spline - 
	    text -
	    textfile -
	    rectangle -
	    oval -
	    polygon {
		setcolor %W [%W ic %O -pen] pen
	    }
	    portal {
		setcolor %W [%W ic %O -border] pen
	    }
	}
				# Pick fill color
	switch -exact $type {
	    rectangle -
	    oval -
	    polygon   -
	    portal    {
		setcolor %W [%W ic %O -fill] fill
	    }
	    pad       {
				# Get color pad surface (possibly through portals)
		set portals %l
		if {[eval llength $portals] == 0} {
		    setcolor %W [lindex [%W config -bg] 4] fill
		} else {
		    set len [llength $portals]
		    if {$len > 1} {
			set surface [lindex $portals [expr $len - 2]]
		    } else {
			set surface %W
		    }
		    set portal [lindex $portals [expr $len - 1]]
		    setcolor %W [$surface ic $portal -fill] fill
		    setcolor %W [$surface ic $portal -border] pen
		}
	    }
	}
    }
}

#
# bindDraw sets up Draw mode event bindings to make a freehand drawing
# with the left mouse button
#
proc bindDraw {PAD} {
    global _pad

    setDrawingWidth $PAD $_pad(PenWidth)

    $PAD bind all <Draw-ButtonPress-1> {
	event_DrawPress %P %i %j %l
    }
    $PAD bind all <Draw-B1-Motion> {
	event_DrawDrag %P %i %j
    }
    $PAD bind all <Draw-ButtonRelease-1> {
	event_DrawRelease %P
    }
}

proc event_DrawPress {PAD i j portals} {
    global _draw _layer _pad
    
    set zoom [eval $PAD getzoom $portals]
    set _draw(id) [$PAD create line $i $j -penwidth [expr $_pad(PenWidth)/$zoom] \
	    -capstyle $_pad(CapStyle) -joinstyle $_pad(JoinStyle) \
	    -pen $_pad(PenColor) -tags "item" -layer $_layer(current)]
    if {$_pad(LineArrow) != "none"} {
	$PAD ic $_draw(id) -arrow $_pad(LineArrow) \
	    -arrowshape "[expr $_pad(ArrowA)/$zoom] [expr $_pad(ArrowB)/$zoom] [expr $_pad(ArrowC)/$zoom]"
    }
    if {$_pad(Roughness) != 0} {
	$PAD ic $_draw(id) -roughness $_pad(Roughness)
    }
    if {$_pad(Undulate)} {
	$PAD ic $_draw(id) -undulate 1
    }
}

proc event_DrawDrag {PAD i j} {
    global _draw

    $PAD coords -append -nooutput $_draw(id) $i $j
}

proc event_DrawRelease {PAD} {
    global _draw

    set _draw(oldid) $_draw(id)
    changesMade $PAD true
    pad_try_select $PAD $_draw(id)
    set _draw(id) ""
}

proc event_DrawCancel {PAD} {
    global _draw

    catch {$PAD delete $_draw(oldid)}
}	

#
# bindText sets up Text mode event bindings to make a new text object by typing
# or clicking with the left mouse button.  Clicking on an existing text object
# allows editing of that text object instead of creating a new one.
#
# The interactions available are summarized here:
#
#                       // Insert key
#      letter:     Insert letter at point
#      Ctrl-y:     Copy from kill buffer to point
#                       // Delete keys
#      Backspace:  Delete char before point
#      Delete:     Delete char before point
#      Ctrl-d:     Delete char at point
#      Ctrl-k:     Delete from point to end-of-line (into kill buffer)
#      Alt-d:      Delete next word (into kill buffer)
#      Ctrl-space: Set mark		      
#      Alt-w:      Copy from mark to point into kill buffer
#      Ctrl-w:     Delete from mark to point (into kill buffer)
#                       // Cursor movement
#      Ctrl-a:     Point to start of line
#      Ctrl-e:     Point to end of line
#      Ctrl-f:     Point forward character
#      Ctrl-b:     Point backward character
#      Ctrl-n:     Point forward line
#      Ctrl-p:     Point backward line
#      Alt-f:      Point forward word
#      Alt-b:      Point backward word
#      Alt-v:      Point forward 10 lines
#      Alt-b:      Point backward 10 lines
#

proc bindText {PAD} {
    global _text tcl_platform

    set _text(buffer) ""	  ;# Kill buffer
    set _text(appendBuffer) 0     ;# True if should append to kill buffer
    set _text(markedTextID) ""    ;# Current text object that has mark


				# This makes sure that text-related things don't
				# get written out to disk.
    $PAD bind bufferText <Write> {set Pad_Write 0; break}
    $PAD bind textbgnd <Write> {set Pad_Write 0; break}

				# Set focus (unix only.  on windows it auto-raises)
	if {$tcl_platform(platform) == "unix"} {
        bind $PAD <Enter> {
	        focus %W
	            if {[%W focus] == ""} {
	            %W focus 1
	        }
        }
	}

				# Display cursor when pointer is moved
    $PAD bind all <Text-Motion> {
	textCursorOn %W
    }

				# Put cursor on text object,
				# or create a new one.
    $PAD bind all <Text-ButtonPress-1> {
	if {%s & $_modifier(Shift)} {		;# Eval when shift pressed
	    set type [%P type %O]
	    if {$type == "text"} {
	        eval [%P ic %O -text]
	    }
	    return
        }

	set CurrentObject %O
	set CurrentType [%W type $CurrentObject]
				# Don't allow editing of textfiles imported by reference
	if {$CurrentType == "textfile"} {
	    if {[%W ic %O -writeformat] == "reference"} {
		set CurrentType ""
	    }
	}
	if {($CurrentType != "text") &&
	    ($CurrentType != "textfile")} {
	    set coords [%W padxy %x [expr %y - $_pad(TextSize)] %l]
	    set xloc [lindex $coords 0]
	    set yloc [lindex $coords 1]
	    set zoom [eval %W getzoom %l]
	    set size [expr double($_pad(TextSize)) / $zoom]

	    set CurrentObject [%W create text -place "$xloc $yloc $size" \
		-anchor nw -pen $_pad(PenColor) -tags "item" -font $_pad(Font) -layer $_layer(current)]
	    %W addtag text $CurrentObject
	    changesMade %W true
	}
	setDefaults %W $CurrentObject
	textSetFocus %W $CurrentObject
	%W text $CurrentObject mark set point @%i,%j
	if {$_text(markedTextID) != $CurrentObject} {
	    unset-mark %W
	    %W text $CurrentObject mark set mark point
	}
	set _text(appendBuffer) 0
    }
  				# Create new text object
    $PAD bind all <Text-KeyPress> {
	if {"%A" != ""} {
	    set type [%W type [%W focus]]
	    set text ""
	    if {$type == "pad"} {
		set coords [%W padxy %x [expr %y - $_pad(TextSize)] %l]
		set xloc [lindex $coords 0]
		set yloc [lindex $coords 1]
		set zoom [eval %W getzoom %l]
		set size [expr double($_pad(TextSize)) / $zoom]
		
		set text [%W create text -place "$xloc $yloc $size" -anchor nw \
		          -pen $_pad(PenColor) -tags "item" -font $_pad(Font) -layer $_layer(current)]
		%W addtag text $text
		textSetFocus %W $text
	    }
	    if {($type == "text") || ($type == "textfile")} {
		set text [%W focus]
	    }
	    if {$text != ""} {
		%W text $text insert point %A
		textUpdateView %W
		%W text $text mark set mark point
	    }
	    
	    changesMade %W true
	    set _text(appendBuffer) 0
	    textCursorOff %W
	}
    }
#
# Misc. keys
#
				# Key entry
    $PAD bind text <Text-KeyPress> {
	if {("%A" != "") && ("%A" != "{}")} {
	    set textid [%W focus]
	    set orig_mark [%W text $textid index mark]
	    %W text $textid insert point "%A"
	    textUpdateMark %W $textid $orig_mark
	    textUpdateView %W
	    set _text(appendBuffer) 0
	    textCursorOff %W
	}
	break
    }

				# Switch to buffer
    $PAD bind text <Text-Control-KeyPress-x><KeyPress-b> {
	textGetData %W "switch-to-buffer %W"
	textCursorOff %W
	break
    }
				# Load file
    $PAD bind text <Text-Control-KeyPress-x><Control-KeyPress-f> {
	textGetData %W "find-file %W"
	textCursorOff %W
	break
    }
				# Exchange point and mark
    $PAD bind text <Text-Control-KeyPress-x><Control-KeyPress-x> {
	exchange-point-and-mark %W
	textCursorOff %W
	break
    }
				# Set mark
    $PAD bind text <Text-Control-KeyPress-space> {
	set-mark %W
	textCursorOff %W
	break
    }
				# Wipe from mark to point into buffer
    $PAD bind text <Text-Alt-KeyPress-w> {
	kill-ring-save %W
	textCursorOff %W
	break
    }
				# Copy from buffer to point
    $PAD bind text <Text-Control-KeyPress-y> {
	yank %W
	textUpdateView %W
	textCursorOff %W
	break
    }
#
# Delete keys
#
				# Delete key
    $PAD bind text <Text-KeyPress-Delete> {
	delete-backward-char %W
	textUpdateView %W
	textCursorOff %W
	break
    }
				# Backspace key
    $PAD bind text <Text-KeyPress-BackSpace> {
	delete-backward-char %W
	textUpdateView %W
	textCursorOff %W
	break
    }
				# Delete char at cursor
    $PAD bind text <Text-Control-KeyPress-d> {
	delete-char %W
	textUpdateView %W
	textCursorOff %W
	break
    }
				# Delete until end of line
    $PAD bind text <Text-Control-KeyPress-k> {
	kill-line %W
	textUpdateView %W
	textCursorOff %W
	break
    }
				# Delete next word
    $PAD bind text <Text-Alt-KeyPress-d> {
	kill-word %W
	textUpdateView %W
	textCursorOff %W
	break
    }
				# Delete from mark to point
    $PAD bind text <Text-Control-KeyPress-w> {
	kill-region %W
	textUpdateView %W
	textCursorOff %W
	break
    }
#
# Cursor motion keys
#
				# Cursor to start of line
    $PAD bind text <Text-Control-KeyPress-a> {
	%W text [%W focus] mark set point "point linestart"
	textUpdateView %W
	set _text(appendBuffer) 0
	textCursorOff %W
	break
    }
				# Cursor to end of line
    $PAD bind text <Text-Control-KeyPress-e> {
	%W text [%W focus] mark set point "point lineend"
	textUpdateView %W
	set _text(appendBuffer) 0
	textCursorOff %W
	break
    }
				# Cursor forward character
    $PAD bind text <Text-Control-KeyPress-f> {
	%W text [%W focus] mark set point "point + 1 char"
	textUpdateView %W
	set _text(appendBuffer) 0
	textCursorOff %W
	break
    }
				# Cursor backward character
    $PAD bind text <Text-Control-KeyPress-b> {
	%W text [%W focus] mark set point "point - 1 char"
	textUpdateView %W
	set _text(appendBuffer) 0
	textCursorOff %W
	break
    }
				# Cursor next line
    $PAD bind text <Text-Control-KeyPress-n> {
	%W text [%W focus] mark set point "point + 1 line"
	textUpdateView %W
	set _text(appendBuffer) 0
	textCursorOff %W
	break
    }
				# Cursor previous line
    $PAD bind text <Text-Control-KeyPress-p> {
	%W text [%W focus] mark set point "point - 1 line"
	textUpdateView %W
	set _text(appendBuffer) 0
	textCursorOff %W
	break
    }
				# Cursor 10 lines down
    $PAD bind text <Text-Control-KeyPress-v> {
	%W text [%W focus] mark set point "point + 10 line"
	textUpdateView %W
	set _text(appendBuffer) 0
	textCursorOff %W
	break
    }
				# Cursor 10 lines up
    $PAD bind text <Text-Alt-KeyPress-v> {
	%W text [%W focus] mark set point "point - 10 line"
	textUpdateView %W
	set _text(appendBuffer) 0
	textCursorOff %W
	break
    }
				# Cursor next word
    $PAD bind text <Text-Alt-KeyPress-f> {
	%W text [%W focus] mark set point "point + 1 char wordend"
	textUpdateView %W
	set _text(appendBuffer) 0
	textCursorOff %W
	break
    }
				# Cursor previous word
    $PAD bind text <Text-Alt-KeyPress-b> {
	%W text [%W focus] mark set point "point - 1 char wordstart"
	textUpdateView %W
	set _text(appendBuffer) 0
	textCursorOff %W
	break
    }
}

#
# bindLine sets up Line mode event bindings to draw single line segments
# with the left mouse button
#
proc bindLine {PAD} {
    $PAD bind all <Line-ButtonPress-1>    {event_LinePress %P %i %j %l}
    $PAD bind all <Line-B1-Motion>       {event_LineDrag %P %i %j %s}
    $PAD bind all <Line-ButtonRelease-1> {event_LineRelease %P}
}

proc event_LinePress {PAD i j portals} {
    global _line _layer _pad

    set zoom [eval $PAD getzoom $portals]
    set _line(id) [$PAD create line $i $j $i $j -penwidth [expr $_pad(PenWidth)/$zoom] \
            -capstyle $_pad(CapStyle) -joinstyle $_pad(JoinStyle) \
	    -pen $_pad(PenColor) -tags "item" -layer $_layer(current)]
    if {$_pad(LineArrow) != "none"} {
	$PAD ic $_line(id) -arrow $_pad(LineArrow) \
	    -arrowshape "[expr $_pad(ArrowA)/$zoom] [expr $_pad(ArrowB)/$zoom] [expr $_pad(ArrowC)/$zoom]"
    }
    if {$_pad(Roughness) != 0} {
	$PAD ic $_line(id) -roughness $_pad(Roughness)
    }
    if {$_pad(Undulate)} {
	$PAD ic $_line(id) -undulate 1
    }
}

proc event_LineDrag {PAD i j state} {
    global _line _modifier

    if {$_line(id) != ""} {
	set coords [$PAD coords $_line(id)]
	set oldcoords [lrange $coords 0 1]
	if {$state & $_modifier(Shift)} {
				# Shift key pressed - draw line at 90 degree intervals
	    set oldi [lindex $oldcoords 0]
	    set oldj [lindex $oldcoords 1]
	    set adi [expr abs($i - $oldi)]
	    set adj [expr abs($j - $oldj)]
	    if {$adi > $adj} {
		set j $oldj
	    } else {
		set i $oldi
	    }
	}
	set newcoords "$oldcoords $i $j"
	eval $PAD coords $_line(id) $newcoords
    }
}

proc event_LineRelease {PAD} {
    changesMade $PAD true
    global _line
    pad_try_select $PAD $_line(id)
}
	
#
# bindMultiLine sets up MultiLine mode event bindings to draw multi-segment
# straight lines.
#
proc bindMultiLine {PAD} {
    global _multiline

    set _multiline(id) ""

    $PAD bind all <MultiLine-ButtonPress-1>   {event_MultiLinePress %P %i %j %l}
    $PAD bind all <MultiLine-B1-Motion>       {event_MultiLineDrag %P %i %j %s}
    $PAD bind all <MultiLine-ButtonRelease-1> {event_MultiLineRelease %P}
    $PAD bind all <MultiLine-Double-ButtonRelease-1> {event_MultiLineDoubleRelease %P}
}

proc event_MultiLinePress {PAD i j portals} {
    global _multiline _pad _layer

    set x $i
    set y $j
    if {$_multiline(id) == ""} {
	set zoom [eval $PAD getzoom $portals]
	set _multiline(id) [$PAD create line $x $y $x $y \
				-penwidth [expr $_pad(PenWidth)/ $zoom] \
				-capstyle $_pad(CapStyle) \
				-joinstyle $_pad(JoinStyle) \
				-pen $_pad(PenColor) -tags "item" -layer $_layer(current)]
	if {$_pad(LineArrow) != "none"} {
	    $PAD ic $_multiline(id) -arrow $_pad(LineArrow)  \
		-arrowshape "[expr $_pad(ArrowA)/$zoom] [expr $_pad(ArrowB)/$zoom] [expr $_pad(ArrowC)/$zoom]"
	}
	if {$_pad(Roughness) != 0} {
	    $PAD ic $_multiline(id) -roughness $_pad(Roughness)
	}
	if {$_pad(Undulate)} {
	    $PAD ic $_multiline(id) -undulate 1
	}
	set _multiline(pts) 2
    } else {
	set coords [$PAD coords $_multiline(id)]
	eval $PAD coords $_multiline(id) "$coords $x $y"
	incr _multiline(pts)
    }
}

proc event_MultiLineDrag {PAD i j state} {
    global _multiline _modifier

    if {$_multiline(id) != ""} {
	set coords [$PAD coords $_multiline(id)]

	if {$state & $_modifier(Shift)} {
				# Shift key pressed - draw line at 90 degree intervals
	    set len [llength $coords]
	    set oldi [lindex $coords [expr $len - 4]]
	    set oldj [lindex $coords [expr $len - 3]]
	    set adi [expr abs($i - $oldi)]
	    set adj [expr abs($j - $oldj)]
	    if {$adi > $adj} {
		set j $oldj
	    } else {
		set i $oldi
	    }
	}

	set newcoords "[lrange $coords 0 [expr 2*$_multiline(pts) - 3]] $i $j"
	eval $PAD coords $_multiline(id) $newcoords
    }
}

proc event_MultiLineRelease {PAD} {
    changesMade $PAD true
}

proc event_MultiLineDoubleRelease {PAD} {
    global _multiline

    set id $_multiline(id)
    multilineEvent $PAD 1
    pad_try_select $PAD $id
    set _multiline(id) ""
}
	
proc multilineEvent {PAD {subtract ""}} {
    global _multiline

    if {$_multiline(id) != ""} {
	if {$subtract != ""} {
	    set coords [$PAD coords $_multiline(id)]
	    set coords [lrange $coords 0 [expr [llength $coords] - 1 - (2 * $subtract)]]
	    eval $PAD coords $_multiline(id) $coords
	}
	set _multiline(id) ""
    }
}
#
# bindMag sets up Magnification mode even bindings to zoom in or out with
# the left mouse button (shift key zooms out) or to draw a zoom bounding box
#
proc bindMag {PAD} {
    $PAD bind all <Mag-ButtonPress-1>      {event_MagPress %P %i %j %x %y %O %s}
    $PAD bind all <Mag-B1-Motion>          {event_MagDrag %P %i %j %x %y %s}
    $PAD bind all <Mag-ButtonRelease-1>    {event_MagRelease %P %x %y %s}
    $PAD bind all <Mag-KeyPress-Shift_R>   {event_MagOut %P}
    $PAD bind all <Mag-KeyPress-Shift_L>   {event_MagOut %P}
    $PAD bind all <Mag-KeyRelease-Shift_R> {event_MagIn %P}
    $PAD bind all <Mag-KeyRelease-Shift_L> {event_MagIn %P}
}

proc event_MagIn {PAD} {
    global _pad
 
	pad_set_cursor $PAD "@$_pad(PadBitmaps)/mag_in_glass.xbm black" "mag_in_glass" 
    
}

proc event_MagOut {PAD} {
    global _pad
   
	pad_set_cursor $PAD "@$_pad(PadBitmaps)/mag_out_glass.xbm black" "mag_out_glass"
    
}

proc event_MagPress {PAD i j x y obj state} {
    global _mag

    set _mag(x) $x
    set _mag(y) $y
    set _mag(padi) $i
    set _mag(padj) $j
    set _mag(origpadi) $i
    set _mag(origpadj) $j
    set _mag(mode) "point"
    set _mag(started) 1
}

proc event_MagDrag {PAD i j x y state} {
    global _mag
 
					# Only proceed if MagPress and MagDrag have been called
    if {![info exists _mag(started)]} {
	return
    }

    if {$_mag(mode) == "point"} {
        if {[expr (abs($_mag(x) - $x)) >= 5] ||
	    [expr (abs($_mag(y) - $y)) >= 5]} {
            set _mag(mode) "rect"    
            set _mag(id) [$PAD create rectangle $_mag(origpadi) $_mag(origpadj) $i $j \
			      -minsize 0 -alwaysrender 1 -transparency 0.5 -penwidth 0.0 -pen "blue"]
        }
    } else {
        set coords [$PAD coords $_mag(id)]
        set newcoords "[lrange $coords 0 1] $i $j"
        eval $PAD coords $_mag(id) $newcoords
    }
}

proc event_MagRelease {PAD x y state} {
    global _mag _modifier

					# Only proceed if MagPress and MagDrag have been called
    if {![info exists _mag(started)]} {
	return
    }
    unset _mag(started)

    set anim 1000	

    set view [$PAD getview]
    set x [lindex $view 0]
    set y [lindex $view 1]
    set z [lindex $view 2]
    
    if {$_mag(mode) == "point"} {
    	if {$state & $_modifier(Shift)} {
					# Shift is held, zoom out
	    $PAD moveto $_mag(origpadi) $_mag(origpadj) [expr .25 * $z] $anim 
        } else {
	    $PAD moveto $_mag(origpadi) $_mag(origpadj) [expr 4 * $z] $anim 
	}
    } elseif {$_mag(mode) == "rect"} {
					# Zoom out so current view fits into box drawn 
					# This uses ratios of the boxes to find a new moveto point
	if {$state & $_modifier(Shift)} {

            set redbox [eval "$PAD create rectangle [$PAD bbox 1] -pen red -transparency 0.5 -penwidth 0"]

	    set drawn_bbox [$PAD bbox $_mag(id)]
            set drawn_xmin [lindex $drawn_bbox 0]
            set drawn_ymin [lindex $drawn_bbox 1]
            set drawn_xmax [lindex $drawn_bbox 2]
            set drawn_ymax [lindex $drawn_bbox 3]

            set curr_bbox [$PAD bbox 1]
            set curr_xmin [lindex $curr_bbox 0]
            set curr_ymin [lindex $curr_bbox 1]
            set curr_xmax [lindex $curr_bbox 2]
            set curr_ymax [lindex $curr_bbox 3]

            set xdim [expr $drawn_xmax - $drawn_xmin] 
            set ydim [expr $drawn_ymax - $drawn_ymin] 

	    if {($xdim == 0) || ($ydim == 0)} {
		$PAD delete $redbox
		$PAD delete $_mag(id)
		unset _mag(id)
		return
	    }	
	    
	    if {$xdim < $ydim} { 
                set ratio [expr $xdim / ($curr_xmax - $curr_xmin)]
	    } else {
                set ratio [expr $ydim / ($curr_ymax - $curr_ymin)]
	    }
	    set nz [expr $ratio * $z]
	    
	    $PAD ic $_mag(id) -sticky 1

            set drawn_xcenter [expr (($drawn_xmax + $drawn_xmin) / 2.0)]
            set drawn_ycenter [expr (($drawn_ymax + $drawn_ymin) / 2.0)]
	    set nx [expr $x - (($drawn_xcenter - $x) / $ratio)]
	    set ny [expr $y - (($drawn_ycenter - $y) / $ratio)]
	    $PAD moveto $nx $ny $nz 1000
	    $PAD delete $redbox
        } else {
	    eval "$PAD centerbbox [$PAD bbox $_mag(id)] $anim 0.5 0.5 1"
        }
        $PAD delete $_mag(id)
	unset _mag(id)
    }
}




#
# bindRectangle sets up Rectangle mode event bindings to draw an outline or
# filled rectangle with the left mouse button
#
proc bindRectangle {PAD} {
    global _rect

    set _rect(id) ""
    set _rect(fill) 1

    $PAD bind all <Rectangle-ButtonPress-1>   {event_RectPress %P %i %j %l}
    $PAD bind all <Rectangle-B1-Motion>       {event_RectDrag %P %i %j %s}
    $PAD bind all <Rectangle-ButtonRelease-1> {event_RectRelease %P}
}

proc event_RectPress {PAD i j portals} {
    global _rect _layer _pad

    set zoom [eval $PAD getzoom $portals]
    set _rect(id) [$PAD create rectangle $i $j $i $j \
            -penwidth [expr $_pad(PenWidth) / $zoom] -pen $_pad(PenColor) -tags "item" \
	    -joinstyle miter -layer $_layer(current)]
    if {$_rect(fill) == 1} {
	$PAD ic $_rect(id) -fill $_pad(FillColor)
    }
}

proc event_RectDrag {PAD i j state} {
    global _rect _modifier

    if {$_rect(id) != ""} {
	set coords [$PAD coords $_rect(id)]
				# If Shift key not pressed draw recangle
	set x0 [lindex $coords 0]
	set y0 [lindex $coords 1]
	if {!($state & $_modifier(Shift))} {
	    set newx1 $i
	    set newy1 $j
	} else {
				# Shift key pressed - draw square
	    set dx [expr $i - $x0]
	    set dy [expr $j - $y0]
	    set signdx [sign $dx]
	    set signdy [sign $dy]
	    if {abs($dx) > abs($dy)} {
		set newx1 [expr $x0 + abs($dx) * $signdx]
		set newy1 [expr $y0 + abs($dx) * $signdy]
	    } else {
		set newx1 [expr $x0 + abs($dy) * $signdx]
		set newy1 [expr $y0 + abs($dy) * $signdy]
	    }
	}
	$PAD coords $_rect(id) $x0 $y0 $newx1 $newy1
    }
}

proc event_RectRelease {PAD} {
    changesMade $PAD true
    global _rect
    pad_try_select $PAD $_rect(id)
}

proc event_RectCancel {PAD} {
    global _rect

    catch {$PAD delete $_rect(id)}
}	


#
# bindOval sets up Oval mode event bindings to draw an outline or
# filled oval with the left mouse button
#
proc bindOval {PAD} {
    global _oval

    set _oval(id) ""
    set _oval(fill) 1

    $PAD bind all <Oval-ButtonPress-1>   {event_OvalPress %P %i %j %l}
    $PAD bind all <Oval-B1-Motion>       {event_OvalDrag %P %i %j %s}
    $PAD bind all <Oval-ButtonRelease-1> {event_OvalRelease %P}
}

proc event_OvalPress {PAD i j portals} {
    global _oval _layer _pad

    set zoom [eval $PAD getzoom $portals]
    set _oval(id) [$PAD create oval $i $j $i $j \
            -penwidth [expr $_pad(PenWidth) / $zoom] -pen $_pad(PenColor) -tags "item" \
	    -joinstyle miter -layer $_layer(current)]
    if {$_oval(fill) == 1} {
	$PAD ic $_oval(id) -fill $_pad(FillColor)
    }
}

proc event_OvalDrag {PAD i j state} {
    global _oval _modifier

    if {$_oval(id) != ""} {
	set coords [$PAD coords $_oval(id)]
				# If Shift key not pressed draw recangle
	set x0 [lindex $coords 0]
	set y0 [lindex $coords 1]
	if {!($state & $_modifier(Shift))} {
	    set newx1 $i
	    set newy1 $j
	} else {
				# Shift key pressed - draw square
	    set dx [expr $i - $x0]
	    set dy [expr $j - $y0]
	    set signdx [sign $dx]
	    set signdy [sign $dy]
	    if {abs($dx) > abs($dy)} {
		set newx1 [expr $x0 + abs($dx) * $signdx]
		set newy1 [expr $y0 + abs($dx) * $signdy]
	    } else {
		set newx1 [expr $x0 + abs($dy) * $signdx]
		set newy1 [expr $y0 + abs($dy) * $signdy]
	    }
	}
	$PAD coords $_oval(id) $x0 $y0 $newx1 $newy1
    }
}

proc event_OvalRelease {PAD} {
    changesMade $PAD true
    global _oval
    pad_try_select $PAD $_oval(id)
}

proc event_OvalCancel {PAD} {
    global _oval

    catch {$PAD delete $_oval(id)}
}	


#
# bindPolygon sets up Polygon mode event bindings to draw an outline or
# filled polygon with the left mouse button
#
proc bindPolygon {PAD} {
    global _poly

    set _poly(id) ""

    $PAD bind all <Polygon-ButtonPress-1> {
	set x %i
	set y %j
	if {$_poly(id) == ""} {
	    set zoom [eval %W getzoom %l]
	    set _poly(id) [%P create polygon $x $y $x $y -penwidth [expr $_pad(PenWidth) / $zoom] \
		    -pen $_pad(PenColor) -tags "item" -layer $_layer(current)]
	    if {$_poly(fill)} {
		%P ic $_poly(id) -fill $_pad(FillColor)
	    }
	    set _poly(pts) 2
	} else {
	    set coords [%P coords $_poly(id)]
	    eval %P coords $_poly(id) "$coords $x $y"
	    incr _poly(pts)
	}
    }
    $PAD bind all <Polygon-Motion> {
	if {$_poly(id) != ""} {
	    set x %i
	    set y %j
	    set coords [%P coords $_poly(id)]
	    set newcoords "[lrange $coords 0 [expr 2*$_poly(pts) - 3]] $x $y"
	    eval %P coords $_poly(id) $newcoords
	}
    }
    $PAD bind all <Polygon-ButtonRelease-1>      {changesMade %W true}
    $PAD bind all <Polygon-Double-ButtonRelease-1> {
          set id [polyEvent %P 2]
          pad_try_select %P $id
    }
}

proc polyEvent {PAD {subtract ""}} {
    global _pad _poly

    set id ""
    if {$_poly(id) != ""} {
	set id $_poly(id)
	set coords [$PAD coords $_poly(id)]
        if {$subtract != ""} {
            set coords [lrange $coords 0 [expr [llength $coords] - 1 - (2 * $subtract)]]
        }
	eval $PAD coords $_poly(id) $coords
	set _poly(id) ""
    }
    return $id
}

#
# bindPortal sets up Portal mode event bindings to
# drag out a portal with the left mouse button.
#
proc bindPortal {PAD} {

    $PAD bind all <Portal-ButtonPress-1> {
	set x %i
	set y %j
	set zoom [eval %W getzoom %l]
	set width [expr 5.0 / $zoom]
	set _portal(id) [%P create portal $x $y $x $y -fill $_pad(FillColor) \
		-borderwidth $width -tags "item drag" -font $_pad(Font) -title "Portal" \
                -layer $_layer(current) -visiblelayers "all - status"]
	set _portal(x1) $x
	set _portal(y1) $y
    }
    $PAD bind all <Portal-B1-Motion> {
	if {$_portal(id) != ""} {
	    set x %i
	    set y %j
	    %P coords -nooutput $_portal(id) $_portal(x1) $_portal(y1) $x $y
	}
    }
    $PAD bind all <Portal-B1-ButtonRelease-1> {
	pad_try_select %P $_portal(id)
	set _portal(id) ""
    }
}

#
# Make 8 handles around the bounding box of the specified object.
# Also, create a dashed rectangle around them.
#
proc makeBBoxHandles {PAD item} {
    global _pad

    if {$_pad(Selection) == "hide"} {
	return
    }

    set bbox [eval $PAD bbox $item]
    set x0 [lindex $bbox 0]
    set y0 [lindex $bbox 1]
    set x1 [lindex $bbox 2]
    set y1 [lindex $bbox 3]
    $PAD create rectangle $x0 $y0 $x1 $y1 -pen red \
        -penwidth [expr .01/[$PAD getzoom]] -events 0 -layer "status" \
        -transparency 0.5 -tags "handle$item bbox$item bbox modifier" -alwaysrender 1
    set x2 [expr ($x0 + $x1)/2.0]
    set y2 [expr ($y0 + $y1)/2.0]

    if {$_pad(Selection) == "show"} {
	makeHandle $PAD $x0 $y0 "handle handle$item modifier h_num${item}_0" "status"
	makeHandle $PAD $x0 $y2 "handle handle$item modifier h_num${item}_1" "status"
	makeHandle $PAD $x0 $y1 "handle handle$item modifier h_num${item}_2" "status"
	makeHandle $PAD $x2 $y1 "handle handle$item modifier h_num${item}_3" "status"
	makeHandle $PAD $x1 $y1 "handle handle$item modifier h_num${item}_4" "status"
	makeHandle $PAD $x1 $y2 "handle handle$item modifier h_num${item}_5" "status"
	makeHandle $PAD $x1 $y0 "handle handle$item modifier h_num${item}_6" "status"
	makeHandle $PAD $x2 $y0 "handle handle$item modifier h_num${item}_7" "status"
    }
}

#
# Convenience function to create a handle
#
proc makeHandle {PAD x y tags layer} {
    set zoom [$PAD getzoom]
    set s [expr 1.0 / $zoom]
    set item [$PAD create rectangle 0 0 6 6 -place "$x $y $s" -sticky z -pen red -fill black -alwaysrender 1 \
		  -tags $tags -layer $layer]
    return $item
}

#
# Define event handlers for bbox handles
#
proc bindHandle {PAD} {

    $PAD bind handle <Select-Enter> {
        %P ic %O -fill lightblue -pen black
	updateStatusText %P {Press Shift to maintain aspect ratio} 1
    }

    $PAD bind handle <Select-Leave> {
        %P ic %O -fill black -pen red
	updateStatusText %P {} 0
    }

    $PAD bind handle <Select-B1-Enter> {
        # Don't change color if button down
    }

    $PAD bind handle <Select-B1-Leave> {
        # Don't change color if button down
    }

    $PAD bind handle <Select-ButtonPress-1> {
        %P ic %O -pen red
	set tags [%P gettags %O]
	set _handle(handle) %O
	regexp {handle([0-9]+)} $tags dummy _handle(object)
	regexp "h_num$_handle(object)_(\[0-9\]+)" $tags dummy _handle(handleNum)
	set _handle(anchor) [%P ic $_handle(object) -anchor]
	set bbox [%P bbox $_handle(object)]
	switch $_handle(handleNum) {
	    0 {set anchor ne; set pt [bbne $bbox]; set _handle(i) [lindex $pt 0]; set _handle(j) [lindex $pt 1]}
	    1 {set anchor e;  set pt [bbe  $bbox]; set _handle(i) [lindex $pt 0]; set _handle(j) [lindex $pt 1]}
	    2 {set anchor se; set pt [bbse $bbox]; set _handle(i) [lindex $pt 0]; set _handle(j) [lindex $pt 1]}
	    3 {set anchor s;  set pt [bbs  $bbox]; set _handle(i) [lindex $pt 0]; set _handle(j) [lindex $pt 1]}
	    4 {set anchor sw; set pt [bbsw $bbox]; set _handle(i) [lindex $pt 0]; set _handle(j) [lindex $pt 1]}
	    5 {set anchor w;  set pt [bbw  $bbox]; set _handle(i) [lindex $pt 0]; set _handle(j) [lindex $pt 1]}
	    6 {set anchor nw; set pt [bbnw $bbox]; set _handle(i) [lindex $pt 0]; set _handle(j) [lindex $pt 1]}
	    7 {set anchor n;  set pt [bbn  $bbox]; set _handle(i) [lindex $pt 0]; set _handle(j) [lindex $pt 1]}
	}
	pad_set_anchor %P $_handle(object) $anchor
	set _handle(aspectRatio) ""
	break
    }

    $PAD bind handle <Select-B1-Motion> {
        resize %P %i %j %s
	break
    }

    $PAD bind handle <Select-B1-ButtonRelease-1> {
        %P ic $_handle(handle) -pen black
	pad_set_anchor %P $_handle(object) $_handle(anchor)
	break
    }
}

#
# Utility function for resize
# Used to switch active handle
#
proc switchHandle {PAD newHandleNum} {
    global _handle

    set newHandle [$PAD find withtag "h_num$_handle(object)_$newHandleNum"]
    $PAD ic $newHandle -fill [$PAD ic $_handle(handle) -fill] -pen [$PAD ic $_handle(handle) -pen]
    $PAD ic $_handle(handle) -fill black -pen red
    set _handle(handle) $newHandle
    set _handle(handleNum) $newHandleNum
}

#
# Resize based on handle motion
#
proc resize {PAD i j state} {
    global _handle _modifier

					# Determine new dimensions of object
    set width [$PAD ic $_handle(object) -width]
    set height [$PAD ic $_handle(object) -height]
    switch $_handle(handleNum) {
	0 {
	    set width [expr $_handle(i) - $i]
	    set height [expr $_handle(j) - $j]
	}
	1 {
	    set width [expr $_handle(i) - $i]
	}
	2 {
	    set width [expr $_handle(i) - $i]
	    set height [expr $j - $_handle(j)]
	}
	3 {
	    set height [expr $j - $_handle(j)]
	}
	4 {
	    set width [expr $i - $_handle(i)]
	    set height [expr $j - $_handle(j)]
	}
	5 {
	    set width [expr $i - $_handle(i)]
	}
	6 {
	    set width [expr $i - $_handle(i)]
	    set height [expr $_handle(j) - $j]
	}
	7 {
	    set height [expr $_handle(j) - $j]
	}
    }
					# Flip if necessary
    if {$width < 0} {
	switch $_handle(handleNum) {
	    0 {switchHandle $PAD 6; pad_set_anchor $PAD $_handle(object) nw}
	    1 {switchHandle $PAD 5; pad_set_anchor $PAD $_handle(object) w}
	    2 {switchHandle $PAD 4; pad_set_anchor $PAD $_handle(object) sw}
	    4 {switchHandle $PAD 2; pad_set_anchor $PAD $_handle(object) se}
	    5 {switchHandle $PAD 1; pad_set_anchor $PAD $_handle(object) e}
	    6 {switchHandle $PAD 0; pad_set_anchor $PAD $_handle(object) ne}
	}
	resize $PAD $i $j $state
	return
    }
    if {$height < 0} {
	switch $_handle(handleNum) {
	    0 {switchHandle $PAD 2; pad_set_anchor $PAD $_handle(object) se}
	    2 {switchHandle $PAD 0; pad_set_anchor $PAD $_handle(object) ne}
	    3 {switchHandle $PAD 7; pad_set_anchor $PAD $_handle(object) n}
	    4 {switchHandle $PAD 6; pad_set_anchor $PAD $_handle(object) nw}
	    6 {switchHandle $PAD 4; pad_set_anchor $PAD $_handle(object) sw}
	    7 {switchHandle $PAD 3; pad_set_anchor $PAD $_handle(object) s}
	}
	resize $PAD $i $j $state
	return
    }

					# Don't shrink too thin, or we'll lose resolution
    if {$width < [expr 5.0 / [$PAD getzoom]]} {
	set width [expr 5.0 / [$PAD getzoom]]
    }
    if {$height < [expr 5.0 / [$PAD getzoom]]} {
	set height [expr 5.0 / [$PAD getzoom]]
    }

					# If shift key down, maintain aspect ratio
    if {$state & $_modifier(Shift)} {
	if {$_handle(aspectRatio) == ""} {
					# First time, define aspect ratio
	    set _handle(aspectRatio) [expr 1.0 * $width / $height]
	    switch $_handle(handleNum) {
		0 -
		2 -
		4 -
		6 {
		    if {$width < $height} {
			set _handle(aspectDim) "width"
		    } else {
			set _handle(aspectDim) "height"
		    }
		}
		1 -
		5 {
		    set _handle(aspectDim) "height"
		}
		3 -
		7 {
		    set _handle(aspectDim) "width"
		}
	    }
	} else {
	    if {$_handle(aspectDim) == "width"} {
		set width [expr $height * $_handle(aspectRatio)]
	    } else {
		set height [expr $width / $_handle(aspectRatio)]
	    }
	}
    } else {
	set _handle(aspectRatio) ""
    }
					# Set new dimensions of object
					# Set anchorpt to fix rounding problems
    $PAD ic $_handle(object) -width $width -height $height -anchorpt "$_handle(i) $_handle(j)"

					# Update handle positions
    set bbox [$PAD bbox $_handle(object)]
    $PAD ic "h_num$_handle(object)_0" -anchorpt "[bbsw $bbox]"
    $PAD ic "h_num$_handle(object)_1" -anchorpt "[bbw  $bbox]"
    $PAD ic "h_num$_handle(object)_2" -anchorpt "[bbnw $bbox]"
    $PAD ic "h_num$_handle(object)_3" -anchorpt "[bbn  $bbox]"
    $PAD ic "h_num$_handle(object)_4" -anchorpt "[bbne $bbox]"
    $PAD ic "h_num$_handle(object)_5" -anchorpt "[bbe  $bbox]"
    $PAD ic "h_num$_handle(object)_6" -anchorpt "[bbse $bbox]"
    $PAD ic "h_num$_handle(object)_7" -anchorpt "[bbs  $bbox]"
    eval $PAD coords "bbox$_handle(object)" $bbox
}

#
# bindReshaper
#
proc bindReshaper {PAD} {
    global _handle

    $PAD bind reshaper <Select-Enter> {
        %P ic %O -fill lightblue -pen black
    }

    $PAD bind reshaper <Select-Leave> {
        %P ic %O -fill black -pen red
    }

    $PAD bind reshaper <Select-ButtonPress-1> {
        %P ic %O -fill red
        set _handle(x) %i
        set _handle(y) %j
	break
    }

    $PAD bind reshaper <Select-B1-Motion> {
        %P slide %O [expr %i - $_handle(x)] [expr %j - $_handle(y)]
	set orig_coords [%P coords $_handle(%O.obj)]
	set new_coords [lreplace $orig_coords $_handle(%O.index) \
			    [expr $_handle(%O.index)+1] %i %j]
        eval %P coords $_handle(%O.obj) $new_coords
        set _handle(x) %i
        set _handle(y) %j
	break
    }

    $PAD bind reshaper <Select-B1-ButtonRelease-1> {
        %P ic %O -fill lightblue
	break
    }

    $PAD bind reshaper <Select-ButtonPress-2> {
        set indx $_handle(%O.index)
        set obj $_handle(%O.obj)
	if {[%P type $obj] == "spline"} {
	    return
	}
        %P delete %O
        eval %P coords $obj [lreplace [%P coords $obj] $indx [expr $indx+1]]
        for {set i $indx} {$i < $_handle($obj.ncoords)-2} {incr i 2} {
            set nextId $_handle($obj.[expr $i + 2].id)
            set _handle($nextId.index) $i
            set _handle($obj.$i.id) $nextId
        }
        incr _handle($obj.ncoords) -2
        unset _handle(%O.obj)
        unset _handle(%O.index)
        unset _handle($obj.$i.id)
	break
    }

    $PAD bind item <Select-ButtonPress-2> {
	set _handle(reshaping) 0
	set type [%P type %O]
				# Check to see if currently reshaping
        if {![info exists _handle(%O.ncoords)]} {
	    return
	}
	if {($type == "rectangle") || ($type == "spline")} {
				# Can't add points to rectangles or splines
	    return
	}
	set _handle(reshaping) 1
        set coords [%P coords %O]
        set xcoord 1
        set indx 0
	if {$type == "portal"} {
	    set wdth [expr [%P ic %O -borderwidth] / 2.0]
	} else {
	    set wdth [expr [%P ic %O -penwidth] / 2.0]
	}
        foreach coord $coords {
            if {$xcoord} {
                set x $coord
                set xcoord 0
            } else {
                set y $coord
                if {$indx > 0} {
                    if {((%i >= $xprev-$wdth && %i <= $x+$wdth) \
                     |   (%i <= $xprev+$wdth && %i >= $x-$wdth)) \
                     && ((%j >= $yprev-$wdth && %j <= $y+$wdth) \
                     |   (%j <= $yprev+$wdth && %j >= $y-$wdth))} {
                        break
                    }
                }
                set xprev $x
                set yprev $y
                set xcoord 1
                incr indx 2
            }
        }
        set id [makeHandle %P %i %j "reshaper handle%O modifier" $_layer(current)]
        eval %P coords %O [linsert $coords $indx %i %j]
        for {set i $_handle(%O.ncoords)} {$i > $indx} {incr i -2} {
            set prevId $_handle(%O.[expr $i - 2].id)
            set _handle($prevId.index) $i
            set _handle(%O.$i.id) $prevId
        }
        set _handle($id.obj) %O
        set _handle($id.index) $indx
        set _handle(%O.$indx.id) $id
        incr _handle(%O.ncoords) 2
	break
    }

    $PAD bind item <Select-B2-Motion> {
	if {$_handle(reshaping)} {
	    break
	}
    }

    $PAD bind item <Select-B2-ButtonRelease> {
	if {$_handle(reshaping)} {
	    break
	}
    }
}

#
# makeResizers
#
proc makeResizeHandles {PAD} {
    global _pad

    set prevSelection $_pad(Selection)
    set _pad(Selection) "show"
    $PAD delete modifier
    foreach item [pad_sel $PAD] {
	makeBBoxHandles $PAD $item
    }
    set _pad(Selection) $prevSelection
}

#
# makeReshapers
#
proc makeReshaperHandles {PAD} {
    global _handle _select _layer

    set zoom [$PAD getzoom]
    set hwidth [expr 2.5 / $zoom]

				# Delete all modifier handles
    $PAD delete modifier
    set items [pad_sel $PAD]
    foreach item $items {
				# Can't reshape things that have a fixed
				# aspect ratio
	if {[pad_fixed_aspect_ratio $PAD $item]} {
	    pad_unselect $PAD $item
	    continue
	}
        set type [$PAD type $item]
        set index 0
        set x_coord 1
        foreach coord [$PAD coords $item] {
            if {$x_coord} {
                set x $coord
                set x_coord 0
            } else {
                set y $coord
		set id [makeHandle $PAD $x $y "reshaper modifier handle$item" $_layer(current)]
                set _handle($id.obj) $item
                set _handle($id.index) $index
                set _handle($item.$index.id) $id
                incr index 2
                set x_coord 1
            }
        }
        set _handle($item.ncoords) $index
    }
}

#
# Stop reshaping any objects being reshaped
#
proc pad_StopReshaping {PAD} {
    global _handle

    foreach reshaper [$PAD find withtag reshaper] {
	set tags [$PAD gettags $reshaper]
	if {[regexp {handle([0-9]+)} $tags m item]} {
	    $PAD deletetag selected $item
	}
	$PAD delete $reshaper
	set reshape_obj $_handle($reshaper.obj)
	set index $_handle($reshaper.index)
	unset _handle($reshaper.obj)
	unset _handle($reshaper.index)
	unset _handle($reshape_obj.$index.id)
	catch {unset _handle($reshape_obj.ncoords)}
    }
}
#
# Stop rotating any objects being rotated
#
proc pad_StopRotating {PAD} {
    foreach handle [$PAD find withtag rotater] {
	set tags [$PAD gettags $handle]
	if {[regexp {handle([0-9]+)} $tags m item]} {
	    $PAD deletetag selected $item
	}
	$PAD delete $handle
    }
}

#
# flipVertical
#
proc flipVertical {PAD} {
    global _select

    set items [pad_sel $PAD]
    pad_StopReshaping $PAD
    pad_StopRotating $PAD
    pad_unselect $PAD all
    eval pad_select $PAD $items
    foreach item $items {
				# Can't flip things that have a fixed
				# aspect ratio
	if {[pad_fixed_aspect_ratio $PAD $item]} {
	    continue
	}
	set bbox [$PAD coords bbox$item]
	set ysum [expr [lindex $bbox 1] + [lindex $bbox 3]]

	set pts ""
	set x_coord 1
	foreach coord [$PAD coords $item] {
	    if {$x_coord} {
		append pts " $coord"
		set x_coord 0
	    } else {
		append pts " [expr $ysum - $coord]"
		set x_coord 1
	    }
	}
	eval $PAD coords $item $pts
    }
}

#
# flipHorizontal
#
proc flipHorizontal {PAD} {
    global _select

    set items [pad_sel $PAD]
    pad_StopReshaping $PAD
    pad_StopRotating $PAD
    pad_unselect $PAD all
    eval pad_select $PAD $items

    foreach item $items {
				# Can't flip things that have a fixed
				# aspect ratio
	if {[pad_fixed_aspect_ratio $PAD $item]} {
	    continue
	}
	set bbox [$PAD coords bbox$item]
	set xsum [expr [lindex $bbox 0] + [lindex $bbox 2]]
	set pts ""
	set x_coord 1
	foreach coord [$PAD coords $item] {
	    if {$x_coord} {
		append pts " [expr $xsum - $coord]"
		set x_coord 0
	    } else {
		append pts " $coord"
		set x_coord 1
	    }
	}
	eval $PAD coords $item $pts
    }
}

#
#
#
proc makeRotateHandles {PAD} {
    global _select _layer

    set objs [pad_sel $PAD]
    foreach object $objs {
				# Can't rotate things that have a fixed
				# aspect ratio
	if {![pad_is_rotatable $PAD $object]} {
	    pad_unselect $PAD $object
	    continue
	}
				# Convert rectangular portals to polygon portals
	if {[$PAD type $object] == "portal"} {
	    set coords [$PAD coords $object]
	    if {[llength $coords] == 4} {
				# Rectangle coordinates - convert to polygon
		set xmin [lindex $coords 0]
		set ymin [lindex $coords 1]
		set xmax [lindex $coords 2]
		set ymax [lindex $coords 3]
		set coords "$xmin $ymin $xmax $ymin $xmax $ymax $xmin $ymax"
		eval $PAD coords $object $coords
	    }
	}
    }
				# Find bbox of selected objects
    set objs [pad_sel $PAD]
    set obj [lindex $objs 0]
    if {$objs != ""} {
	set bbox [eval $PAD bbox $objs]
				# Delete all modifier handles
	$PAD delete modifier
				# Make rotater handles
	set ctr [bbcenter $bbox]
	set xctr [lindex $ctr 0]
	set yctr [lindex $ctr 1]
	makeHandle $PAD $xctr $yctr "rotater handle$obj h_num0 modifier" $_layer(current)
	makeHandle $PAD [lindex $bbox 2] [lindex $bbox 3] "rotater handle$obj h_num1 modifier" \
	    $_layer(current)
    }
}

#
# Apply the specified object transformation to the point (x, y).
#
proc applyTransform {place x y} {
    set x [expr ($x * [lindex $place 2]) + [lindex $place 0]]
    set y [expr ($y * [lindex $place 2]) + [lindex $place 1]]
    return "$x $y"
}

#
# (x0, y0, x1, y1) specifies a rectangle.
# reposition so that it is anchored at 0, 0 with 
# the specified anchor type.
#
proc applyAnchor {anchor x0 y0 x1 y1} {
    set width [expr $x1 - $x0]
    set height [expr $y1 - $y0]
				# First, position object so the nw corner of its
				# bounding box is at the anchor point.  This is
				# necessary because the anchor point could be any place
				# relative to the bounding box.
    set dx 0
    set dy [expr -$height]
    switch $anchor {
	"nw" {
	}
	"n" {
	    set dx [expr $dx - (0.5 * $width)]
	}
	"ne" {
	    set dx [expr $dx - $width]
	}
	"w" {
	    set dy [expr $dy + (0.5 * $height)]
	}
	"center" {
	    set dx [expr $dx - (0.5 * $width)]
	    set dy [expr $dy + (0.5 * $height)]
	}
	"e" {
	    set dx [expr $dx - $width]
	    set dy [expr $dy + (0.5 * $height)]
	}
	"sw" {				
	    set dy [expr $dy + $height]
	}
	"s" {
	    set dx [expr $dx - (0.5 * $width)]
	    set dy [expr $dy + $height]
	}
	"se" {
	    set dx [expr $dx - $width]
	    set dy [expr $dy + $height]
	}
    }
    set x0 [expr $x0 + $dx]
    set y0 [expr $y0 + $dy]
    set x1 [expr $x1 + $dx]
    set y1 [expr $y1 + $dy]

    return "$x0 $y0 $x1 $y1"
}

#
# Rotate the point (x, y) by angle degrees about the point (ctrx, ctry)
#
proc applyRotation {angle ctrx ctry x y} {
    set radians [expr -$angle * 3.14159 / 180.0]
    set dx [expr $x - $ctrx]
    set dy [expr $y - $ctry]
    set x [expr $ctrx + $dx * cos($radians) + $dy * sin($radians)]
    set y [expr $ctry - $dx * sin($radians) + $dy * cos($radians)]

    return "$x $y"
}

#
# bindRotate
#
proc bindRotate {PAD} {
    global _handle

    $PAD bind rotater <Select-Enter> {
        %P ic %O -fill lightblue -pen black
    }

    $PAD bind rotater <Select-Leave> {
        %P ic %O -fill black -pen red
    }

    $PAD bind rotater <Select-ButtonPress-1> {
        %P ic %O -fill red
	set _handle(origx) %i
	set _handle(origy) %j
        set _handle(x) %i
        set _handle(y) %j
	set _handle(images) ""
	set _handle(texts) ""
	foreach obj [pad_sel %P] {
	    set type [%P type $obj]
				# Rotate text about a center anchor
	    if {$type == "text"} {
		lappend _handle(texts) $obj
		set bbox [%P bbox $obj]
		set width [expr [lindex $bbox 2] - [lindex $bbox 0]]
		set height [expr [lindex $bbox 3] - [lindex $bbox 1]]
		%P ic $obj -anchor center
		%P slide $obj [expr 0.5 * $width] [expr -0.5 * $height]
	    }
				# Rotate image shadows, not actual image - they are too slow
	    if {$type == "image"} {
				# Convert rectangle to equivalent polygon so it can be rotated
		set image $obj
		set imageitem [%P ic $image -image]
		set dim [%P image configure $imageitem -dimensions]
		set width [lindex $dim 0]
		set height [lindex $dim 1]
		set place [%P ic $image -place]
		set x0 0
		set y0 0
		set x1 $width
		set y1 $height
				# First apply the transformation to the dimensions of the image
		set pt [applyTransform $place $x0 $y0]
		set x0 [lindex $pt 0]
		set y0 [lindex $pt 1]
		set pt [applyTransform $place $x1 $y1]
		set x1 [lindex $pt 0]
		set y1 [lindex $pt 1]
				# Then anchor the resulting rectangle based on the images anchor
                set anchor [%P ic $image -anchor]
		set pts [applyAnchor $anchor $x0 $y0 $x1 $y1]
		set ax0 [lindex $pts 0]
		set ay0 [lindex $pts 1]
		set ax1 [lindex $pts 2]
		set ay1 [lindex $pts 3]
				# Then rotate the result by the amount the image is rotated
		set angle [%P ic $image -angle]
		set pt [applyRotation $angle [lindex $place 0] [lindex $place 1] $ax0 $ay0]
		set x0 [lindex $pt 0]
		set y0 [lindex $pt 1]
		set pt [applyRotation $angle [lindex $place 0] [lindex $place 1] $ax1 $ay0]
		set x1 [lindex $pt 0]
		set y1 [lindex $pt 1]
		set pt [applyRotation $angle [lindex $place 0] [lindex $place 1] $ax1 $ay1]
		set x2 [lindex $pt 0]
		set y2 [lindex $pt 1]
		set pt [applyRotation $angle [lindex $place 0] [lindex $place 1] $ax0 $ay1]
		set x3 [lindex $pt 0]
		set y3 [lindex $pt 1]
				# Finally, create the polygon we will see.
		set rect [eval %P create polygon $x0 $y0 $x1 $y1 $x2 $y2 $x3 $y3 \
			      -fill \#808080 -pen none -transparency 0.5]
		%P addtag selected $rect
		%P addtag image_shadow $rect
		%P deletetag selected $image
		lappend _handle(images) $image
	    }
	}
	set _handle(prevdegrees) 0
	break
    }

    $PAD bind rotater <Select-B1-Motion> {
        %P slide %O [expr %i - $_handle(x)] [expr %j - $_handle(y)]
	if {%O == [%P find withtag h_num1]} {
	    set bbox [%P bbox h_num0]
	    set x0 [expr ([lindex $bbox 0] + [lindex $bbox 2]) / 2.0]
	    set y0 [expr ([lindex $bbox 1] + [lindex $bbox 3]) / 2.0]

	    set bbox [%P bbox h_num1]
	    set x1 [expr ([lindex $bbox 0] + [lindex $bbox 2]) / 2.0]
	    set y1 [expr ([lindex $bbox 1] + [lindex $bbox 3]) / 2.0]

					# Compute the angle within the triangle formed
					# by the original point, the rotation center,
					# and the new point.
	    set v0x [expr $x1 - $x0]
	    set v0y [expr $y1 - $y0]
	    set v1x [expr $_handle(origx) - $x0]
	    set v1y [expr $_handle(origy) - $y0]
	    set num [expr ($v0x*$v1x) + ($v0y*$v1y)]
	    set denom [expr sqrt($v0x*$v0x + $v0y*$v0y) * sqrt($v1x*$v1x + $v1y*$v1y)]
	    set radians [expr acos($num / $denom)]
	    if {[expr ($v0x*$v1y) - ($v0y*$v1x)] > 0} {
		set radians [expr -1.0 * $radians]
	    }
	    set degrees [expr $radians * 180.0 / 3.14159]
					# If the distance between the center point, and
					# the rotation point is small, then rotate
					# by multiples of 1 or 10 degrees.
	    set zoom [%P getzoom]
	    set dist [expr $zoom * sqrt(($x1-$x0)*($x1-$x0) + ($y1-$y0)*($y1-$y0))]
	    if {$dist < 100} {
		set grid 0.1
	    } elseif {$dist < 200} {
		set grid 1.0
	    } else {
		set grid 1000.0
	    }
	    set degrees [expr round($degrees * $grid) / $grid]
	    updateStatusText %P "$degrees degrees" 1

					# Compute change in angle, and rotate selected items
	    set dtheta [expr $degrees - $_handle(prevdegrees)]
	    %P rotate selected $dtheta $x0 $y0
	    set _handle(prevdegrees) $degrees
	}
	set _handle(x) %i
	set _handle(y) %j
	break
    }

    $PAD bind rotater <Select-B1-ButtonRelease-1> {
        %P ic %O -fill lightblue
	%P delete image_shadow
	foreach image $_handle(images) {
	    %P addtag selected $image
	    set bbox [%P bbox h_num0]
	    set x0 [expr ([lindex $bbox 0] + [lindex $bbox 2]) / 2.0]
	    set y0 [expr ([lindex $bbox 1] + [lindex $bbox 3]) / 2.0]
	    %P rotate $image $_handle(prevdegrees) $x0 $y0
	}
	foreach text $_handle(texts) {
	    set bbox [%P bbox $text]
	    set width [expr [lindex $bbox 2] - [lindex $bbox 0]]
	    set height [expr [lindex $bbox 3] - [lindex $bbox 1]]
	    %P ic $text -anchor nw
	    %P slide $text [expr -0.5 * $width] [expr 0.5 * $height]
	}
	unset _handle(texts)
	unset _handle(images)
	updateStatusText %P "" 0
	break
    }
}
