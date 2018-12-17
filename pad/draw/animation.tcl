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

###############################################################################
#
# This file (animation.tcl) sets up the Pad++ Animation Tools 
# top level interface.
#
# Most of the tcl code that handles the animation internals (like recording, 
# writing out files, and playing) exists in animation_engine.tcl. 
#
# It is important that the function animInit (in animation_engine.tcl) gets 
# called in order to define important animation bindings.  Currently it is
# called in events.tcl. 
#
###############################################################################

#######################################################
#
# proc make.animate {PAD}
#
# Creates the front end of the animation tool.
#
#######################################################
proc make.animate {PAD} {
    
    global _animate 
    global _color 
    global _pad
    global _font
  
    if {[winfo exists .animate]} {
        raise .animate
	return
    }

    bind animate_item <Enter> "animButEnter $PAD %W; break"      
    bind animate_item <Leave> "animButLeave $PAD %W; break"
    bind animate_item_special <ButtonPress-1> "animButDown %W; break"
    bind animate_item_special <ButtonRelease-1> "animButUp %W; break"
    
    # Our main animate window
    toplevel .animate -bg $_color(toolbg)
    wm resizable .animate 0 0

    # Create a recorder frame
    frame .animate.recorderFrame -bg $_color(toolbg)
	
########################################
#
# Primary Animation Controls
#
########################################

    # Fast Rewind Button
    button .animate.recorderFrame.fastRewind \
	-bitmap @$_pad(PadBitmaps)/animation/rew2.xbm \
	-command "animReset F R $PAD" \
	-highlightbackground $_color(toolbg) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightthickness 0
    bindtags .animate.recorderFrame.fastRewind \
	{Button .animate.recorderFrame.fastRewind animate_item .animate all}
    set _animate(msg.animate.recorderFrame.fastRewind) \
	"Reset an animation"
    set _animate(help.animate.recorderFrame.fastRewind) \
	"The selected object will be
moved to its animation start point"

    # Stop Button
    button .animate.recorderFrame.stop \
	-bitmap @$_pad(PadBitmaps)/animation/stop2.xbm \
	-command "animStop $PAD" \
	-highlightbackground $_color(toolbg) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) -highlightthickness 0
    bindtags .animate.recorderFrame.stop \
	{Button .animate.recorderFrame.stop animate_item .animate all}
    set _animate(msg.animate.recorderFrame.stop) \
	"Stop"
    set _animate(help.animate.recorderFrame.stop) \
	"Stop playing or\nrecording an animation"
    
    # Backward Play
    button .animate.recorderFrame.backwardPlay \
	-bitmap @$_pad(PadBitmaps)/animation/back2.xbm \
	-command "animPlayStart F R $PAD" \
	-highlightbackground $_color(toolbg) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg)
    bindtags .animate.recorderFrame.backwardPlay \
	{.animate.recorderFrame.backwardPlay animate_item \
	     animate_item_special .animate all}
    set _animate(msg.animate.recorderFrame.backwardPlay) \
	"Play an animation in reverse"
    set _animate(help.animate.recorderFrame.backwardPlay) \
	"Play an animation in reverse
Select an object and press play"

    # Forward Play
    button .animate.recorderFrame.forwardPlay \
	-bitmap @$_pad(PadBitmaps)/animation/play2.xbm \
	-command "animPlayStart R F $PAD" \
	-highlightbackground $_color(toolbg) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg)
    bindtags .animate.recorderFrame.forwardPlay \
	{.animate.recorderFrame.forwardPlay animate_item \
	     animate_item_special .animate all}
    set _animate(msg.animate.recorderFrame.forwardPlay) \
	"Play an animation"
    set _animate(help.animate.recorderFrame.forwardPlay) \
	"Play an animation
Select an object and press play"

    # Record
    button .animate.recorderFrame.record \
	-bitmap @$_pad(PadBitmaps)/animation/rec2.xbm \
	-command "animRecordStart $PAD" \
	-highlightbackground $_color(toolbg) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg)
    bindtags .animate.recorderFrame.record \
	{.animate.recorderFrame.record animate_item \
	     animate_item_special  .animate all}
    set _animate(msg.animate.recorderFrame.record) \
	"Record an animation (Click and drag an object to record a path)"
    set _animate(help.animate.recorderFrame.record) \
	"Select an object and press record
to create an animation path"

    # Fast Forward
    button .animate.recorderFrame.fastForward \
	-bitmap @$_pad(PadBitmaps)/animation/ff2.xbm \
	-command "animReset R F $PAD" \
	-highlightbackground $_color(toolbg) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightthickness 0
    bindtags .animate.recorderFrame.fastForward \
	{Button .animate.recorderFrame.fastForward animate_item .animate all}
    set _animate(msg.animate.recorderFrame.fastForward) \
	"Go to the end point of an animation"
    set _animate(help.animate.recorderFrame.fastForward) \
	"Selected object will be moved
to its animation endpoint" 

    frame .animate.recorderFrame.blankframe0 -bg $_color(toolbg)
    frame .animate.recorderFrame.blankframe1 -bg $_color(toolbg)
    frame .animate.recorderFrame.blankframe2 -bg $_color(toolbg)
    frame .animate.recorderFrame.blankframe3 -bg $_color(toolbg)
    frame .animate.recorderFrame.blankframe4 -bg $_color(toolbg)
    frame .animate.recorderFrame.blankframe5 -bg $_color(toolbg)

    pack .animate.recorderFrame.fastRewind .animate.recorderFrame.blankframe0 .animate.recorderFrame.stop .animate.recorderFrame.blankframe1 .animate.recorderFrame.backwardPlay .animate.recorderFrame.blankframe3 .animate.recorderFrame.forwardPlay .animate.recorderFrame.blankframe4 .animate.recorderFrame.record .animate.recorderFrame.blankframe5 .animate.recorderFrame.fastForward -side left -ipadx 6

    frame .animate.middle -bg $_color(toolbg)
    frame .animate.bottom -bg $_color(toolbg)

    # Options Check Button
    checkbutton .animate.showopts \
	-text "Show Animation Options" \
	-command "animAddOptions PAD" \
	-variable _animate(options) \
	-onvalue "ON" \
	-offvalue "OFF" \
	-bg $_color(toolbg) \
	-fg black \
	-highlightbackground $_color(toolbg) \
	-font $_font(tools)
    
    frame .animate.b1 -bg $_color(toolbg)

    button .animate.close \
	-text Close \
	-command {destroy .animate} \
	-highlightbackground $_color(toolbg) \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg)
 
    pack .animate.showopts -in .animate.bottom -side left 
    pack .animate.b1 -in .animate.bottom -expand t -fill x -side left
    pack .animate.close -in .animate.bottom -side left 

    frame .animate.f1 -height 2 -bg $_color(toolbg)
    frame .animate.f2 -height 2 -bg $_color(toolbg)

    # Now pack the recorder frame
    pack .animate.recorderFrame  -side top
    pack .animate.f1 .animate.middle .animate.f2 -expand t -fill x 
    pack .animate.bottom -expand t -fill x -side top 

    # Create the options frame
    frame .animate.options -bg $_color(toolbg)

    frame .animate.play -bg $_color(toolbg)
    frame .animate.rec -bg $_color(toolbg)
    frame .animate.multi -bg $_color(toolbg)
    
    # Options stuff #####################################
    
    # Play stuff
    frame .animate.play.heading -bg $_color(toolbg)
    frame .animate.play.body -relief sunken -bg $_color(toolbg)
    frame .animate.play.footer -bg $_color(toolbg)
    frame .animate.play.f1 -height 5 -bg black 
    frame .animate.play.f2 -height 5 -bg black
 
    label .animate.play.playOpts \
	-text "Play Options" \
	-font $_font(tools) \
	-bg $_color(toolbg)

    frame .animate.play.speed -bg $_color(toolbg)
    frame .animate.play.speed.label -bg $_color(toolbg)

    label .animate.play.speed.l -text Speed -bg $_color(toolbg) \
	-font $_font(tools)
    frame .animate.play.speed.blank -bg $_color(toolbg)
    label .animate.play.speedlabel \
	-bg $_color(toolbg) \
	-bitmap @$_pad(PadBitmaps)/animation/speed.xbm     

    pack .animate.play.speed.l .animate.play.speedlabel  -side left -in .animate.play.speed.label 
    pack .animate.play.speed.blank -side left -in .animate.play.speed.label -expand t -fill x
    
    scale .animate.play.speedscale \
	-font $_font(tools) \
	-from $_animate(minSpeedValue) \
	-to $_animate(maxSpeedValue) \
	-variable _animate(speed) \
	-orient horizontal \
	-showvalue 0 \
	-length 165 \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) \
	-activebackground $_color(toolactivebg) \
	-highlightthickness 0
    bindtags .animate.play.speedscale \
	{Scale .animate.play.speedscale animate_item .animate all}
    set _animate(msg.animate.play.speedscale) \
	"Animation play speed adjustment"
    set _animate(help.animate.play.speedscale) \
	"Play speed is slowed by
moving the slider left"

    checkbutton .animate.play.loop \
	-font $_font(tools) \
	-text "Loop Play" \
	-variable _animate(loopSwitch) \
	-onvalue ON \
	-offvalue OFF\
	-highlightbackground $_color(toolbg) \
	-anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg)
    bindtags .animate.play.loop \
	{Checkbutton .animate.play.loop animate_item .animate all}
    set _animate(msg.animate.play.loop) \
	"Continuous play" 
    set _animate(help.animate.play.loop) \
	"Continuously play an animation until
stopped or loop is switched off" 

    # Relative Play Option Button
    checkbutton .animate.play.rel \
	-font $_font(tools) \
	-text "Relative" \
	-variable _animate(REL) \
	-onvalue ON \
        -offvalue OFF\
	-command "" \
	-highlightbackground $_color(toolbg) \
	-activebackground $_color(toolactivebg) \
	-bg $_color(toolbg)
    bindtags .animate.play.rel \
	{Checkbutton .animate.play.rel \
	     animate_item .animate all}
    set _animate(msg.animate.play.rel) \
	"Play animation starting from this point" 
    set _animate(help.animate.play.rel) \
	"An animation will play from
its current position"
    
    pack .animate.play.speed.label -expand t -fill x -side top -in .animate.play.speed 
    pack .animate.play.speedscale -side top -in .animate.play.speed

    pack .animate.play.playOpts -side left -in .animate.play.heading
    pack .animate.play.f2 -expand t -fill x -side left -in .animate.play.heading

    pack .animate.play.speed .animate.play.loop .animate.play.rel -in .animate.play.body -side left -ipadx 5

    pack .animate.play.heading -side top -expand t -fill x
    pack .animate.play.body -side top -expand t -fill y 

    # Record stuff
    frame .animate.rec.heading -bg $_color(toolbg)
    frame .animate.rec.body -relief sunken -bg $_color(toolbg)
    frame .animate.rec.footer -bg $_color(toolbg)
    frame .animate.rec.f1 -height 5 -bg black 
    frame .animate.rec.f2 -height 5 -bg black 

    label .animate.rec.recOpts \
	-text "Record Options" \
	-font $_font(tools) \
	-bg $_color(toolbg)

    checkbutton .animate.rec.append \
	-font $_font(tools) \
	-text "Append to old path" \
	-variable _animate(appendMode) \
	-onvalue ON \
        -offvalue OFF \
	-highlightbackground $_color(toolbg) \
	-activebackground $_color(toolactivebg) \
	-bg $_color(toolbg)
    bindtags .animate.rec.append \
	{Checkbutton .animate.rec.append animate_item .animate all}
    set _animate(msg.animate.rec.append) \
	"Add to a previous animation path"    
    set _animate(help.animate.rec.append) \
	"Append currently recorded path 
to animation's stored path"

    checkbutton .animate.rec.paths \
	-font $_font(tools) \
	-text "show pathlines" \
	-highlightbackground $_color(toolbg) \
	-activebackground $_color(toolactivebg) \
	-bg $_color(toolbg) \
	-state disabled
    bindtags .animate.rec.paths \
	{Checkbutton .animate.rec.paths animate_item .animate all}
    set _animate(msg.animate.rec.paths) \
	"Show pathlines while in select (arrow) mode"
    set _animate(help.animate.rec.paths) \
	"Show pathlines while in
select (arrow) mode path"

    button .animate.rec.delAnim \
	-font $_font(tools) \
	-text "Remove animation" \
	-highlightbackground $_color(toolbg) \
	-activebackground $_color(toolactivebg) \
	-bg $_color(toolbg) \
	-command "animClearObject $PAD"
    bindtags .animate.rec.delAnim \
	{Button .animate.rec.delAnim animate_item .animate all}
    set _animate(msg.animate.rec.delAnim) \
	"Remove animation from selected"
    set _animate(help.animate.rec.delAnim) \
	"Selected object will no longer
be recognized as an animation"

    pack .animate.rec.recOpts -side left -in .animate.rec.heading
    pack .animate.rec.f2 -expand t -fill x -side left -in .animate.rec.heading

    pack .animate.rec.append .animate.rec.delAnim -in .animate.rec.body -side left -ipadx 10

    pack .animate.rec.heading -side top -expand t -fill x
    pack .animate.rec.body -side top -expand t -fill x -ipady 5

    # Multi stuff
    frame .animate.multi.heading -bg $_color(toolbg)
    frame .animate.multi.body -relief sunken -bg $_color(toolbg)
    frame .animate.multi.footer -bg $_color(toolbg)
    frame .animate.multi.f1 -height 5 -bg black 
    frame .animate.multi.f2 -height 5 -bg black
    label .animate.multi.multiOpts \
	-text "Multi Options" \
	-font $_font(tools) \
	-bg $_color(toolbg)

    button .animate.multi.new \
	-font $_font(tools) \
	-text "Create a Multi-group" \
	-command "animNew $PAD" \
	-highlightbackground $_color(toolbg) \
	-activebackground $_color(toolactivebg) \
	-bg $_color(toolbg)
    bindtags .animate.multi.new \
	{Button .animate.multi.new animate_item .animate all}
    set _animate(msg.animate.multi.new) \
	"Create a new multi-animation group from selected animation objects." 
    set _animate(help.animate.multi.new) \
	"Create a new multi-animation group
from selected animation objects."

    button .animate.multi.add \
	-font $_font(tools) \
	-text Add \
	-command "animMAdd $PAD" \
	-highlightbackground $_color(toolbg) \
	-activebackground $_color(toolactivebg) \
	-bg $_color(toolbg)
    bindtags .animate.multi.add \
	{Button .animate.multi.add animate_item .animate all}
    set _animate(msg.animate.multi.add) \
	"Add selected item to the current multi-animation group"
    set _animate(help.animate.multi.add) \
	"Add selected item to the\ncurrent multi-animation group"

    button .animate.multi.remove \
	-font $_font(tools) \
	-text Remove \
	-command "animMRemove $PAD" \
	-highlightbackground $_color(toolbg) \
	-activebackground $_color(toolactivebg) \
	-bg $_color(toolbg)
    bindtags .animate.multi.remove \
	{Button .animate.multi.remove animate_item .animate all}
    set _animate(msg.animate.multi.remove) \
	"Remove selected item from the current multi-animation group"
    set _animate(help.animate.multi.remove) \
	"Remove selected item from the
current multi-animation group"

    pack .animate.multi.multiOpts -side left -in .animate.multi.heading
    pack .animate.multi.f2 -expand t -fill x -side left -in .animate.multi.heading

    frame .animate.multi.b1 -width 20 -bg $_color(toolbg)
    pack .animate.multi.new  .animate.multi.b1 .animate.multi.add .animate.multi.remove -side left  -in .animate.multi.body

    pack .animate.multi.heading -side top  -expand t -fill x
    pack .animate.multi.body -side top -expand t -fill x -ipady 5

##############################
    pack .animate.play .animate.rec .animate.multi -in .animate.options -side top -ipady 15 -expand t -fill x 

    pad_ManageWindow  .animate 0 1
    wm title .animate "Pad++ Animation Tools"
}

######################################################################
#
# proc animAddOptions {PAD}
#
# This procedure adds the options extension to the front end.
#
######################################################################  
proc animAddOptions {PAD} {
    global _animate 
    global _color 
    global _pad
    
    if {$_animate(options) == "ON"} {
	pack .animate.options -expand t -fill x -before .animate.f2
    } else {
	pack forget .animate.options
    }
}

######################################################################
#
# proc animRecordStart {PAD}
#
# This procedure changes the color of the record button, sets the 
# mode to rec.  It is called by pressing the record button.
#
######################################################################
proc animRecordStart {PAD} {

    global _animate 
    global _pad
    global _color

    .animate.play.speed.l config -fg \#709090
    .animate.play.speedlabel config -fg \#709090

    animDisableButton {.animate.recorderFrame.fastRewind .animate.recorderFrame.fastForward .animate.multi.new .animate.multi.add .animate.multi.remove .animate.play.speedscale .animate.play.loop .animate.play.rel}
    
    if {$_animate(mode) == "forwardPlay" || \
	    $_animate(mode) == "backwardPlay"} {
	animPlayStop $PAD
    }

    set _animate(mode) rec
    # Make the record button red
    .animate.recorderFrame.record configure \
	-fg red \
	-activeforeground red 
    
    set _animate(selectedObject) [pad_sel $PAD]
 
    foreach id $_animate(selectedObject) {
	pad_unselect $PAD $id
	pad_hilite $PAD $id $_color(animHilite.0) 
	$PAD addtag "hilite" hilite$id
    }

    selectMode $PAD ""

    $PAD modifier set AnimRecord 

   
    $PAD config -cursor [pad_pointer_cursor $PAD]
    
    set _animate(Rec) ON

    set _animate(hilite.color.count) 0
}

####################################################################
#
# proc animStop {PAD}
#
# This procedure is called when the stop button is pressed.  AnimPlayStop
# is called or animRecordStop is called depending on which mode you 
# are in.
#
####################################################################
proc animStop {PAD} {

    global _animate

    if {$_animate(mode) == "ready"} {
	return
    } elseif {$_animate(mode) == "rec"} {
        animRecordStop $PAD
    } else {
	animPlayStop $PAD
    }
}

####################################################################
#
# proc animRecordStop {PAD}
# 
# This procedure is called when the stop button is pushed and we
# are recording an animation.  It restores the buttons to black.
#
####################################################################
proc animRecordStop {PAD} {

    global _animate
    global _pad
    global _color

    .animate.recorderFrame.record configure \
	-bitmap @$_pad(PadBitmaps)/animation/rec2.xbm \
	-fg black -activeforeground black \
	-relief raised \
	-bg $_color(toolbg)
    
    selectMode $PAD Select

    $PAD delete "hilite"

    # Make the animation record path line invisible
    $PAD ic pathLine -transparency 0 -events 0 -lock 1

    if {[info exists _animate(selectedObject)]} {
	unset _animate(selectedObject)
    }

    set _animate(Rec) OFF
    set _animate(mode) ready

    .animate.play.speed.l config -fg black
    .animate.play.speedlabel config -fg black

    animEnableButton {.animate.recorderFrame.fastRewind .animate.recorderFrame.fastForward .animate.multi.new .animate.multi.add .animate.multi.remove .animate.play.speedscale .animate.play.loop .animate.play.rel}
}

####################################################################
#
# proc animPlayStop {PAD}
#
# This procedure is called when the stop button is pushed and we
# are playing an animation.  It restores the buttons to black.
#
####################################################################
proc animPlayStop {PAD} {

    global _animate
    global _pad
    global _color
    
    if {$_animate(mode) == "ready"} {
	return
    } elseif {$_animate(mode) == "rec"} {
	.animate.recorderFrame.forwardPlay configure \
	    -relief raised \
	    -bg $_color(toolbg)

	.animate.recorderFrame.backwardPlay configure \
	    -relief raised \
	    -bg $_color(toolbg)
    } else {
	.animate.recorderFrame.$_animate(mode) configure \
	    -relief raised \
	    -bg $_color(toolbg)
    }
    
    set _animate(isLooping) false
  
    set _animate(mode) ready
    
    animEnableButton {.animate.recorderFrame.record .animate.recorderFrame.fastForward .animate.recorderFrame.fastRewind} 
    
    foreach id $_animate(defaultPlayID) {
 	pad_select $PAD $id
    }
}

######################################################################
#
# proc animPlayStart {RorF ForR PAD }
#
# This procedure is called when the play button is pressed.
#
######################################################################
proc animPlayStart {RorF ForR PAD} {

    global _animate
    global _pad


    if {$_animate(mode) == "rec"} {
	animRecordStop $PAD
    } elseif {$_animate(mode) == "forwardPlay" } {
	if {$ForR == "F"} {
	    return
	}
	animPlayStop $PAD
    } elseif {$_animate(mode) == "backwardPlay" } {
	if {$ForR == "R"} {
	    return
	}
	animPlayStop $PAD
    }
    
    animDisableButton {.animate.recorderFrame.record .animate.recorderFrame.fastForward .animate.recorderFrame.fastRewind}

    if {$ForR == "F"} {
	set _animate(mode) forwardPlay
    } else {
	set _animate(mode) backwardPlay
    }
    
    set _animate(isLooping) true

    # We set some globals here because otherwise there are scoping problems
    # within animLoopPlay.

    set _animate(ForR) $ForR
    set _animate(RorF) $RorF 

   
    if {$_animate(loopSwitch) == "ON"} {
	animLoopPlay $PAD
    } else { 
	animPlay $RorF $ForR $PAD 
    }
}

############################################################################
#
# proc animLoopPlay {PAD}
#
# This procedure is called when the _animate(loopSwitch) variable is ON and
# the play button has been pressed.  It recursively calls itself and
# animStartPlay.
#
############################################################################
proc animLoopPlay {PAD} {

    global _animate

    if {$_animate(loopSwitch) == "OFF" || $_animate(isLooping) == "false"} {
	animPlayStop $PAD
	return
    } else {
	animPlay $_animate(RorF) $_animate(ForR) $PAD
	after idle animLoopPlay $PAD
    }
}

########################################################################
#
# proc animButEnter {PAD w}
#
# Shows a help message on the Pad status bar.
#
########################################################################
proc animButEnter {PAD w} {  
    global _animate
    global _pad

    if {[info exists _animate(msg$w)]} {
	set text $_animate(msg$w)
    }

    if {$text == ""} {
	set emphasizebg 0
    } else {
	set emphasizebg 1
    }
    if { $w != ".animate.sFrame.options"} {
	if {[$w cget -state] != "disabled"} {
	    $w config -state active
	}
    }

    updateStatusText $PAD $text $emphasizebg
    
    if {$_pad(Help)} {
	set _animate(help_timer) [after 750 animButHelp $w]
    }    
}

########################################################################
#
# proc animButLeave {PAD w}
#
# Clear status bar message.
#
########################################################################
proc animButLeave {PAD w} {
    global _animate
 
    set emphasizebg 0
    updateStatusText $PAD "" $emphasizebg

    if { $w != ".animate.sFrame.options" } {
	if {[$w cget -state] != "disabled"} {
	    $w config -state normal
	}
    }

    if {[info exists _animate(help_timer)]} {
	after cancel $_animate(help_timer)
	unset _animate(help_timer)
    } else {
	if {[winfo exists .animate_help]} {
	    destroy .animate_help
	}
    }
}

########################################################################
#
# proc animButDown {w}
#
#  Called when animate_special buttons are pressed
#
########################################################################
proc animButDown {w} {
    global tkPriv _color

    global tkPriv
    set tkPriv(relief) [lindex [$w config -relief] 4]
    if {[$w cget -state] != "disabled"} {
	set tkPriv(buttonWindow) $w
	$w config -relief sunken -bg $_color(toolselbg) -state normal
	uplevel #0 [list $w invoke]
    }
}

########################################################################
#
# proc animButUp {w}
#
#  Called when animate_special buttons are released
#
########################################################################
proc animButUp {w} {
    global tkPriv _color

    if {$w == $tkPriv(buttonWindow)} {
	set tkPriv(buttonWindow) ""
    }
}

########################################################################
#
#  proc animDisableButton { params }
#
#  Disable a list of buttons
#
########################################################################
proc animDisableButton { params } {
    
    foreach bttn $params {
	if {[winfo exists $bttn]} {
	    $bttn config \
		-state disabled 
	}
    }

}

#########################################################################
#
#  proc animEnableButton { params }
#
#  Enable a list of buttons
#
########################################################################
proc animEnableButton { params } {
    
    foreach bttn $params {
	$bttn config \
	    -state normal
    }
}

#############################################################################
#
# proc animNew {PAD items}
#
# Take selected objects and put them into a multi animation group
#
#############################################################################
proc animNew {PAD} {
    global _animate
    global _color

    set objs [pad_sel $PAD]
    pad_unselect $PAD all

    while {[info exists _animate(multi_$_animate(groupCount))]} {
		incr _animate(groupCount)
    }

    foreach id  $objs {
	if {[info exists _animate(ObjPath.$id.pathF)]} {
	    # add it to the multigroup
	    append _animate(multi_$_animate(groupCount)) $id

	    # Add a general multi tag
	    $PAD addtag animMulti $id

	    # Add the tag that specifies the multi group
	    $PAD addtag multi_$_animate(groupCount) $id	
 
	    pad_hilite $PAD $id $_color(animMulti.selected)
	}
    }
    incr _animate(animMultiCount)
    if {$_animate(animMultiCount) == 8} {
	set _animate(animMultiCount) 0
    }
    incr _animate(groupCount)

    after 750 animUnhilite $PAD $objs
}

#############################################################################
#
# proc animUnhilite {PAD args}
#
# Unhilite stuff.
#
#############################################################################
proc animUnhilite {PAD args} {
    foreach id $args {
	pad_unhilite $PAD $id
    }
}

#############################################################################
# 
# proc animButHelp {w}
#
# Gets called to post help for specified window (if available)
#
#############################################################################
proc animButHelp {w} {
    global _animate

    unset _animate(help_timer)
    if {[info exists _animate(help$w)]} {
	set multiline [regexp "(.*)\n(.*)" $_animate(help$w) dummy line1 line2]
					# Create window if it doesn't already exist
	menu .animate_help -bg yellow -tearoff 0
	if {$multiline} {
	    .animate_help add command -label $line1
	    .animate_help add command -label $line2
	} else {
	    .animate_help add command -label $_animate(help$w)
	}


	set isInRecFrame [regexp ".animate.recorderFrame.*" $w]

	if {$isInRecFrame} {
	    # Then want to place our help box a little lower
	    .animate_help post [expr [winfo rootx $w] + 20] \
		[expr [winfo rooty $w] + 38]
	} else {
	    .animate_help post [expr [winfo rootx $w] + 20] \
		[expr [winfo rooty $w] + 30]
	}
    }
}

#############################################################################
# 
# proc animClearObject {$PAD id}
#
# Clear an object of all its animation associations
#
#############################################################################
proc animClearObject {PAD} {
    global _animate

    set id [pad_sel $PAD]

    foreach obj $id {	
	if {[$PAD hastag "group" $obj]} {
	    append id " [$PAD ic $obj -members]" 
	}
    }
    
    foreach obj $id {
	animDelObject $PAD $obj
	$PAD deletetag animate $obj

	set tags [$PAD gettag $obj]
	set index [lsearch -regexp $tags "multi_*"]

	# Remove any multi assciations
	if {$index != -1} {
	    $PAD deletetag [lindex $tags $index] $obj
	}
    }
}


