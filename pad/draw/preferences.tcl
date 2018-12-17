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

proc make.pref {PAD} {
    global _pad _color _font
    global geometry menu

    if {[winfo exists .pref]} {
	raise .pref
	return
    }
    toplevel .pref -bg $_color(toolbg)
    wm resizable .pref 0 0
    wm title .pref "Pad++ Preferences"

    scale .pref.zoom -label {Zoom Speed} -from 1 -to 20 \
	-command setZoomSpeed -orient horizontal -width 10 -sliderlength 20 \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .pref.zoom set $_pad(ZoomSpeed)

    scale .pref.animation -label {Animation Speed} -from 0 -to 5000 \
	-command setAnimationSpeed -orient horizontal -width 10 -sliderlength 20 \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .pref.animation set $_pad(AnimationSpeed)

    scale .pref.framerate -label {Desired Frame Rate} -from 1 -to 30 \
	-command "setFrameRate $PAD" -orient horizontal -width 10 -sliderlength 20 \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .pref.framerate set $_pad(DesiredFrameRate)

    scale .pref.smallobj -label {Small Object Size} -from 1 -to 100 \
	-command "setSmallObjSize $PAD" -orient horizontal -width 10 -sliderlength 20 \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .pref.smallobj set $_pad(SmallObjSize)

    scale .pref.mediumobj -label {Medium Object Size} -from 1 -to 500 \
	-command "setMediumObjSize $PAD" -orient horizontal -width 10 -sliderlength 20 \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .pref.mediumobj set $_pad(MediumObjSize)

    frame .pref.dummy1 -height 10 -bg $_color(toolbg)

    checkbutton .pref.zoomstyle -text {Zoom follows cursor} -variable _pad(ZoomStyle) -anchor w \
	-onvalue "FollowCursor" -offvalue "AroundCursor" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    checkbutton .pref.help -text {Bubble Help} -variable _pad(Help) -anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    checkbutton .pref.status -text {Status Line} -variable _pad(StatusLine$PAD) -anchor w \
	-command "setStatusLine $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    checkbutton .pref.coords -text {Show Coordinates} -variable _pad(ShowCoords) -anchor w \
	    -command "if {\$_pad(ShowCoords)} {
	                  setCoords $PAD 1
                      } else {
	                  setCoords $PAD 0
		      }" \
  	    -font $_font(tools) \
	    -bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	    -highlightbackground $_color(toolbg)
    checkbutton .pref.links -text {Show Links} -variable _pad(ShowLinks) -anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    
    button .pref.line -text {Line Style...} -anchor w -relief flat \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg) \
	-command "set menu(.lineprops) 1; controlWindow $PAD .lineprops"
			  
    frame .pref.gamma -bg $_color(toolbg)
    label .pref.gamma.label -text "Brightness: " -width 11 -bg $_color(toolbg) -anchor w \
	-font $_font(tools)
    entry .pref.gamma.entry -width 8 -relief sunken -bg $_color(toolbg) \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
					# Fix broken default backspace/delete bindings
    bind .pref.gamma.entry <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
    bind .pref.gamma.entry <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}
    bind .pref.gamma.entry <Enter> {focus %W}
    bind .pref.gamma.entry <Key-Return> "
            set _pad(Gamma) \[%W get\]
            $PAD config -gamma \$_pad(Gamma)
	    $PAD damage;"
    .pref.gamma.entry delete 0 end
    .pref.gamma.entry insert 0 [format "%3.1f" [lindex [$PAD config -gamma] 4]]

    frame .pref.csize -bg $_color(toolbg)
    label .pref.csize.label -text "Cache Size: " -width 11 -bg $_color(toolbg) -anchor w \
	-font $_font(tools)
    entry .pref.csize.entry -width 8 -relief sunken -bg $_color(toolbg) \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
					# Fix broken default backspace/delete bindings
    bind .pref.csize.entry <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
    bind .pref.csize.entry <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}
    bind .pref.csize.entry <Enter> {focus %W}
    bind .pref.csize.entry <Key-Return> "
            set _pad(cacheSize) \[%W get\]
            $PAD cache config -size \$_pad(cacheSize)"
    .pref.csize.entry delete 0 end
    set cacheSize [format "%3.1f" [$PAD cache config -size]]
    .pref.csize.entry insert 0 $cacheSize

    frame .pref.cdir -bg $_color(toolbg)
    label .pref.cdir.label -text "Cache Dir: " -width 11 -bg $_color(toolbg) -anchor w \
	-font $_font(tools)
    entry .pref.cdir.entry -width 8 -relief sunken -bg $_color(toolbg) \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
					# Fix broken default backspace/delete bindings
    bind .pref.cdir.entry <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
    bind .pref.cdir.entry <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}
    bind .pref.cdir.entry <Enter> {focus %W}
    bind .pref.cdir.entry <Key-Return> "
            $PAD cache config -cacheDir \$_pad(cacheDir)
            set _pad(cacheDir) \[$PAD cache config -cacheDir\]"
    .pref.cdir.entry delete 0 end
    set cacheDir [$PAD cache config -dir]
    .pref.cdir.entry insert 0 $cacheDir

    button .pref.close -text Close -command {destroy .pref} -width 15 \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    pack .pref.animation -fill x
    pack .pref.zoom -fill x
    pack .pref.framerate -fill x
    pack .pref.smallobj -fill x
    pack .pref.mediumobj -fill x
    pack .pref.dummy1
    pack .pref.zoomstyle -fill x
    pack .pref.help -fill x
    pack .pref.status -fill x
    pack .pref.coords -fill x
    pack .pref.links -fill x
    pack .pref.line -fill x
    pack .pref.gamma -fill x
    pack .pref.gamma.label -side left
    pack .pref.gamma.entry -side left -fill x
    pack .pref.csize -fill x
    pack .pref.csize.label -side left
    pack .pref.csize.entry -side left -fill x
    pack .pref.cdir -fill x
    pack .pref.cdir.label -side left
    pack .pref.cdir.entry -side left -fill x
    pack .pref.close -side bottom -fill x

    pad_ManageWindow .pref
}

proc setZoomSpeed {value} {
    global _pad

    set _pad(ZoomSpeed) $value
}

proc setAnimationSpeed {value} {
    global _pad

    set _pad(AnimationSpeed) $value
}

proc setFrameRate {PAD value} {
    global _pad

    set _pad(DesiredFrameRate) $value
    $PAD config -desiredFrameRate $value
}

proc setSmallObjSize {PAD value} {
    global _pad

    set _pad(SmallObjSize) $value
    $PAD config -smallObjSize $value
    $PAD damage
    $PAD update
}

proc setMediumObjSize {PAD value} {
    global _pad

    set _pad(MediumObjSize) $value
    $PAD config -mediumObjSize $value
    $PAD damage
    $PAD update
}

proc setStatusLine {PAD} {
    global _pad

    set _pad(StatusLine) $_pad(StatusLine$PAD)
    updateStatusText $PAD {} 0
}
