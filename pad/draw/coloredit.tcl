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
# coloredit lets you select a color by twiddling RGB and/or HSL values.
#

proc make.scale {PAD name var to title} {
    global ce _color _font

    frame $name	-bg $_color(toolbg)
    scale $name.scale -command "update.color $PAD $var" -from $to -to 0 -width 15 -showvalue 0 \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    $name.scale set [set $var]
    label $name.label -text "$title" -pady 0 -padx 0 \
	-font $_font(tools) \
	-bg $_color(toolbg) 
    pack $name.label
    pack $name.scale
}

proc update.color {PAD var value} {
    global ce $var
    
    if {($var == "ce(red)") && ($ce(red_changed) == 1)} {
	set ce(red_changed) 0
	return
    }
    if {($var == "ce(blue)") && ($ce(blue_changed) == 1)} {
	set ce(blue_changed) 0
	return
    }
    if {($var == "ce(green)") && ($ce(green_changed) == 1)} {
	set ce(green_changed) 0
	return
    }
    if {($var == "ce(hue)") && ($ce(hue_changed) == 1)} {
	set ce(hue_changed) 0
	return
    }
    if {($var == "ce(sat)") && ($ce(sat_changed) == 1)} {
	set ce(sat_changed) 0
	return
    }
    if {($var == "ce(light)") && ($ce(light_changed) == 1)} {
	set ce(light_changed) 0
	return
    }

    if {$value == "none"} {
	set ce(color) "none"
	catch {.ce.f.patch.frame.color configure -background [Pad_Color $PAD none]}
    } elseif {[string index $value 0] == "#"} {
	set var "color"
	set ce(color) $value
	set ce(red) [expr "0x[string range $value 1 2]"]
	set ce(green) [expr "0x[string range $value 3 4]"]
	set ce(blue) [expr "0x[string range $value 5 6]"]
	catch {.ce.f.patch.frame.color configure -background $ce(color)}
    } else {
	if {[info exists ce(red)]} {
	    set $var $value
	    set ce(color) [format "#%02x%02x%02x" $ce(red) $ce(green) $ce(blue)]
	    catch {.ce.f.patch.frame.color configure -background $ce(color)}
	}
    }
    if {[winfo exists .ce.f.patch.value]} {
	.ce.f.patch.value delete @0 end
	.ce.f.patch.value insert 0 "$ce(color)"
	if {($var == "") || ($var == "none")} {
	} elseif {$var == "ce(red)" || $var == "ce(blue)" || $var == "ce(green)"} {
	    rgb.changed $PAD
	} elseif {$var == "ce(hue)" || $var == "ce(sat)" || $var == "ce(light)"} {
	    hsl.changed
	} else {
	    rgb.changed $PAD
	    hsl.changed
	}
    }
}

proc ce_min args {
	set x [lindex $args 0]
	foreach y $args {
		if {$y < $x} {set x $y}
	}
	return $x
}

proc ce_max args {
	set x [lindex $args 0]
	foreach y $args {
		if {$y > $x} {set x $y}
	}
	return $x
}

proc floor {x} {
	if {$x < 0} {set t [expr {0-$x}]} {set t $x}
	set s [format %.0f $t]
	if {$t != $x} {return "-$s"} {return $s}
}

# The code for translating from RGB to HSL and HSL to RGB is ripped
# off from Fundamentals of Computer Graphics, Foley & Van Dam.

proc rgb.changed {PAD {style ""}} {
    global ce

    set MIN [ce_min $ce(red) $ce(green) $ce(blue)]
    set MAX [ce_max $ce(red) $ce(green) $ce(blue)]
    set ce(light) [expr {($MIN+$MAX)/2}]
    if {$MIN == $MAX} {
	set ce(sat) 0
	set ce(hue) 0
    } else {
	if {$ce(light) < 128} {
	    set ce(sat) [expr {(256*($MAX-$MIN))/($MAX+$MIN)}]
	} else {
	    set ce(sat) [expr {(256*($MAX-$MIN))/(512-$MAX-$MIN)}]
	}
	set d [expr {$MAX-$MIN}].0
	set rc [expr {($MAX-$ce(red))/$d}]
	set gc [expr {($MAX-$ce(green))/$d}]
	set bc [expr {($MAX-$ce(blue))/$d}]
	if {$ce(red) == $MAX} {
	    set ce(hue) [expr {$bc-$gc}]
	} else {
	   if {$ce(green) == $MAX} {
		set ce(hue) [expr {2+$rc-$bc}]
	   } else {
		set ce(hue) [expr {4+$gc-$rc}]
	   }
	}
	set ce(hue) [expr {$ce(hue)*60}]
	if {$ce(hue) < 0} {set ce(hue) [expr {$ce(hue)+360}]}
    }
    set ce(hue) [format %.0f $ce(hue)]
    if {[winfo exists .ce]} {
	set red [.ce.scales.red.scale get]
	if {$red != $ce(red)} {
	    .ce.scales.red.scale set $ce(red)
	    set ce(red_changed) 1
	}
	set green [.ce.scales.green.scale get]
	if {$green != $ce(green)} {
	    .ce.scales.green.scale set $ce(green)
	    set ce(green_changed) 1
	}
	set blue [.ce.scales.blue.scale get]
	if {$blue != $ce(blue)} {
	    .ce.scales.blue.scale set $ce(blue)
	    set ce(blue_changed) 1
	}

	set hue [.ce.scales.hue.scale get]
	if {$hue != $ce(hue)} {
	    .ce.scales.hue.scale set $ce(hue)
	    set ce(hue_changed) 1
	}
	set sat [.ce.scales.sat.scale get]
	if {$sat != $ce(sat)} {
	    .ce.scales.sat.scale set $ce(sat)
	    set ce(sat_changed) 1
	}
	set light [.ce.scales.light.scale get]
	if {$light != $ce(light)} {
	    .ce.scales.light.scale set $ce(light)
	    set ce(light_changed) 1
	}
    }

    set ce(color) [format "#%02x%02x%02x" $ce(red) $ce(green) $ce(blue)]
    if {$style == "fill"} {
	setcolor $PAD $ce(color) fill
    }
    if {$style == "pen"} {
	setcolor $PAD $ce(color) pen
    }
}

proc ce_value {n1 n2 hue} {
    if {$hue > 360} {set hue [expr {$hue-360}]}
    if {$hue < 0} {set hue [expr {$hue+360}]}
    if {$hue < 60} {
	set r [expr {$n1+($n2-$n1)*$hue/60}]
    } else {
	if {$hue < 180} {
	    set r $n2
	} else {
	    if {$hue < 240} {
		set r [expr {$n1+($n2-$n1)*(240-$hue)/60}]
	    } else {
		set r $n1
	    }
	}
    }
    set r [format %.0f [floor $r]]
    return $r
}

proc hsl.changed {} {
    global ce

    if {$ce(light) < 128} {
	set m2 [expr {$ce(light)*(255+$ce(sat))/256.0}]
    } else {
	set m2 [expr {$ce(light)+$ce(sat)-$ce(light)*$ce(sat)/256.0}]
    }
    set m1 [expr {2*$ce(light)-$m2}]
    if {$ce(sat) == 0} {
	set ce(red) $ce(light)
	set ce(green) $ce(light)
	set ce(blue) $ce(light)
    }
    set ce(red) [ce_value $m1 $m2 [expr {$ce(hue)+120}]]
    set ce(green) [ce_value $m1 $m2 $ce(hue)]
    set ce(blue) [ce_value $m1 $m2 [expr {$ce(hue)-120}]]

    .ce.scales.red.scale set $ce(red)
    .ce.scales.green.scale set $ce(green)
    .ce.scales.blue.scale set $ce(blue)
    set ce(red_changed) 1
    set ce(green_changed) 1
    set ce(blue_changed) 1
}

proc make.ce {PAD} {
    global _pad ce _color _draw _font
    
    if {$_pad(PenColor) == ""} {
	set _pad(PenColor) "none"
    }
    if {$_pad(FillColor) == ""} {
	set _pad(FillColor) "none"
    }

    set ce(red)   0
    set ce(green) 0
    set ce(blue)  0
    set ce(hue)   0
    set ce(sat)   0
    set ce(light) 0
    set ce(oldFill) $_pad(FillColor)
    set ce(oldPen) $_pad(PenColor)
    set ce(oldBgnd) [lindex [$PAD config -bg] 4]
    if {![info exists ce(palette)]} {
	set ce(palette_index) 0
	set ce(palette) [lindex $_color(Palettes) $ce(palette_index)]
    }

				# These changed variables are used to stop a racing
				# condition caused by a bug in Tk.  Now, changing
				# a scale's value won't trigger it's command to
				# be activated recursively.
    set ce(hue_changed) 0
    set ce(sat_changed) 0
    set ce(light_changed) 0
    set ce(red_changed) 0
    set ce(green_changed) 0
    set ce(blue_changed) 0
    
    toplevel .ce -bg $_color(toolbg)
    wm resizable .ce 0 0

    bindDrawButtons $PAD

    frame .ce.scales -bg $_color(toolbg)
    make.scale $PAD .ce.scales.red ce(red) 255 "R"
    make.scale $PAD .ce.scales.green ce(green) 255 "G"
    make.scale $PAD .ce.scales.blue ce(blue) 255 "B"
    frame .ce.scales.dummy0 -bg $_color(toolbg) -width 30
    make.scale $PAD .ce.scales.hue ce(hue) 360 "H"
    make.scale $PAD .ce.scales.sat ce(sat) 255 "S"
    make.scale $PAD .ce.scales.light ce(light) 255 "V"
    pack .ce.scales.red -side left
    pack .ce.scales.green -side left
    pack .ce.scales.blue -side left
    pack .ce.scales.dummy0 -side left
    pack .ce.scales.hue -side left
    pack .ce.scales.sat -side left
    pack .ce.scales.light -side left

    frame .ce.dummy1 -bg $_color(toolbg)
    frame .ce.palette -bg black

    button .ce.palette.copy -bitmap @$_pad(PadBitmaps)/color_config.xbm \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightthickness 0 \
	-padx 0 -pady 0 -width 14 -height 14 -anchor w
    bind .ce.palette.copy <ButtonPress-1> "ce.copypalette $PAD \[winfo pointerx .\] \[winfo pointery .\]; break"

    for {set i 0} {$i < 16} {incr i} {
	frame .ce.palette.c$i -bg [lindex $_color($ce(palette)) $i] -width 12
	bind .ce.palette.c$i <ButtonPress-1> "update.color $PAD \"\" \[%W cget -bg\]"
    }

    button .ce.palette.up -bitmap @$_pad(PadBitmaps)/color_arrowup.xbm \
	-command "ce.prevpalette $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightthickness 0 \
	-padx 0 -pady 0 -width 14 -height 7 -anchor nw -state disabled
    button .ce.palette.down -bitmap @$_pad(PadBitmaps)/color_arrowdown.xbm \
	-command "ce.nextpalette $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightthickness 0 \
	-padx 0 -pady 0 -width 14 -height 7 -anchor nw

    frame .ce.dummy2 -bg $_color(toolbg)

			# Widgets for setting colors
    frame .ce.f -bg $_color(toolbg)
    frame .ce.f.patch -bg $_color(toolbg)
    label .ce.f.patch.label -text "Current Color:" -pady 0 \
	-font $_font(tools) \
	-bg $_color(toolbg)
    entry .ce.f.patch.value -width 7 \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
    bind .ce.f.patch.value <KeyPress> {
	%W config -fg red
    }
    bind .ce.f.patch.value <BackSpace> {
	%W config -fg red
	%W delete [expr [%W index insert] -1]
    }
    bind .ce.f.patch.value <Delete> {
	%W config -fg red
	%W delete [expr [%W index insert] -1]
    }
    bind .ce.f.patch.value <Leave> {
	%W config -fg black
	catch {update.color %W "" [%W get]}
    }
    bind .ce.f.patch.value <Key-Return> {
	%W config -fg black
	catch {update.color %W "" [%W get]}
    }
    button .ce.f.patch.none -text "None" -command "update.color $PAD none none" -padx 4 -pady 0 \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    frame .ce.f.patch.frame -bg "black"
    frame .ce.f.patch.frame.color -bg $_color(toolbg) -width 40
    frame .ce.f.dummy3 -bg $_color(toolbg)

    frame .ce.f.current -bg $_color(toolbg)
    frame .ce.f.current.buttons -bg $_color(toolbg)
    frame .ce.f.current.rect -bg $_color(toolbg)
    frame .ce.f.current.rect.pen -bg [Pad_Color $PAD $_pad(PenColor)] -relief raised -borderwidth 2
    frame .ce.f.current.rect.pen.fill -bg [Pad_Color $PAD $_pad(FillColor)] -borderwidth 2

    bind .ce.f.current.rect.pen <Enter> {
	.ce.f.current.rect.pen.fill config -relief sunken
    }
    bind .ce.f.current.rect.pen <Leave> {
	.ce.f.current.rect.pen.fill config -relief flat
    }
    bind .ce.f.current.rect.pen <ButtonPress> "update.color $PAD \"\" \[lindex \[%W config -bg\] 4\]"

    bind .ce.f.current.rect.pen.fill <Enter> {
	%W config -relief raised
	.ce.f.current.rect.pen config -relief sunken
    }
    bind .ce.f.current.rect.pen.fill <Leave> {
	%W config -relief flat
	.ce.f.current.rect.pen config -relief raised
    }
    bind .ce.f.current.rect.pen.fill <ButtonPress> "update.color $PAD \"\" \[lindex \[%W config -bg\] 4\]"

    label .ce.f.current.buttons.change -text "Change:" -anchor n \
	-font $_font(tools) \
	-bg $_color(toolbg) 
    frame .ce.f.current.buttons.colors -bg $_color(toolbg)
    button .ce.f.current.buttons.colors.pen \
	-font $_font(tools) \
	-command "setcolor $PAD \$ce(color) pen" \
	-text "Pen" -padx 4 -pady 0 -width 4 -anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    button .ce.f.current.buttons.colors.fill \
	-font $_font(tools) \
	-command "setcolor $PAD \$ce(color) fill" \
	-text "Fill" -padx 4 -pady 0 -width 4 -anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    button .ce.f.current.buttons.colors.bgnd \
	-font $_font(tools) \
	-command "$PAD config -bg \$ce(color)" \
	-text "Bgnd" -padx 4 -pady 0 -width 4 -anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    frame .ce.dummy4 -bg $_color(toolbg)
    frame .ce.buttons -bg $_color(toolbg)
    button .ce.buttons.revert -command "ce.revert $PAD" -text "Revert" -pady 0\
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    button .ce.buttons.save -command {destroy .ce} -text "Close" -pady 0 \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    pack .ce.scales
    pack .ce.dummy1     -ipady 1 -fill x
    pack .ce.palette    -ipady 0 -expand t -fill both

    pack .ce.palette.copy -side left -expand 0 -fill y -anchor nw

    for {set i 0} {$i < 16} {incr i} {
	pack .ce.palette.c$i -side left -expand t -fill both
    }

    pack .ce.palette.up -side top -expand 0 -fill y -anchor nw
    pack .ce.palette.down -side bottom -expand 0 -fill y -anchor nw

    pack .ce.dummy2 -ipady 1 -fill x
    pack .ce.f -expand t -fill both
    pack .ce.f.patch -expand t -fill both
    pack .ce.f.patch.none -side left
    pack .ce.f.patch.label -side left -fill y
    pack .ce.f.patch.value -side left -fill y
    pack .ce.f.patch.frame -side left -expand t -fill both
    pack .ce.f.patch.frame.color -side left -expand t -fill both
    pack .ce.f.dummy3 -ipady 1 -fill x

    pack .ce.f.current -expand t -fill both
    pack .ce.f.current.buttons -side left -fill both
    pack .ce.f.current.buttons.change -side left -fill y -pady 2
    pack .ce.f.current.buttons.colors -side left -fill y
    pack .ce.f.current.buttons.colors.pen -side top -fill y
    pack .ce.f.current.buttons.colors.fill -side top -fill y
    pack .ce.f.current.buttons.colors.bgnd -side top -fill y
    pack .ce.f.current.rect -side right -expand t -fill both
    pack .ce.f.current.rect.pen -expand t -fill both -padx 5
    pack .ce.f.current.rect.pen.fill -expand t -fill both -padx 5 -pady 5

    pack .ce.dummy4 -ipady 1 -fill x
    pack .ce.buttons -fill x
    pack .ce.buttons.revert -side left -expand t -fill x
    pack .ce.buttons.save -side right -expand t -fill x

    pad_ManageWindow .ce 0
    wm minsize .ce 20 20
    wm title .ce "Pad++ Color Editor"

    update.color $PAD red $ce(red)
}

#
# Copy the palette to the drawing tools or to the pad surface
#
proc ce.copypalette {PAD x y} {
    global _color _font

    if {![winfo exists .copypalette]} {
	menu .copypalette \
	    -font $_font(tools) \
	    -bg $_color(menubg) -tearoff 0 \
	    -activebackground $_color(menuactivebg)
	.copypalette add command \
	    -label {Copy to Drawing Tools} \
	    -command "drawColorPalette $PAD"
	.copypalette add command \
	    -label {Copy to Pad++ surface} \
	    -command "ce.localpalette $PAD"
    }

    tk_popup .copypalette $x $y
}

#
# Make a simple local tool out of the palette
#
proc ce.localpalette {PAD} {
    global _color _pad ce env

		# If palette doesn't exist, then make one
    set palette [$PAD find withtag "colorpalette"]
    set created 0
    if {$palette == ""} {

	if {![info exists ce(palette)]} {
	    set ce(palette_index) 0
	    set ce(palette) [lindex $_color(Palettes) $ce(palette_index)]
	}

	set created 1
	$PAD bind colorpalette <Enter> "updateStatusText $PAD {Set pen color} 1"
	$PAD bind colorpalette <Leave> "updateStatusText $PAD {} 0"

		# Create rectangles and place them in a grid
	set grid [$PAD create grid]
	set row 0
	set col 0
	for {set i 0} {$i < 16} {incr i} {
	    set rect [$PAD create rectangle 0 0 15 15 -penwidth 0 -tags "colorpaletteentry colorpalette$i"]
	    $PAD grid $rect -in $grid -row $row -column $col

	    if {$col == 7} {
		set col 0
		incr row
	    } else {
		incr col
	    }
	}
	$PAD grid arrange $grid
	$PAD ic $grid -anchor sw -place "0 0 1"
					
		# Add packaging around palette (rectangle, label, and close button)
	set handle [$PAD create rectangle -2 -2 122 55 -penwidth 2 -pen black -fill gray80 \
			-joinstyle miter -tags "drag"]
	set _pad($handle.upselect) -1

	set label [$PAD create text -text "Color Palette" -font "Times-10" -tags "drag" \
		       -anchor nw -place "17 52 1"]
	set _pad($label.upselect) -1

	set close [$PAD create line 0 0 10 0 10 10 0 10 0 0 10 10 0 10 10 0 \
		       -pen black -penwidth 0 -joinstyle miter -capstyle butt]
	$PAD ic $close -place "7 44 1"

	$PAD create group -members "$handle $label $grid $close" -tags "colorpalette pen" -sticky z

		# Add current color indicator with bindings for selecting
		# fill vs. pen
	$PAD create rectangle 98 37 119 51 -tags "colorpalettecurrent" -fill $_pad(FillColor) -pen $_pad(PenColor)\
	    -penwidth 3 -joinstyle miter
	$PAD addgroupmember colorpalettecurrent colorpalette

		# Make color palette get created at the right size independent of zoom
	$PAD ic colorpalette -z [expr 1.0 / [$PAD getzoom]]

		# Make event bindings for color palette
	$PAD bind $close <ButtonPress-1> {%W delete colorpalette}
	$PAD bind $handle <ButtonPress-1> {%W raise colorpalette}
        $PAD bind $label <ButtonPress-1> {%W raise colorpalette}
	$PAD bind colorpaletteentry <ButtonPress-1> {
	    if {[%W hastag "pen" colorpalette]} {
		setcolor %W [%W ic %O -fill] pen
	    } else {
		setcolor %W [%W ic %O -fill] fill
	    }
	    break
	}
	$PAD bind colorpaletteentry <B1-Motion> {break}
	$PAD bind colorpaletteentry <ButtonRelease-1> {break}

	$PAD bind colorpalettecurrent <ButtonPress-1> {
	    if {[%W hastag "pen" colorpalette]} {
		%W deletetag "pen" colorpalette
		%W addtag "fill" colorpalette
		pad_flash %W colorpalettecurrent fill
	    } else {
		%W deletetag "fill" colorpalette
		%W addtag "pen" colorpalette
		pad_flash %W colorpalettecurrent pen
	    }
	    break
	}
	$PAD bind colorpalettecurrent <B1-Motion> {break}
	$PAD bind colorpalettecurrent <ButtonRelease-1> {break}

				# Don't want color palette to get written out
	$PAD bind colorpalette <Write> {set Pad_Write 0; return ""}
    }

		# Set palette to current drawing palette's colors
    for {set i 0} {$i < 16} {incr i} {
        $PAD ic colorpalette$i -fill [lindex $_color($ce(palette)) $i] -pen none
    }
		# Put palette in a reasonable place (center of screen - normal size)
    set view [$PAD getview]
    set place [$PAD ic colorpalette -place]
    set dx [expr [lindex $view 0] - [lindex $place 0]]
    set dy [expr [lindex $view 1] - [lindex $place 1]]
    if {$created} {
	set speed 0
    } else {
	set speed $_pad(AnimationSpeed)
    }
    $PAD raise colorpalette
    $PAD slide colorpalette $dx $dy $speed
}

#
# Change to the next palette if there is one
#
proc ce.nextpalette {PAD} {
    global ce _color

				# Access the next palette
    set palettes [llength $_color(Palettes)]
    if {![info exists ce(palette_index)]} {
	set ce(palette_index) 0
    } else {
	incr ce(palette_index)
	if {$ce(palette_index) >= $palettes} {
	    set ce(palette_index) [expr $palettes - 1]
	    return
	}
    }

				# Make arrow en/disabled appropriately
    .ce.palette.up config -state normal
    if {$ce(palette_index) >= [expr $palettes - 1]} {
	.ce.palette.down config -state disabled
    }

				# Set palette to new colors
    set ce(palette) [lindex $_color(Palettes) $ce(palette_index)]
    for {set i 0} {$i < 16} {incr i} {
	.ce.palette.c$i config -bg [lindex $_color($ce(palette)) $i]
    }
}

#
# Change to the previous palette if there is one
#
proc ce.prevpalette {PAD} {
    global ce _color

				# Access the previous palette
    set palettes [llength $_color(Palettes)]
    if {![info exists ce(palette_index)]} {
	set ce(palette_index) 0
    } else {
	incr ce(palette_index) -1
	if {$ce(palette_index) < 0} {
	    set ce(palette_index) 0
	    return
	}
    }

				# Make arrow en/disabled appropriately
    .ce.palette.down config -state normal
    if {$ce(palette_index) == 0} {
	.ce.palette.up config -state disabled
    }

				# Set palette to new colors
    set ce(palette) [lindex $_color(Palettes) $ce(palette_index)]
    for {set i 0} {$i < 16} {incr i} {
	.ce.palette.c$i config -bg [lindex $_color($ce(palette)) $i]
    }
}

proc ce.revert {PAD} {
    global _pad ce

    set _pad(FillColor) $ce(oldFill)
    set _pad(PenColor) $ce(oldPen)
    $PAD config -bg $ce(oldBgnd)

    set ce(red) [expr 0x[string range $_pad(FillColor) 1 2]]
    set ce(green) [expr 0x[string range $_pad(FillColor) 3 4]]
    set ce(blue) [expr 0x[string range $_pad(FillColor) 5 6]]
    if {[winfo exists .ce]} {rgb.changed $PAD fill}

    set ce(red) [expr 0x[string range $_pad(PenColor) 1 2]]
    set ce(green) [expr 0x[string range $_pad(PenColor) 3 4]]
    set ce(blue) [expr 0x[string range $_pad(PenColor) 5 6]]
    if {[winfo exists .ce]} {rgb.changed $PAD pen}
}

#
# Set the color of the specified items to the specified color.
# If items not specified, then apply to selected and focus items.
# Set "pen" or "fill" depending on style
# If applied to a group, then apply to group members
#
proc setcolor {PAD color {style "pen"} {tagOrId ""}} {
    global _pad

    if {$tagOrId == ""} {
	set ids "[pad_sel $PAD] [$PAD focus]"
    } else {
	set ids [$PAD find -groupmembers withtag $tagOrId]
    }

    update.color $PAD "" $color

    if {$style == "fill"} {
	set _pad(FillColor) $color
	if {[winfo exists .ce]} {
	    .ce.f.current.rect.pen.fill config -bg [Pad_Color $PAD $color]
	}
	if {[winfo exists .draw.color.pen.fill]} {
	    .draw.color.pen.fill config -bg [Pad_Color $PAD $color]
	}
	$PAD ic colorpalettecurrent -fill $color
				# Change fill color of objects
	foreach id $ids {
	    set type [$PAD type $id]
	    switch -exact $type {
		rectangle -
		polygon   -
		oval      -
		button    -
		scrollbar -
		label     -
		textfield -
		textarea  -
		frame     -
		portal    {$PAD itemconfig $id -fill $color}
		group     {
		    foreach member [$PAD ic $id -members] {
			setcolor $PAD $color $style $member
		    }
		}
	    }
	}
    }

    if {$style == "pen"} {
	set _pad(PenColor) $color
	if {[winfo exists .ce]} {
	    .ce.f.current.rect.pen config -bg [Pad_Color $PAD $color]
	    .ce.f.patch.frame.color config -bg [Pad_Color $PAD $color]
	}
	if {[winfo exists .draw.color.pen]} {
	    .draw.color.pen config -bg [Pad_Color $PAD $color]
	}
	$PAD ic colorpalettecurrent -pen $color
	foreach id $ids {
	    set type [$PAD type $id]
	    switch -exact $type {
		line      -
		spline    -
		text      -
		textfile  -
		textfield -
		textarea  -
		rectangle -
		polygon   -
		oval      -
		button    -
		label     -
		portal    {$PAD itemconfig $id -pen $color}
		group     {
		    foreach member [$PAD ic $id -members] {
			setcolor $PAD $color $style $member
		    }
		}
	    }
	}
    }
}

proc Pad_Color {PAD color} {
    if {$color == "none"} {
	if {[winfo exists .ce]} {
	    set color [lindex [.ce config -bg] 4]
	} else {
	    set color [lindex [$PAD config -bg] 4]
	}
    }
    
    return $color
}
