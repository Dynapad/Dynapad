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
# These colors are used to make the "radiobutton" style interface
# for the basic mode.  Each button is drawn with background set
# to one of these values which are taken from the option database
#

proc make.draw {PAD} {
    global geometry
    global _pad _oval _rotate env
    global _draw _color _font
    global _layer
    global tcl_platform

    set _draw(PAD) $PAD
    toplevel .draw -bg $_color(toolbg)
    wm resizable .draw 0 0

    bindDrawButtons $PAD
    
    frame .draw.options -bg $_color(toolbg)
    frame .draw.modes -bg $_color(toolbg)
    frame .draw.modes.left -bg $_color(toolbg)
    frame .draw.modes.right -bg $_color(toolbg)
    frame .draw.v1 -width 5 -bg $_color(toolbg)


#####################################################################################################
#####################################################################################################

#
# Panning mode
#
    button .draw.modes.hand -bitmap @$_pad(PadBitmaps)/hand.xbm \
	-command "selectMode $PAD Pan" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.hand {.draw.modes.hand tooltip draw_button .draw all}
    set _draw(help.draw.modes.hand) "Pan view\n(Control-key in any mode)"
    set _draw(msg.draw.modes.hand) {Pan view}
    set _draw(lock.draw.modes.hand) 0
#
# Select mode
#
    button .draw.modes.pointer -bitmap @$_pad(PadBitmaps)/pointer.xbm \
	-command "selectMode $PAD Select" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.pointer {.draw.modes.pointer tooltip draw_button .draw all}
    set _draw(help.draw.modes.pointer) "Select objects"
    set _draw(msg.draw.modes.pointer) {Select objects (shift key allows multiple selections)}
    set _draw(lock.draw.modes.pointer) 0
#
# Portal mode
#
    button .draw.modes.portal -bitmap @$_pad(PadBitmaps)/portal.xbm \
	-command "selectMode $PAD Portal" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.portal {.draw.modes.portal tooltip draw_button .draw all}
    set _draw(help.draw.modes.portal) "Portal\nDouble-click to lock"
    set _draw(msg.draw.modes.portal) {Portal (view to another place on the pad)}
    set _draw(lock.draw.modes.portal) 1
#
# Hyperlink mode
#
    button .draw.modes.link -bitmap @$_pad(PadBitmaps)/hyperlink.xbm \
	-command "selectMode $PAD LinkCreate" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.link {.draw.modes.link tooltip draw_button .draw all}
    set _draw(help.draw.modes.link) "Hyperlink"
    set _draw(msg.draw.modes.link) {Hyperlink (select source and then destination or spacebar)}
    set _draw(lock.draw.modes.link) 0
#
# Drawing mode
#
    button .draw.modes.drawing -bitmap @$_pad(PadBitmaps)/drawing.xbm \
	-command "selectMode $PAD Draw" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.drawing {.draw.modes.drawing tooltip draw_button .draw all}
    set _draw(help.draw.modes.drawing) "Free-hand drawing\nDouble-click to lock"
    set _draw(msg.draw.modes.drawing) {Free-hand drawing}
    set _draw(lock.draw.modes.drawing) 1
#
# Text mode
#
    button .draw.modes.text -bitmap @$_pad(PadBitmaps)/letter.xbm \
	-command "selectMode $PAD Text" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.text {.draw.modes.text tooltip draw_button .draw all}
    set _draw(help.draw.modes.text) "Text"
    set _draw(msg.draw.modes.text) {Text entry}
    set _draw(lock.draw.modes.text) 0
#
# Line drawing mode
#
    button .draw.modes.line -bitmap @$_pad(PadBitmaps)/line.xbm \
	-command "selectMode $PAD Line" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.line {.draw.modes.line tooltip draw_button .draw all}
    set _draw(help.draw.modes.line) "Line\nDouble-click to lock"
    set _draw(msg.draw.modes.line) {Straight line segment}
    set _draw(lock.draw.modes.line) 1
#
# Multiple line drawing mode
#
    button .draw.modes.multiline -bitmap @$_pad(PadBitmaps)/lines.xbm \
	-command "selectMode $PAD MultiLine" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.multiline {.draw.modes.multiline tooltip draw_button .draw all}
    set _draw(help.draw.modes.multiline) "Poly-line\nDouble-click to lock"
    set _draw(msg.draw.modes.multiline) {Multiple straight line segments}
    set _draw(lock.draw.modes.multiline) 1
#
# Rectangle drawing mode
#
    button .draw.modes.rect -bitmap @$_pad(PadBitmaps)/rect.xbm \
	-command "set _rect(fill) 0; selectMode $PAD Rectangle" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.rect {.draw.modes.rect tooltip draw_button .draw all}
    set _draw(help.draw.modes.rect) "Rectangle\nDouble-click to lock"
    set _draw(msg.draw.modes.rect) {Rectangle outline (shift key for squares)}
    set _draw(lock.draw.modes.rect) 1
#
# Filled rectangle drawing mode
#
    button .draw.modes.filledRect -bitmap @$_pad(PadBitmaps)/filledRect.xbm \
	-command "set _rect(fill) 1; selectMode $PAD Rectangle" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.filledRect {.draw.modes.filledRect tooltip draw_button .draw all}
    set _draw(help.draw.modes.filledRect) "Filled rectangle\nDouble-click to lock"
    set _draw(msg.draw.modes.filledRect) {Filled rectangle (shift key for squares)}
    set _draw(lock.draw.modes.filledRect) 1
#
# Oval drawing mode
#
    button .draw.modes.oval -bitmap @$_pad(PadBitmaps)/oval.xbm \
	-command "set _oval(fill) 0; selectMode $PAD Oval" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.oval {.draw.modes.oval tooltip draw_button .draw all}
    set _draw(help.draw.modes.oval) "Oval\nDouble-click to lock"
    set _draw(msg.draw.modes.oval) {Oval outline (shift key for circles)}
    set _draw(lock.draw.modes.oval) 1
#
# Filled oval drawing mode
#
    button .draw.modes.filledOval -bitmap @$_pad(PadBitmaps)/filledOval.xbm \
	-command "set _oval(fill) 1; selectMode $PAD Oval" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.filledOval {.draw.modes.filledOval tooltip draw_button .draw all}
    set _draw(help.draw.modes.filledOval) "Filled oval\nDouble-click to lock"
    set _draw(msg.draw.modes.filledOval) {Filled oval (shift key for circles)}
    set _draw(lock.draw.modes.filledOval) 1
#
# Polygon drawing mode
#
    button .draw.modes.poly -bitmap @$_pad(PadBitmaps)/polygon.xbm \
	-command "set _poly(fill) 0; selectMode $PAD Polygon" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.poly {.draw.modes.poly tooltip draw_button .draw all}
    set _draw(help.draw.modes.poly) "Polygon\nDouble-click to lock"
    set _draw(msg.draw.modes.poly) {Polygon outline}
    set _draw(lock.draw.modes.poly) 1
#
# Filled polygon drawing mode
#
    button .draw.modes.filledPoly -bitmap @$_pad(PadBitmaps)/filledPolygon.xbm \
	-command "set _poly(fill) 1; selectMode $PAD Polygon" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.filledPoly {.draw.modes.filledPoly tooltip draw_button .draw all}
    set _draw(help.draw.modes.filledPoly) "Filled polygon\nDouble-click to lock"
    set _draw(msg.draw.modes.filledPoly) {Filled polygon}
    set _draw(lock.draw.modes.filledPoly) 1
#
# Color picker
#
    button .draw.modes.pick -bitmap @$_pad(PadBitmaps)/pick.xbm \
	-command "selectMode $PAD Pick" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.pick {.draw.modes.pick tooltip draw_button .draw all}
    set _draw(help.draw.modes.pick) "Pick color of object"
    set _draw(msg.draw.modes.pick) {Pick color of object}
    set _draw(lock.draw.modes.pick) 0
#
# Magnification mode
#
    button .draw.modes.mag -bitmap @$_pad(PadBitmaps)/mag.xbm \
	-command "selectMode $PAD Mag" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    bindtags .draw.modes.mag {.draw.modes.mag tooltip draw_button .draw all}
    set _draw(help.draw.modes.mag) "Magnification\nAlt-key in any mode"
    set _draw(msg.draw.modes.mag) {Magnification (click to zoom in, shift-click to zoom out, or draw zoom box)}
    set _draw(lock.draw.modes.mag) 0

################################################################################################
################################################################################################

#
# Indicator of current drawing colors
#
    frame .draw.color -bg $_color(toolbg)
    frame .draw.color.pen -bg [Pad_Color $PAD $_pad(PenColor)] -borderwidth 2 -relief raised
    frame .draw.color.pen.fill -bg [Pad_Color $PAD $_pad(FillColor)] -height 15
    set bg [lindex [.draw.modes config -bg] 4]
    pack .draw.color.pen -fill x
    pack .draw.color.pen.fill -fill x -padx 4 -pady 4
    bind .draw.color <Enter> "updateStatusText $PAD {Pen and fill colors} 1"
    bind .draw.color <Leave> "updateStatusText $PAD {} 0"
    bind .draw.color.pen <Enter> "updateStatusText $PAD {Pen and fill colors} 1"
    bind .draw.color.pen <Leave> "updateStatusText $PAD {} 0"
    bind .draw.color.pen.fill <Enter> "updateStatusText $PAD {Pen and fill colors} 1"
    bind .draw.color.pen.fill <Leave> "updateStatusText $PAD {} 0"
    bind .draw.color.pen <ButtonPress> "
        set menu(.ce) 1
        controlWindow $PAD .ce
    "
    bind .draw.color.pen.fill <ButtonPress> "
        set menu(.ce) 1
        controlWindow $PAD .ce
    "

#
# Pen width 
#
    set pw .draw.penwidth
    pad $pw -width 30 -height 30 -bg $bg -defaultEventHandlers 0
    bind $pw <Configure> {
	set w [lindex [%W config -width] 4]
	set h [lindex [%W config -height] 4]
	%W moveto [expr 0.5 * $w] [expr 0.5 * $h] 1.0
    }

				# Draw arrow
    $pw create polygon 0 0 5 5 -5 5 0 0 0 15 -tags pointer -fill black -anchor s
    setDrawingWidth $PAD $_pad(PenWidth)
				# Draw lines of different widths
    set x1 0
    set x2 0
    for {set i 0} {$i < 7} {incr i} {
	$pw create rectangle $x1 1 $x2 15 -fill black
	set x1 [expr $x2 + 4]
	set x2 [expr int($x1 * 1.5)]
    }

    bind $pw <Enter> "updateStatusText $PAD {Pen width} 1"
    bind $pw <Leave> "updateStatusText $PAD {} 0"

    $pw bind all <Enter> {%W ic pointer -pen red -fill red}
    $pw bind all <B1-Enter> {# Don't want to change color when button down}
    $pw bind all <Leave> {%W ic pointer -pen white -fill white}
    $pw bind all <B1-Leave> {# Don't want to change color when button down}
    $pw bind all <ButtonPress> "
        setDrawingWidth $PAD \[expr 0.5 * %i\]
        updateObjectWidths $PAD
    "
    $pw bind all <B1-Motion> "
        setDrawingWidth $PAD \[expr 0.5 * %i\]
        updateObjectWidths $PAD
    "

#
# Close button
#
    button .draw.close -text Close -command {destroy .draw} \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

################################################################################################
#
# Layout interface
#
################################################################################################


    frame .draw.modes.d1 -height 5 -bg $_color(toolbg)
    frame .draw.modes.d2 -height 5 -bg $_color(toolbg)

    pack .draw.modes.left -side left
    pack .draw.modes.right -side right
    pack .draw.modes.hand .draw.modes.portal .draw.modes.d1 .draw.modes.drawing\
        .draw.modes.line .draw.modes.rect .draw.modes.oval .draw.modes.poly \
        .draw.modes.pick -in .draw.modes.left
    pack .draw.modes.pointer .draw.modes.link .draw.modes.d2 .draw.modes.text \
        .draw.modes.multiline .draw.modes.filledRect .draw.modes.filledOval \
        .draw.modes.filledPoly .draw.modes.mag -in .draw.modes.right

    pack .draw.modes -side top -fill y

    pack .draw.color -side top -fill x
    pack .draw.penwidth -side top -fill x -expand t
    pack .draw.close -side top -fill x

    global _mode
    if {[info exists _mode]} {
	switch $_mode {
	    Mag {.draw.modes.mag config -bg $_color(toolselbg) -relief sunken}
	    Pan {.draw.modes.hand config -bg $_color(toolselbg) -relief sunken}
	    Select {.draw.modes.pointer config -bg $_color(toolselbg) -relief sunken}
	    Pick {.draw.modes.pick config -bg $_color(toolselbg) -relief sunken}
	    Draw {.draw.modes.drawing config -bg $_color(toolselbg) -relief sunken}
	    Text {.draw.modes.text config -bg $_color(toolselbg) -relief sunken}
	    Line {.draw.modes.line config -bg $_color(toolselbg) -relief sunken}
	    MultiLine {.draw.modes.multiline config -bg $_color(toolselbg) -relief sunken}
	    Rectangle {
		if {![info exists _rect(fill)]} {
		    set _rect(fill) 0
		}
		if {$_rect(fill)} {
		    .draw.modes.rect config -bg $_color(toolselbg) -relief sunken
		} else {
		    .draw.modes.filledRect config -bg $_color(toolselbg) -relief sunken
		}
	    }
	    Oval {
		if {![info exists _oval(fill)]} {
		    set _oval(fill) 0
		}
		if {$_oval(fill)} {
		    .draw.modes.oval config -bg $_color(toolselbg) -relief sunken
		} else {
		    .draw.modes.filledOval config -bg $_color(toolselbg) -relief sunken
		}
	    }
	    Polygon {
		if {![info exists _poly(fill)]} {
		    set _poly(fill) 0
		}
		if {$_poly(fill)} {
		    .draw.modes.poly config -bg $_color(toolselbg) -relief sunken
		} else {
		    .draw.modes.filledPoly config -bg $_color(toolselbg) -relief sunken
		}
	    }
	    Portal {.draw.modes.portal config -bg $_color(toolselbg) -relief sunken}
	    demoImage {.draw.modes.demoImage config -bg $_color(toolselbg) -relief sunken}
	    LinkCreate {.draw.modes.link config -bg $_color(toolselbg) -relief sunken}
	}
    } else {
	.draw.modes.hand config -bg $_color(toolselbg) -relief sunken
    }


################################################################################################

    pad_ManageWindow .draw
    wm iconname .draw "Pad++"

    update
				# On windows, the skinny drawing window can't be resized,
				# so make it fatter so it can be resized.
    if {$tcl_platform(platform) == "windows"} {
        wm geometry .draw "100x[winfo height .draw]"
    }

				# Update penwidth drawing based un
				# actual size of widget
    set w [lindex [.draw.penwidth config -width] 4]
    set h [lindex [.draw.penwidth config -height] 4]
    .draw.penwidth moveto [expr 0.5 * $w] [expr 0.5 * $h] 1.0
}

#
# setDrawingWidth
#
#   Set the system drawing width to the specified value in pixels.
#   If the penwidth tool is visible, then update it as well.
#
proc setDrawingWidth {PAD value} {
    global geometry _pad

    if {$value < 0} {
	set value 0
    }
    set _pad(PenWidth) [max 1 $value]

    if {[winfo exists .draw.penwidth]} {
	set width [lindex [.draw.penwidth config -width] 4]
	set height [lindex [.draw.penwidth config -height] 4]

				# Don't draw pointer past edge of tool window
	if {$value >= [expr 0.5 * $width]} {
	    set value [expr (0.5 * $width) - 0.5]
	}
	set x [expr 2.0 * $value]
        .draw.penwidth ic pointer -place "$x 16 1"
    }
}

#
# Change the width of all objects on the specied pad surface
# to the current drawing width.
#
proc updateObjectWidths {PAD} {
    global _pad

    set objs [pad_sel $PAD]
    set zoom [$PAD getzoom]
    foreach obj $objs {
	set type [$PAD type $obj]
	set size [$PAD ic $obj -z]
	if {($type == "line") ||
	    ($type == "rectangle") ||
	    ($type == "oval") ||
	    ($type == "spline") ||
	    ($type == "polygon")} {
		$PAD ic $obj -penwidth [expr $_pad(PenWidth) / ($size * $zoom)]
	    } elseif {($type == "portal")} {
		$PAD ic $obj -borderwidth [expr $_pad(PenWidth) / ($size * $zoom)]
	    }

	if {[lmember [$PAD gettags $obj] selected]} {
	    pad_unselect $PAD $obj
	    pad_select $PAD $obj
	}
    }
}

#
# Indicator of color palette
# Can only be called after the color palette has been created
#
proc drawColorPalette {PAD} {
    global _color ce

    if {![winfo exists .draw]} {
	return
    }

    if {![winfo exists .draw.palette]} {
	frame .draw.palette
	frame .draw.palette.r1 -borderwidth 0
	frame .draw.palette.r2 -borderwidth 0
	pack .draw.palette -expand 1 -fill both -after .draw.color
	pack .draw.palette.r1 -expand 1 -fill both
	pack .draw.palette.r2 -expand 1 -fill both

	bind .draw.palette <Enter> "updateStatusText $PAD {Set pen color} 1"
	bind .draw.palette <Leave> "updateStatusText $PAD {} 0"

	for {set i 0} {$i < 16} {incr i} {
	    if {$i < 8} {
		set row "r1"
	    } else {
		set row "r2"
	    }
	    frame .draw.palette.$row.c$i -width 10 -height 15
	    bind .draw.palette.$row.c$i <ButtonPress-1> "setcolor $PAD \[%W cget -bg\] pen"
	    pack .draw.palette.$row.c$i -expand 1 -fill both -side left
	}
    } else {
	pack .draw.palette -after .draw.color
    }

    for {set i 0} {$i < 16} {incr i} {
	if {$i < 8} {
	    set row "r1"
	} else {
	    set row "r2"
	}
	.draw.palette.$row.c$i config -bg [lindex $_color($ce(palette)) $i]
    }
}

################################################################################
################################################################################

#
# Define event bindings for drawing palette buttons
#
proc bindDrawButtons {PAD} {
    bind tooltip <Enter> "draw_butEnter $PAD %W; break"
    bind tooltip <Leave> "draw_butLeave $PAD %W; break"
    bind draw_button <ButtonPress-1> {draw_butDown %W; break}
    bind draw_button <ButtonRelease-1> {draw_butUp %W; break}
    bind draw_button <Double-ButtonRelease-1> {
	draw_butUp %W
	if {$_draw(lock%W)} {
	    pad_lock_mode %W
	}
	break
    }
}

#
# Mouse has entered Draw buton
#
proc draw_butEnter {PAD w {text ""}} {
    global tkPriv _draw _pad

    if {[info exists _draw(msg$w)]} {
	set text $_draw(msg$w)
    }

    if {$text == ""} {
	set emphasizebg 0
    } else {
	set emphasizebg 1
    }
    updateStatusText $PAD $text $emphasizebg

    if {[$w cget -state] != "disabled"} {
	$w config -state active
	if {$tkPriv(buttonWindow) == $w} {
	    $w configure -state active -relief sunken
	}
    }
    set tkPriv(window) $w

    if {$_pad(Help)} {
	set _draw(help_timer) [after 750 draw_butHelp $w]
    }
}

#
# Mouse has left Draw buton
#
proc draw_butLeave {PAD w} {
    global tkPriv _draw

    set emphasizebg 0
    updateStatusText $PAD "" $emphasizebg

    if {[$w cget -state] != "disabled"} {
	$w config -state normal
    }
    set tkPriv(window) ""

    if {[info exists _draw(help_timer)]} {
	after cancel $_draw(help_timer)
	unset _draw(help_timer)
    } else {
	if {[winfo exists .draw_help]} {
	    destroy .draw_help
	}
    }
}

#
# Mouse has pressed on Draw button
#
proc draw_butDown {w} {
    global tkPriv _color

    global tkPriv
    set tkPriv(relief) [lindex [$w config -relief] 4]
    if {[$w cget -state] != "disabled"} {
	set tkPriv(buttonWindow) $w
	$w config -relief sunken -bg $_color(toolselbg) -state normal
	uplevel #0 [list $w invoke]
    }
}

#
# Mouse has released from Draw button
#
proc draw_butUp {w} {
    global tkPriv _color

    if {$w == $tkPriv(buttonWindow)} {
	set tkPriv(buttonWindow) ""
    }
}

#
# Gets called to post help for specified window (if available)
#
proc draw_butHelp {w} {
    global _draw _font

    unset _draw(help_timer)
    if {[info exists _draw(help$w)]} {
	set multiline [regexp "(.*)\n(.*)" $_draw(help$w) dummy line1 line2]
					# Create window if it doesn't already exist
	menu .draw_help -bg yellow -tearoff 0 \
	    -font $_font(tools)
	if {$multiline} {
	    .draw_help add command -label $line1
	    .draw_help add command -label $line2
	} else {
	    .draw_help add command -label $_draw(help$w)
	}

	.draw_help post [expr [winfo rootx $w] + ([winfo width $w] / 2)] [expr [winfo rooty $w] + [winfo height $w] + 3]
    }
}

