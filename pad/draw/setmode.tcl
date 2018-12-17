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

#
# Magnificaion Mode
#
proc setMagMode {PAD} {
    global _pad _color
   
  
    pad_set_cursor $PAD "@$_pad(PadBitmaps)/mag_in_glass.xbm black" "mag_in_glass"
    if {[winfo exists .draw]} {
        .draw.modes.mag config -background $_color(toolselbg) -relief sunken
    }

    $PAD modifier set Mag
}

proc unsetMagMode {PAD} {
}

proc setPanMode {PAD} {
    global _pad _color

    if {[pad_sel $PAD] != ""} {
        pad_unselect $PAD all
    }

    pad_set_cursor $PAD "@$_pad(PadBitmaps)/hand.xbm $_pad(PadBitmaps)/handmask.xbm black white" "hand"
    if {[winfo exists .draw]} {
        .draw.modes.hand config -background $_color(toolselbg) -relief sunken
    }

    $PAD modifier set Run
}

proc unsetPanMode {PAD} {
}

proc setSelectMode {PAD} {
    global _pad _color

    $PAD config -cursor [pad_pointer_cursor $PAD]
    
    if {[winfo exists .draw]} {
        .draw.modes.pointer config -background $_color(toolselbg) -relief sunken
    }

    $PAD modifier set Select
}

proc unsetSelectMode {PAD} {
    pad_unselect $PAD all
    pad_StopReshaping $PAD
    pad_StopRotating $PAD
}

proc setPickMode {PAD} {
    global _pad _color

    if {[$PAD find withtag selected] != ""} {
        pad_unselect $PAD all
    }
    if {[winfo exists .draw]} {
        .draw.modes.pick config -background $_color(toolselbg) -relief sunken
    }
    
    pad_set_cursor $PAD "@$_pad(PadBitmaps)/pick.xbm black" "pick"
    $PAD modifier set Pick
}

proc unsetPickMode {PAD} {
}

proc setDrawMode {PAD} {
    global _pad _color _draw

    if {[$PAD find withtag selected] != ""} {
        pad_unselect $PAD all
    }
   
    pad_set_cursor $PAD "@$_pad(PadBitmaps)/pencil.xbm $_pad(PadBitmaps)/pencilmask.xbm black white" "padpencil"

    if {[winfo exists .draw]} {
        .draw.modes.drawing config -background $_color(toolselbg) -relief sunken
    }

    set _draw(id) ""
    $PAD modifier set Draw
}

proc unsetDrawMode {PAD} {
}

#
# Text mode
#
proc setTextMode {PAD} {
    global _pad _color
    if {[$PAD find withtag selected] != ""} {
        pad_unselect $PAD all
    }
    $PAD config -cursor xterm
    if {[winfo exists .draw]} {
        .draw.modes.text config -background $_color(toolselbg) -relief sunken
    }

    if {[info exists _text(buffer)]} {
        updateBufferText $PAD $_text(buffer)
    }
    $PAD modifier set Text
}

proc unsetTextMode {PAD} {
    global _text
    
    $PAD delete mark
    $PAD delete textbgnd
    set _text(markIndex) ""
    updateBufferText $PAD ""
}

#
# Line drawing mode
#
proc setLineMode {PAD} {
    global _pad _color
    global _poly _multiline

    if {[$PAD find withtag selected] != ""} {
        pad_unselect $PAD all
    }

    pad_set_cursor $PAD "@$_pad(PadBitmaps)/crosshair.xbm black" "crosshair"
    if {[winfo exists .draw]} {
        .draw.modes.line config -background $_color(toolselbg) -relief sunken
    }

    set line(id) ""
    $PAD modifier set Line
}

proc unsetLineMode {PAD} {
}

#
# Multiple line drawing mode
#
proc setMultiLineMode {PAD} {
    global _pad _color
    global _poly _multiline
   
    if {[$PAD find withtag selected] != ""} {
        pad_unselect $PAD all
    }

    pad_set_cursor $PAD "@$_pad(PadBitmaps)/crosshair.xbm black" "crosshair"

    if {[winfo exists .draw]} {
        .draw.modes.multiline config -background $_color(toolselbg) -relief sunken
    }

    set _multiline(id) ""
    $PAD modifier set MultiLine
}

proc unsetMultiLineMode {PAD} {
}

#
# Rectangle drawing mode
#
proc setRectangleMode {PAD} {
    global _pad _color _rect

    if {[$PAD find withtag selected] != ""} {
        pad_unselect $PAD all
    }

    pad_set_cursor $PAD "@$_pad(PadBitmaps)/crosshair.xbm black" "crosshair"

    if {[winfo exists .draw]} {
	if { $_rect(fill) == 0 } {
            .draw.modes.rect config -background $_color(toolselbg) -relief sunken
        } else {
            .draw.modes.filledRect config -background $_color(toolselbg) -relief sunken
	}
    }

    $PAD modifier set Rectangle
}

proc unsetRectangleMode {PAD} {
}

#
# Oval drawing mode
#
proc setOvalMode {PAD} {
    global _pad _color _oval

    if {[$PAD find withtag selected] != ""} {
        pad_unselect $PAD all
    }
   
    pad_set_cursor $PAD "@$_pad(PadBitmaps)/crosshair.xbm black" "crosshair"
    
    if {[winfo exists .draw]} {
	if {![info exists _oval(fill)]} {
	    set _oval(fill) 0
	}
	if { $_oval(fill) == 0 } {
            .draw.modes.oval config -background $_color(toolselbg) -relief sunken
        } else {
            .draw.modes.filledOval config -background $_color(toolselbg) -relief sunken
	}
    }

    $PAD modifier set Oval
}

proc unsetOvalMode {PAD} {
}

#
# Polygon drawing mode
#
proc setPolygonMode {PAD} {
    global _pad _color _poly
    global _poly _multiline

    if {[$PAD find withtag selected] != ""} {
        pad_unselect $PAD all
    }

    pad_set_cursor $PAD "@$_pad(PadBitmaps)/crosshair.xbm black" "crosshair"
   
    if {[winfo exists .draw]} {
	if { $_poly(fill) == 0 } {
            .draw.modes.poly config -background $_color(toolselbg) -relief sunken
        } else {
            .draw.modes.filledPoly config -background $_color(toolselbg) -relief sunken
	}
    }

    set _poly(id) {}
    $PAD modifier set Polygon
}

proc unsetPolygonMode {PAD} {
}

#
# Portal mode
#
proc setPortalMode {PAD} {
    global _pad _color

    if {[$PAD find withtag selected] != ""} {
        pad_unselect $PAD all
    }

    pad_set_cursor $PAD "@$_pad(PadBitmaps)/portalpencil.xbm black" "portalpencil"
   

    if {[winfo exists .draw]} {
        .draw.modes.portal config -background $_color(toolselbg) -relief sunken
    }

    $PAD modifier set Portal
}

proc unsetPortalMode {PAD} {
}

#
# Hyperlink mode
#
proc setHyperLinkMode {PAD} {
    global _pad _color

    if {[$PAD find withtag selected] != ""} {
        pad_unselect $PAD all
    }

    pad_set_cursor $PAD "@$_pad(PadBitmaps)/hyperlink.xbm black" "hyperlink"  

    if {[winfo exists .draw]} {
        .draw.modes.link config -background $_color(toolselbg) -relief sunken
    }

    $PAD modifier set LinkCreate

    if {$_pad(ShowLinks)} {
	link_ShowConnections $PAD
    }
}

proc unsetHyperLinkMode {PAD} {
    global _link

    $PAD delete linkconnect
    set _link(current) ""
}

proc setSelectHtmlMode {PAD} {
    $PAD modifier set SelectHtml
}

proc unsetSelectHtmlMode {PAD} {
    global _color
    
    set objs [$PAD find withtag htmlselected]
    foreach itemid $objs {
	$PAD ic $itemid -border $_color(htmlborder)
	$PAD deletetag htmlselected $itemid
    }
}

proc setSelectHtmlObjMode {PAD} {
    $PAD modifier set SelectHtmlObj
}

proc unsetSelectHtmlObjMode {PAD} {
    set objs [$PAD find withtag htmlselectedobj]
    foreach itemid $objs {
	set type [$PAD type $itemid]
	if {$type != "image"} {
	    $PAD ic $itemid -pen black
	}
	$PAD deletetag htmlselectedobj $itemid
    }
}

proc setSelectHtmlPageMode {PAD} {
    $PAD modifier set SelectHtmlPage
}

proc unsetSelectHtmlPageMode {PAD} {
    global _color

    set objs [$PAD find withtag htmlselectedpage]
    foreach itemid $objs {
	$PAD ic $itemid -border $_color(htmlborder)
	$PAD deletetag htmlselectedpage $itemid
    }
}

proc selectMode {PAD mode} {
    global _mode
    global _poly _multiline
    global _color

    pad_unlock_mode

				# Close up any open drawings
    if {$_poly(id) != ""} {
	polyEvent $PAD 1
    }
    if {$_multiline(id) != ""} {
	multilineEvent $PAD 1
    }
				# Delete any active snoops (property viewers)
    $PAD delete snoop

# Set focus to pad surface
    $PAD focus 1

    if {![info exists _mode]} { set _mode "Run" }
    if {[winfo exists .draw]} {
        foreach i [winfo children .draw.modes] {
	    if {[lindex [$i config -background] 4] != $_color(toolbg)} {
				# Catch because some children are frames
				# that shouldn't be set.
		catch {$i config -background $_color(toolbg) -relief raised -foreground black}
	    }
	}
    }
    $PAD modifier set ""
	        # Execute special code when leaving
                # certain modes
    switch $_mode {
	Select {
	    unsetSelectMode $PAD
	}
        Text {
	    unsetTextMode $PAD
        }
        LinkCreate {
	    unsetHyperLinkMode $PAD
        }
	Mag {
            unsetMagMode $PAD
        }
        Pan {
	    unsetPanMode $PAD
	}
	Pick {
	    unsetPickMode $PAD
	}
        SelectHtml {
	    unsetSelectHtmlMode $PAD
        }
        SelectHtmlObj {
	    unsetSelectHtmlObjMode $PAD
        }
        SelectHtmlPage {
	    unsetSelectHtmlPageMode $PAD
        }
    }

    set _mode $mode

    switch $_mode {
	Mag			{setMagMode    $PAD}
        Pan			{setPanMode    $PAD}
	Select			{setSelectMode $PAD}
	Pick			{setPickMode   $PAD}
	Draw			{setDrawMode   $PAD}
	Text			{setTextMode   $PAD}
	Line			{setLineMode   $PAD}
        MultiLine		{setMultiLineMode $PAD}
	Rectangle		{setRectangleMode $PAD}
	Oval			{setOvalMode      $PAD}
	Polygon			{setPolygonMode   $PAD}
	Portal			{setPortalMode    $PAD}
	LinkCreate		{setHyperLinkMode   $PAD}
        SelectHtml              {setSelectHtmlMode  $PAD}
        SelectHtmlObj           {setSelectHtmlObjMode  $PAD}
        SelectHtmlPage          {setSelectHtmlPageMode $PAD}
    }
}
