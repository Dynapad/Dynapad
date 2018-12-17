# "(c) Copyright 1993-1995 Pad++ Consortium {University of New Mexico (UNM),
# New York University (NYU), and Bell Communications Research (Bellcore)},
# All Rights Reserved."  Licensee can not remove or obscure any of the
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
# checkButton initiatialization routine
# Needs to be called before checkbutton can be created
#
proc pad_CheckbuttonInit {PAD} {
    global env _checkbutton _widget
                                # Create checkbutton types and options
    $PAD addtype checkbutton pad_Checkbutton

    $PAD addoption checkbutton -activefill  "pad_CheckbuttonConfig -activefill"  #6cb2cc
    $PAD addoption checkbutton -borderwidth "pad_CheckbuttonConfig -borderwidth" #0.5
    $PAD addoption checkbutton -boxfill     "pad_CheckbuttonConfig -boxfill" red2
    $PAD addoption checkbutton -command     "pad_CheckbuttonConfig -command" ""
    $PAD addoption checkbutton -fill        "pad_CheckbuttonConfig -fill"   \#4c92aa
    $PAD addoption checkbutton -font        "pad_CheckbuttonConfig -font"   $_widget(font)
    $PAD addoption checkbutton -height      "pad_CheckbuttonConfig -height" 0
    $PAD addoption checkbutton -help        "pad_CheckbuttonConfig -help" \
                                          "This is the default checkbutton"
    $PAD addoption checkbutton -image       "pad_CheckbuttonConfig -image"  ""
    $PAD addoption checkbutton -padx        "pad_CheckbuttonConfig -padx"   2.5
    $PAD addoption checkbutton -pady        "pad_CheckbuttonConfig -pady"   2.0
    $PAD addoption checkbutton -pen         "pad_CheckbuttonConfig -pen"    gray20
    $PAD addoption checkbutton -text        "pad_CheckbuttonConfig -text"   ""
    $PAD addoption checkbutton -value       "pad_CheckbuttonConfig -value"  0
    $PAD addoption checkbutton -width       "pad_CheckbuttonConfig -width"  0

				# Load in checkbutton KPL code
    kpl eval '$env(PADHOME)/draw/chkbutn.kpl source

                        # Initialize default attrs array
    set _checkbutton(attrs) {activefill borderwidth boxfill command fill font\
	height help image padx pady pen text value width}
    pad_CheckbuttonInitattrs $PAD _checkbutton
}

#
# Button attributes array initiatialization routine.
# Should be called before button is created
#
proc pad_CheckbuttonInitattrs {PAD button {id ""}} {
    global _widget

    pad_set_prop $button PAD $PAD
    pad_set_prop $button id $id
    pad_set_prop $button activefill #6cb2cc
    pad_set_prop $button active 0
    pad_set_prop $button arm 0
    pad_set_prop $button borderwidth 0.5
    pad_set_prop $button boxfill [$PAD color alloc red2]
    pad_set_prop $button command ""
    pad_set_prop $button fill #4c92aa
    pad_set_prop $button font $_widget(font)
    pad_set_prop $button fontsize [pad_get_font_size $_widget(font)]
    pad_set_prop $button height 0
    pad_set_prop $button help "This is the default checkbutton"
    pad_set_prop $button image ""
    pad_set_prop $button padx 2.5
    pad_set_prop $button pady 2.5
    pad_set_prop $button pen [$PAD color alloc gray20]
    pad_set_prop $button portals ""
    pad_set_prop $button text "Hello"
    pad_set_prop $button userwidth 0
    pad_set_prop $button userheight 0
    pad_set_prop $button value 0
    pad_set_prop $button width 0
}

################################################################
#
# Pad checkbutton widget
# usage: "pathName create checkbutton ?option value ...?
# where legal options are:
#   -activefill : Active fill color (controls border also)
#   -borderwidth: Border thickness
#   -boxfill : Fill color of checkbox
#   -command : Script to be evaluated when checkbutton is activated
#   -fill    : Fill color (controls border also)
#   -font    : Font name
#   -height  : Height of checkbutton in pixels
#   -help    : HelpText of checkbutton
#   -image   : background image
#   -padx    : horizontal padding of the button text
#   -pady    : vertical padding of the button text
#   -pen     : Text label color
#   -text    : Text label
#   -value   : Value of checkbutton (0 = off; 1 = on)
#   -width   : Width of checkbutton in pixels
#
# There are also some commands of the form "pathName checkbutton command ?option...?
#    invoke  : Invokes the command associated with the checkbutton
#
#
# For each checkbutton widget, a global array is created with the following elements:
#   'PAD'         - Pad widget checkbutton is on
#   'active'      - 1 if checkbutton is currently active
#   'activefill'  - Active fill color (controls border)
#   'activefill_border'  - Name of active border (from border alloc)
#   'arm'         - 1 if checkbutton is curently armed
#   'borderwidth' - Border thickness
#   'boxfill'     - Fill color of checkbox
#   'command'     - Tcl script to be executed (appended with value)
#   'fill'        - Fill color (controls border)
#   'fill_border' - Name of border from border alloc
#   'font'        - Font name
#   'height'      - Height of checkbutton
#   'help'        - HelpText of checkbutton
#   'id'          - Id of checkbutton
#   'image'       - background image
#   'padx'        - horizontal padding
#   'pady'        - vertical padding
#   'pen'         - Pen color
#   'portals'     - List of portals checkbutton is in
#   'text'        - Text label
#   'userheight'  - User Height of checkbutton
#   'userwidth'   - User Width of checkbutton
#   'value'       - Value of checkbutton (0 = off; 1 = on)
#   'width'       - Width of checkbutton
#
################################################################

################################################################
#
# CheckButton creation
#
################################################################

proc pad_Checkbutton {PAD} {
    set id [$PAD create kpl]
    set button _checkbutton$id

    pad_CheckbuttonInitattrs $PAD $button $id
    pad_widget_reborder $button
    pad_widget_reborder $button activefill
    pad_CheckbuttonSetDim $PAD $button
    pad_widget_resize $button
 
    $PAD ic $id -anchor sw -renderscript "'$button checkbutton_render"

    $PAD bind $id <Run-Enter>           "pad_checkbutton_enter $button %l; break"
    $PAD bind $id <Run-Leave>           "pad_checkbutton_leave $button; break"
    $PAD bind $id <Run-ButtonPress-1>   "pad_checkbutton_arm $button; break"
    $PAD bind $id <Run-B1-Motion>       "pad_checkbutton_motion $button %U %V; break"
    $PAD bind $id <Run-ButtonRelease-1> "pad_checkbutton_activate $button; break"
    $PAD bindtags $id specific

                        # Setup event binding for writing out
    $PAD bind $id <Write> {
        return [pad_CheckButtonWrite %P %O]
    }

                        # Free up resources properly when widget deleted
    $PAD bind $id <Delete> {
        set widget "_checkbutton%O"
        set image [pad_get_prop $widget image]
        if {$image != ""} {
            %P image free $image
        }
        unset $widget
    }

    return $id
}

#
# Routine for computing button width and height based on
# its text or image value and user settings.
#
proc pad_CheckbuttonSetDim {PAD button} {
    set userw [pad_get_prop $button userwidth]
    set userh [pad_get_prop $button userheight]

    if { $userw == 0 || $userh == 0 } {
        set padx  [pad_get_prop $button padx]
        set pady  [pad_get_prop $button pady]
        set text [pad_get_prop $button text]
        set image [pad_get_prop $button image]
        if { [string compare $image ""] == 0} {
            set font [pad_get_prop $button font]
	    set textheight [pad_get_prop $button fontsize]
            set rinfo [$PAD font bbox $text $font]
            set w [expr [lindex $rinfo 2] - [lindex $rinfo 0] \
		+ 3*$padx + $textheight]
            set h [expr [lindex $rinfo 3] - [lindex $rinfo 1]]
	    if {$font != "System"} {
		set h [expr $h + $pady]
	    }
        } else {
            set imgdims [$PAD info image getdim $image]
            set w [expr [lindex $imgdims 0] + 2*$padx]
            set h [expr [lindex $imgdims 1] + 2*$pady]
        }
    }

    if { $userw == 0 } {
        pad_set_prop $button width $w
    }

    if { $userh == 0} {
        pad_set_prop $button height $h
    }
}

################################################################
#
# CheckButton configure
#
################################################################

proc pad_CheckbuttonConfig args {
    set option [lindex $args 0]
    set PAD [lindex $args 1]
    set id [lindex $args 2]
    set got_value 0

    if {[llength $args] >= 4} {
	set value [lindex $args 3]
	set got_value 1
    }

    set button _checkbutton$id

    if {$got_value} {
        switch -exact -- $option {

        -activefill {
	    pad_set_prop $button activefill $value
            pad_widget_reborder $button activefill
        }

        -borderwidth {
            if {$value < 0} {
                return -code error \
                    "-borderwidth expected number > 0, got $value"
            }
	    pad_set_prop $button borderwidth $value
            $PAD damage $id
        }

        -boxfill {
	    $PAD color free [pad_get_prop $button boxfill]
	    pad_set_prop $button boxfill [$PAD color alloc $value]
            $PAD damage $id
        }

        -command {
	    pad_set_prop $button command $value
        }

        -fill {
	    pad_set_prop $button fill $value
            pad_widget_reborder $button
        }

        -font {
	    pad_set_prop $button font $value
	    pad_set_prop $button fontsize [pad_get_font_size $value]
            pad_CheckbuttonSetDim $PAD $button
            pad_widget_resize $button
        }

	-help { pad_set_prop $button help "$value" }

        -image {
            # set button width & height to the image size
            # assume value is allocated image data
            pad_set_prop $button image $value
            pad_CheckbuttonSetDim $PAD $button
            pad_widget_resize $button
        }

        -padx {
            pad_set_prop $button padx $value
            pad_CheckbuttonSetDim $PAD $button
            pad_widget_resize $button
        }

        -pady {
            pad_set_prop $button pady $value
            pad_CheckbuttonSetDim $PAD $button
            pad_widget_resize $button
        }

	-pen {
	    $PAD color free [pad_get_prop $button pen]
	    pad_set_prop $button pen [$PAD color alloc $value]
	}

	-text {
	    pad_set_prop $button text $value
            pad_CheckbuttonSetDim $PAD $button
            pad_widget_resize $button
	}

        -value { pad_set_prop $button value $value }

        -width  {
	    pad_set_prop $button width $value
            pad_set_prop $button userwidth 1
	    pad_widget_resize $button
	}

        -height {
            pad_set_prop $button height $value
            pad_set_prop $button userheight 1
            pad_widget_resize $button
        }

        default {return -code error}
      }
    }

    set option_name [string range $option 1 end]
    return [pad_get_prop $button $option_name]
}

#
# Routine for writing out neccessary tcl commands
# to create the button.  Only attributes with non-default
# values are written out.
#
proc pad_CheckButtonWrite {PAD obj} {
    global _checkbutton

    set result ""
    set widget "_checkbutton$obj"; # button attr array

    set attrs $_checkbutton(attrs)
    foreach attr $attrs {
        set value [$PAD ic $obj -$attr]
        if {$attr == "width"} {
            if {[pad_get_prop $widget userwidth] == 0} {
                continue
            }
        }
        if {$attr == "height"} {
            if {[pad_get_prop $widget userheight] == 0} {
                continue
            }
        }

        if {$attr == "image"} {
            set image [pad_get_prop $widget image]
            if {$image != ""} {
                set imagefile [$PAD info image getname $image]
                append result "set image \[\$PADLOAD image alloc $imagefile\]\n"
                append result "\$PADLOAD ic \$Pad_ID -image \$image\n"
                continue
            }
        }

        if {"$value" != $_checkbutton($attr)} {
            append  result "\$PADLOAD ic \$Pad_ID -$attr"
            lappend result $value
            append  result "\n"
        }
    }
    return $result
}

################################################################
#
# CheckButton event handlers   (see button.tcl for other handlers)
#
################################################################

proc pad_checkbutton_activate {button} {
    set PAD [pad_get_prop $button PAD]

    if {[pad_get_prop $button arm]} {
	pad_set_prop $button arm 0
	set command [pad_get_prop $button command]
	if {$command != ""} {
	    uplevel #0 "eval [pad_get_prop $button command]"
	}
	if {[pad_get_prop $button value] == 0} {
            pad_set_prop $button value 1
	} else {
            pad_set_prop $button value 0
	}
        $PAD damage [pad_get_prop $button id]
	foreach portal [pad_get_prop $button portals] {
	    catch {$PAD damage $portal}
	}
    }
}

proc pad_checkbutton_arm {button} {
    set PAD [pad_get_prop $button PAD]

    pad_set_prop $button arm 1
    $PAD damage [pad_get_prop $button id]
    foreach portal [pad_get_prop $button portals] {
	catch {$PAD damage $portal}
    }
}

proc pad_checkbutton_motion {button u v} {
    set PAD [pad_get_prop $button PAD]
    set w [pad_get_prop $button width]
    set h [pad_get_prop $button height]

    if {[bbenclosedoron $u $v "0 0 $w $h"]} {
        if {[pad_get_prop $button arm] == 0} {
            pad_set_prop $button active 1
            pad_set_prop $button arm 1
            $PAD damage [pad_get_prop $button id]
            foreach portal [pad_get_prop $button portals] {
                catch {$PAD damage $portal}
            }
        }
    } elseif {[pad_get_prop $button arm] == 1} {
        pad_set_prop $button active 0
        pad_set_prop $button arm 0
        $PAD damage [pad_get_prop $button id]
        foreach portal [pad_get_prop $button portals] {
            catch {$PAD damage $portal}
        }
    }
}

proc pad_checkbutton_enter {button portals} {
    set PAD [pad_get_prop $button PAD]

    pad_set_prop $button cursor [lindex [$PAD config -cursor] 4]
    $PAD config -cursor "top_left_arrow"
    set help [pad_get_prop $button help]
    if {$help != ""} {
	updateStatusText $PAD $help 1
    }
    pad_set_prop $button active 1
    set portal [eventPortal $portals]
    if {$portal != ""} {
        set prtls [pad_get_prop $button portals]
        if {[lsearch $prtls $portal] == -1} {
            pad_set_prop $button portals [lappend prtls $portal]
        }
    }
    $PAD damage [pad_get_prop $button id]
    foreach portal [pad_get_prop $button portals] {
        catch {$PAD damage $portal}
    }
}

proc pad_checkbutton_leave {button} {
    set PAD [pad_get_prop $button PAD]

    catch {$PAD config -cursor [pad_get_prop $button cursor]}
    set help [pad_get_prop $button help]
    if {$help != ""} {
	updateStatusText $PAD "" 0
    }
    pad_set_prop $button active 0
    $PAD damage [pad_get_prop $button id]
    foreach portal [pad_get_prop $button portals] {
        catch {$PAD damage $portal}
    }
}
