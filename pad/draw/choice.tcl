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
# Choice initiatialization routine
# Needs to be called before choice can be created
#

proc choice_init {PAD} {
    global env
				# Load in choice KPL code
    kpl eval '$env(PADHOME)/draw/choice.kpl source
}

################################################################
#
# Pad choice widget
# usage: "pathName create choice ?option value ...?
# where legal options are:
#   -activefill : Active fill color (controls border also)
#   -borderwidth: Border thickness
#   -command : Script to be evaluated when choice is activated
#   -fill    : Fill color (controls border also)
#   -font    : Font name
#   -name    : Name of choice
#   -pen     : Text label color
#   -choices : List of choices
#   -value   : Access current value (must be one of the choices)
#   -x       : Lower left corner of choice
#   -y       : Upper right corner of choice
#   -width   : Width of choice in pixels
#   -height  : Height of choice in pixels
#
# There are also some commands of the form "pathName choice command ?option...?
#    invoke  : Invokes the command associated with the choice
#
#
# For each choice widget, a global array is created with the following elements:
#   'PAD'         - Pad widget choice is on
#   'active'      - 1 if choice is currently active
#   'activefill'  - Active fill color (controls border)
#   'activefill_border'  - Name of active border (from border alloc)
#   'arm'         - 1 if choice is curently armed
#   'borderwidth' - Border thickness
#   'command'     - Tcl script to be executed (appended with value)
#   'fill'        - Fill color (controls border)
#   'fill_border' - Name of border from border alloc
#   'font'        - Font name
#   'height'      - Height of choice
#   'id'          - Id of choice
#   'name'        - Widget name (sets a tag)
#   'pen'         - Pen color
#   'choices'     - List of choices
#   'value'       - Current value
#   'width'       - Width of choice
#   'x'           - Lower left corner of choice
#   'y'           - Upper right corner of choice
#
################################################################

################################################################
#
# Choice creation
#
################################################################


proc padcreate_choice args {
    global _widget

    set PAD [lindex $args 0]
    set id [$PAD create kpl]
    set choice _choice$id

    pad_set_prop $choice PAD $PAD
    pad_set_prop $choice id $id

    pad_set_prop $choice fill #4c92aa
    pad_set_prop $choice activefill #6cb2cc
    pad_set_prop $choice active 0
    pad_set_prop $choice arm 0
    pad_set_prop $choice x 0
    pad_set_prop $choice y 0
    pad_set_prop $choice width 120
    pad_set_prop $choice height 35
    pad_set_prop $choice borderwidth 0.5
    pad_set_prop $choice command ""
    pad_set_prop $choice name ""
    pad_set_prop $choice nchoices 0
    pad_set_prop $choice choices ""
    pad_set_prop $choice value ""
    pad_set_prop $choice pick_value 1
    pad_set_prop $choice pen [$PAD color alloc gray20]
    pad_set_prop $choice font $_widget(font)

    pad_widget_reborder $choice
    pad_widget_reborder $choice activefill
    pad_widget_resize $choice
 
    $PAD ic $id -renderscript "'$choice choice_render"

    $PAD bind $id <Run-Enter>           "pad_choice_enter $choice; break"
    $PAD bind $id <Run-Leave>           "pad_choice_leave $choice; break"
    $PAD bind $id <Run-ButtonPress-1>   "pad_choice_arm $choice %U %V; break"
    $PAD bind $id <Run-B1-Motion>       "pad_choice_motion $choice %U %V; break"
    $PAD bind $id <Run-ButtonRelease-1> "pad_choice_activate $choice %U %V; break"

    return $id
}

################################################################
#
# Choice configure
#
################################################################

proc padconf_choice args {
    if {[llength $args] < 3} {
	return -code error {Invalid option. Expected one of:
	-activefill -bordersize -command -fill -font -name -pen -choice
	-value -x -y -width -height}
    }
    set PAD [lindex $args 0]
    set id [lindex $args 1]
    set option [lindex $args 2]
    set got_value 0

    if {[llength $args] >= 4} {
	set value [lindex $args 3]
	set got_value 1
    }

    set choice _choice$id
    global $choice

    if {$got_value} {
        switch -exact -- $option {

        -activefill {
	    pad_set_prop $choice activefill $value
            pad_widget_reborder $choice activefill
        }

        -borderwidth {
            if {$value < 0} {
                return -code error \
                    "-borderwidth expected number > 0, got $value"
            }
	    pad_set_prop $choice borderwidth $value
            $PAD damage $id
        }

        -command {
	    pad_set_prop $choice command $value
        }

        -fill {
	    pad_set_prop $choice fill $value
            pad_widget_reborder $choice
        }

        -font {
	    pad_set_prop $choice font $value
        }

        -name {
	    pad_set_prop $choice name $value
	    pad_widget_rename $choice
        }

	-pen {
	    $PAD color free [pad_get_prop $choice pen]
	    pad_set_prop $choice pen [$PAD color alloc $value]
	}

	-choices {
	    pad_set_prop $choice choices $value
	    set i 0
	    foreach item $value {
		pad_set_prop $choice [expr $i + 1] $item
		incr i
	    }
	    pad_set_prop $choice value [lindex $value 0]
	    pad_set_prop $choice nchoices $i
	}

	-value {
	    set choices [pad_get_prop $choice choices]
	    set pick 1
	    foreach item $choices {
		if {$value == $item} {
		    pad_set_prop $choice value $value
		    pad_set_prop $choice pick_value $pick
		}
		incr pick
	    }
	}

        -x { pad_set_prop $choice x $value; pad_widget_resize $choice }
        -y { pad_set_prop $choice y $value; pad_widget_resize $choice }
        -width  { pad_set_prop $choice width  $value; pad_widget_resize $choice }
        -height { pad_set_prop $choice height $value; pad_widget_resize $choice }

        default {return -code error}
      }
    }

    set option_name [string range $option 1 end]
    return [pad_get_prop $choice $option_name]
}

################################################################
#
# Choice command
#
################################################################

proc padcmd_choice args {
    if {[llength $args] < 2} {
	return -code error "usage: \"pathName choice option ?args?\""
    }
    set PAD [lindex $args 0]
    set option [lindex $args 1]
    set choice _choice$id

    switch -exact -- $option {
	set {
	    if {[llength $args] != 4} {
		return -code error "usage: \"pathName choice set tagOrId value\""
	    }
	    set id [lindex [$PAD find withtag [lindex $args 2]] 0]
	    set value [lindex $args 3]
	    return [$PAD ic $id -value $value]
	}
	get {
	    if {[llength $args] != 3} {
		return -code error "usage: \"pathName choice get tagOrId\""
	    }
	    set id [lindex [$PAD find withtag [lindex $args 2]] 0]
	    return [$PAD ic $id -value]
	}
	default {return -code error "Unknown choice option: $option.\nTry: set, get."}
    }
}

################################################################
#
# Choice action functions
#
################################################################
proc pad_choice_arm {choice u v} {
    set PAD [pad_get_prop $choice PAD]
    set id [pad_get_prop $choice id]

    set pick [kpl eval '$choice $u $v choice_pick]
    pad_set_prop $choice pick_value $pick

    pad_set_prop $choice arm 1
    $PAD damage $id
}

proc pad_choice_motion {choice u v} {
    set PAD [pad_get_prop $choice PAD]
    set id [pad_get_prop $choice id]

    set pick [kpl eval '$choice $u $v choice_pick]
    if {$pick == 0} {
        pad_set_prop $choice arm 0
    } else {
        pad_set_prop $choice arm 1
    }
    if {[pad_get_prop $choice arm]} {
	pad_set_prop $choice pick_value $pick
    }
    $PAD damage $id
}

proc pad_choice_activate {choice u v} {
    set PAD [pad_get_prop $choice PAD]
    set id [pad_get_prop $choice id]

    set pick_value [expr [pad_get_prop $choice pick_value] - 1]
    set current_value [lindex [pad_get_prop $choice choices] $pick_value]
    pad_set_prop $choice value $current_value

    if {[pad_get_prop $choice arm]} {
	set command [pad_get_prop $choice command]
	if {$command != ""} {
	    uplevel #0 "eval [pad_get_prop $choice command] $value"
	}
	pad_set_prop $choice arm 0
    }
    if {[pad_get_prop $choice pick_value] == 0} {
	pad_set_prop $choice active 0
    }
    $PAD damage $id
}

proc pad_choice_enter {choice} {
    set PAD [pad_get_prop $choice PAD]
    set id [pad_get_prop $choice id]

    pad_set_prop $choice active 1
    $PAD damage $id
}

proc pad_choice_leave {choice} {
    set PAD [pad_get_prop $choice PAD]
    set id [pad_get_prop $choice id]

    pad_set_prop $choice active 0
    $PAD damage $id
}
