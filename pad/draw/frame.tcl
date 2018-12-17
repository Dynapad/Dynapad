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
# Frame initiatialization routine
# Needs to be called before frame can be created
#

proc frame_init {PAD} {
    global env
				# Load in frame KPL code
    kpl eval '$env(PADHOME)/draw/frame.kpl source
}

proc frame_relief_num {relief} {
    switch -exact -- $relief {
    raised { return 1 }
    flat { return 2 }
    sunken { return 4 }
    groove { return 8 }
    ridge { return 16 }
    }
}


################################################################
#
# Pad frame widget
# usage: "pathName create frame ?option value ...?
# where legal options are:
#   -borderwidth: Border thickness
#   -fill    : Fill color (controls border also)
#   -font    : Font name
#   -name    : Name of frame
#   -pen     : Text label color
#   -text    : Text label
#   -x       : Lower left corner of frame
#   -y       : Upper right corner of frame
#   -width   : Width of frame in pixels
#   -height  : Height of frame in pixels
#   -relief  : Relief type
#
################################################################

################################################################
#
# Frame creation
#
################################################################


proc padcreate_frame args {
    global _widget

    set PAD [lindex $args 0]
    set id [$PAD create kpl]
    set frame _frame$id
    $PAD ic $id -renderscript "'$frame frame_render"

    pad_set_prop $frame PAD $PAD
    pad_set_prop $frame id $id

    pad_set_prop $frame fill [pad_get_prop _color padbg]
    pad_set_prop $frame fill_color [$PAD color alloc [pad_get_prop $frame fill]]
    pad_set_prop $frame x 0
    pad_set_prop $frame y 0
    pad_set_prop $frame width 30
    pad_set_prop $frame height 10
    pad_set_prop $frame borderwidth 0.5
    pad_set_prop $frame name ""
    pad_set_prop $frame text ""
    pad_set_prop $frame relief "raised"
    pad_set_prop $frame relief_num [frame_relief_num [pad_get_prop $frame relief]]
    pad_set_prop $frame pen [$PAD color alloc gray20]
    pad_set_prop $frame font System

    pad_widget_reborder $frame
    pad_widget_resize $frame

    return $id
}

################################################################
#
# Frame configure
#
################################################################

proc padconf_frame args {
    if {[llength $args] < 3} {
	return -code error {Invalid option. Expected one of:
	-bordersize -fill -font -name -pen -text
	-x -y -width -height}
    }
    set PAD [lindex $args 0]
    set id [lindex $args 1]
    set option [lindex $args 2]
    set got_value 0

    if {[llength $args] >= 4} {
	set value [lindex $args 3]
	set got_value 1
    }

    set frame _frame$id
    global $frame

    if {$got_value} {
        switch -exact -- $option {

        -borderwidth {
            if {$value < 0} {
                return -code error \
                    "-borderwidth expected number > 0, got $value"
            }
	    pad_set_prop $frame borderwidth $value
            $PAD damage $id
        }

        -fill {
	    pad_set_prop $frame fill $value
            pad_widget_reborder $frame
	    $PAD color free [pad_get_prop $frame fill_color]
	    pad_set_prop $frame fill_color [$PAD color alloc $value]
        }

        -font {
	    pad_set_prop $frame font $value
        }

        -name {
	    pad_set_prop $frame name $value
	    pad_widget_rename $frame
        }

	-pen {
	    $PAD color free [pad_get_prop $frame pen]
	    pad_set_prop $frame pen [$PAD color alloc $value]
	}

	-relief {
	    pad_set_prop $frame relief $value
    	    pad_set_prop $frame relief_num [frame_relief_num $value]
	}

	-text {
	    pad_set_prop $frame text $value
	}

        -x { pad_set_prop $frame x $value; pad_widget_resize $frame }
        -y { pad_set_prop $frame y $value; pad_widget_resize $frame }
        -width  { pad_set_prop $frame width  $value; pad_widget_resize $frame }
        -height { pad_set_prop $frame height $value; pad_widget_resize $frame }

        default {return -code error}
      }
    }

    set option_name [string range $option 1 end]
    return [pad_get_prop $frame $option_name]
}
