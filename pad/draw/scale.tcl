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
# Scale initiatialization routine
# Needs to be called before scale can be created
#

proc pad_ScaleInit {PAD} {
    global env _scale _widget
                                # Create scale types and options
    $PAD addtype scale pad_Scale

    $PAD addoption scale -activefill   "pad_ScaleConfig -activefill" #6cb2cc
    $PAD addoption scale -barsize      "pad_ScaleConfig -barsize"  16
    $PAD addoption scale -borderwidth  "pad_ScaleConfig -borderwidth"  0.5
    $PAD addoption scale -command      "pad_ScaleConfig -command"  ""
    $PAD addoption scale -fill         "pad_ScaleConfig -fill"  $_widget(fill)
    $PAD addoption scale -font         "pad_ScaleConfig -font"  Times-5
    $PAD addoption scale -from         "pad_ScaleConfig -from"  0
    $PAD addoption scale -help         "pad_ScaleConfig -help" \
					  "This is the default scale"
    $PAD addoption scale -orient       "pad_ScaleConfig -orient"  h
    $PAD addoption scale -pen          "pad_ScaleConfig -pen"  gray20
    $PAD addoption scale -precision    "pad_ScaleConfig -precision"  0
    $PAD addoption scale -showrange    "pad_ScaleConfig -showrange"  1
    $PAD addoption scale -showvalue    "pad_ScaleConfig -showvalue"  1
    $PAD addoption scale -state        "pad_ScaleConfig -state"  1
    $PAD addoption scale -text         "pad_ScaleConfig -text"  "Default Scale"
    $PAD addoption scale -to           "pad_ScaleConfig -to"  100
    $PAD addoption scale -troughheight "pad_ScaleConfig -troughheight"  9
    $PAD addoption scale -troughwidth  "pad_ScaleConfig -troughwidth"  100
    $PAD addoption scale -value        "pad_ScaleConfig -value"  10

                                # Load in scale KPL code
    kpl eval '$env(PADHOME)/draw/scale.kpl source

    set _scale(attrs) {activefill barsize borderwidth command fill font from\
	help orient pen precision showrange showvalue state text to\
	troughheight troughwidth value}
    pad_ScaleInitattrs $PAD _scale
}

#
# Scale attributes array initiatialization routine.
# Should be called beforescale is created
#
proc pad_ScaleInitattrs { PAD scale {id ""}} {
    global _widget

    set FROM		 0
    set TO		 100
    set VALUE		 10
    set TROUGH_WIDTH	 100
    set TROUGH_HEIGHT	 9

    pad_set_prop $scale PAD $PAD
    pad_set_prop $scale id $id

    pad_set_prop $scale active 0
    pad_set_prop $scale activefill #6cb2cc
    pad_set_prop $scale arm 0
    pad_set_prop $scale barsize 16
    pad_set_prop $scale borderwidth 0.5
    pad_set_prop $scale command ""
    pad_set_prop $scale disabledfill [$PAD color alloc grey40]
    pad_set_prop $scale fill $_widget(fill)
    pad_set_prop $scale font Times-5
    pad_set_prop $scale from $FROM
    pad_set_prop $scale from_text " $FROM "
    pad_set_prop $scale height [expr $TROUGH_HEIGHT + 25]
    pad_set_prop $scale help "This is the default scale"
    pad_set_prop $scale offset 0
    pad_set_prop $scale orient h
    pad_set_prop $scale pen [$PAD color alloc gray20]
    pad_set_prop $scale portals ""
    pad_set_prop $scale precision 0
    pad_set_prop $scale showrange 1
    pad_set_prop $scale showvalue 1
    pad_set_prop $scale state 1
    pad_set_prop $scale text "Default Scale"
    pad_set_prop $scale to $TO
    pad_set_prop $scale to_text " $TO "
    pad_set_prop $scale troughheight $TROUGH_HEIGHT
    pad_set_prop $scale troughwidth $TROUGH_WIDTH
    pad_set_prop $scale tx 1
    pad_set_prop $scale ty 9
    pad_set_prop $scale value $VALUE
    pad_set_prop $scale val_text " $VALUE "
    pad_set_prop $scale width [expr $TROUGH_WIDTH + 2]

    set bbox [$PAD font bbox " $FROM " $_widget(font)]
    pad_set_prop $scale from_size [expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
    pad_set_prop $scale dh [expr ([lindex $bbox 3]-[lindex $bbox 1])/2.]
    set bbox [$PAD font bbox " $TO " $_widget(font)]
    pad_set_prop $scale dw [set dw [expr [lindex $bbox 2]-[lindex $bbox 0]]]
    pad_set_prop $scale df 0
    pad_set_prop $scale to_size [expr $dw/2.]
    set bbox [$PAD font bbox " $VALUE " $_widget(font)]
    pad_set_prop $scale val_size [expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
    SetScaleRatio $scale

    return $id
}

################################################################
#
# Pad scale widget
# usage: "pathName create scale ?option value ...?
# where legal options are:
#   -activefill   : Active fill color (controls border also)
#   -barsize      : Width (or height) of bar
#   -borderwidth  : Border thickness
#   -command      : Script to be evaluated when scale is activated (on release)
#   -fill         : Fill color (controls border also)
#   -font         : Font of scale text
#   -from         : Minimum value of scale
#   -help         : Help text of scale
#   -orient       : v if orientation is vertical; h otherwise
#   -pen          : Text label color
#   -precision    : number of decimal places
#   -showrange    : Show range of scale (1=show, 0=don't)
#   -showvalue    : Show range of value (1=show, 0=don't)
#   -state        : 1 if state of scale is normal; 0 otherwise
#   -text         : Label of scale
#   -to           : Maximum value of scale
#   -troughheight : Height of scale trough in pixels
#   -troughwidth  : Width of scale trough in pixels
#   -value        : Current value of scale
#
# There are also some commands of the form "pathName scale command ?option...?
#    invoke  : Invokes the command associated with the scale
#
#
# For each scale widget, a global array is created with the following elements:
#   'PAD'         - Pad widget scale is on
#   'active'      - 1 if scale is currently active
#   'activefill'  - Active fill color (controls border)
#   'activefill_border'  - Name of active border (from border alloc)
#   'arm'         - 1 if scale is curently armed
#   'barsize'     - Width (or Height) of bar
#   'borderwidth' - Border thickness
#   'command'     - Tcl script to be executed (appended with value)
#   'disabledfill'- Disabled fill color
#   'df'          - The text width of the value fraction (vertical scales only)
#   'dh'          - Half of the text number height (vertical scales only)
#   'dw'          - The largest text number width (vertical scales only)
#   'fill'        - Fill color (controls border)
#   'fill_border' - Name of border from border alloc
#   'font'        - Font of scale text
#   'from'        - Minimum value of scale
#   'height'      - Height of scale
#   'help'        - Help text of scale
#   'id'          - Id of scale
#   'offset'      - Offset in scale units that scale was picked
#   'orient'      - v if orientation is vertical; h if horizontal
#   'pen'         - Color of scale text
#   'portals'     - List of portals scale is in
#   'precision'   - Scale value precision (0=integer, >=1=real)
#   'showrange'   - Show range of scale (1=show, 0=don't)
#   'showvalue'   - Show value of scale (1=show, 0=don't)
#   'state'       - State of scale (1=normal, 0=disabled)
#   'text'        - Label of scale
#   'to'          - Maximum value of scale
#   'troughheight'- Height of scale trough
#   'troughwidth' - Width of scale trough
#   'tx'          - X coordinate of scale trough
#   'ty'          - Y coordinate of scale trough
#   'value'       - Current value of scale
#   'width'       - Width of scale
#
################################################################

################################################################
#
# Scale creation
#
################################################################

set _widget(activefill) #7cc2ea

proc pad_Scale {PAD} {
    set id [$PAD create kpl]
    $PAD ic $id -anchor sw

    set scale _scale$id
    pad_ScaleInitattrs $PAD $scale $id

    pad_widget_reborder $scale
    pad_widget_reborder $scale activefill
    pad_scale_resize $scale

    $PAD ic $id -renderscript "'$scale scale_render"

    $PAD bind $id <Run-Motion>        "pad_scale_motion $scale %U %V %l; break"
    $PAD bind $id <Run-ButtonPress-1> "pad_scale_arm $scale %U %V; break"
    $PAD bind $id <Run-B1-Motion>     "pad_scale_drag $scale %U %V; break"
    $PAD bind $id <Run-ButtonRelease-1> "pad_scale_activate $scale %U %V
	break"
    $PAD bind $id <Run-Enter>       "pad_scale_enter $scale; break"
    $PAD bind $id <Run-Leave>       "pad_scale_leave $scale; break"
    $PAD bind $id <KeyPress-Left>   "pad_scale_arrow_key $scale h l; break"
    $PAD bind $id <KeyPress-Right>  "pad_scale_arrow_key $scale h r; break"
    $PAD bind $id <KeyPress-Up>     "pad_scale_arrow_key $scale v r; break"
    $PAD bind $id <KeyPress-Down>   "pad_scale_arrow_key $scale v l; break"
    $PAD bindtags $id specific

                        # Setup event binding for writing out
    $PAD bind $id <Write> { return [pad_ScaleWrite %P %O] }

                        # Free up resources properly when widget deleted
    $PAD bind $id     <Delete>      "unset $scale"

    return $id
}

#
# Routine for writing out neccessary tcl commands
# to create the scale.  Only attributes with non-default
# values are written out.
#
proc pad_ScaleWrite {PAD obj} {
    global _scale

    set result ""
    set widget "_scale$obj"; # scale attr array
    global $widget

    set attrs $_scale(attrs)
    foreach attr $attrs {
        set value [$PAD ic $obj -$attr]
        if {"$value" != $_scale($attr)} {
            append  result "\$PADLOAD ic \$Pad_ID -$attr"
            lappend result $value
            append  result "\n"
        }
    }
    return $result
}

#
# Determines the ratio of barsize to troughwidth; used by render code
#
proc SetScaleRatio {scale} {
    pad_set_prop $scale ratio [expr \
	([pad_get_prop $scale troughwidth] - [pad_get_prop $scale barsize]\
	 - 2.*[pad_get_prop $scale borderwidth]) \
	/ ([pad_get_prop $scale to] - [pad_get_prop $scale from])]
}

#
# pad_scale_resize
#
#   Called when a scale's width or height property has changed.
#   Resets the scale's bbox.
#
proc pad_scale_resize {scale {orient h}} {
    set PAD [pad_get_prop $scale PAD]
    set id [pad_get_prop $scale id]

    if {$orient == "h"} {
        set x2 [pad_get_prop $scale width]
        set y2 [pad_get_prop $scale height]
    } else {
        set x2 [pad_get_prop $scale height]
        set y2 [pad_get_prop $scale width]
    }

    $PAD ic $id -bb "0:0 $x2:$y2"
    $PAD damage $id
}

#
# Scale configure
#

proc pad_ScaleConfig args {
    set option [lindex $args 0]
    set PAD [lindex $args 1]
    set id [lindex $args 2]
    set got_value 0

    if {[llength $args] >= 4} {
	set value [lindex $args 3]
	set got_value 1
    }

    set scale _scale$id

    if {$got_value} {
#	if {[list [pad_get_prop $scale $option]] == [list $value]} { return }
        switch -exact -- $option {
        -activefill {
	    pad_set_prop $scale activefill $value
	    pad_widget_reborder $scale $value
	}

        -barsize {
            if {$value < 0} {
                return -code error \
                        "-barsize expected number > 0, got $value"
            }
	    pad_set_prop $scale barsize $value
            $PAD damage $id
        }

        -borderwidth {
            if {$value < 0} {
                return -code error \
                    "-borderwidth expected number > 0, got $value"
            }
	    pad_set_prop $scale borderwidth $value
            $PAD damage $id
        }

        -command { pad_set_prop $scale command $value }

        -fill { pad_set_prop $scale fill $value ; pad_widget_reborder $scale }

        -font {
	    pad_set_prop $scale font $value
	    set bbox [$PAD font bbox " [pad_get_prop $scale from] " $value]
	    pad_set_prop $scale from_size \
		[expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
	    pad_set_prop $scale dh [expr ([lindex $bbox 3]-[lindex $bbox 1])/2.]
	    set bbox [$PAD font bbox " [pad_get_prop $scale to] " $value]
	    pad_set_prop $scale dw \
		[set dw [expr [lindex $bbox 2]-[lindex $bbox 0]]]
	    pad_set_prop $scale to_size [expr $dw/2.]
	    set bbox [$PAD font bbox " [pad_get_prop $scale value] " $value]
	    pad_set_prop $scale val_size \
		[expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
	    if {[set prec [pad_get_prop $scale precision]] > 0} {
	        set string "."
		for {set i 0} {$i < $prec} {incr i} {
		    append string 0
		}
	        set bbox [$PAD font bbox "$string" $value]
	        pad_set_prop $scale df [expr [lindex $bbox 2]-[lindex $bbox 0]]
	    }
	    $PAD damage $id
	}

        -from {
	    pad_set_prop $scale from $value
	    pad_set_prop $scale from_text " $value "
	    SetScaleRatio $scale
	    set bbox [$PAD font bbox " $value " [pad_get_prop $scale font]]
	    set dw [expr [lindex $bbox 2]-[lindex $bbox 0]]
	    pad_set_prop $scale from_size [expr $dw/2.]
	    set old_dw [pad_get_prop $scale dw]
	    if {$dw == $old_dw} {
		$PAD damage $id
	    } else {
	        set to_size [expr 2*[pad_get_prop $scale to_size]]
	        if {$dw < $to_size} {
		    set dw $to_size
	        }
	        if {[pad_get_prop $scale orient] == "v"} {
	            if {[pad_get_prop $scale showvalue]} {
		       pad_set_prop $scale tx \
			   [expr $dw + [pad_get_prop $scale df]]
		    }
		    set h_diff [expr $dw - $old_dw]
	            if {[pad_get_prop $scale showrange]} {
		       pad_set_prop $scale height \
			   [expr [pad_get_prop $scale height] + 2*$h_diff]
	            } else {
		       pad_set_prop $scale height \
			   [expr [pad_get_prop $scale height] + $h_diff]
	            }
	            pad_scale_resize $scale v
		} else {
		    $PAD damage $id
	        }
	        pad_set_prop $scale dw $dw
	    }
	}

        -help { pad_set_prop $scale help "$value" }

	-orient {
	    set value [string index $value 0]
	    if {($value != "h") && ($value != "v")} {
		return -code error "-orient expected 'v*ertical' or\
		    'h*orizontal', got: $value"
	    }
	    pad_set_prop $scale orient $value
	    set troughheight [pad_get_prop $scale troughheight]
	    if {$value == "h"} {
		pad_set_prop $scale tx 1
		if {[pad_get_prop $scale showrange]} {
		    pad_set_prop $scale ty 9
		    set height [expr $troughheight + 25]
		} else {
		    pad_set_prop $scale ty 1
		    set height [expr $troughheight + 16]
		}
		if {![pad_get_prop $scale showvalue]} {
		    set height [expr $height - 7]
		}
		if {[pad_get_prop $scale text] == ""} {
		    set height [expr $height - 8]
		}
	    } else {
		set dw [pad_get_prop $scale dw]
		set df [pad_get_prop $scale df]
		pad_set_prop $scale ty 1
		if {[pad_get_prop $scale showrange]} {
		    set height [expr $troughheight + $dw]
		} else {
		    set height [expr $troughheight + 1]
		}
		if {[pad_get_prop $scale showvalue]} {
		   pad_set_prop $scale tx [expr $dw + $df]
		   set height [expr $height + $dw + $df]
		} else {
		   pad_set_prop $scale tx 1
		   set height [expr $height + 1]
		}
	    }
	    pad_set_prop $scale height $height
	    pad_scale_resize $scale $value
	}

        -pen {
	    $PAD color free [pad_get_prop $scale pen]
	    pad_set_prop $scale pen [$PAD color alloc $value]
	}

        -precision {

	    if {$value < 0 || int($value) != $value} {
                return -code error \
                    "-precision expected integer >= 0, got $value"
	    }

	    pad_set_prop $scale precision $value
	    ConfigScaleValue $PAD $scale $id [pad_get_prop $scale value]

	    if {$value == 0} {
	        set df 0
	    } else {
	        set string "."
		for {set i 0} {$i < $value} {incr i} {
		    append string 0
		}
	        set bbox [$PAD font bbox "$string" [pad_get_prop $scale font]]
	        set df [expr [lindex $bbox 2]-[lindex $bbox 0]]
	    }
	    set old_df [pad_get_prop $scale df]
	    if {$df != $old_df} {
		if {[pad_get_prop $scale orient] == "v" \
	         && [pad_get_prop $scale showvalue]} {
		    pad_set_prop $scale tx [expr [pad_get_prop $scale dw] +$df]
		    pad_set_prop $scale height \
			[expr [pad_get_prop $scale height] + $df - $old_df]
	            pad_scale_resize $scale v
	        }
	        pad_set_prop $scale df $df
	    }
	}

	-showrange {
	    if {$value != 1 && $value != 0} {
		return -code error "-showrange expected 0 or 1, got: $value"
	    }
	    pad_set_prop $scale showrange $value
	    set orient [pad_get_prop $scale orient]
	    set height [pad_get_prop $scale height]
	    if {$orient == "h"} {
	        if {$value == 0} {
		   pad_set_prop $scale ty 1
		   pad_set_prop $scale height [expr $height - 8]
	        } else {
		   pad_set_prop $scale ty 9
		   pad_set_prop $scale height [expr $height + 8]
	        }
	    } else {
		pad_set_prop $scale ty 1
	        set dw [pad_get_prop $scale dw]
	        if {$value == 0} {
		   pad_set_prop $scale height [expr $height + 1 - $dw]
	        } else {
		   pad_set_prop $scale height [expr $height - 1 + $dw]
	        }
	    }
	    pad_scale_resize $scale $orient
        }

	-showvalue {
	    if {$value != 1 && $value != 0} {
		return -code error "-showrange expected 0 or 1, got: $value"
	    }
	    pad_set_prop $scale showvalue $value
	    set orient [pad_get_prop $scale orient]
	    set height [pad_get_prop $scale height]
	    if {$orient == "h"} {
	        if {$value == 0} {
		   pad_set_prop $scale height [expr $height - 7]
	        } else {
		   pad_set_prop $scale height [expr $height + 7]
	        }
	    } else {
	        set dw [pad_get_prop $scale dw]
	        set df [pad_get_prop $scale df]
	        if {$value == 0} {
		   pad_set_prop $scale tx 1
		   pad_set_prop $scale height [expr $height + 1 - $dw - $df]
	        } else {
		   pad_set_prop $scale tx [expr $dw + $df]
		   pad_set_prop $scale height [expr $height - 1 + $dw + $df]
	        }
	    }
	    pad_scale_resize $scale $orient
        }

	-state {
	    if {$value != 1 && $value != 0} {
		return -code error "-state expected 0 or 1, got: $value"
	    }
	    pad_set_prop $scale state $value
	    $PAD damage $id
        }

	-text {
	    set oldtext [pad_get_prop $scale text]
	    pad_set_prop $scale text $value
	    if {[pad_get_prop $scale orient] == "h"} {
		set height [pad_get_prop $scale height]
	        if {$value == "" && $oldtext != ""} {
	            pad_set_prop $scale height [expr $height - 8]
	        } elseif {$value != "" && $oldtext == ""} {
	            pad_set_prop $scale height [expr $height + 8]
	        }
	        pad_scale_resize $scale h
	    } else {
	        $PAD damage $id
	    }
	}

        -to {
	    pad_set_prop $scale to $value
	    pad_set_prop $scale to_text " $value "
	    SetScaleRatio $scale
	    set bbox [$PAD font bbox " $value " [pad_get_prop $scale font]]
	    set dw [expr [lindex $bbox 2]-[lindex $bbox 0]]
	    pad_set_prop $scale to_size [expr $dw/2.]
	    set old_dw [pad_get_prop $scale dw]
	    if {$dw == $old_dw} {
		$PAD damage $id
	    } else {
	        set from_size [expr 2*[pad_get_prop $scale from_size]]
	        if {$dw < $from_size} {
		    set dw $from_size
	        }
	        if {[pad_get_prop $scale orient] == "v"} {
	            if {[pad_get_prop $scale showvalue]} {
		       pad_set_prop $scale tx \
			   [expr $dw + [pad_get_prop $scale df]]
		    }
		    set h_diff [expr $dw - $old_dw]
	            if {[pad_get_prop $scale showrange]} {
		       pad_set_prop $scale height \
			   [expr [pad_get_prop $scale height] + 2*$h_diff]
	            } else {
		       pad_set_prop $scale height \
			   [expr [pad_get_prop $scale height] + $h_diff]
	            }
	            pad_scale_resize $scale v
		} else {
		    $PAD damage $id
		}
	        pad_set_prop $scale dw $dw
	    }
        }

        -troughheight {
	    pad_set_prop $scale troughheight $value
	    pad_set_prop $scale height [expr $value + 25]
	    pad_scale_resize $scale [pad_get_prop $scale orient]
	}

        -troughwidth {
	    pad_set_prop $scale troughwidth $value
	    pad_set_prop $scale width [expr $value + 2]
	    pad_scale_resize $scale [pad_get_prop $scale orient]
	}

	-value { ConfigScaleValue $PAD $scale $id $value }

        default {return -code error}
      }
    }

    set option_name [string range $option 1 end]
    return [pad_get_prop $scale $option_name]
}

proc ConfigScaleValue {PAD scale id value} {
    set precision [pad_get_prop $scale precision]
    if {$precision > 0} {
        set size [expr [string length [expr int($value)]] + $precision]
        set value [format "%$size.${precision}f" $value]
    } else {
        set value [expr int($value)]
    }
    set bbox [$PAD font bbox " $value " [pad_get_prop $scale font]]
    pad_set_prop $scale val_size \
        [expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
    pad_set_prop $scale value $value
    pad_set_prop $scale val_text " $value "
    $PAD damage [pad_get_prop $scale id]
    foreach portal [pad_get_prop $scale portals] {
        catch {$PAD damage $portal}
    }
}

################################################################
#
# Scale event handlers
#
################################################################

proc pad_scale_arm {scale u v} {

    if {[pad_get_prop $scale active]} {
        set PAD [pad_get_prop $scale PAD]
        set pick [kpl eval '$scale $u $v scale_pick]
        pad_set_prop $scale arm 1
        if {[lindex $pick 0] == "bar"} {	# If point on bar
	    pad_set_prop $scale offset [lindex $pick 1]
            $PAD damage [pad_get_prop $scale id]
            foreach portal [pad_get_prop $scale portals] {
                catch {$PAD damage $portal}
            }
        } else {
	    pad_set_prop $scale offset [expr -.5*[pad_get_prop $scale barsize]]
	    pad_scale_drag $scale $u $v
        }
    }
}

proc pad_scale_drag {scale u v} {
    if {[pad_get_prop $scale arm]} {
	set offset [pad_get_prop $scale offset]
	set value [kpl eval '$scale $u $v $offset scale_get]
	ConfigScaleValue [pad_get_prop $scale PAD] $scale \
	    [pad_get_prop $scale id] $value
	set command [pad_get_prop $scale command]
	if {$command != ""} {
	    uplevel #0 "eval $command $value"
	}
    }
}

proc pad_scale_activate {scale u v} {

    set PAD [pad_get_prop $scale PAD]
    if {[pad_get_prop $scale arm]} {
	set value [pad_get_prop $scale value]
	pad_set_prop $scale arm 0
	set x1 [pad_get_prop $scale tx]
	set y1 [pad_get_prop $scale ty]
	if {[pad_get_prop $scale orient] == "h"} {
            set x2 [expr $x1 + [pad_get_prop $scale troughwidth]]
            set y2 [expr $y1 + [pad_get_prop $scale troughheight]]
	} else {
            set y2 [expr $x1 + [pad_get_prop $scale troughwidth]]
	    if {[pad_get_prop $scale showvalue]} {
                set x1 [expr [pad_get_prop $scale dw]+[pad_get_prop $scale df]]
	    }
            set x2 [expr $x1 + [pad_get_prop $scale troughheight]]
	}

        if {![bbenclosedoron $u $v "$x1 $y1 $x2 $y2"]} {
            pad_set_prop $scale active 0
        }
        $PAD damage [pad_get_prop $scale id]
        foreach portal [pad_get_prop $scale portals] {
            catch {$PAD damage $portal}
        }
    }
}

proc pad_scale_enter {scale} {
    set PAD [pad_get_prop $scale PAD]

    if {[pad_get_prop $scale arm]} {
        pad_set_prop $scale active 1
        $PAD damage [pad_get_prop $scale id]
        foreach portal [pad_get_prop $scale portals] {
            catch {$PAD damage $portal}
        }
    }
    pad_set_prop $scale cursor [lindex [$PAD config -cursor] 4]
    set help [pad_get_prop $scale help]
    if {$help != ""} {
	updateStatusText $PAD $help 1
    }
}

proc pad_scale_leave {scale} {
    set PAD [pad_get_prop $scale PAD]

    catch {$PAD config -cursor [pad_get_prop $scale cursor]}
    set help [pad_get_prop $scale help]
    if {$help != ""} {
	updateStatusText $PAD "" 0
    }
    if {[pad_get_prop $scale active]} {
        pad_set_prop $scale active 0
        $PAD damage [pad_get_prop $scale id]
        foreach portal [pad_get_prop $scale portals] {
            catch {$PAD damage $portal}
        }
    }
}

proc pad_scale_motion {scale u v portals} {
    global _widget

    if {[pad_get_prop $scale state] == 1} {
        set PAD [pad_get_prop $scale PAD]
        set id [pad_get_prop $scale id]
        set active [pad_get_prop $scale active]

	set x1 [pad_get_prop $scale tx]
	set y1 [pad_get_prop $scale ty]
	if {[pad_get_prop $scale orient] == "h"} {
            set x2 [expr $x1 + [pad_get_prop $scale troughwidth]]
            set y2 [expr $y1 + [pad_get_prop $scale troughheight]]
	} else {
            set y2 [expr $x1 + [pad_get_prop $scale troughwidth]]
	    if {[pad_get_prop $scale showvalue]} {
                set x1 [expr [pad_get_prop $scale dw]+[pad_get_prop $scale df]]
	    }
            set x2 [expr $x1 + [pad_get_prop $scale troughheight]]
	}
        set inscale [bbenclosedoron $u $v "$x1 $y1 $x2 $y2"]

        if {!$active && $inscale} {
            $PAD config -cursor "top_left_arrow"
            pad_set_prop $scale active 1
            $PAD damage $id
	    $PAD focus $id
	    focus $PAD
            set portal [eventPortal $portals]
            if {$portal != ""} {
                set prtls [pad_get_prop $scale portals]
                if {[lsearch $prtls $portal] == -1} {
                    pad_set_prop $scale portals [lappend prtls $portal]
                }
            }
            foreach portal [pad_get_prop $scale portals] {
                catch {$PAD damage $portal}
	    }
        } elseif {$active && !$inscale} {
	    catch {$PAD config -cursor [pad_get_prop $scale cursor]}
            pad_set_prop $scale active 0
            $PAD damage $id
            foreach portal [pad_get_prop $scale portals] {
                catch {$PAD damage $portal}
            }
        }
    }
}

proc pad_scale_arrow_key {scale orient dir} {

    if {[pad_get_prop $scale active] \
     && $orient == [pad_get_prop $scale orient]} {
	set value [pad_get_prop $scale value]
	set from [pad_get_prop $scale from]
	set to [pad_get_prop $scale to]
	if {($value > $from && $dir == "l") \
	 || ($value < $to && $dir == "r")} {
	    set range [expr $to - $from]
	    set prec [pad_get_prop $scale precision]
	    if {$prec > 0} {
		set dv [expr 1./pow(10,$prec)]
	    } elseif {$range <= 100}  {
		set dv 1
	    } elseif {$range <= 500}  { 
		set dv 5
	    } elseif {$range <= 1000} { 
		set dv 10
	    } elseif {$range <= 2000} { 
		set dv 20
	    } elseif {$range <= 5000} { 
		set dv 50
	    } else {
		set dv 100
	    }
	    if {$dir == "l"} {
		set value [expr $value - $dv]
	    } else {
		set value [expr $value + $dv]
	    }
	    ConfigScaleValue [pad_get_prop $scale PAD] $scale \
		[pad_get_prop $scale id] $value
	    set command [pad_get_prop $scale command]
	    if {$command != ""} {
	        uplevel #0 "eval $command $value"
            }
	}
    }
}
