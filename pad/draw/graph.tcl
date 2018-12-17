
#
# Graph initialization routine
# Needs to be called before graph can be created
#
proc pad_GraphInit {PAD} {
    global env _graph _color
                                        # Create graph types and options
    $PAD addtype graph pad_Graph

    $PAD addoption graph -apen        "pad_GraphConfig -apen" red
    $PAD addoption graph -borderwidth "pad_GraphConfig -borderwidth" 0.5
    $PAD addoption graph -data        "pad_GraphConfig -data" \
	{0 0 1 .5 2 1.5 3 3 4 7 5 9 6 7 7 2.5 8 4 9 5 10 3}
    $PAD addoption graph -fill        "pad_GraphConfig -fill" $_color(menubg)
    $PAD addoption graph -font        "pad_GraphConfig -font" Times-4
    $PAD addoption graph -gpen        "pad_GraphConfig -gpen" gray70
    $PAD addoption graph -height      "pad_GraphConfig -height" 120
    $PAD addoption graph -help        "pad_GraphConfig -help" \
	"This is the default graph"
    $PAD addoption graph -lpen        "pad_GraphConfig -lpen" gray20
    $PAD addoption graph -pen         "pad_GraphConfig -pen" blue
    $PAD addoption graph -width       "pad_GraphConfig -width" 122
    $PAD addoption graph -xmin        "pad_GraphConfig -xmin" 0
    $PAD addoption graph -xmax        "pad_GraphConfig -xmax" 10
    $PAD addoption graph -ymin        "pad_GraphConfig -ymin" 0
    $PAD addoption graph -ymax        "pad_GraphConfig -ymax" 10

				# Load in graph KPL code
    kpl eval '$env(PADHOME)/draw/graph.kpl source

                                # Initialize default attrs array
    set _graph(attrs) {apen borderwidth data fill font gpen height help lpen\
	pen width xmin xmax ymin ymax}
    pad_GraphInitattrs $PAD _graph
}

#
# Graph attributes array initiatialization routine.
# Should be called before graph is created
#
proc pad_GraphInitattrs { PAD graph {id ""}} {
    global _widget _color

    pad_set_prop $graph PAD $PAD
    pad_set_prop $graph id $id

    pad_set_prop $graph apen [$PAD color alloc red]
    pad_set_prop $graph borderwidth 0.5
    pad_set_prop $graph fill $_color(menubg)
    pad_set_prop $graph font Times-4
    pad_set_prop $graph gpen [$PAD color alloc gray70]
    pad_set_prop $graph height 120
    pad_set_prop $graph help "This is the default graph"
    pad_set_prop $graph lpen [$PAD color alloc gray20]
    pad_set_prop $graph pen [$PAD color alloc blue]
    pad_set_prop $graph width 122
    pad_set_prop $graph xmin 0
    pad_set_prop $graph xmax 10
    pad_set_prop $graph ymin 0
    pad_set_prop $graph ymax 10
					    # set some fake data
    pad_set_prop $graph data {0 0 1 .5 2 1.5 3 3 4 7 5 9 6 7 7 2.5 8 4 9 5 10 3}
    pad_set_prop $graph npts 11

    set bbox [$PAD font bbox " 0 " [pad_get_prop $graph font]]
    pad_set_prop $graph labeldx [expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
    pad_set_prop $graph labeldy [expr ([lindex $bbox 3]-[lindex $bbox 1])/2.]
    set bbox [$PAD font bbox " 10 " [pad_get_prop $graph font]]
    set size [expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
    pad_set_prop $graph xmax_size $size
    pad_set_prop $graph ymax_size $size
}

################################################################
#
# Pad graph widget
# usage: "pathName create graph ?option value ...?
# where legal options are:
#   -apen     : Axis color
#   -borderwidth: Border thickness
#   -data     : X-Y Data of graph
#   -fill     : Fill color (controls border also)
#   -font     : Font of graph text
#   -gpen     : Grid color
#   -height   : Height of graph in pixels
#   -help     : Help text of graph
#   -lpen     : Label text color
#   -pen      : Graph line color
#   -width    : Width of graph in pixels
#   -xmax     : Top axis X value of graph
#   -xmin     : Bottom X axis value of graph
#   -ymin     : Right Y axis value of graph
#
# For each graph widget, a global array is created with the following elements:
#   'PAD'         - Pad widget graph is on
#   'apen'        - Axis color of graph
#   'borderwidth' - Border thickness
#   'data'        - Data of graph
#   'fill'        - Fill color (controls border)
#   'fill_border' - Name of border from border alloc
#   'font'        - Font of graph text
#   'gpen'        - Color of graph grid
#   'height'      - Height of graph
#   'help'        - Help text of graph
#   'id'          - Id of graph
#   'labeldx'     - Half-width of label text
#   'labeldy'     - Half-height of label text
#   'lpen'        - Color of graph text
#   'npts'        - Number of graph data points ((length of data)/2)
#   'pen'         - Color of graph
#   'width'       - Width of graph
#   'xmax'        - Top axis X value of graph
#   'xmax_size'   - Half-width of xmax text
#   'xmin'        - Bottom X axis value of graph
#   'ymax'        - Left Y axis value of graph
#   'ymax_size'   - Half-width of ymax text
#   'ymin'        - Right Y axis value of graph
#
################################################################

################################################################
#
# Graph creation
#
################################################################

proc pad_Graph {PAD} {
    set id [$PAD create kpl]
    set graph _graph$id

    pad_GraphInitattrs $PAD $graph $id

    # uncomment to make scales that change the y axis min and max values
    # after 0 "pad_GraphMakeScales $PAD $id"

    pad_widget_reborder $graph
    pad_widget_resize $graph
    RescaleGraph $id

    $PAD ic $id -renderscript "'$graph graph_render"

    $PAD bind $id <Run-Enter> "
	set help \[pad_get_prop $graph help\]
	if {\$help != {}} {
	    updateStatusText $PAD \$help 1
	}
    "
    $PAD bind $id <Run-Leave> "
	set help \[pad_get_prop $graph help\]
	if {\$help != {}} {
	    updateStatusText $PAD {} 1
	}
    "

                        # Setup event binding for writing out
    $PAD bind $id <Write> { return [pad_GraphWrite %P %O] }

                        # Free up resources properly when widget deleted
    $PAD bind $id <Delete> "unset _graph$id"

    return $id
}

proc pad_GraphMakeScales {PAD graph} {
    set s1 [$PAD create scale -from 5 -to 20 -x -73 -y 35 -value 10 \
	-orient v -help "The maximum Y-axis value"]
    set s2 [$PAD create scale -from -10 -to 5 -x -73 -y -60 -value 0 \
	-orient v -help "The minimum Y-axis value"]
    $PAD ic $s1 -command "DoGraphScaleCmd $PAD $graph $s1 $s2 ymax" -z .25
    $PAD ic $s2 -command "DoGraphScaleCmd $PAD $graph $s1 $s2 ymin" -z .25
}

#
# Routine for writing out neccessary tcl commands
# to create the graph.  Only attributes with non-default
# values are written out.
#
proc pad_GraphWrite {PAD obj} {
    global _graph

    set result ""
    set widget "_graph$obj"; # scale attr array
    global $widget

    set attrs $_graph(attrs)
    foreach attr $attrs {
        set value [$PAD ic $obj -$attr]
        if {"$value" != $_graph($attr)} {
            append  result "\$PADLOAD ic \$Pad_ID -$attr"
            lappend result $value
            append  result "\n"
        }
    }
    return $result
}

#
# Graph configure
#

proc pad_GraphConfig args {
    set option [lindex $args 0]
    set PAD [lindex $args 1]
    set id [lindex $args 2]
    set got_value 0

    if {[llength $args] >= 4} {
	set value [lindex $args 3]
	set got_value 1
    }

    set graph _graph$id
    global $graph

    if {$got_value} {
	switch -exact -- $option {
	-apen {
	    $PAD color free [pad_get_prop $graph apen]
	    pad_set_prop $graph apen [$PAD color alloc $value]
	    $PAD damage $id
	}

	-borderwidth {
	    if {$value < 0} {
		return -code error \
		    "-borderwidth expected number > 0, got $value"
	    }
	    pad_set_prop $graph borderwidth $value
	    $PAD damage $id
	}

	-data {
	    pad_set_prop $graph data "$value"
	    pad_set_prop $graph npts [expr [llength $value]/2]
	    RescaleGraph $id
	    $PAD damage $id
	}

	-fill { pad_set_prop $graph fill $value ; pad_widget_reborder $graph }

	-font {
	    global _widget
	    pad_set_prop $graph font $value
	    set bbox [$PAD font bbox " [pad_get_prop $graph ymin] " $value]
	    pad_set_prop $graph labeldx \
		[expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
	    pad_set_prop $graph labeldy \
		[expr ([lindex $bbox 3]-[lindex $bbox 1])/2.]
	    set bbox [$PAD font bbox " [pad_get_prop $graph xmax] " $value]
	    pad_set_prop $graph xmax_size \
		[expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
	    set bbox [$PAD font bbox " [pad_get_prop $graph ymax] " $value]
	    pad_set_prop $graph ymax_size \
		[expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
	    $PAD damage $id
	}

	-gpen {
	    $PAD color free [pad_get_prop $graph gpen]
	    pad_set_prop $graph gpen [$PAD color alloc $value]
	    $PAD damage $id
	}

	-height { pad_set_prop $graph height $value; pad_widget_resize $graph }
	-help { pad_set_prop $graph help "$value" }

	-lpen {
	    $PAD color free [pad_get_prop $graph lpen]
	    pad_set_prop $graph lpen [$PAD color alloc $value]
	    $PAD damage $id
	}

	-pen {
	    $PAD color free [pad_get_prop $graph pen]
	    pad_set_prop $graph pen [$PAD color alloc $value]
	    $PAD damage $id
	}

	-width { pad_set_prop $graph width $value ; pad_widget_resize $graph }
	-xmax {
	    pad_set_prop $graph xmax $value
	    set bbox [$PAD font bbox " $value " [pad_get_prop $graph font]]
	    pad_set_prop $graph xmax_size \
		[expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
	    RescaleGraph $id
	    $PAD damage $id
	}
	-xmin {
	    pad_set_prop $graph xmin $value
	    RescaleGraph $id
	    $PAD damage $id
	}

	-ymax {
	    pad_set_prop $graph ymax $value
	    set bbox [$PAD font bbox " $value " [pad_get_prop $graph font]]
	    pad_set_prop $graph ymax_size \
		[expr ([lindex $bbox 2]-[lindex $bbox 0])/2.]
	    RescaleGraph $id
	    $PAD damage $id
	}
	-ymin {
	    pad_set_prop $graph ymin $value
	    RescaleGraph $id
	    $PAD damage $id
	}

	default {return -code error}
      }
    }

    set option_name [string range $option 1 end]
    return [pad_get_prop $graph $option_name]
}


proc RescaleGraph {id} {
    global _graph$id

    set xmin [set _graph${id}(xmin)]
    set xrange [expr 100.0/([set _graph${id}(xmax)] - $xmin)]
    set ymin [set _graph${id}(ymin)]
    set yrange [expr 100.0/([set _graph${id}(ymax)] - $ymin)]

    set i 0     
    set j 0
    foreach coord [set _graph${id}(data)] {
	if {$i % 2 } {
	    set coord [expr 13 + ($coord - $ymin)*$yrange]
	} else {
	    set coord [expr 15 + ($coord - $xmin)*$xrange]
	}
	if {$i == 0} {
	    set line "$coord"
	} else {
	    append line ":$coord"
	}
    
	if {$i == 14} {
	    set x $coord
	} elseif {$i == 15} {
	    set _graph${id}(data.$j) $line
	    set line "$x:$coord"
	    set i 1
	    incr j
	}
	incr i
    }
    if {$i == 2} {
	set _graph${id}(nlines) $j
    } else {
	set _graph${id}(data.$j) $line
	set _graph${id}(nlines) [expr $j + 1]
    }
}


proc DoGraphScaleCmd {PAD id s1 s2 prop value} {

    if {$prop == "ymax"} {
	$PAD ic $id -ymax [set ymax $value]
	set ymin [pad_get_prop _graph$id ymin]
    } else {
	$PAD ic $id -ymin [set ymin $value]
	set ymax [pad_get_prop _graph$id ymax]
    }
    set dy [expr $ymax - $ymin]
    if {$dy > 1} {
	set halfway [expr $ymin + $dy/2]
	$PAD ic $s1 -from $halfway -to [expr $ymax + $dy]
	$PAD ic $s2 -to $halfway -from [expr $ymin - $dy]
    } elseif {$prop == "ymax"} {
	$PAD ic $s1 -to [expr $ymax + 1]
	$PAD ic $s2 -to [expr $ymax - 1] -from [expr $ymax - 2]
    } else {
	$PAD ic $s1 -from [expr $ymin + 1] -to [expr $ymin + 2]
	$PAD ic $s2 -from [expr $ymin - 1]
    }
}
