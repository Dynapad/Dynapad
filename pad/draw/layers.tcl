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
# Make a dialog box for controlling PadDraw layers,
# both what layers are visible, and what is the
# current layer (i.e., where new objects get created).
#
proc make.layer {PAD} {
    global _color _layer _font

    if {[winfo exists .layer]} {
        raise .layer
	return
    }
    toplevel .layer
    wm resizable .layer 0 0
    label .layer.title -text "Pad++ Main View Layers" \
	-font $_font(tools) \
	-bg $_color(toolbg) 
    pack .layer.title -ipady 10

    layer_MakeEntries $PAD .layer 1

    button .layer.new -text "New Layer" -command "layer_NewLayer $PAD .layer 1" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    button .layer.close -text "Close" -command {destroy .layer} \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    pack .layer.new -fill x
    pack .layer.close -fill x

    pad_ManageWindow .layer 0
    wm title .layer "Pad++ Layers"
}

proc layer_MakePortalLayerProps {PAD item} {
    global _color _layer _font

    set item [lindex $item 0]
    if {[$PAD type $item] != "portal"} {
	return
    }

    set layerid 1
    set layer ".layer$layerid"
    while {[winfo exists $layer]} {
	incr layerid
	set layer ".layer$layerid"
    }

    toplevel $layer -bg $_color(toolbg) 
    wm resizable $layer 0 0
    wm title $layer "Pad++ Portal Layers"

    label $layer.title -text "Pad++ Portal Layers" \
	-font $_font(tools) \
	-bg $_color(toolbg)
    pack $layer.title -pady 10

    layer_MakeEntries $PAD $layer $item

    button $layer.close -text "Close" -command "destroy $layer" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    pack $layer.close -fill x

    pad_ManageWindow $layer 0 0
    wm title $layer "Pad++ Portal Layers"
}

proc layer_MakeEntries {PAD frame view} {
    global _layer _color _font

    set layers [lremove [$PAD layer names] "all"]
    set num_layers [llength $layers]
    set _layer(layers) $layers
    set _layer(num_layers) 0
				# Create an entry for each layer
    set current_layer -1
    for {set i 0} {$i < $num_layers} {incr i} {
	set layer [lindex $layers $i]
	layer_NewLayer $PAD $frame $view $layer
	if {$layer == "main"} {
	    set current_layer $i
	}
    }
    if {$current_layer != -1} {
	layer_SetCurrent $PAD $view $current_layer
    }

    button $frame.all -text "All Layers" -command "layer_SetAllVisible $PAD $frame $view" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    pack $frame.all -fill x
}

#
# Add a new entry to the layer dialog box
# for a specific layer.  If no layer_name is specified,
# then create a dummy layer name.
#
proc layer_NewLayer {PAD frame view {layer_name ""}} {
    global _layer _color _font

    set layer_num $_layer(num_layers)
    incr _layer(num_layers)
    set layers $_layer(layers)
    if {$layer_name == ""} {
	set layer_name "layer[expr $layer_num + 1]"
	lappend layers $layer_name
	set _layer(layers) $layers
    }

				# Make the frame
    set row $frame.f$layer_num
    frame $row -bg $_color(toolbg)

				# Make the radiobutton controlling current layer
    radiobutton $row.active -variable _layer($view.activebutton) \
	-font $_font(tools) \
	-command "layer_SetCurrent $PAD $view $layer_num" -value $layer_num \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    if {$layer_name == "main"} {
	set _layer($view.activebutton) $layer_num
    }

				# Make the checkbutton controlling layer visibility
				# If not sure, make layer visible
    if {![info exists _layer($view.visible$layer_num)]} {
	set _layer($view.visible$layer_num) 1
    }
    checkbutton $row.visible -variable _layer($view.visible$layer_num) \
	-font $_font(tools) \
	-command "layer_SetVisible $PAD $view" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

				# Make the entry for the layer name
    entry $row.label -width 10 -relief flat \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
    $row.label insert 0 $layer_name
    $row.label config -state disabled
				# Make event bindings on entry
    bind $row.label <ButtonPress-1> {
	if {[info exists _layer(activelabel)]} {
	    $_layer(activelabel) config -state disabled -relief flat
	}
	%W config -state normal -relief sunken
	focus %W
	set _layer(activelabel) %W
	set _layer(activelabelname) [%W get]
    }
					# Fix broken default backspace/delete bindings
    bind $row.label <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
    bind $row.label <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}

    bind $row.label <Key-Return> "
	layer_ChangeLayerName $PAD $view \$_layer(activelabelname) \[%W get\]
	unset _layer(activelabel)
	%W config -state disabled -relief flat
    "
    bind $row.label <Leave> "
	if {\[info exists _layer(activelabel)\]} {
	    if {\$_layer(activelabel) == \"%W\"} {
		layer_ChangeLayerName $PAD $view \$_layer(activelabelname) \[%W get\]
		unset _layer(activelabel)
		%W config -state disabled -relief flat
	    }
	}	    
    "

				# Make this the current layer
    layer_SetCurrent $PAD $view $layer_num

				# Pack the layer entry
    if {[winfo exists $frame.all]} {
	set beforecmd "-before $frame.all"
    } else {
	set beforecmd ""
    }
    eval pack $row -fill x -expand t $beforecmd
    pack $row.active -side left
    pack $row.visible -side left
    pack $row.label -side left
}

proc layer_SetCurrent {PAD view layernum} {
    global _layer

    set _layer(current) [lindex $_layer(layers) $layernum]
    set _layer($view.activebutton) $layernum
    layer_SetVisible $PAD $view
}

proc layer_SetVisible {PAD view} {
    global _layer

				# Find active layers
    set all 1
    set layers ""
    for {set i 0} {$i < $_layer(num_layers)} {incr i} {
	if {$_layer($view.visible$i)} {
	    lappend layers [lindex $_layer(layers) $i]
	} else {
	    set all 0
	}
    }

    if {$all} {
	$PAD ic $view -visiblelayers all
    } else {
	if {$layers == ""} {
	    $PAD ic $view -visiblelayers none
	} else {
	    $PAD ic $view -visiblelayers $layers
	}
    }
}

proc layer_SetAllVisible {PAD frame view} {
    global _layer
    
    set layers [lremove [$PAD layer names] "all"]
    foreach layer $layers {
	if {![lmember $_layer(layers) $layer]} {
	    lappend _layer(layers) $layer
	    layer_NewLayer $PAD $frame $view $layer
	}
    }
    for {set i 0} {$i < $_layer(num_layers)} {incr i} {
	set _layer($view.visible$i) 1
	layer_SetVisible $PAD $view
    }
}


proc layer_ChangeLayerName {PAD view old_name new_name} {
    global _layer

    regsub $old_name $_layer(layers) $new_name _layer(layers)
    foreach item [$PAD find -groupmembers all] {
	if {[$PAD ic $item -layer] == $old_name} {
	    $PAD ic $item -layer $new_name
	}
    }
    if {$_layer(current) == "$old_name"} {
	set _layer(current) $new_name
    }
    layer_SetVisible $PAD $view
}
