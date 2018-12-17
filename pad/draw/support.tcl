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

#################################################################

proc confs {{PAD .pad}} {
    set confs [$PAD config]
    foreach item $confs {
	puts [format "%20s = %s" [lindex $item 0] [lindex $item 4]]
    }
}

proc types {{PAD .pad}} {
    foreach i [$PAD find all] {puts [format "%3d: %10s" $i [$PAD type $i]]}
}

proc tags {{PAD .pad}} {
    foreach i [$PAD find all] {puts [format "%3d: %s" $i [$PAD gettags $i]]}
}

proc objs {{PAD .pad}} {
    foreach obj [$PAD find -groupmembers all] {
	puts [format "%3d: %10s, Tags: (%s)" $obj [$PAD type $obj] [$PAD gettags $obj]]
    }
}

proc layers {{PAD .pad}} {
    foreach obj [$PAD find -groupmembers all] {
	puts [format "%3d: %10s, (Layer: %s)" $obj [$PAD type $obj] [$PAD ic $obj -layer]]
    }
}

proc trees {{PAD .pad}} {
    set root [$PAD find withtag treeroot]
    if {$root == ""} {
	return
    }
    _tree_print_info $PAD "Root list" $root
    foreach subroot [$PAD tree getchildren $root] {
	_tree_print_info $PAD "Root" $subroot
	foreach child [$PAD tree getchildren $subroot] {
	    _tree_recurse $PAD $child 2
	}
    }
}

proc _tree_recurse {PAD node level} {
    for {set i 0} {$i < $level} {incr i} {
	puts -nonewline "  "
    }
    _tree_print_info $PAD "Node" $node
    foreach child [$PAD tree getchildren $node] {
	_tree_recurse $PAD $child [expr $level + 1]
    }
}

proc _tree_print_info {PAD type node} {
    set link [$PAD tree getlink $node]
    set focus [$PAD tree getfocus $node]
    puts -nonewline "$type: $node"
    if {$link != ""} {
	puts -nonewline " (link: $link)"
    }
    puts [format " (focus:%5.2f)" $focus]
}

#################################################################

#
# Debugging scripts by Stephen Uhler, from Linux Journal, October 1995
# Slightly modified by BBB
#

#
# Sets a breakpoint in tcl code.  Valid commands are:
#   up       - Move stack frame up one
#   down     - Move stack frame down one
#   continue - Continue, i.e., leave breakpoing
#   ?        - Show current line
#
proc breakpoint {} {
    set max [expr [info level] - 1]
    set current $max
    show $current
    while {1} {
	puts -nonewline stderr "#$current: "
	gets stdin line
	while {![info complete $line]} {
	    puts -nonewline stderr "? "
	    append line \n[gets stdin]
	}
	switch -- $line {
	    up {if {$current > 0} {
		show [incr current -1]
	      }
	    }
	    down {if {$current < $max} {
		show [incr current]
	      }
	    }
	    continue {puts stderr "Resuming execution"; return}
	    ? {show $current}
	    default {
		catch {uplevel #$current $line } result
		puts stderr $result
	    }
	}
    }
}

proc show {current} {
    if {$current > 0} {
	set info [info level $current]
	set proc [lindex $info 0]
	puts stderr "$current: Procedure $proc {[info args $proc]}"
	set index 0
	foreach arg [info args $proc] {
	    puts stderr "\t$arg = [lindex $info [incr index]]"
	}
    } else {
	puts stderr "Top level"
    }
}

##################################

proc tcoord {} {
    for {set i 1} {$i <= 50} {incr i} {
	set id [.pad create rectangle .1 .1 .8 .8 -penwidth .02]
	.pad pushcoordframe $id
    }
    .pad resetcoordframe
}

proc pa {array} {
    global $array
    foreach element [array names $array] {
	puts -nonewline "$array"
	puts "($element) = [set [set array]($element)]"
    }   
}

proc ta {} {
    import .pad /home/bederson/aar/aar.outline
}

proc tb {} {
    .pad create button -text Hello -command {puts Hello}
}

proc tc {} {
    set x 0
    for {set i 100} {$i < 400} {incr i 1} {
	set color "#$i"
	.pad create line $x 0 $x 100 -penwidth 1.5 -pen $color
	incr x 1
    }
}

proc tm {} {
    for {set i 0} {$i < 50} {incr i 1} {
	for {set j 0} {$j < 50} {incr j 1} {
	    .pad create line $i $j [expr $i + 0.5] [expr $j + 0.5] -penwidth .3 -pen blue -alwaysrender 1
	}
    }
    puts "done"
}

proc ti {} {
    global env
    set image [.pad image alloc $env(PADHOME)/images/unm_logo.gif]
    .pad create image -image $image -tags "item image_item"
}

proc tg {} {
    set lines [.pad find withtag line]
    pad_group .pad $lines
}

proc tu {} {
    pad_ungroup .pad [.pad find all]
}

proc th {} {
#    import .pad http://found.cs.nyu.edu
    import .pad html/test.html
#    import .pad http://www.cs.unm.edu/~bederson
}

proc tp {} {
    if {[info command .p] == ""} {
	pad .p
    }
    .p delete all
    .p create line 0 0 .5 .5 -penwidth .1
    .pad create portal 0 0 1 1 -lookon .p
}

proc tt {} {
    .pad delete all
    source draw/timeline.tcl
    .pad moveto 0 0 1
    import .pad draw/calendar.timeline
}

proc to {} {
    global env

    source $env(PADHOME)/draw/outline.tcl
#    import .pad "/home/bederson/text/aar.outline"
#    import .pad pad.outline
#    import .pad /home/bederson/tcl_class/pad_teach.outline
    import .pad t.outline
#     import .pad events.outline
}

proc tw {} {
    .pad write pad.pad
}

proc tr {} {
    .pad delete all
    .pad update
    .pad read pad.pad
}

proc fs {} {
    return [.pad find -groupmembers withtag selected]
}

