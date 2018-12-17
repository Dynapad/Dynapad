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

proc makeGrid {PAD} {
    global _grid

    if {[info exists _grid(obj)]} {
	return $_grid(obj)
    }
    set obj [$PAD create tcl -maxsize -1 -events 0 -clipping 0]
    set _grid(obj) $obj
    set _grid(lines) 5
    set _grid(axis_color) [$PAD color alloc "gray20"]
    set _grid(grid1_color) [$PAD color alloc "gray65"]
    set _grid(grid2_color) [$PAD color alloc "gray55"]

				# Handle grid deletion
    $PAD bind $obj <Delete> {grid_Delete %W}
				# Don't want grid to get written out
    $PAD bind $obj <Write> {set Pad_Write 0; return ""}

    $PAD itemconfigure $obj -renderscript "grid $PAD $obj" 
    $PAD itemconfigure $obj -viewscript "viewgrid $PAD $obj"
    $PAD itemconfigure $obj -bb "gridbb $PAD"
    $PAD lower $obj

    return $obj
}

proc grid_Delete {PAD} {
    global _grid

    unset _grid(obj)
    set _grid(status) 0
}

proc grid {PAD obj} {
    global _grid

    set view [$PAD getview]
    set x [lindex $view 0]
    set y [lindex $view 1]
    set zoom [lindex $view 2]

    set level [expr pow(2, int(log($zoom)/log(2)))]

    rendergrid $PAD $obj $x $y $zoom [expr 2 * $level] $_grid(grid1_color)
    rendergrid $PAD $obj $x $y $zoom $level $_grid(grid2_color)
}

proc viewgrid {PAD obj} {
    set bbox [gridbb $PAD]
    set center [bbcenter $bbox]
    $PAD ic $obj -bb "gridbb $PAD" -place "$center 1"
}


proc rendergrid {PAD obj x y zoom level color} {
    global _grid

    set w [winfo width $PAD]
    set h [winfo height $PAD]
    set deltaw [expr $w / ($_grid(lines) * $level)]
    set deltah [expr $h / ($_grid(lines) * $level)]

    set bbox [gridbb $PAD]

    set xmin [lindex $bbox 0]
    set ymin [lindex $bbox 1]
    set xmax [lindex $bbox 2]
    set ymax [lindex $bbox 3]

    $PAD render config -linewidth 0 -color $color

    for {set i $xmin} {$i <= $xmax} {set i [expr $i + $deltaw]} {
	$PAD render draw line $i $ymin $i $ymax
    }
    for {set i $ymin} {$i <= $ymax} {set i [expr $i + $deltah]} {
	$PAD render draw line $xmin $i $xmax $i
    }

    $PAD render config -color $_grid(axis_color)
    $PAD render draw line $xmin 0 $xmax 0
    $PAD render draw line 0 $ymin 0 $ymax
}

proc gridbb {PAD} {
    global _grid

				# Get visible bounding box
    set viewbbox [$PAD bbox 1]
    set xmin [lindex $viewbbox 0]
    set ymin [lindex $viewbbox 1]
    set xmax [lindex $viewbbox 2]
    set ymax [lindex $viewbbox 3]

				# Snap grid to delta
    set view [$PAD getview]
    set zoom [lindex $view 2]
    set level [expr pow(2, int(log($zoom)/log(2)))]
    set w [winfo width $PAD]
    set h [winfo height $PAD]
    set deltaw [expr $w / ($_grid(lines) * $level)]
    set deltah [expr $h / ($_grid(lines) * $level)]

    set xmin [expr floor($xmin / $deltaw) * $deltaw]
    set ymin [expr floor($ymin / $deltah) * $deltah]
    set xmax [expr ceil($xmax / $deltaw) * $deltaw]
    set ymax [expr ceil($ymax / $deltah) * $deltah]

    return "$xmin $ymin $xmax $ymax"
}


