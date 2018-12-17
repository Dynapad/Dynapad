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

###############################################################

proc makeMagnifyTest {PAD} {
				# Don't let magnify lenses get written out
    $PAD bind magnify <Write> {set Pad_Write 0; break}

    set view [$PAD getview]
    set x [lindex $view 0]
    set y [lindex $view 1]
    set zoom [lindex $view 2]
    set portal1 [$PAD create portal -75 -75 75 75 -fill #e05050 -minsize 10 \
	    -tags "trackView magnify item drag" -borderwidth 5 -title "Magnifier" \
	    -place "[expr $x - (50 / $zoom)] [expr $y + (75 / $zoom)] [expr 1.0 / $zoom]"]
    $PAD ic $portal1 -view "0 0 [expr 2.0 * $zoom]"
    trackView $PAD $portal1
}

###############################################################

proc makeClockTest {PAD} {
				# Don't let clocks get written out
    $PAD bind clock <Write> {set Pad_Write 0; break}

    set view [$PAD getview]
    set x [lindex $view 0]
    set y [lindex $view 1]
    set zoom [lindex $view 2]
    set obj [$PAD create rectangle -1 -1 11 11 -fill white -minsize 10 \
	    -place "[expr $x + (50 / $zoom)] $y [expr 10 / $zoom]" -tags "clock"]
    global $obj
    set [set obj](data) [$PAD getdate]
    $PAD itemconfig $obj -renderscript "tclClockRender $PAD $obj"
    $PAD itemconfig $obj -timerscript "updateClockTime $PAD $obj" -timerrate 1000
    set zoom [$PAD getzoom]
    set portal [$PAD create portal 0 0 125 125 -fill yellow3 -minsize 10 \
	    -tags "trackView clocklens item clock drag" -borderwidth 3 -title "Analog Clock" \
	    -place "[expr $x - (125 / $zoom)] [expr $y + (50 / $zoom)] [expr 1.0 / $zoom]"]
    trackView $PAD $portal
    $PAD bind $portal <PortalIntercept> {break}
    set [set obj](fillColor) [$PAD color alloc red4]
    set [set obj](penColor) [$PAD color alloc black]
    $PAD bind $obj <Delete> {
	%P color free [set %O(fillColor)]
	%P color free [set %O(penColor)]
    }

    proc clockRender$portal {PAD obj} {
	analogClockRender $PAD $obj
    }
    return $obj
}

proc updateClockTime {PAD obj} {
    global $obj
    set [set obj](data) [$PAD getdate]
    $PAD damage $obj
}

proc tclClockRender {PAD obj} {
    set portal [lindex [$PAD render config -portals] end]
    if {[catch {clockRender$portal $PAD $obj}]} {
	digitalClockRender $PAD $obj
    }
}

proc digitalClockRender {PAD obj} {
    global $obj _clock

    set size [eval $PAD getsize $obj [$PAD render config -portals]]
    $PAD render config -linewidth 0 -color [set [set obj](penColor)]
    $PAD render draw line 0 0 10 0 10 10 0 10 0 0

    set info [set [set obj](data)]
    set date [lrange $info 0 2]
    set time [lindex $info 3]
    set hour [string range $time 0 1]
    set meridian "am"
    if {$hour > 12} {set hour [expr $hour - 12]; set meridian "pm"}
    set minute [string range $time 3 4]
    set second [string range $time 6 7]
    $PAD render config -linewidth 0.1 -color [set [set obj](penColor)] -font "Times-1"
    if {$size > 250} {
	$PAD render config -fontheight 1.2
	$PAD render draw text "$hour:$minute:$second$meridian" 0.5 6
	$PAD render draw text "$date" .5 3
    } elseif {$size > 100} {
	$PAD render config -fontheight 1.2
	$PAD render draw text "$hour:$minute:$second$meridian" 0.5 4.5
    } elseif {$size > 60} {
	$PAD render config -fontheight 2.0
	$PAD render draw text "$hour:$minute$meridian" 0.5 4.5
    } else {
	$PAD render config -fontheight 3.0
	$PAD render draw text "$hour$meridian" 0.5 4
    }
}

proc analogClockRender {PAD obj} {
    global $obj

    set size [eval $PAD getsize $obj [$PAD render config -portals]]
    $PAD render config -linewidth 0 -color [set [set obj](penColor)]
    $PAD render draw line 0 0 10 0 10 10 0 10 0 0

    set date [set [set obj](data)]
    set time [lindex $date 3]
    set hour [string range $time 0 1]
    if {[string index $hour 0] == 0} {
	set hour [string index $hour 1]
    }
    if {$hour > 12} {set hour [expr $hour - 12]}
    set minute [string range $time 3 4]
    if {[string index $minute 0] == 0} {
	set minute [string index $minute 1]
    }
    set second [string range $time 6 7]
    if {[string index $second 0] == 0} {
	set second [string index $second 1]
    }

				# Render hour labels
    if {$size > 50} {
	$PAD render config -linewidth 0.1 -color [set [set obj](penColor)] -fontheight 0.9
	for {set i 1} {$i <= 12} {incr i} {
	    set angle [expr 6.28 * ($i / 12.0)]
	    set x [expr 4.5 + 4.3 * sin($angle)]
	    set y [expr 4.5 + 4.3 * cos($angle)]
	    $PAD render draw text $i $x $y
	}

				# Render second hand
	set sangle [expr 6.28 * ($second / 60.0)]
	set x [expr 5 + 3.5 * sin($sangle)]
	set y [expr 5 + 3.5 * cos($sangle)]
	$PAD render config -linewidth 0.1 -color [set [set obj](fillColor)]
	$PAD render draw line 5 5 $x $y
    } else {
    }

				# Render minute hand
    $PAD render config -linewidth 0.2 -color [set [set obj](penColor)]
    set mangle [expr 6.28 * ($minute / 60.0)]
    set x [expr 5 + 4 * sin($mangle)]
    set y [expr 5 + 4 * cos($mangle)]
    $PAD render draw line 5 5 $x $y
    $PAD render config -linewidth 0.5 -capstyle round
    $PAD render draw line $x $y $x $y

				# Render hour hand
    set hangle [expr ($mangle / 12.0) + (6.28 * ($hour / 12.0))]
    set x [expr 5 + 2.7 * sin($hangle)]
    set y [expr 5 + 2.7 * cos($hangle)]
    $PAD render config -linewidth 0.2
    $PAD render draw line 5 5 $x $y

				# Render center dot
    $PAD render config -capstyle round -linewidth 1.0
    $PAD render draw line 5 5 5 5
}

######################################################################

proc makeChartTest {PAD} {
    global _chart

				# Don't let charts get written out
    $PAD bind chart <Write> {set Pad_Write 0; break}

    set view [$PAD getview]
    set x [lindex $view 0]
    set y [lindex $view 1]
    set zoom [lindex $view 2]
    set dx [expr 150 / $zoom]
    set dy [expr 150 / $zoom]
    set obj(0) [$PAD create rectangle -1 -1 12 12 -fill white -minsize 10 \
	    -place "$x $y [expr 10 / $zoom]" -tags "chart drag"]
    set obj(1) [$PAD create rectangle -1 -1 12 12 -fill white -minsize 10 \
	    -place "[expr $x + $dx] $y [expr 10 / $zoom]" -tags "chart drag"]
    set obj(2) [$PAD create rectangle -1 -1 12 12 -fill white -minsize 10 \
	    -place "[expr $x + $dx] [expr $y + $dy] [expr 10 / $zoom]" -tags "chart drag"]
    set obj(3) [$PAD create rectangle -1 -1 12 12 -fill white -minsize 10 \
	    -place "$x [expr $y + $dy] [expr 10 / $zoom]" -tags "chart drag"]
    for {set i 0} {$i < 4} {incr i} {
	$PAD itemconfig $obj($i) -renderscript "dataRender $PAD $obj($i)"
	set _chart(xrange$obj($i)) "0 20"
	set _chart(yrange$obj($i)) "0 15"
	set _chart(fill$obj($i)) [$PAD color alloc blue4]
	set _chart(pen$obj($i)) [$PAD color alloc black]
	$PAD bind $obj($i) <Delete> {
	    %P color free $_chart(fill%O)
	    %P color free $_chart(pen%O)
	}
    }

    set _chart(data$obj(0)) "{10 8.04} {8 6.95} {13 7.58} {9 8.81} {11 8.33} {14 9.96} {6 7.24} {4 4.26} {12 10.84} {7 4.82} {5 5.68}"
    set _chart(data$obj(1)) "{10 9.14} {8 8.14} {13 8.74} {9 8.77} {11 9.26} {14 8.1} {6 6.13} {4 3.1} {12 9.13} {7 7.26} {5 4.74}"
    set _chart(data$obj(2)) "{10 7.46} {8 6.77} {13 12.74} {9 7.11} {11 7.81} {14 8.84} {6 6.08} {4 5.39} {12 8.15} {7 6.42} {5 5.73}"
    set _chart(data$obj(3)) "{8 6.58} {8 5.76} {8 7.71} {8 8.84} {8 8.47} {8 7.04} {8 5.25} {19 12.5} {8 5.56} {8 7.91} {8 6.89}"

    set portal1 [$PAD create portal 0 0 150 150 -fill yellow3 -minsize 10 \
	    -tags "trackView item chart drag" -borderwidth 5 -title "Scatter Plot" \
	    -place "[expr $x - (175 / $zoom)] [expr $y + (75 / $zoom)] [expr 1.0 / $zoom]"]
    set portal2 [$PAD create portal 0 0 150 150 -fill pink2 -minsize 10 \
	    -tags "trackView item chart drag" -borderwidth 5 -title "Bar Chart" \
	    -place "[expr $x - (175 / $zoom)] [expr $y - (100 / $zoom)] [expr 1.0 / $zoom]"]
    set zoom [$PAD getzoom]
    trackView $PAD $portal1
    trackView $PAD $portal2
    $PAD bind $portal1 <PortalIntercept> {break}
    $PAD bind $portal2 <PortalIntercept> {break}

    proc itemRender$portal1 {PAD obj} {
	scatterRender $PAD $obj
    }

    proc itemRender$portal2 {PAD obj} {
	barRender $PAD $obj
    }
}

proc dataRender {PAD obj} {
    global _chart

    set data $_chart(data$obj)
    set portal [lindex [$PAD render config -portals] end]
    if {[catch {itemRender$portal $PAD $obj}]} {
	textRender $PAD $obj
    }
}

proc textRender {PAD obj} {
    global _chart

    $PAD render config -linewidth 0 -color $_chart(pen$obj)
    $PAD render draw line 0 0 11 0 11 11 0 11 0 0
    set data $_chart(data$obj)
    set index 0
    set y -1
    $PAD render config -linewidth 0.1 -font Times-0.85
    foreach datum $data {
	set xdatum [lindex $datum 0]
	set ydatum [lindex $datum 1]
	set x [expr 0.5 + ($index % 2)*5.5]
	set y [expr $y + 1.7 * (($index + 1) % 2)]
	$PAD render draw text "($xdatum,$ydatum)" $x $y
	incr index
    }
}

proc scatterRender {PAD obj} {
    global _chart

    $PAD render config -linewidth 0.2 -color $_chart(pen$obj)
    $PAD render draw line 0 11 0 0 11 0
    $PAD render draw line 10.7 -0.3 11 0 10.7 0.3
    $PAD render draw line -0.3 10.7 0 11 0.3 10.7
    set data $_chart(data$obj)
    set s 0.2
    set xscale [expr 10.0 / ([lindex $_chart(xrange$obj) 1] - [lindex $_chart(xrange$obj) 0])]
    set yscale [expr 10.0 / ([lindex $_chart(yrange$obj) 1] - [lindex $_chart(yrange$obj) 0])]
    foreach datum $data {
	set x [expr 0.5 + $xscale * [lindex $datum 0]]
	set y [expr $yscale * [lindex $datum 1]]
	$PAD render config -linewidth 0.1 -color $_chart(fill$obj)
	$PAD render draw line [expr $x - $s] [expr $y - $s] [expr $x + $s] [expr $y + $s]
	$PAD render draw line [expr $x + $s] [expr $y - $s] [expr $x - $s] [expr $y + $s]
    }
}

proc barRender {PAD obj} {
    global _chart

    $PAD render config -linewidth 0.2 -color $_chart(fill$obj)
    $PAD render draw line 0 11 0 0 11 0
    $PAD render draw line 10.7 -0.3 11 0 10.7 0.3
    $PAD render draw line -0.3 10.7 0 11 0.3 10.7
    set data $_chart(data$obj)
    set xscale [expr 10.0 / ([lindex $_chart(xrange$obj) 1] - [lindex $_chart(xrange$obj) 0])]
    set yscale [expr 10.0 / ([lindex $_chart(yrange$obj) 1] - [lindex $_chart(yrange$obj) 0])]
				# Compute averaged histogram
				# Initialize buckets
    for {set i 0} {$i < 10} {incr i} {
	set bucket($i) 0.0
	set count($i) 0
    }
				# Accumulate
    foreach datum $data {
	set x [expr $xscale * [lindex $datum 0]]
	set y [expr $yscale * [lindex $datum 1]]
	set index [expr int($x)]
	set bucket($index) [expr $bucket($index) + $y]
	incr count($index)
    }
				# Normalize
    for {set i 0} {$i < 10} {incr i} {
	if {$count($i) > 0} {
	    set bucket($i) [expr $bucket($i) / $count($i)]
	}
    }
    $PAD render config -linewidth 0.1 -color $_chart(fill$obj)
    for {set i 0} {$i < 10} {incr i} {
	set x $i
	set y $bucket($i)
	$PAD render draw line $x 0 $x $y [expr $x + 1] $y [expr $x + 1] 0
    }
}

###########################################################################

proc makeNumberTest {PAD} {
    global _number _pad

				# Don't let numbers get written out
    $PAD bind number <Write> {set Pad_Write 0; break}

    set view [$PAD getview]
    set x [lindex $view 0]
    set y [lindex $view 1]
    set zoom [lindex $view 2]
    set obj [$PAD create rectangle 0 0 10 10 -fill white -anchor sw -minsize 10 \
	    -place "[expr $x + (50 / $zoom)] $y [expr 10 / $zoom]" -tags "number drag"]
    $PAD itemconfig $obj -renderscript "tclNumberRender $PAD $obj"
    set _number($obj.data) "0"
    set _number($obj.pick) ""
    set _number($obj.font) Times-8
    set portal1 [$PAD create portal 0 0 150 150 -fill yellow3 -minsize 10 \
	    -tags "trackView item number drag" -borderwidth 5 -title "Slider" \
	    -place "[expr $x - (150 / $zoom)] [expr $y + (75 / $zoom)] [expr 1.0 / $zoom]"]
    set portal2 [$PAD create portal 0 0 150 150 -fill pink2 -minsize 10 \
	    -tags "trackView item number drag" -borderwidth 5 -title "Dial" \
	    -place "[expr $x - (150 / $zoom)] [expr $y - (100 / $zoom)] [expr 1.0 / $zoom]"]

    trackView $PAD $portal1
    trackView $PAD $portal2

    proc numberRender$portal1 {PAD obj} {
	sliderRender $PAD $obj
    }
    proc numberRender$portal2 {PAD obj} {
	dialRender $PAD $obj
    }
    proc numberPick$portal1 {PAD obj x y} {
	sliderPick $PAD $obj $x $y
    }
    proc numberPick$portal2 {PAD obj x y} {
	dialPick $PAD $obj $x $y
    }

    set _number($obj.fill) [$PAD color alloc red4]
    set _number($obj.activefill) [$PAD color alloc red]
    set _number($obj.pen) [$PAD color alloc black]
    set _number($obj.line) [$PAD color alloc purple4]
    $PAD bind $obj <Delete> {
	%P color free $_number(%O.fill)
	%P color free $_number(%O.activefill)
	%P color free $_number(%O.pen)
	%P color free $_number(%O.line)
    }

    $PAD bind $obj <Enter> {
	focus %W
	%W focus %O
	break
    }
    $PAD bind $obj <Key> {
	if {[string match {[0-9]} %K]} {
	    set _number(%O.data) %K
	    %W damage %O
	}
	break
    }
    $PAD bind $obj <ButtonPress-1> {
	set obj %O
	set portal [lindex %l end]
	set coords [numberObjCoords %W $obj %i %j]
	set x [lindex $coords 0]
	set y [lindex $coords 1]
	if {[catch {numberPick$portal %P $obj $x $y}]} {
	    set _number($obj.pick) "pan"
	}
	if {$_number($obj.pick) == "pan"} {
	    continue
	} else {
	    %W damage $obj
	    break
	}
    }
    $PAD bind $obj <B1-Motion> {
	set obj %O
	set pick $_number($obj.pick)
	if {(($pick == "slider") || ($pick == "dial"))} {
	    set coords [numberObjCoords %W $obj %i %j]
	    set x [lindex $coords 0]
	    set x [expr ($x - 2) / 0.65]
	    set x [expr int($x)]
	    if {$x < 0} {set x 0}
	    if {$x > 9} {set x 9}
	    set _number($obj.data) $x
	    %P damage %O
	    break
	} elseif {$pick == "pan"} {
	    continue
	}
    }
    $PAD bind $obj <ButtonRelease-1> {
	set obj %O
	if {$_number($obj.pick) == "pan"} {
	    continue
	}
	set _number($obj.pick) ""
	%W damage $obj
	break
    }
}

proc numberObjCoords {PAD obj padx pady} {
    set place [$PAD itemconfig $obj -place]
    set xloc [lindex $place 0]
    set yloc [lindex $place 1]
    set size [lindex $place 2]
    set x [expr ($padx - $xloc) / $size]
    set y [expr ($pady - $yloc) / $size]

    return "$x $y"
}

proc tclNumberRender {PAD obj} {
    global _number

    set portal [lindex [$PAD render config -portals] end]
    if {[catch {numberRender$portal $PAD $obj}]} {
	numberRender $PAD $obj
    }
}

proc numberRender {PAD obj} {
    global _number

    $PAD render config -linewidth 0 -color $_number($obj.pen)
    $PAD render draw line 0 0 10 0 10 10 0 10 0 0
    set data $_number($obj.data)

    set pick $_number($obj.pick)
    if {($pick == "dial") || ($pick == "slider")} {
	$PAD render config -color $_number($obj.fill)
    } else {
	$PAD render config -color $_number($obj.pen)
    }
    $PAD render config -linewidth 1.0 -font $_number($obj.font)
    $PAD render draw text $data 2.2 1
}

proc sliderRender {PAD obj} {
    global _number

    $PAD render config -linewidth 0 -color $_number($obj.pen)
    $PAD render draw line 0 0 10 0 10 10 0 10 0 0
    set data $_number($obj.data)

    $PAD render config -linewidth 0.3 -color $_number($obj.line)
    $PAD render draw line 1 4 9 4 9 6 1 6 1 4

    if {$_number($obj.pick) == "slider"} {
	$PAD render config -color $_number($obj.activefill)
    } elseif {$_number($obj.pick) == "dial"} {
	$PAD render config -color $_number($obj.fill)
    } else {
	$PAD render config -color $_number($obj.line)
    }
    $PAD render config -linewidth 1.5 -capstyle round
    set x [expr 2 + (0.65 * $data)]
    $PAD render draw line $x 3 $x 7

    $PAD render config -linewidth 0.1 -color $_number($obj.pen) -font Times-1.5
    $PAD render draw text $data 4.5 0.5
}

proc sliderPick {PAD obj x y} {
    global _number

    set data $_number($obj.data)
    set datax [expr 2 + (0.65 * $data)]
    if {($y >= 2) && ($y <= 8) &&
        ([expr abs($x - $datax)] <= 1)} {
        set _number($obj.pick) "slider"
    } else {
	set _number($obj.pick) "pan"
    }
}

proc dialRender {PAD obj} {
    global _number

    $PAD render config -linewidth 0 -color $_number($obj.pen)
    $PAD render draw line 0 0 10 0 10 10 0 10 0 0
    set data $_number($obj.data)

    $PAD render config -linewidth 0.3 -color $_number($obj.line)
    for {set i 0} {$i < 10} {incr i} {
	set dist [expr 0.1 * (4.5 - $i)]
	set x1 [expr 1 + (0.9 * $i)]
	set x2 [expr $x1 + $dist]
	$PAD render draw line $x1 9 $x2 8
	if {$i == $data} {set datax2 [expr $x1 + 2*$dist]}
    }

    if {$_number($obj.pick) == "dial"} {
	$PAD render config -color $_number($obj.activefill)
    } elseif {$_number($obj.pick) == "slider"} {
	$PAD render config -color $_number($obj.fill)
    } else {
	$PAD render config -color $_number($obj.line)
    }
    $PAD render config -linewidth 0.3
    $PAD render draw line 5 1 $datax2 7
    $PAD render config -capstyle round -linewidth 1.0
    $PAD render draw line 5 1

    $PAD render config -linewidth 0.1 -color $_number($obj.pen) -font Times-1.5
    $PAD render draw text $data 1 1
}

proc dialPick {PAD obj x y} {
    global _number

    set _number($obj.pick) "dial"
}
