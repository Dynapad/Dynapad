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

proc importTimeline {PAD filename} {
    global geom

    set months(0) Jan
    set months(1) Feb
    set months(2) Mar
    set months(3) Apr
    set months(4) May
    set months(5) Jun
    set months(6) Jul
    set months(7) Aug
    set months(8) Sep
    set months(9) Oct
    set months(10) Nov
    set months(11) Dec

    set fid [open $filename]
    set view [$PAD getview]
    set geom(size) [expr 1.0 / [lindex $view 2]]
    set geom(x) [expr [lindex $view 0] - 0.4 * $geom(size)]
    set geom(y) [lindex $view 1]
    set geom(width) [expr 0.8 * $geom(size)]
    gets $fid line
    set geom(min) [lindex $line 0]
    gets $fid line
    set geom(max) [lindex $line 0]
    gets $fid line
    set geom(title) $line

#
# Create time line
#
    global lineid
    set lineid [$PAD create line $geom(x) $geom(y) [expr $geom(x) + $geom(width)] $geom(y) \
	    -pen blue -penwidth 0.01 -maxsize -1 -tags timeline]
#
# Create entries
#
    gets $fid line
    set line [string trimleft $line " "]
    while {![eof $fid]} {
	if {$line != ""} {
	    set parsedLine [timelineParse $fid $line]
	    set start [lindex $parsedLine 0]
	    set end  [lindex $parsedLine 1]
	    set data [lindex $parsedLine 2]
	    set line [lindex $parsedLine 3]

	    timelineLayout $PAD $start $end $data
	} else {
	    gets $fid line
	}
    }
    close $fid

#
# Create timeline labels
#
    set titleXloc [expr $geom(x) + $geom(width) * 0.5]
    set titleYloc [expr $geom(y) - $geom(width) * 0.08]
    set titleSize [expr 5.0 * $geom(size) / ($geom(max) - $geom(min) + 1.0)]
    set titleid [$PAD create text -text $geom(title) -place "$titleXloc $titleYloc $titleSize" -anchor n -pen "#004a86" -tags timeline]
    $PAD scale $titleid [expr [bbwidth [$PAD bbox $lineid]] / [bbwidth [$PAD bbox $titleid]]]
    for {set i $geom(min)} {$i <= $geom(max)} {incr i} {
	                            # Draw years
	set p [expr (double($i) - $geom(min) + 0.5) / ($geom(max) - $geom(min) + 1.0)]
	set xloc [expr $geom(x) + $p * $geom(width)]
	set yloc [expr $geom(y) - $geom(width) * 0.015]
	set size [expr 3.0 * $geom(size) / ($geom(max) - $geom(min) + 1.0)]
	$PAD create text -text $i -place "$xloc $yloc $size" -tags "timeline" -anchor n -pen "#001550"
				    #Draw decades
	if {[expr $i / 10 * 10] == $i} {
	    set decade [expr $i / 10 * 10 + 5]
	    set p [expr (double($decade) - $geom(min) + 0.5) / ($geom(max) - $geom(min) + 1.0)]
	    set decadeXloc [expr $geom(x) + $p * $geom(width)]
	    set decadeYloc [expr $geom(y) - $geom(width) * 0.03]
	    set decadeSize [expr 25 * $geom(size) / ($geom(max) - $geom(min) + 1.0)]
	    $PAD create text -text "${i}s" -place "$decadeXloc $decadeYloc $decadeSize" -tags "timeline" -anchor n -pen "#003070"
	}
                                    # Draw months
	for {set month 0} {$month < 12} {incr month} {
	    set p [expr (double($i) + ($month / 12.0) - $geom(min)) / ($geom(max) - $geom(min) + 1.0)]
	    set monthXloc [expr $geom(x) + $p * $geom(width)]
	    set monthYloc [expr $geom(y) - $geom(width) * 0.01]
	    set monthSize [expr $geom(size) / ($geom(max) - $geom(min) + 1.0)]
	    $PAD create text -text $months($month) -place "$monthXloc $monthYloc $monthSize" -tags "timeline" -anchor nw -pen "#000530"
	}
    }

#    global axisid
#    set axisid [$PAD create tcl -anchor "none"]
#    set bb [$PAD bbox $lineid]
#    .pad itemconfigure $axisid -renderscript "timelineAxis $PAD"
#    .pad itemconfigure $axisid -bb "set bb"

#    timelineEventBind $PAD
}

proc timelineAxis {PAD} {
    global geom

    set view [$PAD getview]
    set x [lindex $view 0]
    set y [lindex $view 1]
    set zoom [lindex $view 2]

    set w2 [expr 0.4 / $zoom]
    set h2 $w2
    set xmin [expr $x - $w2]
    set xmax [expr $x + $w2]
    set ymin [expr $y - $h2]
    set ymax [expr $y + $h2]

    $PAD render config -linewidth [expr 3.0 / 72] -color red
    $PAD render draw line $xmin $geom(y) $xmax $geom(y)
}

proc timelineParse {fid line} {
    gets $fid nextLine
    set nextLine [string trimleft $nextLine " "]

    set spacepos [string first " " $line]
    set data [string range $line [expr $spacepos + 1] end]
    while {[timelineContinuation $nextLine]} {
	append data "\n" [string trimleft $nextLine "& "]
	gets $fid nextLine
	set nextLine [string trimleft $nextLine " "]
    }
    
    set date [lindex $line 0]
    if {[set colonpos [string first ":" $date]] == -1} {
	regsub -all "/" $date " " start
	set end ""
    } else {
	regsub -all "/" [string range $date 0 [expr $colonpos - 1]] " " start
	regsub -all "/" [string range $date [expr $colonpos + 1] end] " " end
    }

    set line $nextLine

    return "\{$start\} \{$end\} \{$data\} \{$line\}"
}


proc timelineContinuation {line} {

    if {[string range $line 0 0] == "&"} {
	return 1
    }

    return 0
}


proc timelineLayout {PAD start end data} {
    global geom

    set startmonth 1
    set startday 1
    switch -exact [llength $start] {
	1 {
	    set startyear $start
	}
	2 {
	    set startmonth [lindex $start 0]
	    set startyear  [lindex $start 1]
	}
	3 {
	    set startmonth [lindex $start 0]
	    set startday   [lindex $start 1]
	    set startyear  [lindex $start 2]
	}
    }

    set endmonth 1
    set endday 1
    switch -exact [llength $end] {
	0 {
	    switch -exact [llength $start] {
		1 {
		    set endmonth $startmonth
		    set endday $startday
		    set endyear [expr $startyear + 1]
		}
		2 {
		    set endmonth [expr $startmonth + 1]
		    set endday $startday
		    set endyear $startyear
		}
		3 {
		    set endmonth $startmonth
		    set endday [expr $startday + 1]
		    set endyear $startyear
		}
	    }
	}
	1 {
	    set endyear $end
	}
	2 {
	    set endmonth [lindex $end 0]
	    set endyear  [lindex $end 1]
	}
	3 {
	    set endmonth [lindex $end 0]
	    set endday   [lindex $end 1]
	    set endyear  [lindex $end 2]
	}
    }
    puts "start $startmonth/$startday/$startyear, end $endmonth/$endday/$endyear"

    set tclPrecision 8
    set p1 [expr ((double($startmonth) - 1) / 12) + ((double($startday) - 1) / 365)]
    set p1y [expr double($startyear)]
    set p2 [expr ((double($endmonth) - 1) / 12) + ((double($endday) - 1) / 365)]
    set p2y [expr double($endyear)]
    set p [expr ((0.5 * ($p1 + $p1y + $p2 + $p2y)) - $geom(min)) / ($geom(max) - $geom(min) + 1.0)]
    set timewidth [expr ($p2y - $p1y) + ($p2 - $p1)]
    set tclPrecision 6

    set xloc [expr $geom(x) + $p * $geom(width)]
    set yloc $geom(y)
    set size $geom(size)
    if {[string range $data 0 0] == "\["} {
	set data [string trim $data "\[\]"]
	set id [eval $data]
	$PAD itemconfig $id -place "$xloc $yloc $size" -tags "timeline"
    } else {
	set id [$PAD create text -text $data -place "$xloc $yloc $size" -anchor s -tags "timeline"]
    }
    if {$id == ""} {
	return
    }
    set bb [$PAD bbox $id]
    set desiredWidth [expr 0.95 * $timewidth * $geom(width) / ($geom(max) - $geom(min) + 1.0)]
    $PAD scale $id [expr $desiredWidth / [bbwidth $bb]]
    timelineMoveUpSoNotOverlapping $PAD $id

#    puts "\"eval $PAD create rectangle [$PAD bbox $id]\""
#    set rectid [eval $PAD create rectangle [$PAD bbox $id]]
#    puts "rectid = $rectid, bbox=[$PAD bbox $rectid]"
#    $PAD scale $id 0.95

    set bb [$PAD bbox $id]
    set bbw [bbwidth $bb]
    set newy [expr [lindex $bb 1] - 0.002 * [$PAD scale $id]]
    set $id [$PAD create line [lindex $bb 0] $newy [lindex $bb 2] $newy -penwidth [expr 0.01 * $bbw] \
	    -maxsize -1 -tags timeline]
    timelineMoveUpSoNotOverlapping $PAD $id
}

proc timelineMoveUpSoNotOverlapping {PAD id} {

    set ids [eval $PAD find overlapping [$PAD bbox $id]]
    set width [bbwidth [$PAD bbox $id]]
#    puts "ids = $ids, length = [llength $ids]"
    while {[llength $ids] > 1} {
	set idPos [lsearch -exact $ids $id]
	set ids [lreplace $ids $idPos $idPos ""]
	set idsBb [eval $PAD bbox $ids]
	set xloc [lindex [$PAD itemconfig $id -place] 0]
	set newy [expr [lindex $idsBb 3] + 0.1 * $width]
#	puts "orig place = [$PAD itemconfig $id -place]"
#	puts "ids=$ids, idsBb=$idsBb, width=$width"
        $PAD slide $id 0 [expr 0.1 * $width]
#	puts "new place = [$PAD itemconfig $id -place]"
	set newbb [$PAD bbox $id]
	set ids [eval $PAD find overlapping [$PAD bbox $id]]
    }
}

proc bindTimeline {PAD} {
    $PAD bind timeline <Enter> "
	if {\[$PAD type %O\] == \"text\"} {
	    $PAD itemconfig %O -pen red
	}
    "
    $PAD bind timeline <Leave> "
	if {\[$PAD type %O\] == \"text\"} {
	    $PAD itemconfig %O -pen black
	}
    "
    $PAD bind timeline <ButtonPress-1> "
        $PAD center %O \$_pad(AnimationSpeed)
    "
}
