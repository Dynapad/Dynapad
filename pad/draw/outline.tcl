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
# This file reads in a text file in "outline" format, and creates
# pad text objects in outline form - i.e., objects deeper in the
# hierarchy are smaller.  
#
# The interaction with the outline is as follows:
# * Left-ButtonPress on an object zooms in to center the object at a size so that 
#   it and any one of its siblings would fill up the screen horizontally.  
#   (Thus, clicking on the next sibling never zooms).
# * Alt-Left-ButtonPress evaluates the text.  The variable PAD is guaranteed
#   to contain the value of the pad surface being loaded onto.
#
# Outline format is specified as following:
# * Text objects are separated by blank lines.
# * The depth in the hierarchy of each text object is specified by
#   a string of asterisks ('*') at the beginning of the first line
#   of text.
# * If the first nonblank character of the object is '[', then the
#   entire text object is evaluated as a tcl string.
#

proc openOutlineObject {PAD filename} {
    global outline

    if [file exists $filename] {
	set outline(fid) [open $filename]

	set obj ""
	while {([set eof [gets $outline(fid) line]] != -1) && ($line != "")} {
	    if {$obj != ""} {
		append obj "\n"
	    }
	    append obj $line
	}
	set outline(nextObj) $obj
	set outline(level) -1
	set outline(nextlevel) [outlineGetlevel 0 $obj]
	set outline(prevID) ""
	set outline(newlevel) ""
	set outline(maxlevels) 10
	set outline(prevlevel) ""
	set outline(lastprevlevel) ""

	set outline(framecolor-1) "#7070ff"
	set outline(framecolor0) "#4040ff"
	set outline(framecolor1) "#4040bf"
	set outline(framecolor2) "#40407f"
	set outline(framecolor3) "#40403f"
	set outline(framecolor4) "#40400f"
	set outline(framecolor5) "#40400f"
	set outline(framecolor6) "#40400f"
	set outline(framecolor7) "#40400f"
	set outline(framecolor8) "#40400f"
	set outline(framecolor9) "#404000"

	set z [$PAD getzoom]
	for {set i 0} {$i < $outline(maxlevels)} {incr i} {
	    set outline(frame$i) ""
	    set outline(bboxFactor$i) [expr 1 / ($z * pow(4, $i))]
#
# Uncomment the next line to turn off colored outline boxes
#
#	    set outline(framecolor$i) \"\"
	}
	set outline(bboxFactor-1) [expr 2 / $z]
    }
}

proc nextOutlineObject {filename} {
    global outline

    set obj $outline(nextObj)
    set outline(lastprevlevel) $outline(prevlevel)   ;# Save state
    set outline(lastlevel) $outline(level)           ;# Save state
    set outline(prevlevel) $outline(level)
    set outline(level) $outline(nextlevel)
    if {$outline(fid) != ""} {
	set nobj ""
	set first 1
	while {([set eof [gets $outline(fid) line]] != -1) && \
		($first || ($line != ""))} {
	    if {$line != ""} {
		if {$nobj != ""} {
		    append nobj "\n"
		}
		append nobj $line
		set first 0
	    }
	}
	set outline(nextObj) $nobj
	set outline(nextlevel) [outlineGetlevel 0 $nobj]
	if {$eof == -1} {
	    close $outline(fid)
	    set outline(fid) ""
	}
    } else {
	set outline(nextObj) ""
    }

    if {[string range [string trimleft $outline(nextObj) " \t"] 0 0] == "#"} {
	set lpl $outline(lastprevlevel)
	set ll  $outline(lastlevel)
	set pl  $outline(prevlevel)
        set l   $outline(level)

	nextOutlineObject $filename

	set outline(lastprevlevel) $lpl
	set outline(lastlevel) $ll
	set outline(prevlevel) $pl
	set outline(level) $l
    }

    return $obj
}

#
# Global variables used by the outline program:
#  outline(sib$id)   = Next sibling??
#  outline(next$id)  = next object
#  outline(prev$id)  = previous object
#  outline(level$id) = level of object
#  outline(xmax$id)  = Maximum x value of object
#  outline(bgnd$id)  = Id of background
#

proc importOutline {PAD filename} {
    global env outline _pad

    set view [$PAD getview]
    set outlinePos(x) [lindex $view 0]
    set outlinePos(y) [lindex $view 1]
    set outlinePos(size) [expr 1.0 / [lindex $view 2]]

    openOutlineObject $PAD $filename
    while {[set obj [nextOutlineObject $filename]] != ""} {
	set buf [string trimleft $obj " *"]
	
	if {[outlineCommand $obj]} {
	    set command [string trim $obj " *\[\]"]
	    set id [eval $command]
	    $PAD itemconfig $id -maxsize -1
	} else {
	    set id [$PAD create text -text $buf -maxsize -1 -font $_pad(Font)]
	}
	if {$id != ""} {
	    set outlinePos(y) [outlineLayout $PAD $id $outline(level) \
		    $outline(nextlevel) \
		    $outlinePos(x) $outlinePos(y) $outlinePos(size)]
	    if {($outline(prevlevel) == $outline(level)) && ($outline(prevID) != "")} {
		set outline(sib$outline(prevID)) $id
	    } else {
		if {$outline(newlevel) != ""} {
		    set outline(sib$outline(prevID)) $outline(newlevel)
		} else {
		    set outline(sib$outline(prevID)) $id
		}
		set outline(newlevel) $id
	    }
	    updateHierarchy $PAD $id
	    set outline(next$outline(prevID)) $id
	    set outline(prev$id) $outline(prevID)
	    set outline(level$id) $outline(level)
	    set outline(prevID) $id
	} else {
				# Couldn't create object, restore state
	    set outline(prevlevel) $outline(lastprevlevel)
	    set outline(level) $outline(lastlevel)
	}
    }
    createHierarchicalFrames $PAD
    set outline(sib$id) $outline(newlevel)
    outlineProcessSiblingBBox $PAD
}

proc updateHierarchy {PAD id} {
    global outline

    if {$outline(prevlevel) == -1} {
	set outline(root) $id
	set outline(parent$id) root
        set outline(childrenroot) $id
	set outline(children$id) ""
    } else {
	if {$outline(prevlevel) == $outline(level)} {
	    set outline(parent$id) $outline(parent$outline(prevID))
	    set outline(children$id) ""
            append outline(children$outline(parent$id)) " $id"
	} else {
	    if {$outline(prevlevel) < $outline(level)} {
		set outline(parent$id) $outline(prevID)
	        set outline(children$id) ""
		append outline(children$outline(prevID)) " $id"
	    } else {
		if {$outline(prevlevel) > $outline(level)} {
		    set outline(parent$id) $outline(prevID)
		    for {set i $outline(level)} {$i <= $outline(prevlevel)} {incr i} {
			set outline(parent$id) $outline(parent$outline(parent$id))
		    }
                    set outline(children$id) ""
                    append outline(children$outline(parent$id)) " $id"
		}
	    }
	}
    }
}

proc printHierarchy {PAD} {
    global outline

    puts ""
    puts "Outline Hierarchy"
    printHierarchyR $PAD root
}

proc printHierarchyR {PAD id} {
    global outline

    if {$id != "root"} {
	set output ""
	for {set i 0} {$i < $outline(level$id)} {incr i} {
	    append output "  "
	}
	append output "$id"
	puts $output
    }
    foreach child $outline(children$id) {
	printHierarchyR $PAD $child
    }
}

proc createHierarchicalFrames {PAD} {
    global outline

    createHierarchicalFramesR $PAD root
}

proc createHierarchicalFramesR {PAD id} {
    global outline

    foreach child $outline(children$id) {
	createHierarchicalFramesR $PAD $child
    }
    if {$id == "root"} {
	set level -1
	set bbox $outline(coords$outline(root))
	foreach child $outline(children$id) {
	    set bbox [bbunion $bbox $outline(coords$child)]
	}
	set coords [eval outlineBgndCoords $bbox $outline(bboxFactor$level)]
	set rect [eval $PAD create rectangle $coords -pen none -joinstyle miter \
		-fill $outline(framecolor$level) -penwidth 0 -maxsize -1 -events 0]
	$PAD lower $rect
    } else {
	set outline(coords$id) [$PAD bbox $outline(bgnd$id)]
	if {$outline(children$id) != ""} {
	    set bgnd $outline(bgnd$id)
	    set level $outline(level$id)
            set bbox $outline(coords$id)
	    foreach child $outline(children$id) {
		set bbox [bbunion $bbox $outline(coords$child)]
	    }
	    set outline(coords$id) $bbox
	    set coords [eval outlineBgndCoords $bbox $outline(bboxFactor$level)]
	    set rect [eval $PAD create rectangle $coords -pen none -joinstyle miter \
		    -fill $outline(framecolor$level) -penwidth 0 -maxsize -1 -events 0]
	    $PAD lower $rect $bgnd
	}
    }
}

#
# Go through lists of siblings to compute largest width of bbox, and make all
# siblings have the same bbox width.
#
proc outlineProcessSiblingBBox {PAD} {
    global outline

    foreach id [$PAD find withtag outline] {
	set x2 [lindex [$PAD bbox $id] 2]
	set outline(xmax$id) $x2
	set sid $outline(sib$id)
	while {($sid != "") && ($sid != $id)} {
	    set sibx2 [lindex [$PAD bbox $sid] 2]
	    if {$sibx2 > $outline(xmax$id)} {
		set outline(xmax$id) $sibx2
	    }
	    set sid $outline(sib$sid)
	}
    }
}

#
# Given an outline object id, return the coords of it's background
# (actually, a little bigger than the outline object.)
#
proc outlineBgndCoords {x1 y1 x2 y2 scaleFactor} {
    set x1 [expr $x1 - $scaleFactor]
    set y1 [expr $y1 - $scaleFactor]
    set x2 [expr $x2 + $scaleFactor]
    set y2 [expr $y2 + $scaleFactor]

    return "$x1 $y1 $x2 $y2"
}

proc outlineBgndExtendCoords {x1 y1 x2 y2 scaleFactor} {
    set y1 [expr $y1 - $scaleFactor]
    set x2 [expr $x2 + $scaleFactor]

    return "$x1 $y1 $x2 $y2"
}

proc outlineGetlevel {level buf} {
    set starpos [string first "*" $buf]
    if {$starpos == 0} {
	set level [outlineGetlevel [expr 1 + $level] [string range $buf \
		[expr 1 + $starpos] end]]
    }
    
    return $level
}

proc outlineContinuation {line} {
    set buf [string trimleft $line " *"]

    if {[string range $buf 0 0] == "-"} {
	return 1
    }

    return 0
}

proc outlineCommand {line} {
    set buf [string trimleft $line " *"]

    if {[string range $buf 0 0] == "\["} {
	return 1
    }

    return 0
}

proc outlineLayout {PAD id level nextlevel x y z} {
    global outline

    set scaleFactor [expr $z / pow(4, $level)]
    set nextScaleFactor [expr $z / pow(4, $nextlevel)]

    set type [$PAD type $id]
    set newx [outlineXoffset $level $x $z]
    $PAD itemconfig $id -place "$newx $y 1"
    if {($type == "text") || ($type == "textfile")} {
	$PAD scale $id $scaleFactor

	$PAD itemconfig $id -anchor nw
	set bboxFactor [expr 0.01 * $scaleFactor]
	set coords [eval outlineBgndCoords [$PAD bbox $id] $bboxFactor]
	set width [expr 0.3 * $scaleFactor]
	set bgndId [eval $PAD create rectangle $coords -fill #d0d0d0 -joinstyle miter \
		-pen #707070 -penwidth $width -maxsize -1 -events 0]
	$PAD lower $bgndId $id
	$PAD slide $id 0 -$bboxFactor
	$PAD slide $bgndId 0 -$bboxFactor
	set bb [$PAD bbox $bgndId]
	set outline(bgnd$id) $bgndId
    } else {
	set bb [$PAD bbox $id]
	set desiredHeight [expr 4.0 * $scaleFactor]
	$PAD scale $id [expr $desiredHeight / [bbheight $bb]]
	set bb [$PAD bbox $id]
	$PAD slide $id [expr $newx - [lindex $bb 0]] [expr $y - [lindex $bb 3]]
	set bb [$PAD bbox $id]
	set outline(bgnd$id) $id
    }
    $PAD addtag outline $id
    
    if {[$PAD type $id] == "html"} {
	global htmlBgnd
	eval $PAD coords $htmlBgnd($id) [htmlBgndCoords $PAD $id]

	set width [$PAD itemconfig $htmlBgnd($id) -penwidth]
	set newwidth [expr [bbwidth [$PAD bbox $id]] / 0.50]
	$PAD itemconfig $htmlBgnd($id) -penwidth $newwidth
	set bb [$PAD bbox $htmlBgnd($id)]
    }

    if {[$PAD type $id] == "portal"} {
	if {[lmember [$PAD gettags $id] trackView]} {
	    trackView $PAD $id
	}
    }

    set dy [expr 3.0 * $nextScaleFactor]
    return [expr [lindex $bb 1] - $dy]
}

proc outlineXoffset {level x z} {
    set xoffset $x


    for {set i 0} {$i < $level} {incr i} {
	set xoffset [expr $xoffset + (1.0 * $z / pow(4, $i))]
    }

    return $xoffset
}

proc bindOutline {PAD} {
    global tcl_platform

    if {$tcl_platform(platform) == "unix"} {
        bind $PAD <Enter> {
	      focus %W
	      if {[%W focus] == ""} {
	          %W focus 1
	      }
        }
    }
    $PAD bind outline <Run-Enter> {
	set id [%W find withtag current]
	if {[%W type $id] == "text"} {
	    %W itemconfig $id -pen #c03030
	}
    }

    $PAD bind outline <Run-Leave> {
	set id [%W find withtag current]
	if {[%W type $id] == "text"} {
	    %W itemconfig $id -pen black
	}
    }

    $PAD bind outline <Run-ButtonPress-1> {
	panEvent %P %W "press" %O %s %x %y %i %j %l

	set id %O
	if {[%W type $id] == "text"} {
	    %W itemconfig $id -pen #ff7070
	}
	break
    }

    $PAD bind outline <Run-B1-Motion> {
	panEvent %P %W "motion" %O %s %x %y %i %j %l
	break
    }

    $PAD bind outline <Run-ButtonRelease-1> {
	panEvent %P %W "release" %O %s %x %y %i %j %l
	if {[%W type $id] == "text"} {
	    %W itemconfig $id -pen #c03030
	}
	if {$_pan(started) == 0} {
	    set id %O
	    set state %s
	    set outline(currentID) $id
	    set bbox [%W bbox $id]
	    set x1 [lindex $bbox 0]
	    set y1 [lindex $bbox 1]
	    set y2 [lindex $bbox 3]
	    if {[%W type $id] == "text"} {
		set x2 $outline(xmax$id)
		set yctr 0.8
	    } else {
		set x2 [lindex $bbox 2]
		set yctr 0.5
	    }
	    if {$state & 1} {   ;# Shift key pressed
	        set portals ""
	    } else {
		set portals %l
	    }
	    if {$state & 8} {  ;# Alt key pressed
	        set command [%W itemconfig $id -text]
	        set PAD %W
		puts [eval $command]
  	    } else {
		eval %W centerbbox -twostep $x1 $y1 $x2 $y2 $_pad(AnimationSpeed) 0.5 $yctr .9 $portals
	    }
	}
	break
    } 
    $PAD bind all <Run-KeyPress-n> {
	if {![info exists outline(currentID)]} {return}
	set id $outline(currentID)
	if {$id == ""} {return}
	if {![info exists outline(next$id)]} {return}
	set nid $outline(next$id)
	if {$nid == ""} {return}
	set outline(currentID) $nid
	set bbox [%W bbox $nid]
	set x1 [lindex $bbox 0]
	set y1 [lindex $bbox 1]
	set y2 [lindex $bbox 3]
	if {[%W type $nid] == "text"} {
	    %W itemconfig $nid -pen #c03030
	    set x2 $outline(xmax$nid)
	    set yctr 0.8
	} else {
	    set x2 [lindex $bbox 2]
	    set yctr 0.5
	}
	%W centerbbox -twostep $x1 $y1 $x2 $y2 $_pad(AnimationSpeed) 0.5 $yctr .9
	if {[%W type $nid] == "text"} {
	    %W itemconfig $nid -pen black
	}
    }
    $PAD bind all <Run-KeyPress-p> {
	if {![info exists outline(currentID)]} {return}
	set id $outline(currentID)
	if {$id == ""} {return}
	if {![info exists outline(next$id)]} {return}
	set pid $outline(prev$id)
	if {$pid == ""} {return}
	set outline(currentID) $pid
	set bbox [%W bbox $pid]
	set x1 [lindex $bbox 0]
	set y1 [lindex $bbox 1]
	set y2 [lindex $bbox 3]
	if {[%W type $pid] == "text"} {
	    %W itemconfig $pid -pen #c03030
	    set x2 $outline(xmax$pid)
	    set yctr 0.8
	} else {
	    set x2 [lindex $bbox 2]
	    set yctr 0.5
	}
	%W centerbbox -twostep $x1 $y1 $x2 $y2 $_pad(AnimationSpeed) 0.5 $yctr .9
	if {[%W type $pid] == "text"} {
	    %W itemconfig $pid -pen black
	}
    }
    $PAD bind all <Run-KeyPress-N> {
	if {![info exists outline(currentID)]} {return}
	set id $outline(currentID)
	if {$id == ""} {return}
	set level $outline(level$id)
	set found 0
	while {!$found} {
	    if {![info exists outline(next$id)]} {return}
	    set nid $outline(next$id)
	    if {$nid == ""} {return}
	    if {$outline(level$nid) <= $level} {
		set found 1
	    } else {
		set id $nid
	    }
	}
	set outline(currentID) $nid
	set bbox [%W bbox $nid]
	set x1 [lindex $bbox 0]
	set y1 [lindex $bbox 1]
	set y2 [lindex $bbox 3]
	if {[%W type $nid] == "text"} {
	    %W itemconfig $nid -pen #c03030
	    set x2 $outline(xmax$nid)
	    set yctr 0.8
	} else {
	    set x2 [lindex $bbox 2]
	    set yctr 0.5
	}
	%W centerbbox -twostep $x1 $y1 $x2 $y2 $_pad(AnimationSpeed) 0.5 $yctr .9
	if {[%W type $nid] == "text"} {
	    %W itemconfig $nid -pen black
	}
    }
    $PAD bind all <Run-KeyPress-P> {
	if {![info exists outline(currentID)]} {return}
	set id $outline(currentID)
	if {$id == ""} {return}
	set level $outline(level$id)
	set found 0
	while {!$found} {
	    if {![info exists outline(prev$id)]} {return}
	    set pid $outline(prev$id)
	    if {$pid == ""} {return}
	    if {$outline(level$pid) <= $level} {
		set found 1
	    } else {
		set id $pid
	    }
	}
	set outline(currentID) $pid
	set bbox [%W bbox $pid]
	set x1 [lindex $bbox 0]
	set y1 [lindex $bbox 1]
	set y2 [lindex $bbox 3]
	if {[%W type $pid] == "text"} {
	    %W itemconfig $pid -pen #c03030
	    set x2 $outline(xmax$pid)
	    set yctr 0.8
	} else {
	    set x2 [lindex $bbox 2]
	    set yctr 0.5
	}
	%W centerbbox -twostep $x1 $y1 $x2 $y2 $_pad(AnimationSpeed) 0.5 $yctr .9
	if {[%W type $pid] == "text"} {
	    %W itemconfig $pid -pen black
	}
    }
}

proc outlineNewPad {PAD} {
    set viewer .outlinePad
    if {[winfo exists $viewer]} {
	destroy $viewer
    }
    toplevel $viewer
    pad $viewer.pad -width 300 -height 300
    button $viewer.dismiss -text "Close" -command "destroy $viewer"
    pack $viewer.pad -expand t -fill both
    pack $viewer.dismiss -fill x

    set padGeo [winfo geometry [winfo toplevel $PAD]]
    set plus1pos [string first + $padGeo]
    set plus2pos [string last + $padGeo]
    set x [string range $padGeo [expr $plus1pos + 1] [expr $plus2pos - 1]]
    set y [string range $padGeo [expr $plus2pos + 1] end]
    wm geometry $viewer "+[expr 50 + $x]+[expr 50 + $y]"
    wm title $viewer "Pad++ Outline Eval"
    wm minsize $viewer 50 50

    bindPan $viewer.pad
    bindZoom $viewer.pad
    bindOutline $viewer.pad

    return $viewer.pad
}
