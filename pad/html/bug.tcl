proc makeWebBug {PAD group container} {
    global _bug

    set bug [$PAD create rectangle -1.5 -1.5 1.5 1.5]
    $PAD scale $bug 4
    $PAD addgroupmember $bug $group

    set _bug(noiseindex$bug)  0.12345432
    set _bug(noiseinc$bug)  0.01
    set _bug(container$bug) $container
    set _bug(angle$bug) 0.0
    set _bug(zoom$bug) [lindex [$PAD getview] 2]
    $PAD itemconfigure $bug -renderscript "bugrender $PAD $bug"
    $PAD itemconfigure $bug -timerscript "bugmove $PAD $bug" -timerrate 200

    bugmove $PAD $bug
}

proc bugmove {PAD bug} {
    global _bug
#
# Restore static variables
#
    set noiseindex $_bug(noiseindex$bug)
    set noiseinc $_bug(noiseinc$bug)
    set angle $_bug(angle$bug)
    set zoom $_bug(zoom$bug)
    set container $_bug(container$bug)
#
# Compute bug travel
#
    set pi 3.14
    set noiseindex [expr $noiseindex + $noiseinc]
    set angleinc [$PAD noise $noiseindex]
    set angle [expr $angle + (20.0 * $angleinc)]
    if {$angle > 2*$pi} {set angle [expr $angle - 2*$pi]}
    if {$angle < 0} {set angle [expr $angle + 2*$pi]}
    set xinc [expr cos($angle)]
    set yinc [expr sin($angle)]
    set xtravel [expr 5.0 * $xinc / $zoom]
    set ytravel [expr 5.0 * $yinc / $zoom]
#
# Move bug one step
#
    $PAD slide $bug $xtravel $ytravel

#
# Keep bug in container
#
				# Bug container

    set viewbbox [$PAD bbox $container]
    set xmin [lindex $viewbbox 0]
    set ymin [lindex $viewbbox 1]
    set xmax [lindex $viewbbox 2]
    set ymax [lindex $viewbbox 3]
				# Bug bounding box
    set bb [$PAD bbox $bug]
    set bbxmin [lindex $bb 0]
    set bbymin [lindex $bb 1]
    set bbxmax [lindex $bb 2]
    set bbymax [lindex $bb 3]

    if {$bbxmin < $xmin} {
	$PAD slide $bug [expr $xmin - $bbxmin] 0
    }
    if {$bbymin < $ymin} {
	$PAD slide $bug 0 [expr $ymin - $bbymin]
    }
    if {$bbxmax > $xmax} {
	$PAD slide $bug [expr $xmax - $bbxmax] 0
    }
    if {$bbymax > $ymax} {
	$PAD slide $bug 0 [expr $ymax - $bbymax]
    }
#
# Get bug rendered
#
    $PAD damage $bug

#
# Save static variables
#
    set _bug(noiseindex$bug) $noiseindex
    set _bug(angle$bug) $angle
    set _bug(xinc$bug) $xinc
    set _bug(yinc$bug) $yinc
}

#
# Draw bug
#
proc bugrender {PAD bug} {
    global _bug

    set xinc $_bug(xinc$bug)
    set yinc $_bug(yinc$bug)

    $PAD setcolor BlueViolet
    $PAD drawpolygon $xinc $yinc [expr -.3 * $yinc] [expr .3 * $xinc] [expr .3 * $yinc] [expr -.3 * $xinc] $xinc $yinc
    $PAD setlinewidth 0
    $PAD setcolor Black
    $PAD drawline $xinc $yinc [expr 1.5 * $xinc] [expr 1.5 * $yinc]
}

set container [$Pad_CurrentHTMLPad create rectangle 0 0 100 60 -pen blue4 -fill yellow4]
set group [$Pad_CurrentHTMLPad create group -members $container -tags bug -divisible 0]
makeWebBug $Pad_CurrentHTMLPad $group $container
