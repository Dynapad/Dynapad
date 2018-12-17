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

# Bob the Rat,
# by rrh oct 18,1996
# an elaboration of the animated bug (see below)


proc makebob {PAD} {
    global _bob

    set _bob(timerrate) 100         ;# Delay between bob animation steps (in milliseconds)

    set bob [$PAD create rectangle -2.5 -2.5 2.5 2.5 -place center]
    $PAD scale $bob 0.2
    set zoom [$PAD getzoom]

    if {[info exists _bob(list)]} {
	set prevbob [lindex $_bob(list) [expr [llength $_bob(list)] - 1]]
	set _bob(noiseindex$bob)  [expr $_bob(angle$prevbob) * 100]
	set _bob(noiseinc$bob)  [expr $_bob(angle$prevbob) / 25.0]
    } else {
	set _bob(noiseindex$bob)  0.0
	set _bob(noiseinc$bob)  0.01
    }
    set _bob(angle$bob) 0.0
    set _bob(zoom$bob)  $zoom

    set _bob(incr$bob) 0.0

    set _bob(fillColor) [$PAD color alloc gray80]
    set _bob(penColor) [$PAD color alloc gray15]
    set _bob(tailColor) [$PAD color alloc pink]
    set _bob(eyeColor) [$PAD color alloc white]
    set _bob(pupilColor) [$PAD color alloc black]
    if {![info exists _bob(timer)]} {
	set _bob(timer) [after $_bob(timerrate) bobmove $PAD]
    }
    $PAD itemconfigure $bob -renderscript "bobrender $PAD $bob"
    set _bob(list) [lappend _bob(list) $bob]
    bobmoveindividual $PAD $bob

    $PAD bind $bob <Delete> {
	%P color free $_bob(fillColor)
	%P color free $_bob(penColor)
	set _bob(list) [lremove $_bob(list) %O]
	unset _bob(xinc%O)
	unset _bob(yinc%O)
	unset _bob(angle%O)
	unset _bob(incr%O)
	unset _bob(noiseinc%O)
	unset _bob(noiseindex%O)
	unset _bob(zoom%O)
	if {$_bob(list) == ""} {
	    if {[info exists _bob(timer)]} {
		after cancel $_bob(timer)
		unset _bob(timer)
		unset _bob(list)
	    }
	}
    }

    return $bob
}

proc bobmove {PAD} {
    global _bob

    foreach bob $_bob(list) {
	bobmoveindividual $PAD $bob
    }
    
    set _bob(timer) [after $_bob(timerrate) bobmove $PAD]
}

proc bobmoveindividual {PAD bob} {
    global _bob

#
# Restore static variables
#
    set noiseindex $_bob(noiseindex$bob)
    set noiseinc $_bob(noiseinc$bob)
    set angle $_bob(angle$bob)
    set zoom $_bob(zoom$bob)
    set incr $_bob(incr$bob)

#
# Is bob visible?
#
				# Pad-View bounding box

    set viewbbox [$PAD bbox 1]
    set xmin [lindex $viewbbox 0]
    set ymin [lindex $viewbbox 1]
    set xmax [lindex $viewbbox 2]
    set ymax [lindex $viewbbox 3]
				# bob bounding box
    set bb [$PAD bbox $bob]
    set bbxmin [lindex $bb 0]
    set bbymin [lindex $bb 1]
    set bbxmax [lindex $bb 2]
    set bbymax [lindex $bb 3]

    set visible 1
    set wall -1
    if {$bbxmin < $xmin} {
	set visible 0
	set wall 0
    }
    if {$bbymin < $ymin} {
	set visible 0
	set wall 1
    }
    if {$bbxmax > $xmax} {
	set visible 0
	set wall 2
    }
    if {$bbymax > $ymax} {
	set visible 0
	set wall 3
    }
#
# Compute bob travel
#
    set pi 3.14
    set noiseindex [expr $noiseindex + $noiseinc]
    if {[$PAD find withtag cheese] == ""} {
      set angleinc [$PAD noise $noiseindex]
    } else {

      set cheeselist [$PAD find withtag cheese]
      #puts $cheeselist
      set xobj [$PAD itemconfigure $cheeselist -x ]
      set yobj [$PAD itemconfigure $cheeselist -y ]
      set xcmp [$PAD itemconfigure $bob -x ]
      set ycmp [$PAD itemconfigure $bob -y ]


      set dx [expr $xobj - $xcmp]
      set dy [expr $yobj - $ycmp]
      set d  [expr sqrt( $dx * $dx + $dy *$dy) ]

      if {$d < 15} {
	$PAD deletetag cheese $cheeselist
	if { [$PAD hastag "realcheese" $cheeselist] } {
	  $PAD delete $cheeselist
	}
        set angleinc [$PAD noise $noiseindex]

      } else {
      
	set targetangle  [expr atan2( $dy, $dx) ]
	set myangle  [expr atan2( sin($angle), cos($angle)) ]
        set angleinc [expr $targetangle - $myangle ]
	if {$angleinc > $pi} {set angleinc [expr $angleinc - 2*$pi]}
	if {$angleinc < -$pi} {set angleinc [expr $angleinc + 2*$pi]}
	if {$angleinc > .2 } {
	  set angleinc .2
	}
	if {$angleinc < -.2 } {
	  set angleinc -.2
	}
        set angleinc [expr $angleinc + 0.75* [$PAD noise [expr 12* $noiseindex] ] ]
      }
      #  source bob.tcl
      #  makecheese .pad

    }
        set _bob(dangle$bob) $angleinc
    set angle [expr $angle + $angleinc]
    if {$angle > 2*$pi} {set angle [expr $angle - 2*$pi]}
    if {$angle < 0} {set angle [expr $angle + 2*$pi]}
    set xinc [expr cos($angle)]
    set yinc [expr sin($angle)]
    set xtravel [expr 10 * $xinc / $zoom]
    set ytravel [expr 10 * $yinc / $zoom]


#
# Move bob one step
#
    if {($visible ||
          [$PAD find withtag cheese] != "" || 
	 ($wall == 0 &&
	  ($angle < 0.5*$pi ||
	   $angle > 1.5*$pi)) ||
	 ($wall == 1 &&
	  ($angle < $pi)) ||
	 ($wall == 2 &&
	  ($angle > 0.5*$pi &&
	   $angle < 1.5*$pi)) ||
	 ($wall == 3 &&
	  ($angle > $pi)))} {
	$PAD slide $bob $xtravel $ytravel
        set incr [expr $incr + 1]
    } else {
	$PAD damage $bob
	set angle [expr $angle + .5]
        set incr [expr $incr + 0.125]
    }

#
# Save static variables
#
    set _bob(noiseindex$bob) $noiseindex
    set _bob(angle$bob) $angle
    set _bob(xinc$bob) $xinc
    set _bob(yinc$bob) $yinc
    
    set _bob(incr$bob) $incr
}

set bob_body  { {0 -1} {0.4 -0.5} {0.4 0.5} \
		{0.15  0.9} \
		{0.25  1.3} \
		{0  1.8}  \
		{-0.25  1.3} \
		{-0.15  0.9} \
		{-0.4 0.5} {-0.4 -0.5}}
set n_bob_body  [llength $bob_body]

#
# Draw bob
#
proc bobrender {PAD bob} {
    global _bob
    global bob_body
    global n_bob_body
    global bob_poly

    set xinc $_bob(xinc$bob)
    set yinc $_bob(yinc$bob)


    # FRONT LEGS
    set len [expr sin( $_bob(incr$bob) * 0.75) +0.5 ]
    set x1 0.3
    set x2 0.6
    set x3 0.7
    set y1 [expr 0.4 + 0.0 * $len]
    set y2 [expr 0.4 + 0.2 * $len]
    set y3 [expr 0.4 + 0.8 * $len]
    $PAD render config -linewidth 0.11 -color $_bob(penColor)
    $PAD render draw line \
	[expr $x1 * $yinc + $y1 * $xinc] \
	[expr $y1 * $yinc - $x1 * $xinc] \
	[expr $x2 * $yinc + $y2 * $xinc] \
	[expr $y2 * $yinc - $x2 * $xinc] 
    $PAD render draw line \
	[expr $x2 * $yinc + $y2 * $xinc] \
	[expr $y2 * $yinc - $x2 * $xinc] \
	[expr $x3 * $yinc + $y3 * $xinc] \
	[expr $y3 * $yinc - $x3 * $xinc] 


    $PAD render draw line \
	[expr -$x1 * $yinc + $y1 * $xinc] \
	[expr $y1 * $yinc +  $x1 * $xinc] \
	[expr -$x2 * $yinc + $y2 * $xinc] \
	[expr  $y2 * $yinc + $x2 * $xinc] 
    $PAD render draw line \
	[expr -$x2 * $yinc + $y2 * $xinc] \
	[expr $y2 * $yinc +  $x2 * $xinc] \
	[expr -$x3 * $yinc + $y3 * $xinc] \
	[expr  $y3 * $yinc + $x3 * $xinc] 

    # REAR LEGS
    set len [expr sin( $_bob(incr$bob) * 0.75) +0.5 ]
    set x1 0.3
    set x2 0.6
    set x3 0.7
    set y1 [expr -0.4 - 0.0 * $len]
    set y2 [expr -0.4 - 0.2 * $len]
    set y3 [expr -0.4 - 0.8 * $len]
    $PAD render config -linewidth 0.11 -color $_bob(penColor)
    $PAD render draw line \
	[expr $x1 * $yinc + $y1 * $xinc] \
	[expr $y1 * $yinc - $x1 * $xinc] \
	[expr $x2 * $yinc + $y2 * $xinc] \
	[expr $y2 * $yinc - $x2 * $xinc] 
    $PAD render draw line \
	[expr $x2 * $yinc + $y2 * $xinc] \
	[expr $y2 * $yinc - $x2 * $xinc] \
	[expr $x3 * $yinc + $y3 * $xinc] \
	[expr $y3 * $yinc - $x3 * $xinc] 


    $PAD render draw line \
	[expr -$x1 * $yinc + $y1 * $xinc] \
	[expr $y1 * $yinc +  $x1 * $xinc] \
	[expr -$x2 * $yinc + $y2 * $xinc] \
	[expr  $y2 * $yinc + $x2 * $xinc] 
    $PAD render draw line \
	[expr -$x2 * $yinc + $y2 * $xinc] \
	[expr $y2 * $yinc +  $x2 * $xinc] \
	[expr -$x3 * $yinc + $y3 * $xinc] \
	[expr  $y3 * $yinc + $x3 * $xinc] 

    # TAIL
    set a1 [expr sin( $_bob(incr$bob) * 0.5) *0.15]
    set a2 [expr cos( $_bob(incr$bob) * 0.5) *0.05]
    set x1 0.0
    set y1 -0.7
    set x2 [expr $x1 + 0.8 * sin($a1 - $_bob(dangle$bob)*1.5) ]
    set y2 [expr $y1 - 0.8 * cos($a1 - $_bob(dangle$bob)*1.5) ]
    set x3 [expr $x2 + 1.0 * sin($a2-$a1 - $_bob(dangle$bob)*1.5) ]
    set y3 [expr $y2 - 1.0 * cos($a2-$a1 - $_bob(dangle$bob)*1.5) ]
    $PAD render config -linewidth 0.11 -color $_bob(tailColor)
    $PAD render draw line \
	[expr $x1 * $yinc + $y1 * $xinc] \
	[expr $y1 * $yinc - $x1 * $xinc] \
	[expr $x2 * $yinc + $y2 * $xinc] \
	[expr $y2 * $yinc - $x2 * $xinc] 

    $PAD render config -linewidth 0.05 -color $_bob(tailColor)

    $PAD render draw line \
	[expr $x2 * $yinc + $y2 * $xinc] \
	[expr $y2 * $yinc - $x2 * $xinc] \
	[expr $x3 * $yinc + $y3 * $xinc] \
	[expr $y3 * $yinc - $x3 * $xinc] 


    # WHISKERS
    $PAD render config -linewidth 0.01 -color $_bob(penColor)
    set y1 1.6
    set y2 [expr $y1 - 0.2]
    set y3 [expr $y1 - 0.1]
    $PAD render draw line [expr $y1 * $xinc] [expr $y1 * $yinc ] \
	[expr 0.4 * $yinc + $y2 * $xinc] [expr $y2 * $yinc - 0.4 * $xinc] 
    $PAD render draw line [expr $y1 * $xinc] [expr $y1 * $yinc ] \
	[expr 0.4 * $yinc + $y1 * $xinc] [expr $y1 * $yinc - 0.4 * $xinc] 
    $PAD render draw line [expr $y1 * $xinc] [expr $y1 * $yinc ] \
	[expr 0.4 * $yinc + $y3 * $xinc] [expr $y3 * $yinc - 0.4 * $xinc] 
    $PAD render draw line [expr $y1 * $xinc] [expr $y1 * $yinc ] \
	[expr -0.4 * $yinc + $y2 * $xinc] [expr $y2 * $yinc + 0.4 * $xinc] 
    $PAD render draw line [expr $y1 * $xinc] [expr $y1 * $yinc ] \
	[expr -0.4 * $yinc + $y1 * $xinc] [expr $y1 * $yinc + 0.4 * $xinc] 
    $PAD render draw line [expr $y1 * $xinc] [expr $y1 * $yinc ] \
	[expr -0.4 * $yinc + $y3 * $xinc] [expr $y3 * $yinc + 0.4 * $xinc] 

    $PAD render config -linewidth 0 -color $_bob(fillColor)

    set bob_poly   {$PAD render draw polygon}
    for {set i 0} {$i < $n_bob_body } {incr i} {
	set x [lindex [lindex $bob_body $i] 0]
	set y [lindex [lindex $bob_body $i] 1]
	set nx [expr $x * $yinc + $y * $xinc]
	set ny [expr $y * $yinc - $x * $xinc]
	set bob_poly [lappend bob_poly $nx $ny]
    }
    eval $bob_poly

    # EYES
    $PAD render config -linewidth 0.11 -color $_bob(eyeColor)
    set x1 0.15
    set y1 1.35
    set y2 1.40
    $PAD render draw line \
	[expr $x1 * $yinc + $y1 * $xinc] \
	[expr $y1 * $yinc - $x1 * $xinc] \
	[expr $x1 * $yinc + $y1 * $xinc] \
	[expr $y1 * $yinc - $x1 * $xinc] 

    $PAD render draw line \
	[expr -$x1 * $yinc +  $y1 * $xinc] \
	[expr  $y1 * $yinc +  $x1 * $xinc] \
	[expr -$x1 * $yinc +  $y1 * $xinc] \
	[expr  $y1 * $yinc +  $x1 * $xinc] 

    $PAD render config -linewidth 0.11 -color $_bob(pupilColor)
    $PAD render draw line \
	[expr $x1 * $yinc + $y2 * $xinc] \
	[expr $y2 * $yinc - $x1 * $xinc] \
	[expr $x1 * $yinc + $y2 * $xinc] \
	[expr $y2 * $yinc - $x1 * $xinc] 

    $PAD render draw line \
	[expr -$x1 * $yinc +  $y2 * $xinc] \
	[expr  $y2 * $yinc +  $x1 * $xinc] \
	[expr -$x1 * $yinc +  $y2 * $xinc] \
	[expr  $y2 * $yinc +  $x1 * $xinc] 


}

set cheesenoiseindex 0.5

proc makecheese {PAD} {
  global cheesenoiseindex
  if {[$PAD find withtag selected] != ""} {
    foreach id [$PAD find withtag selected] {
      $PAD addtag cheese $id
    }
  } else {
    set cheese [ $PAD create polygon -5.546413 11.532099 11.591986 17.734568 \
			             15.182889 15.612669 17.631233 13.817219 \
				     18.610569 8.920533 18.610569 -3.810850 \
				     -5.546413 -0.219947 \
				     -fill yellow ]
    set c2 [ $PAD create line -5.546413 11.532099 18.610569 8.920533 ]
    set g [$PAD create group -members "$cheese $c2" \
			     -tags "cheese realcheese" -place center]
    $PAD scale $g 0.075
      set cheesenoiseindex [expr $cheesenoiseindex + 1]
      set x [$PAD noise $cheesenoiseindex]
      set y [$PAD noise [expr $cheesenoiseindex + 10] ]
      set viewbbox [$PAD bbox 1]
      set xmin [lindex $viewbbox 0]
      set ymin [lindex $viewbbox 1]
      set xmax [lindex $viewbbox 2]
      set ymax [lindex $viewbbox 3]
      set x [expr $x * 0.85 * ($xmax-$xmin) + [lindex [$PAD getview] 0] ]
      set y [expr $y * 0.85 * ($ymax-$ymin) + [lindex [$PAD getview] 1] ]
    $PAD item $g -x $x
    $PAD item $g -y $y

  }
}

##
##  The original animated bug
##

proc makeBug {PAD} {
    global _bug

    set _bug(timerrate) 100         ;# Delay between bug animation steps (in milliseconds)

    set bug [$PAD create rectangle -1.5 -1.5 1.5 1.5 -place center]
    $PAD scale $bug 0.2
    set zoom [$PAD getzoom]

    if {[info exists _bug(list)]} {
	set prevbug [lindex $_bug(list) [expr [llength $_bug(list)] - 1]]
	set _bug(noiseindex$bug)  [expr $_bug(angle$prevbug) * 100]
	set _bug(noiseinc$bug)  [expr $_bug(angle$prevbug) / 25.0]
    } else {
	set _bug(noiseindex$bug)  0.0
	set _bug(noiseinc$bug)  0.01
    }
    set _bug(angle$bug) 0.0
    set _bug(zoom$bug)  $zoom

    set _bug(fillColor) [$PAD color alloc blueviolet]
    set _bug(penColor) [$PAD color alloc black]
    $PAD itemconfigure $bug -renderscript "bugrender $PAD $bug"
    if {![info exists _bug(timer)]} {
	set _bug(timer) [after $_bug(timerrate) bugmove $PAD]
    }
    set _bug(list) [lappend _bug(list) $bug]
    bugmoveindividual $PAD $bug

    $PAD bind $bug <Delete> {
	%P color free $_bug(fillColor)
	%P color free $_bug(penColor)
	set _bug(list) [lremove $_bug(list) %O]
	unset _bug(xinc%O)
	unset _bug(yinc%O)
	unset _bug(angle%O)
	unset _bug(noiseinc%O)
	unset _bug(noiseindex%O)
	unset _bug(zoom%O)
	if {$_bug(list) == ""} {
	    if {[info exists _bug(timer)]} {
		after cancel $_bug(timer)
		unset _bug(timer)
		unset _bug(list)
	    }
	}
    }

    return $bug
}

proc bugmove {PAD} {
    global _bug

    foreach bug $_bug(list) {
	bugmoveindividual $PAD $bug
    }
    
    set _bug(timer) [after $_bug(timerrate) bugmove $PAD]
}

proc bugmoveindividual {PAD bug} {
    global _bug

#
# Restore static variables
#
    set noiseindex $_bug(noiseindex$bug)
    set noiseinc $_bug(noiseinc$bug)
    set angle $_bug(angle$bug)
    set zoom $_bug(zoom$bug)

#
# Is bug visible?
#
				# Pad-View bounding box

    set viewbbox [$PAD bbox 1]
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

    set visible 1
    set wall -1
    if {$bbxmin < $xmin} {
	set visible 0
	set wall 0
    }
    if {$bbymin < $ymin} {
	set visible 0
	set wall 1
    }
    if {$bbxmax > $xmax} {
	set visible 0
	set wall 2
    }
    if {$bbymax > $ymax} {
	set visible 0
	set wall 3
    }
#
# Compute bug travel
#
    set pi 3.14
    set noiseindex [expr $noiseindex + $noiseinc]
    set angleinc [$PAD noise $noiseindex]
    set angle [expr $angle + $angleinc]
    if {$angle > 2*$pi} {set angle [expr $angle - 2*$pi]}
    if {$angle < 0} {set angle [expr $angle + 2*$pi]}
    set xinc [expr cos($angle)]
    set yinc [expr sin($angle)]
    set xtravel [expr 10 * $xinc / $zoom]
    set ytravel [expr 10 * $yinc / $zoom]

#
# Move bug one step
#
    if {($visible ||
	 ($wall == 0 &&
	  ($angle < 0.5*$pi ||
	   $angle > 1.5*$pi)) ||
	 ($wall == 1 &&
	  ($angle < $pi)) ||
	 ($wall == 2 &&
	  ($angle > 0.5*$pi &&
	   $angle < 1.5*$pi)) ||
	 ($wall == 3 &&
	  ($angle > $pi)))} {
	$PAD slide $bug $xtravel $ytravel
    } else {
	$PAD damage $bug
	set angle [expr $angle + 0.5]
    }

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

    $PAD render config -linewidth 0.1 -color $_bug(fillColor)
    $PAD render draw polygon $xinc $yinc [expr -.3 * $yinc] [expr .3 * $xinc] \
	    [expr .3 * $yinc] [expr -.3 * $xinc] $xinc $yinc
    $PAD render config -linewidth 0 -color $_bug(penColor)
    $PAD render draw line $xinc $yinc [expr 1.5 * $xinc] [expr 1.5 * $yinc]
}
