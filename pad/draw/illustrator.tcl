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

# Description
#
# this file will try to import an Adobe Illustrator (tm) file by converting
# the illustrator objects into pad objects.
#
# Known Limitations/Bugs
#
# - the color mapping from Illustrator's CMYK model to pad's RGB model is
#   a bit off
#
# - holes aren't handled well. a hole is where an object has a
#   counter clockwise winding inside a clockwise wound object. The inner
#   object is defined counter clockwise to 'cut out' the inner shape from
#   the outer shape
#
# - text size and line width is approximate, you'll have to tweak some
#
# Some Guides
#
# lowercase colors (g,k,x) = fill color
# uppercase colors         = stroke color
# g = grey scale
# k = CMYK value
# x = named CMYK value
#
# lowercase path (s,f,b)   = closes path
# uppercase path           = leaves path open
# s = stroke
# f = fill
# b = both stroke and fill
#
# *u = compound path start
# *U = compound path end
#

# i've declared the global array _ill to hold the necessary variables
# for this converter

proc importIllustrator {PAD filename} {
    global _ill

# some inits

    set capstyle "butt"
    set joinstyle "miter"
    set fillcolor "000000"
    set pencolor "000000"
    set text_size 12
    set text_anchor sw
    set text_angle 0
    set penwidth 1.0
    set _ill(pointdex) 0
    set start 0

    if {[file exists $filename]} {
	set illfile [open $filename]
	while {[gets $illfile line] != -1} {
# get the bounding box
	    if {[regexp {%%HiResBoundingBox:} $line]} {
		set token [split $line]
		set minx [lindex $token 1]
		set miny [lindex $token 2]
		set maxx [lindex $token 3]
		set maxy [lindex $token 4]
		set _ill(cenx) [expr ($minx + $maxx) / 2.0]
		set _ill(ceny) [expr ($miny + $maxy) / 2.0]
	    }
	    if {[regexp {%%EndSetup} $line]} {
		set start 1
	    }
	    if {$start != 1} {continue}
	    if {[regexp {^%} $line]} {continue}
	    set line [string trimright $line]
	    set token [split $line]
	    set lastdex [expr [llength $token] - 1]
	    set origkey [lindex $token $lastdex]
	    set key [string tolower $origkey]

# color checks
	    if {((($key == "k") && ($lastdex == 4)) || (($key == "x") && ($lastdex > 4)))} {
		set hexcolor [cmyk2rgb [lindex $token 0] [lindex $token 1] [lindex $token 2] [lindex $token 3]]

		$PAD color alloc #$hexcolor
		if {[regexp {[A-Z]} $origkey]} {
		    set pencolor $hexcolor
		} elseif {[regexp {[a-z]} $origkey]} {
		    set fillcolor $hexcolor
		}
	    } elseif {(($key == "g") && ($lastdex == 1))} {
		set hexcolor [grey2rgb [lindex $token 0]]
		$PAD color alloc #$hexcolor
		if {[regexp {[A-Z]} $origkey]} {
		    set pencolor $hexcolor
		} elseif {[regexp {[a-z]} $origkey]} {
		    set fillcolor $hexcolor
		}
# line width, join, miter goo
	    } elseif {(($key == "j") || ($key == "w"))} {
		set dex 1
		while {$dex <= $lastdex} {
		    set val [lindex $token [expr $dex - 1]]
		    if {$origkey == "J"} {
			if {$val == 0} {
			    set capstyle "butt"
			} elseif {$val == 1} {
			    set capstyle "round"
			} elseif {$val == 2} {
			    set capstyle "projecting"
			}
		    } elseif {$origkey == "j"} {
		        if {$val == 0} {
			    set joinstyle "miter"
			} elseif {$val == 1} {
			    set joinstyle "round"
			} elseif {$val == 2} {
			    set joinstyle "bevel"
			}
		    } elseif {$origkey == "w"} {
			set penwidth $val
		    }
		    incr dex 2
		}
		# no color, dump the points
	    } elseif {(($origkey == "n") && ($lastdex == 0))} {
		set _ill(pointdex) 0
	    } elseif {(($origkey == "B") && ($lastdex == 0))} {
# polygon---fill------no pen
# line------no fill---pen
		set tmpdex $_ill(pointdex)
		set _ill(cmd) [list $PAD create polygon]
		out_the_points
		lappend _ill(cmd) -tags \"item\" -penwidth $penwidth
		lappend _ill(cmd) -joinstyle $joinstyle
		lappend _ill(cmd) -fill #$fillcolor -pen none
		eval $_ill(cmd)
		set _ill(pointdex) $tmpdex
		set _ill(cmd) [list $PAD create line]
		out_the_points
		lappend _ill(cmd) -tags \"item\" -penwidth $penwidth
		lappend _ill(cmd) -capstyle $capstyle -joinstyle $joinstyle
                lappend _ill(cmd) -pen #$pencolor
                eval $_ill(cmd)
	    } elseif {(($key == "f") && ($lastdex == 0))} {
# polygon---fill------no pen
		set _ill(cmd) [list $PAD create polygon]
		out_the_points
		lappend _ill(cmd) -tags \"item\" -penwidth $penwidth
		lappend _ill(cmd) -joinstyle $joinstyle
                lappend _ill(cmd) -fill #$fillcolor -pen none
		eval $_ill(cmd)
	    } elseif {(($origkey == "S") && ($lastdex == 0))} {
# line------no fill---pen
		set _ill(cmd) [list $PAD create line]
		out_the_points
		lappend _ill(cmd) -tags \"item\" -penwidth $penwidth
		lappend _ill(cmd) -capstyle $capstyle -joinstyle $joinstyle
		lappend _ill(cmd) -pen #$pencolor
		eval $_ill(cmd)
	    } elseif {(($origkey == "s") && ($lastdex == 0))} {
# polygon---no fill---pen
		set _ill(cmd) [list $PAD create polygon]
		out_the_points
		lappend _ill(cmd) -tags \"item\" -penwidth $penwidth
		lappend _ill(cmd) -joinstyle $joinstyle
		lappend _ill(cmd) -fill none -pen #$pencolor
	 	eval $_ill(cmd)
	    } elseif {(($origkey == "b") && ($lastdex == 0))} {
# polygon---fill------pen
		set _ill(cmd) [list $PAD create polygon]
		out_the_points
		lappend _ill(cmd) -tags \"item\" -penwidth $penwidth
		lappend _ill(cmd) -joinstyle $joinstyle
		lappend _ill(cmd) -fill #$fillcolor -pen #$pencolor
		eval $_ill(cmd)
	    } elseif {(($origkey == "TO") && ($lastdex == 0))} {
# output the text object
# drop the last \n

		set text_stream [string trimright $text_stream]

                set _ill(cmd) [list $PAD create text -text "$text_stream"]
		lappend _ill(cmd) -place "$textx $texty 1" -font "Times-$text_size"
                if {$text_angle > 0.1} {
                    lappend _ill(cmd) -angle $text_angle
                    set text_anchor sw
                }
		lappend _ill(cmd) -pen #$fillcolor -tags "item text"
		lappend _ill(cmd) -anchor $text_anchor
		eval $_ill(cmd)
	    } elseif {(($origkey == "To") && ($lastdex == 1))} {
# new text element, clean out text_stream
		set text_stream ""
                set text_anchor sw
	    } elseif {(($origkey == "Ta") && ($lastdex == 1))} {
# text alignment
                set align [lindex $token 0]
                if {$align == 0} {
                    set text_anchor w
                } elseif {$align == 1} {
                    set text_anchor c
                } elseif {$align == 2} {
                    set text_anchor e
                }
	    } elseif {($origkey == "Tf")} {
# font and size (font isn't attempted)
		set text_size [expr [lindex $token [expr $lastdex - 1]] * .6]
	    } elseif {(($origkey == "Tp") && ($lastdex == 7))} {
# text placement
		set textx [expr [lindex $token 4] - $_ill(cenx)]
 		set texty [expr [lindex $token 5] - $_ill(ceny)]
                set text_angle [expr acos([lindex $token 0]) / 3.141592654 * 180.0]
	    } elseif {(($origkey == "TX") || ($origkey == "Tj") || ($origkey == "Tx") ||
		       ($origkey == "TK") || ($origkey == "Tk"))} {
# build the text stream
# escape the quotes, drop the parens, convert newlines

		regexp {^\((.*)\) T[x|X|j]} $line junk tmp
		regsub -all {\"} $tmp {"} tmp
		regsub -all {\\\(} $tmp {(} tmp
		regsub -all {\\\)} $tmp {)} tmp
		regsub -all {\\r} $tmp "\n" tmp
		append text_stream "$tmp"

	    } elseif {((($key == "m") || ($key == "l")) && ($lastdex == 2))} {
# moveto or lineto
		in_the_point [lindex $token 0] [lindex $token 1]

	    } elseif {(($key == "v") && ($lastdex == 4))} {
# curves
		bezier $_ill([expr $_ill(pointdex) - 2]) $_ill([expr $_ill(pointdex) - 1]) $_ill([expr $_ill(pointdex) - 2]) $_ill([expr $_ill(pointdex) - 1]) [lindex $token 0] [lindex $token 1] [lindex $token 2] [lindex $token 3]

	    } elseif {(($key == "y") && ($lastdex == 4))} {
		bezier $_ill([expr $_ill(pointdex) - 2]) $_ill([expr $_ill(pointdex) - 1]) [lindex $token 0] [lindex $token 1] [lindex $token 2] [lindex $token 3] [lindex $token 2] [lindex $token 3]

	    } elseif {(($key == "c") && ($lastdex == 6))} {
		bezier $_ill([expr $_ill(pointdex) - 2]) $_ill([expr $_ill(pointdex) - 1]) [lindex $token 0] [lindex $token 1] [lindex $token 2] [lindex $token 3] [lindex $token 4] [lindex $token 5]
	    }
	}
    }
}


proc _ill_min {a b} {

    if {($a > $b)} {
	return $b
    } else {
	return $a
    }
}


proc cmyk2rgb {c m y k} {

    if {($k == 1)} {

# black item
	set rgb "000000"
    } else {
	set r [expr round (255 * (1 - [_ill_min 1 [expr $c + $k]]))]
	set g [expr round (255 * (1 - [_ill_min 1 [expr $m + $k]]))]
	set b [expr round (255 * (1 - [_ill_min 1 [expr $y + $k]]))]
	regsub -all { } [format "%2x%2x%2x" $r $g $b] 0 rgb
    }
    return $rgb
}


proc grey2rgb {g} {

    set g [expr round ($g * 255)]
    regsub -all { } [format "%2x%2x%2x" $g $g $g] 0 rgb
    return $rgb
}


proc in_the_point {x y} {
    global _ill

    set _ill($_ill(pointdex)) $x
    incr _ill(pointdex)
    set _ill($_ill(pointdex)) $y
    incr _ill(pointdex)
}


proc out_the_points {} {
    global _ill

    set dex 0
    while {($dex < $_ill(pointdex))} {
 	lappend _ill(cmd) [expr $_ill($dex) - $_ill(cenx)]
	incr dex
	lappend _ill(cmd) [expr $_ill($dex) - $_ill(ceny)]
	incr dex
    }
    set _ill(pointdex) 0
}


# compute a bezier curve on the endpoints and two control
# points...taken from Haeberli
proc bezier {x0 y0 x1 y1 x2 y2 x3 y3} {
    set midx [expr ($x0+(3*$x1)+(3*$x2)+$x3)/8.0]
    set midy [expr ($y0+(3*$y1)+(3*$y2)+$y3)/8.0]
    set linx [expr ($x0+$x3)/2.0]
    set liny [expr ($y0+$y3)/2.0]
    set dx   [expr $midx-$linx]
    set dy   [expr $midy-$liny]
    set mag  [expr ($dx*$dx)+($dy*$dy)]
# tweak the second item in the < as a tolerance of
# bezier smoothness (1 is reasonable
    if {($mag < 1)} {
	in_the_point $x0 $y0
	in_the_point $x1 $y1
	in_the_point $x2 $y2
	in_the_point $x3 $y3
    } else {
	set ax0 $x0
	set ay0 $y0
	set ax1 [expr ($x0+$x1)/2.0]
	set ay1 [expr ($y0+$y1)/2.0]
	set ax2 [expr ($x0+(2*$x1)+$x2)/4.0]
	set ay2 [expr ($y0+(2*$y1)+$y2)/4.0]
	set ax3 $midx
	set ay3 $midy
	bezier $ax0 $ay0 $ax1 $ay1 $ax2 $ay2 $ax3 $ay3

	set bx0 $midx
	set by0 $midy
	set bx1 [expr ($x1+(2*$x2)+$x3)/4.0]
	set by1 [expr ($y1+(2*$y2)+$y3)/4.0]
	set bx2 [expr ($x2+$x3)/2.0]
	set by2 [expr ($y2+$y3)/2.0]
	set bx3 $x3
	set by3 $y3
	bezier $bx0 $by0 $bx1 $by1 $bx2 $by2 $bx3 $by3
    }
}
































