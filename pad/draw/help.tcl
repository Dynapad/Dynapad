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
# help.tcl
#
# Create a oval layout of help pages in a Pad++ window.
# It assumes that this help is the only thing on the pad surface,
# and creates things wherever it chooses (which happens to be
# around the origin).
#
# Help is created from a text file that contains help text in
# a very simple format.  Text is placed as formatted, and
# there are two simple tags (in the style of html) that control
# things.  (The tags must appear on a new line
# <page> starts a new page of text
# <tcl> and </tcl> pairs enclose Tcl code which will be evaluated,
# and the result (as determined by a list of returned ID's)
# will be put on the page.
#
# It uses the global array _help
#
# It defines one public procedure "pad_help" to create help on a pad surface.
# All the other procedures are private, and start with "help_"
#
proc pad_help {PAD title filename} {
    global _help _pad _color

    set _pad(StatusLine$PAD) 0
    updateStatusText $PAD ""
    
    set _help(prev) ""
    set orig_step [expr 3.1415 / 6]
    set step $orig_step
    set two_pi 6
    set lastBtnId ""
    set firstPageId ""
    set eof 0
    
    set file [open $filename r]
    
    for {set i 0} {$i < $two_pi} {set i [expr $i + $step]} {
	
	set tcltext ""

				# Read next page from file
	if {$eof == 1} {
	    set text "<empty>"
	} else {
				# Get lines from the file
	    set text ""
	    set count -1
	    set lineno 0
	    while 1 {
				# Stop after 24 lines
		if {$lineno > 24} {
		    break;
		}
		
				# Read a line from the file
		set count [gets $file line]
		if {$count < 0} {
		    set eof 1
		    break
		}
		
				# See if the line is a special command
		if {[string index $line 0] == "<"} {
		    if {$line == "<page>"} {
			break
		    } elseif {$line == "<tcl>"} {
			set tcltext [help_tcl_text $file]
			break
		    } else {
			continue;
		    }
		}
				# Not a special command - append line to page text
		set text "$text\n$line"
		set lineno [expr $lineno + 1]
	    }
	}
	
	set  a [expr 4.71239 - $i]
	set  x [expr cos($a) * 1100 - 50]
	set  y [expr sin($a) * 800]
	set  z [expr sin($a + 3.1415) + 2]
	
				# Speed up when you are further away
	set step [expr $orig_step * (0.8 + ($z - 2) * 0.2)]

				# Speed up as you go around
	set step [expr $step * (1 + ($i * 0.2 / $two_pi))]
	
				# Create the page
	set id [$PAD create rectangle 0 0 100 130 -anchor sw -place "$x $y $z" \
		-penwidth 2 -fill #ffe4bc \
		-pen gray60 -alwaysrender 1]
	
	
	set next [$PAD create button -text "Next" -command \
		"help_go_page $PAD \$_help(next.$id)" \
		-fill $_color(toolbg) \
		-z [expr 0.5 * $z]]
	$PAD layout position 0.99 0.007 -ref $id 1 0 $next
	
				# Create text for page
	set text_id [$PAD create text -z [expr $z * 3.0] -font $_pad(Font) \
		-text $text -anchor "nw" -events 0]
	$PAD layout position .1 .9 -ref $id 0 1 $text_id
	
	if {$tcltext != ""} {
	    set obj [eval $tcltext]
	    help_place $PAD $obj $text_id $id
	}
	
				# Perform event bindings
	if {$firstPageId == ""} {
	    set firstPageId $id
	} else {
	    set _help(next.$lastPageId) $id
	}
	
	set lastPageId $id
	
	$PAD bind $id <Run-ButtonPress-1> {
	    set _help(pagePlace) [%W getview]
	}
	
	$PAD bind $id <Run-ButtonRelease-1> {
	    if {$_help(pagePlace) == [%W getview]} {
		help_go_page %W [%W find withtag current]
	    }
	}
	
    }
    
    set _help(next.$id) $firstPageId
    
    close $file
    
    set t [$PAD create text -text $title -font "Times-1" -place "0 200 100" -pen #502959 -anchor center]
    set p [$PAD create button -text "Start..." -command "help_go_page $PAD $firstPageId" -z 10 \
	       -fill $_color(toolbg)]
    $PAD layout position 0 0 -ref $t 0 1.2 $p
    
    set p [$PAD create button -text "Index..." -sticky 1 \
	       -fill $_color(toolbg) \
	       -command "help_create_index $PAD"]
    $PAD addtag "help_index_create" $p
    help_configure $PAD
    
    bind $PAD <Configure> {help_configure %W}
}

proc help_configure {PAD} {
    $PAD layout position 1 1 -ref 1 1 1 help_index_create
    
    if {[$PAD find withtag "help_index_portal"] != ""} {
	$PAD layout position 1 1 -ref 1 1 1 help_index_portal
	$PAD layout position 0 0 -ref help_index_portal 0 1 help_index_close
    }
}

proc help_go {PAD id} {
    global _help

    $PAD ic $_help(prev) -pen gray40
    $PAD ic $id -pen #f36f3b
    set _help(prev) $id
    $PAD center $id 1000 0.5 0.5 0.9
}

proc help_go_page {PAD id} {
    global _help

    set thisView [$PAD getview]
    $PAD center $id 0 0.5 0.5 0.9
    set newView [$PAD getview]
    eval $PAD moveto $thisView
    if {$thisView == $newView} {
	help_go $PAD $_help(next.$id)
    } else {
	help_go $PAD $id
    }
}

proc help_tcl_text {file} {
    set tcltext ""
    while 1 {
	set count [gets $file line]
	if {$line == "</tcl>"} {
	    break
	} else {
	    set tcltext "$tcltext\n$line"
	}
    }
    return $tcltext
}

proc help_place {PAD obj text page} {
    set obox [$PAD bbox $obj]
    set tbox [$PAD bbox $text]
    set pbox [$PAD bbox $page]

    set yavail [expr ([lindex $tbox 1] - [lindex $pbox 1])]
    set xavail [expr [bbwidth $pbox]]

    set xofs [expr $xavail * 0.1]
    set xavail [expr $xavail - $xofs]
    set yofs [expr $yavail * 0.4]
    set yavail [expr $yavail - $yofs]


    set xfac [expr $xavail / [bbwidth $obox]]
    set yfac [expr $yavail / [bbheight $obox]]

    if {$xfac < $yfac} {
	set zfac $xfac 
    } else {
	set zfac $yfac
    }

    if {$zfac <= 0} {set zfac 1.0}

    $PAD scale $obj $zfac

    set obox [$PAD bbox $obj]
    set dx [expr [lindex $tbox 0] - [lindex $obox 0]] 
    set dy [expr [lindex $tbox 1] - [lindex $obox 3] - (0.2 * $yofs)] 
    $PAD slide $obj $dx $dy
}

#
# Create "Index" portal that acts like a simple map to
# let the user go to a specific help page.
#
proc help_create_index {PAD} {
    global _font _color

    if {[$PAD find withtag "help_index_portal"] == ""} {
	set zoom [$PAD getzoom]
	set portal [$PAD create portal 0 0 100 100 \
		-fill white -sticky 1 -z [expr 1.0 / $zoom]]
	$PAD addtag "help_index_portal" $portal
	set maxdim [maxdim [$PAD bbox all]]
	$PAD ic $portal -view "0 0 [expr 100.0 / $maxdim]"

	set button [$PAD create button -text "Close" -sticky 1 \
			-fill $_color(toolbg) \
			-command "help_delete_index $PAD" -z [expr 1.0 / $zoom]]
	$PAD addtag "help_index_close" $button

				# Hide button that creates index
	$PAD ic help_index_create -transparency 0 -events 0
	$PAD addtag "hide" help_index_create

	help_configure $PAD
    }
}

#
# Delete "Index" portal, and make button to create help
# visible again.
#
proc help_delete_index {PAD} {
    if {[$PAD find withtag "help_index_portal"] != ""} {
	$PAD delete help_index_portal
	$PAD delete help_index_close

				# Show button that creates index
	$PAD ic help_index_create -transparency 1 -events 1
	$PAD deletetag "hide" help_index_create
    }
}
