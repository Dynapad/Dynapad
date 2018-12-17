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

proc make.search {PAD} {
    global typeText typeName typeImage typeDirectory typeHTML geometry
    global _color _font

    if {[winfo exists .search]} {
	raise .search
	return
    }

	# -- CREATE OBJECTS

    toplevel .search -bg $_color(toolbg)
    wm resizable .search 0 0
    wm title .search "Pad++ Search"

	# create search-string objects
    frame .search.stringframe -borderwidth 2 -bg $_color(toolbg)
    label .search.stringlabel -text "Search For: " \
	-font $_font(tools) \
	-bg $_color(toolbg)
    entry .search.string -relief sunken -width 40 \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
    button .search.stringall -text All \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

	# create search-type objects
    frame .search.typeframe -borderwidth 2 -bg $_color(toolbg)
	frame .search.typeframe.f -bg $_color(toolbg)
	frame .search.typeframe.r1 -bg $_color(toolbg)
	frame .search.typeframe.r2 -bg $_color(toolbg)
	label .search.typelabel -text "Within:" -anchor w -bg $_color(toolbg)
    checkbutton .search.typetext -text {Object Contents} -variable typeText -anchor w -relief flat  \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    checkbutton .search.typename -text {Object Name} -variable typeName -anchor w -relief flat \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    checkbutton .search.typeimage -text {Images} -variable typeImage -anchor w -relief flat \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    checkbutton .search.typedirectory -text {Directories} -variable typeDirectory -anchor w -relief flat \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    checkbutton .search.typehtml -text {HTML Objects} -variable typeHTML -anchor w -relief flat \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    set typeText 1

	# create search-result objects
    frame .search.resultframe -borderwidth 2 -bg $_color(toolbg)
    label .search.resultlabel -text "Result:" -anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg)
    listbox .search.resultbox -relief sunken \
	-font $_font(tools) \
	-bg $_color(toolbg) -selectbackground $_color(toolselbg) \
	-highlightbackground $_color(toolbg)
    scrollbar .search.resultscroll -command searchScroll \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

	# create action buttons
    frame .search.ack -bg $_color(toolbg)
    button .search.search -text Search -command "
			searchForText $PAD \[.search.string get\]
	" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    button .search.goto -text "Zoom To" -command "gotoTextSelection" -state disabled \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    button .search.reset -text Clear -command "searchClear $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    button .search.close -text Close -command "
        searchClear $PAD
        destroy .search
    " \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

	# -- PACK OBJECTS

	# search string
    pack .search.stringframe .search.typeframe \
		.search.resultframe  -side top -fill x -padx 2m -pady 2m
    pack .search.stringlabel -in .search.stringframe -side left
    pack .search.string      -in .search.stringframe -side right -expand t -fill x
    pack .search.stringall   -in .search.stringframe -side right

	# search type
    pack .search.typelabel .search.typeframe.f -in .search.typeframe -side left
    pack .search.typeframe.r1 .search.typeframe.r2 -in .search.typeframe.f -fill x
    pack .search.typetext .search.typename .search.typeimage \
		-in .search.typeframe.r1 -side left -fill x
	pack .search.typedirectory .search.typehtml \
		-in .search.typeframe.r2 -side left -fill x

	# result box
	pack .search.resultlabel -in .search.resultframe -side top -fill x
    pack .search.resultbox    -in .search.resultframe -expand t -fill both -side left
    pack .search.resultscroll -in .search.resultframe -fill y -side left

	# action buttons
    pack .search.ack -fill x -side bottom
    pack .search.search .search.goto .search.reset .search.close \
			-in .search.ack -side left -fill x -expand t

	# bind actions
					# Fix broken default backspace/delete bindings
    bind .search.string <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
    bind .search.string <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}
    bind .search.string <Enter> {focus %W}
    bind .search.string <Key-Return> "searchForText $PAD \[.search.string get\]"
    .search.stringall config -command ".search.string delete 0 end; searchForText $PAD {}"

    bind .search.resultbox <ButtonPress-1> {
	%W selection clear 0 end
        %W selection set [%W nearest %y]
    	set index [.search.resultbox curselection]
        if {[catch {set string [.search.resultbox get $index]}]} {
			.search.goto configure -state disabled
        } else {
			.search.goto configure -state normal
	}
    }
    bind .search.resultbox <Double-ButtonPress-1> "gotoTextSelection"

    pad_ManageWindow .search
    wm title .search "Pad++ Search"
}

proc gotoTextSelection {} {
    global _pad _search

    set index [.search.resultbox curselection]
    if {[info exists _search($index)]} {
	set surface [lindex $_search($index) 0]
	set id [lindex $_search($index) 1]
	$surface center -twostep $id $_pad(AnimationSpeed)
    }
}

#
# Find all the items with 'string' in them on all the surfaces,
# using the specified method (withtag, withtext, withname, etc.),
# and return a list of items where each item is a list with two 
# elements: surface and id.
# Don't return items with null text
#
proc searchFindItems {PAD method string} {
    set pads [$PAD getpads]
    set items ""
    if {$string == ""} {
	set string ".*"
    }
    foreach pad $pads {
	set ids [$pad find -regexp $method $string]
	foreach id $ids {
	    set type [$pad type $id]
	    set null 0
	    if {($type == "text") || ($type == "textfile")} {
		if {[$pad ic $id -text] == ""} {
		    set null 1
		}
	    }
	    if {($type == "directory")} {
		if {[$pad ic $id -file] == ""} {
		    set null 1
		}
	    }
	    if {[lmember [$pad ic $id -tags] logo] ||
	        [lmember [$pad ic $id -tags] status] ||
	        [lmember [$pad ic $id -tags] statusbg]} {
		    set null 1
	    }
	    if {!$null} {
		lappend items "$pad $id"
	    }
	}
    }
    return $items
}

#
# Clear search results
# (delete markers and searchbox results)
#
proc searchClear {PAD} {
    global _search

    foreach pad [$PAD getpads] {
	$pad delete searchResult
    }

    .search.resultbox delete 0 end
    .search.goto configure -state disabled
    if {[info exists _search(index)]} {
	for {set i 0} {$i < $_search(index)} {incr i} {
	    unset _search($i)
	}
    }
    set _search(index) 0
}

proc searchForText {PAD string} {
    global _search typeText typeName typeImage typeDirectory typeHTML

    set items ""
    if {$typeText} {
	set newitems [searchFindItems $PAD withtext $string]
	if {$newitems != ""} {
	    eval lnewappend items $newitems
	}
    }
    if {$typeName} {
	set newitems [searchFindItems $PAD withname $string]
	if {$newitems != ""} {
	    eval lnewappend items $newitems
	}
    }

    set types ""
    if {$typeImage} {lappend types image}
    if {$typeDirectory} {lappend types directory}
    if {$typeHTML} {lappend types html}

    foreach type $types {
	set newitems [searchFindItems $PAD withname $string]
	foreach item $newitems {
	    set surface [lindex $item 0]
	    set id [lindex $item 1]
	    if {[$surface type $id] == $type} {
		if {$newitems != ""} {
		    lnewappend items $item
		}
	    }
	}
    }

    searchClear $PAD

    foreach item $items {
	set surface [lindex $item 0]
	set id [lindex $item 1]
	set type [$surface type $id]
	if {($type == "text") ||
	    ($type == "textfile")} {
		.search.resultbox insert end "[$surface itemconfig $id -text]"
		set _search($_search(index)) $item
		incr _search(index)
	    }
	if {($type == "textfile") ||
	    ($type == "directory")} {
		.search.resultbox insert end "\[[$surface itemconfig $id -file]\]"
		set _search($_search(index)) $item
		incr _search(index)
	    }

	set marker [eval $surface create rectangle [$surface bbox $id]]
	$surface ic $marker -renderscript "marker $surface $id" -alwaysrender 1 -tags "searchResult"
    }

    .search.resultbox see 0
    eval .search.resultscroll set [.search.resultbox yview]
}

#
# Marker
# Drawn as a outline around the specified object with a fixed
# width line so it has a minimum size
#

proc marker {PAD obj} {
    set view [$PAD getview]
    set x [lindex $view 0]
    set y [lindex $view 1]
    set zoom [lindex $view 2]

    set bb [$PAD bbox $obj]
    set x1 [lindex $bb 0]
    set y1 [lindex $bb 1]
    set x2 [lindex $bb 2]
    set y2 [lindex $bb 3]

    $PAD render config -linewidth [expr [bbheight $bb] * 0.1] -color blue
    $PAD render draw line $x1 $y1 $x2 $y1 $x2 $y2 $x1 $y2 $x1 $y1
}

proc searchScroll {args} {
    eval .search.resultbox yview $args
    eval .search.resultscroll set [.search.resultbox yview]
}
