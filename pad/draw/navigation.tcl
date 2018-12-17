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
# Define post-write procedure so bookmarks get written out in .pad files
#
proc Pad_postWriteBookmark {PAD} {
    global bookmark

    # write tcl code to recreate bookmark snapshots

    if {[info exists bookmark] && [info exists bookmark(index)] && \
	$bookmark(index) > 0} {
	set result "\# Code for recreating bookmark snapshots\n"
	append result "global bookmark\n"
	append result "set bookmark(index) 0\n"
	append result "set bookmark(pad) \$PADLOAD\n"
	append result "set bookmark(mode) Bookmark\n"
	append result "set bookmark(deleteflag) 0\n"
	append result "set bookmark(grabFactor) $bookmark(grabFactor)\n"
	append result "make.bookmark \$PADLOAD\n"
	append result "raise \$PADLOAD\n"
	append result "if \[winfo exists .bookmark\] \{lower .bookmark\}\n"
	append result "set origView \[\$PADLOAD getview\]\n"
	
	for {set i 0} {$i < $bookmark(index)} {incr i} {	
	    if [info exists bookmark(view$i)] {
		append result "eval \$PADLOAD moveto $bookmark(view$i.view)\n"
		append result "\$PADLOAD update\n"
		append result "addBookmark \$PADLOAD\n"
	    }
	}

	append result "eval \$PADLOAD moveto \$origView\n"

    } else {
	set result ""
    }

    return $result
}

proc padmoveto {PAD val} {
    global _pad

    if {[llength $val] == 2} {
        set z [$PAD getzoom]
	lappend val $z
    }
    if {[llength $val] == 3} {
         lappend val $_pad(AnimationSpeed)
    }
    eval "$PAD moveto $val"
    .bookmark.view.go delete 0 end
}

proc addBookmark {PAD} {
    global geometry bookmark _pad

    set bbox [$PAD bbox 1]

    # replace existing bookmark for this view
    for {set i 0} {$i < $bookmark(index)} {incr i} {
	if {[info exists bookmark(view$i)] && \
	    $bbox == $bookmark(view$i)} {
	    set bookmark(mode) BookmarkDelete
	    adjustMainView $bookmark(navpad) $i
	    set bookmark(mode) Bookmark
	    break
	}
    }

    # store the view bbox
    set bookmark(view$bookmark(index)) [$PAD bbox 1]

    # store the view and its image so it can be displayed later
    set viewIndex view$bookmark(index)
    append viewIndex .view
    set bookmark($viewIndex) [$PAD getview]
    set imageIndex view$bookmark(index)
    append imageIndex .image
    set bookmark($imageIndex) [grabBookMarkImage $PAD]

    incr bookmark(index)

    if { ![winfo exists .bookmark] } {
	make.bookmark $PAD
    } else {
	set newpad $bookmark(navpad)
	set treeRoot $bookmark(tree)
	set snap [createBookmark $newpad [expr $bookmark(index)-1]]
	$newpad tree create $snap
	insertNode $newpad $snap $treeRoot
	updateBookmarkView $newpad
    }
}

proc grabBookMarkImage {PAD} {
    #
    # Grab image of the current view
    #
    global bookmark

    set view [$PAD getview]
    set vx [lindex $view 0]
    set vy [lindex $view 1]
    set vz [lindex $view 2]
    set bb [$PAD bbox 1]
    set origWidth [expr [bbwidth $bb]*$vz]
    set origHeight [expr [bbheight $bb]*$vz]

    # turn off the status layer
    set layers [$PAD ic 1 -visiblelayers]
    append layers " -status"
    $PAD ic 1 -visiblelayers $layers
    $PAD update

    # grab the image and restore the layers
    set height [expr $bookmark(grabFactor)/100.0 * $origHeight]
    set width [expr $bookmark(grabFactor)/100.0 * $origWidth]
    set im [$PAD grab -path $PAD -dim "$width $height" \
		0 0 $origWidth $origHeight]
    $PAD image configure $im -rgb 0
    append layers " status"
    $PAD ic 1 -visiblelayers $layers
    return $im
}

proc make.bookmark {PAD} {
    global _pad _color _font
    global menu geometry bookmark

    if {[winfo exists .bookmark]} {
        raise .bookmark
	return
    }

    if {![info exists bookmark(index)]} {
	set bookmark(index) 0
    }

    set bookmark(pad) $PAD
    set bookmark(mode) Bookmark
    if {![info exists bookmark(grabFactor)]} {
	set bookmark(grabFactor) 100
    }

    toplevel .bookmark
    frame .bookmark.f
    pack .bookmark.f -side top -expand true -fill both

    # create a new pad
    set bb [$PAD bbox 1]
    set vz [lindex [$PAD getview] 2]
    set origWidth [expr [bbwidth $bb]*$vz]
    set origHeight [expr [bbheight $bb]*$vz]

    set newpad .bookmark.f.pad
    set bookmark(navpad) $newpad
    set origw $_pad(Width)
    set origh $_pad(Height)
    set _pad(Width) 200
    set _pad(Height) 200
    set menu(.bookmark) 1

    startPadDraw $newpad

    $newpad ic 1 -visiblelayers "all -status"
    #$newpad config -bg [lindex [$PAD config -bg] end]

    # Delete paddraw things we don't want
    destroy [menubarGetName $newpad]; 
    set _pad(Width) $origw
    set _pad(Height) $origh

    # create a tree root for bookmark images to hang off of
    set treeRoot [$newpad tree createroot]
    set bookmark(tree) $treeRoot

    # create tree nodes for bookmarks and insert them in the tree
    for {set i 0} {$i < $bookmark(index)} {incr i} {
	if [info exists bookmark(view$i)] {
	    set snap [createBookmark $newpad $i]
	    $newpad tree create $snap
	    insertNode $newpad $snap $treeRoot
	}
    }

    # center the tree laytout
    set bookmark(deleteflag) 0
    updateBookmarkView $newpad

    # Create the button controls
    frame .bookmark.row1  -bg $_color(toolbg)

    set addmark .bookmark.row1.addmark
    button $addmark -text "Add Bookmark" -command "addBookmark $PAD" \
	    -bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	    -highlightbackground $_color(toolbg)

    set home .bookmark.row1.home
    button $home -text "Go Home" \
	-command "$PAD moveto 0.0 0.0 1.0; $PAD update -dissolve 2" \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    set delete .bookmark.row1.delete
    checkbutton $delete -text "Delete" -variable bookmark(deleteflag) \
	-command {deleteBookmark} \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    set row2 .bookmark.row2
    frame $row2 -bg $_color(toolbg)

    set view $row2

    button $view.get -text "Get View:" \
	-command "$view.go delete 0 end; \
                  $view.go insert 0 \[eval format \\\"%.2f %.2f %.2f\\\" \[$PAD getview\]\]" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    entry $view.go -width 20 -relief sunken -font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)

    # Fix broken default backspace/delete bindings
    bind $view.go <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
    bind $view.go <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}
    bind $view.go <Enter> {focus %W}
    bind $view.go <Key-Return> "padmoveto $PAD \[%W get\]"
    $view.go delete 0 end

    set close .bookmark.close
    button $close -text "Close" -command {destroy .bookmark} \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    set scale $row2.scale
    scale $scale -orient horizontal -label "Image Resolution" \
	-variable bookmark(grabFactor) -length 120 \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)

    pack .bookmark.row1 -fill x
    pack $addmark -side left
    pack $home -fill x -side left
    pack $delete -fill x -side left
    pack $row2 -fill x
    pack $scale -fill x -side left
    pack $view.get -fill x -side left
    pack $view.go -fill x -side left
    pack $close -side top -fill x

    pad_ManageWindow .bookmark 0 0
    wm title .bookmark "Pad++ Bookmarks"

    set callback [bind $newpad <Configure>]
    append callback "\nupdateBookmarkView $newpad"
    bind $newpad <Configure> "$callback"
}

proc adjustMainView {PAD index} {
    global bookmark _pad

    if {$bookmark(mode) == "Bookmark"} {
	# take the main pad to selected bookmark
	eval $bookmark(pad) centerbbox -twostep $bookmark(view$index) \
	    $_pad(AnimationSpeed) .5 .5 1

    } else {
	# delete selected snapshot from the tree
	set snap $bookmark(view$index.snap)
	$PAD tree delete $snap
	$PAD image free $bookmark(view$index.image)
	unset bookmark(view$index) bookmark(view$index.view) \
	    bookmark(view$index.snap) bookmark(view$index.image) \
	    bookmark(view$index.imageobj)
	updateBookmarkView $PAD
	set bookmark(deleteflag) 0
	deleteBookmark
    }
}

proc updateBookmarkView {PAD} {
    global bookmark _pad

    # center the tree in the bookmark window
    set treeRoot $bookmark(tree)
    $PAD tree setspacing treenode 200 100
    $PAD tree layout $treeRoot
    eval $PAD center treenode $_pad(AnimationSpeed) 0.5 0.5 1
}

proc deleteBookmark {} {
    global bookmark

    if {$bookmark(deleteflag) == 1} {
	.bookmark.f config -cursor "crosshair red white"
	set bookmark(mode) BookmarkDelete
    } else {
	.bookmark.f config -cursor {}
	set bookmark(mode) Bookmark
    }
}

proc createBookmark {PAD index} {
    global bookmark _pad

    set view $bookmark(view$index.view)
    set vx [lindex $view 0]
    set vy [lindex $view 1]
    set vz [lindex $view 2]

    set image [$PAD create image -image $bookmark(view$index.image)]
    set bb [$PAD bbox $image]
    set bbox [eval $PAD create rectangle $bb -pen seagreen3 \
		  -tags bookmarkBBox]
    $PAD ic bookmarkBBox -penwidth [expr 5.0/[$PAD getzoom]]
    set snap [$PAD create group -members "$bbox $image" -info $index -divisible 0]
    set bookmark(view$index.snap) $snap
    set bookmark(view$index.imageobj) $image
	
    $PAD bind $snap <Enter> "hiliteBookmark %P %O $bbox"
    $PAD bind $snap <Leave> "unhiliteBookmark %P %O $bbox"
    $PAD bind $snap <ButtonRelease-1> "adjustMainView %P $index"

    return $snap
}

proc hiliteBookmark {PAD snap bbox} {
    #
    # Creates a transparent rectangle over the bookmark in the main pad
    # if the current view covers the bookmak.  In delete mode, just
    # changes the bookmark bbox border to red.
    #
    global bookmark

    if {$bookmark(deleteflag) == 0} {
	$PAD ic $bbox -pen seagreen3
	set index [$PAD ic $snap -info]
	set viewBB [$bookmark(pad) bbox 1]
	if {[compareBBoxes $viewBB $bookmark(view$index)] == "parent"} {
	    set bookmark(mark) [eval $bookmark(pad) create rectangle \
				    $bookmark(view$index) -fill yellow \
				    -pen none -transparency 0.25]
	    $bookmark(pad) raise $bookmark(mark)
	}
    } else {
	$PAD ic $bbox -pen red
    }
}

proc unhiliteBookmark {PAD snap bbox} {
    #
    # Undo the work done by hiliteBookmark (deletes the transparent bookmark
    # bbox from the main view).
    #
    global bookmark

    $PAD ic $bbox -pen gray20
    if {$bookmark(deleteflag) == 0} {
	if [info exists bookmark(mark)] {
	    $bookmark(pad) delete $bookmark(mark)
	    unset bookmark(mark)
	}
    }
}


proc insertNode {PAD obj rootObj} {
    #
    # Insert obj in given tree based on its relationship with existing nodes.
    # Assumptions:
    #   - this routine is called with the root of the tree (unless it is 
    #   called recursively from within here).
    #   - tree is rooted with a treeroot.
    #   - tree nodes already been created for obj and rootObj
    #   - sibilings are ordered from lowest to highest.
    #

    # get rootObj & obj relationship
    set rel [compareNodes $PAD $rootObj $obj]

    if {$rel == "parent"} {
	# find relationship with children
	set highSib ""
	foreach child [$PAD tree getchildren $rootObj] {
	    if { [$PAD isvisible $child] == 0} {
		puts "insertNode: ignoring invisible treenode $child"
		continue
	    }
	    set rel [compareNodes $PAD $child $obj]
	    if {$rel == "parent"} {
		# go down the tree using this child
		return [insertNode $PAD $obj $child]

	    } elseif {$rel == "highSib"} {
		# found parent and first sibling higher than obj,
                # but lets check the others for parent-child rel.
		if {$highSib == ""} {
		    set highSib $child
		}

	    } elseif {$rel == "lowSib"} {
		# keep on going until first higher sibling is found
		continue

	    } elseif {$rel == "child"} {
		# make obj to be its parent and continue
		$PAD tree addnode $child $obj; # or reparent?
		continue
	    } else {
		puts "insertNode: invalid relation $rel between $child & $obj"
	    }
	}

	if {$highSib == ""} {
	    # obj must be highest child (or the only one)
	    $PAD tree addnode $obj $rootObj
	} else {
            # obj has a sibiling in the tree
	    $PAD tree addnode $obj $rootObj
	    $PAD tree lower $obj $highSib
	}

    } else {
	# impossible to reach this
	puts "insertNode: invalid state, called with $rootObj, invalid relation $rel between $rootObj & $obj"
	$PAD tree addnode $obj $rootObj
    }
}

proc compareNodes {PAD n1 n2} {
    #
    # Return node1's relationship to node2 based on their bbox: 
    #    parent: node1 contains node2
    #    child:  node2 contains node1
    #    lowSib: node1 would be lower in sibling order
    #    highSib: node1 would be higher in sibling order
    #

    #  check for special case of treeroot objs
    if [$PAD hastag $n1 treeroot] {
	return parent
    } elseif [$PAD hastag $n2 treeroot] {
	return child
    } elseif {[$PAD hastag treenode $n1] && ![$PAD isvisible $n1]} {
	return parent
    }

    # compare views of the two nodes
    global bookmark
    return [compareBBoxes $bookmark(view[$PAD ic $n1 -info]) \
		$bookmark(view[$PAD ic $n2 -info])]
}

proc compareBBoxes {bb1 bb2} {
    #
    # Return bb1 relationship to bb2 based on their bbox: 
    #    parent: bb1 contains bb2
    #    child:  bb2 contains bb1
    #    lowSib: bb1 would be lower in sibling order
    #    highSib: bb1 would be higher in sibling order
    #

    set bb1x1 [lindex $bb1 0]
    set bb1y1 [lindex $bb1 1]
    set bb1x2 [lindex $bb1 2]
    set bb1y2 [lindex $bb1 3]
    set bb2x1 [lindex $bb2 0]
    set bb2y1 [lindex $bb2 1]
    set bb2x2 [lindex $bb2 2]
    set bb2y2 [lindex $bb2 3]

    if {[bbenclosedoron $bb2x1 $bb2y1 $bb1] && [bbenclosedoron $bb2x2 $bb2y2 $bb1]} {
	return parent

    } elseif {[bbenclosedoron $bb1x1 $bb1y1 $bb2] && [bbenclosedoron $bb1x2 $bb1y2 $bb2]} {
	return child

    } else {
	# compare view centers
	set bb1c [bbcenter $bb1]
	set bb2c [bbcenter $bb2]
	set bb1cx [lindex $bb1c 0]
	set bb2cx [lindex $bb2c 0]
	if {$bb1cx < $bb2cx} {
	    return lowSib
	} elseif {$bb1cx > $bb2cx} {
	    return highSib
	} elseif {[lindex $bb1c 1] < [lindex $bb2c 1]} {
	    return lowSib
	} else {
	    return highSib
	}
    }
}
