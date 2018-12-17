# "(c) Copyright 1993-1995 Pad++ Consortium {University of New Mexico (UNM),
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

######################################################################################
#
# This Web Browser code uses Pad++ to create individual HTML pages,
# and uses the Pad++ tree layout code to layout and animate those pages.
# This Tcl code specifies event bindings (to follow hyperlinks), and
# manages the layout by calling the tree code appropriately.  It also
# deals with image maps, focus, history, forward/back buttons, and other
# parts of interaction with the web browser.
#
# Pages can optionally have scrollbars if they are too long.  If so,
# then the web page will be in a group along with the scrollbar.
# The tree code always has pointers to the groups as tree nodes, so
# the user of this code must be careful as to whether they have the id
# of a page or a group (called items within).  There are two functions,
# (html_GetItem and html_GetHTML) which go back and forth between these.
#
######################################################################################


#
# Initialize globals
#
if {![info exists _html(initialized)]} {
    set _html(initialized)        1        ;# Only set these variables once
    set _html(focus)              -1       ;# Current focus page
    set _html(focus1)             -1       ;# Previous focus page
    set _html(focus_size)         $_pad(HTMLTextSize) ;# Size of focus page (compared to un-focused page)
    set _html(focus_offset)       0        ;# Offset focus page is from NW corner of top-level window
    set _html(focus_prop)         0        ;# Depth foci propogate (0 means just self)
    set _html(focus_mag)          5        ;# Magnification of focus node (compared to non-focus node)
    set _html(spacing)            6        ;# Spacing between nodes
    set _html(foci)               1        ;# Number of foci active
    set _html(animation_time)     500      ;# Time in milliseconds for HTML animations
    set _html(magic_scale_factor) 0.5      ;# Used to determine how big a page is when created
    set _html(viewmode)   "directviewchange"   ;# Current view mode
    set _html(buttons)            0        ;# Navigation buttons aren't visible to start
    set _html(buttons_deleted)    0        ;# Navigation buttons have been deleted
    set _html(history)            ""       ;# List of pages recently visited
    set _html(history_index)      -1       ;# Current page within history
    set _html(scrollbar)          1        ;# True if long pages should get scrollbar
    set _html(page_size)          100      ;# Length above which pages get scrollbars
    set _html(follow_all_links)   0        ;# True while following links recursively
    set _html(min_size)           250      ;# Size below which anchors can't be followed (in pixels)
}

######################################################################################
#
# Code to create "Web Tools" control window
#
######################################################################################

#
# Create HTML window
#
proc make.html {PAD} {
    global menu geometry _pad _html _color _font

    if {[winfo exists .html]} {
        raise .html 
	return
    }

					# If there is no HTML root, create one.
					# Do this here so that various parameters can
					# be set when this window is created.
    if {![info exists _html(root)]} {
	set view [$PAD getview]
	html_CreateRoot $PAD $view
    }

    toplevel .html -bg $_color(toolbg)
    wm resizable .html 0 0

    label .html.label -text "Location (URL):" \
	-font $_font(tools) \
	-bg $_color(toolbg)
    entry .html.text -relief sunken -width 20 \
	-font $_font(tools) \
	    -bg $_color(toolbg) \
	    -highlightbackground $_color(toolbg)
    if {[info exists _html(url)]} {
	.html.text insert 0 $_html(url)
    }
    bind .html.text <Key-Return> "
        set temp \[%W get\]
        import $PAD \$temp
    "

    radiobutton .html.view1 -text "Camera" -command "html_SetViewMode $PAD" \
	-font $_font(tools) \
	-variable _html(new_viewmode) -value "camera" -anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    radiobutton .html.view2 -text "No view change" -command "html_SetViewMode $PAD" \
	-font $_font(tools) \
	-variable _html(new_viewmode) -value "noviewchange" -anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    radiobutton .html.view3 -text "Zoom out, animate, zoom in" -command "html_SetViewMode $PAD" \
	-font $_font(tools) \
	-variable _html(new_viewmode) -value "indirectviewchange" -anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    radiobutton .html.view4 -text "Direct animation to page" -command "html_SetViewMode $PAD" \
	-font $_font(tools) \
	-variable _html(new_viewmode) -value "directviewchange" -anchor w \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    set _html(new_viewmode) $_html(viewmode)

    button .html.delete -text "Delete Current Page"  -command "pad_fade_delete $PAD \[html_GetItem $PAD \$_html(focus)\]" \
	-anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    button .html.follow -text "Follow all links"  -command "html_FollowAllLinks $PAD \$_html(focus)" \
	-anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    button .html.followr -text "Follow all links recursively"  \
        -command "html_SetupFollowAllLinks $PAD \$_html(focus) 1" \
	-anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

					# Advanced options
    frame .html.optionstart -height 2 -bg $_color(toolbg)
    checkbutton .html.options -text "Layout options" -variable _html(options) \
	-command html_ToggleOptions -anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    frame .html.advanced -bg $_color(toolbg)
    checkbutton .html.advanced.scrollbar -text "Scrollbars" -variable _html(scrollbar) \
	-anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    scale .html.advanced.size -label {Focus Page Size} -from 4 -to 20 \
	-command "html_SetFontSize $PAD" -orient horizontal \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .html.advanced.size set $_html(focus_size)
    scale .html.advanced.speed -label {Animation Speed (ms)} -from 0 -to 1000 \
	-command "html_SetAnimationSpeed $PAD" -orient horizontal \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .html.advanced.speed set $_html(animation_time)
    scale .html.advanced.foci -label {Number of foci} -from 0 -to 10 \
	-command "html_SetNumFoci $PAD" -orient horizontal \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .html.advanced.foci set $_html(foci)
    scale .html.advanced.propogation -label {Focus propogation} -from 0 -to 10 \
	-command "html_SetPropogation $PAD" -orient horizontal \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .html.advanced.propogation set $_html(focus_prop)
    scale .html.advanced.focusmag -label {Focus magnification} -from 1 -to 20 \
	-command "html_SetFocusMag $PAD" -orient horizontal \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .html.advanced.focusmag set $_html(focus_mag)
    scale .html.advanced.spacing -label {Layout spacing} -from 0 -to 100 \
	-command "html_SetSpacing $PAD" -orient horizontal \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    .html.advanced.spacing set $_html(spacing)
    frame .html.optionend -height 2 -bg $_color(toolbg)

    button .html.close -text "Close"  -command "destroy .html" -anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    
    # pack the items 

    pack .html.label    -expand t -fill x
    pack .html.text     -expand t -fill x
    pack .html.view1    -expand t -fill x
    pack .html.view2    -expand t -fill x
    pack .html.view3    -expand t -fill x
    pack .html.view4    -expand t -fill x

    pack .html.optionstart          -expand t -fill x
    pack .html.options              -expand t -fill x -pady 5
    pack .html.advanced.scrollbar   -expand t -fill x
    pack .html.advanced.size        -expand t -fill x
    pack .html.advanced.speed       -expand t -fill x
    pack .html.advanced.foci        -expand t -fill x
    pack .html.advanced.propogation -expand t -fill x
    pack .html.advanced.focusmag    -expand t -fill x
    pack .html.advanced.spacing     -expand t -fill x
    pack .html.optionend            -expand t -fill x -pady 3

    pack .html.delete   -fill x -pady 3
    pack .html.follow   -fill x -pady 3
    pack .html.followr  -fill x -pady 3
    pack .html.close    -fill x -pady 3

    pad_ManageWindow .html 0 1
    wm title .html "Web Pages"
}

#
# Setup recursive link following
#
proc html_SetupFollowAllLinks {PAD html {recursive 0} {recursion_level 0}} {
    global _html

    if {$_html(follow_all_links) == 0} {
	set _html(follow_all_links) 1
	if {[winfo exists .html.followr]} {
	    .html.followr config -text "Stop following links" -fg red
	}
	html_FollowAllLinks $PAD $html $recursive $recursion_level
	if {[winfo exists .html.followr]} {
	    .html.followr config -text "Follow all links recursively" -fg black
	}
    } else {
	set _html(follow_all_links) 0
	if {[winfo exists .html.followr]} {
	    .html.followr config -text "Follow all links recursively" -fg black
	}
    }
}

#
# Toggle the visibility of the layout options
#
proc html_ToggleOptions {} {
    global _html

    if {$_html(options)} {
	pack .html.advanced -expand t -fill both -before .html.optionend
    } else {
	pack forget .html.advanced
    }
}

#
# Set view type
#
proc html_SetViewMode {PAD} {
    global _html

					# Leaving this mode
    switch -exact $_html(viewmode) {
	camera {
	    $PAD delete camera
	    $PAD delete camera_portal
	    html_SetNumFoci $PAD 1
	}
    }
					# Entering this mode
    set _html(viewmode) $_html(new_viewmode)
    switch -exact $_html(viewmode) {
	camera {
	    html_SetNumFoci $PAD 0
	    html_CreateCamera $PAD
	}
	default {
	    html_SetFocus $PAD $_html(focus)
	}
    }

    html_Layout $PAD $_html(focus)
}

#
# Set font size withn page is focused
#
proc html_SetFontSize {PAD value} {
    global _html

    set _html(focus_size) $value

    if {[$PAD find withtag $_html(focus)] != ""} {
	set view [html_CalculateBBoxView $PAD $_html(focus) [$PAD bbox $_html(focus)]]
	eval $PAD moveto $view
    }
}

#
# Set animation speed
#
proc html_SetAnimationSpeed {PAD value} {
    global _html

    $PAD tree setanimatespeed [html_GetItem $PAD $_html(root)] $value
}

#
# Set the number of foci
#
proc html_SetNumFoci {PAD value} {
    global _html

    if {$value == $_html(foci)} {
	return
    }
					# If number of foci decreases, then
					# unfocus those foci
    for {set i $value} {$i < $_html(foci)} {incr i} {
	set num $i
	if {$num == 0} {
	    set num ""
	}
	if {[info exists _html(focus$num)] && [$PAD tree isnode [html_GetItem $PAD $_html(focus$num)]]} {
	    $PAD tree setfocus [html_GetItem $PAD $_html(focus$num)] 0.0 $_html(focus_prop)
	}
    }

    if {[winfo exists .html.advanced.foci]} {
	.html.advanced.foci set $value
    }
    set _html(foci) $value
    $PAD tree layout [html_GetItem $PAD $_html(root)]
}

#
# Set the number of levels that focus size propogates
#
proc html_SetPropogation {PAD value} {
    global _html

					# Catch since focus might not be defined yet
    catch {$PAD tree setfocus [html_GetItem $PAD $_html(focus)] 0.0 $_html(focus_prop)}

    set _html(focus_prop) $value
    html_SetFocus $PAD $_html(focus)
    $PAD tree layout [html_GetItem $PAD $_html(root)]
}

#
# Set the size of focused pages relative to unfocused ones
#
proc html_SetFocusMag {PAD value} {
    global _html

    $PAD tree setfocusmag [html_GetItem $PAD $_html(root)] $value
    $PAD tree layout [html_GetItem $PAD $_html(root)]
}

#
# Set the spacing between links
#
proc html_SetSpacing {PAD value} {
    global _html

    $PAD tree setspacing [html_GetItem $PAD $_html(root)] $value
    $PAD tree layout [html_GetItem $PAD $_html(root)]
}

######################################################################################
#
# Set up event bindings to make HTML active, follow links, etc.
#
######################################################################################

proc bindHTML {PAD} {
    global _html
				#
				# Don't write out html nodes or connecting links.
				#
    $PAD bind html <Write> {set Pad_Write 0; break}
    $PAD bind treelink <Write> {set Pad_Write 0; break}
    $PAD bind treeroot <Write> {set Pad_Write 0; break}

    ######
    # Events on HTML page
    ######

    $PAD bind html <Run-ButtonPress-1> {
	panEvent %P %W "press" %O %s %x %y %i %j %l
	break
    }

    $PAD bind html <Run-B1-Motion> {
	panEvent %P %W "motion" %O %s %x %y %i %j %l
	break
    }

    $PAD bind html <Run-ButtonRelease-1> {
	panEvent %P %W "release" %O %s %x %y %i %j %l
	if {$_pan(started) == 0} {
	    if {%s & $_modifier(Shift)} {
				# Shift key pressed
		html_Parent %P
	    } else {
		set item [html_GetItem %P %O]
		if {[%P tree getfocus $item] != 0} {
				# If a page that currently has the focus is clicked on,
				# remove the focus from it, and relayout pages so this page becomes centered
		    %P tree setfocus $item 0.0 0.0
		    set root $_html(root) 
		    set root_item [html_GetItem %P $root]
		    %P tree computelayout $root_item
		    set bbox [%P tree getlayoutbbox $item]
		    set view "[bbcenter $bbox] [%P getzoom]"
		    %P tree animatelayout $root_item -view $view
		} else {
				# If a page that does not have the focus is clicked on,
				# then give it the focus
		    html_ActivatePage %P %O
		}
	    }
	}
	break
    }

    ######
    # Events on HTML anchors
    ######

    $PAD bind htmlanchor <Run-Enter> {html_EnterAnchor %P %O %l; break}
    $PAD bind htmlanchor <Run-Leave> {html_LeaveAnchor %P %O %l; break}

    $PAD bind htmlanchor <Run-ButtonPress-1> {
	panEvent %P %W "press" %O %s %x %y %i %j %l
	break
    }

    $PAD bind htmlanchor <Run-B1-Motion> {
	panEvent %P %W "motion" %O %s %x %y %i %j %l
	break
    }

    $PAD bind htmlanchor <Run-ButtonRelease-1> {
	panEvent %P %W "release" %O %s %x %y %i %j %l
	if {$_pan(started) == 0} {
	    html_ActivateAnchor %P %O %s %U %V %l
	}
	break
    }

    ######
    # Event on deleting a page
    ######

		# When a html page is deleted, promote
		# its children
    $PAD bind html <Delete> {
	set url [%W itemconfigure %O -url]
	unset _html(url.$url)
	after 0 {catch {%P tree layout [html_GetItem %P $_html(root)]}}
    }
}

#
# Set the cursor to be special when over an anchor
#
proc html_SetAnchorCursor {PAD} {
    global _pad
   
    $PAD config -cursor [pad_pointer_cursor $PAD]
   
}

#
# Set the cursor to be regular when not over an anchor
#
proc html_SetDefaultCursor {PAD} {
    global _pad

    pad_set_cursor $PAD "@$_pad(PadBitmaps)/hand.xbm $_pad(PadBitmaps)/handmask.xbm black white" "hand"
    
}

#
# Process ButtonRelease on an html page
# Set focus to page, and add page to traversal history
#
proc html_ActivatePage {PAD html {index ""}} {
    global _html

    html_SetFocus $PAD $html $index
    html_AddToHistory $PAD $html
}

#
# Process event when cursor goes over and leaves an anchor
#
proc html_EnterAnchor {PAD anchor portals} {
    global _html

    set orig_url [$PAD ic $anchor -url]
    if {$orig_url == ""} {
	return
    }
    set html [$PAD ic $anchor -html]
    set size [eval $PAD getsize $html $portals]
    if {$size < $_html(min_size)} {
			# HTML too small, so don't do anything
	return
    }

    html_SetAnchorCursor $PAD

			# Check to see if it's already in the world
    $PAD itemconfig $anchor -state "active"
    set url_index [html_ParseURL $orig_url]
    set url [lindex $url_index 0]
    set index [lindex $url_index 1]

    if {[info exists _html(url.$url)]} {
	html_HighlightPage $PAD $_html(url.$url)
    }
    if {[$PAD ic $anchor -ismap]} {
	set label "Image Map: $url"
    } else {
	set label $orig_url
    }
    updateStatusText $PAD $label 1
}

proc html_LeaveAnchor {PAD anchor portals} {
    global _html

    set url [$PAD ic $anchor -url]
    if {$url == ""} {
	return
    }
    
    html_SetDefaultCursor $PAD

    set url_index [html_ParseURL $url]
    set url [lindex $url_index 0]
    set index [lindex $url_index 1]

    if {[info exists _html(url.$url)]} {
	html_UnHighlightPage $PAD $_html(url.$url)
	$PAD itemconfig $anchor -state "visited"
    } else {
	$PAD itemconfig $anchor -state "unvisited"
    }
    updateStatusText $PAD "" 0
}

#
# Process ButtonRelease on an htmlanchor
#   Go to parent if shift key is pressed.
#   Don't follow links on pages that are too small
#   Handle regular links, and image maps
#
proc html_ActivateAnchor {PAD anchor state U V portals {donescript ""}} {
    global _html _modifier

    set new_html ""
    set url [$PAD ic $anchor -url]
    if {$url == ""} {
	return ""
    }
    set html [$PAD ic $anchor -html]
    set item [html_GetItem $PAD $html]

    if {$state & $_modifier(Shift)} {
				# Shift key pressed
	set parent [html_GetHTML $PAD [$PAD tree getparent $item]]
	html_ActivatePage $PAD $parent
    } else {
	set ismap [$PAD ic $anchor -ismap]
	set size [eval $PAD getsize $item $portals]
	if {$size < $_html(min_size)} {
					# HTML too small, so select it instead of following link
	    html_ActivatePage $PAD $html
					# Anchor size has now changed, so call enter anchor again
					# in case cursor should change too.
	    html_EnterAnchor $PAD $anchor ""
	    return
	}
	
	if {$ismap} {
	    set imagetoken [$PAD ic $anchor -image]
	    set dim [$PAD info image getdim $imagetoken]
	    set x [expr int($U)]
	    set y [expr int([lindex $dim 1] - $V)]
	    append url "?$x,$y"
	    html_FollowImageMap $PAD $html $anchor $url
	} else {
	    set url_index [html_ParseURL $url]
	    set url [lindex $url_index 0]
	    set index [lindex $url_index 1]

	    set new_html [html_FollowLink $PAD $html $anchor $url $index $donescript]
	}
    }

    return $new_html
}

#
# Given an url, parse it for an index.
# Return a list with two elements, the new url, and the index.
#
proc html_ParseURL {url} {
    set tail [file tail $url]
    set index ""
    if {[string first "#" $tail] != -1} {	
					# This is an index anchor
	set wordstart [string wordstart $url 1000]
	set index [string range $url $wordstart end]
	set url [string range $url 0 [expr $wordstart -2]]
    }

    return "$url $index"
}

#
# Highlight and unhilight a page and the link going to it.
#
proc html_HighlightPage {PAD html} {
    global _color

    set item [html_GetItem $PAD $html]

    $PAD ic $html -border $_color(htmlactiveborder)
    set link [$PAD tree getlink $item]
    $PAD ic $link -pen $_color(htmlactivelink)
}

proc html_UnHighlightPage {PAD html} {
    global _color

    set item [html_GetItem $PAD $html]

    $PAD ic $html -border $_color(htmlborder)
    set link [$PAD tree getlink $item]
    $PAD ic $link -pen $_color(htmllink)
}

######################################################################################
#
# Code to import HTML pages, and maintain tree,
# follow links, define event bindings, etc.
#
######################################################################################

#
# Import a url, and make it a new root of the tree.
# If no HTML master root exists, then create one
#
proc importHtml {PAD url} {
    global _html

    set view [$PAD getview]
    set _html(scale) [expr $_html(magic_scale_factor) / [lindex $view 2]]
					# Create a root for the HTML tree
					# (or reposition it if there are no pages yet)
    html_CreateRoot $PAD $view

					# If URL already loaded, then go to it.
    if {[info exists _html(url.$url)]} {
	html_ActivatePage $PAD $_html(url.$url)
	return 
    }

    set html [html_ImportChild $PAD $url]
    if {$html == ""} {
	updateStatusMsg $PAD "Can't load url: $url"
	return
    }
    set item [html_GetItem $PAD $html]
    $PAD ic $item -z $_html(scale)
    $PAD tree setscale $item $_html(scale)
    $PAD tree addnode $item [html_GetItem $PAD $_html(root)]
    html_ActivatePage $PAD $html

    return $item
}

#
# Helper routine.  Given a URL, create an html page, and
# add it to the tree.  This node still needs to be sized,
# and put in the proper place within the tree.
#
proc html_ImportChild {PAD url {donescript ""}} {
    global _html _pad _color

    if {($_html(buttons) == 0) && ($_html(buttons_deleted) == 0)} {
	html_CreateButtons $PAD
    }

    set cursor [lindex [$PAD configure -cursor] 4]
 
    pad_set_cursor $PAD "@$_pad(PadBitmaps)/hourglass.xbm $_pad(PadBitmaps)/hourglassmask.xbm black white" "hourglass"
    
    set html [html_CreatePage $PAD $url $donescript]
    $PAD config -cursor $cursor
    if {$html == ""} {
	return
    }
    
    set _html(url.$url) $html
    set _html($html.updated) 0
		
    set item [html_GetItem $PAD $html]
    $PAD tree create $item
    catch {set link [$PAD tree getlink $item]}
    if {$link != -1} {
      $PAD ic $link -pen $_color(htmllink)
    }
					
    return $html
}

#
# Given a URL, create an html page with or without a scrollbar
#
proc html_CreatePage {PAD url donescript} {
    global _html _pad _color

    if {[catch {set html [$PAD create html -url $url -borderwidth 1.5 \
			      -border $_color(htmlborder) \
			      -maxsize -1 -tags "html group" -font $_pad(Font) \
			      -updatescript "html_UrlUpdate $PAD" \
			      -donescript "html_UrlDone $PAD \"$donescript\"" \
			      -errorscript "html_UrlError $PAD"]}]} {
	return ""
    }

    if {$_html(scrollbar)} {
	$PAD ic $html -anchor nw
	$PAD create group -members $html
    }

    return $html
}

#
# Some HTML items are group members.
# This returns the group the html page is a member of,
# or the html page if it is not a member of a group.
#
proc html_GetItem {PAD html} {
    set group [$PAD getgroup $html]
    while {$group != ""} {
	set html $group
	set group [$PAD getgroup $html]
    }

    return $html
}

#
# Since some HTML items are group members, but the tree
# nodes are the groups themselves, it is often necessary
# to find the actual HTML item given the group member.
# This function returns that HTML item.
#
proc html_GetHTML {PAD item} {
    set type [$PAD type $item]
    if {$type == "html"} {
	return $item
    }
    if {($type == "group") || ($type == "panel")} {
	foreach member [$PAD ic $item -members] {
	    return [html_GetHTML $PAD $member]
	}
    }
    return ""
}

#
# This gets called whenever an HTML scrollbar gets modified
#
proc html_ScrollCallback {PAD sb html value} {
    global _html

    set height [$PAD ic $html -height]
    $PAD ic $html -place "0 [expr $_html(page_size) + $height - $value] 1"
}

#
# This gets called when an HTML page is completed loading in
# to update the scrollbar properties
#
proc html_UpdateScroll {PAD sb html} {
    global _html

    set height [$PAD ic $html -height]
    $PAD ic $sb -to $height -value $height -_html(page_size) [expr ($height - $_html(page_size)) * $_html(page_size) / $height]
}
#
# Create a new root for the HTML tree.
# Or, if one already exists and it has no children, reposition it
# to the current view.
#
proc html_CreateRoot {PAD view} {
    global _html

    if {[info exists _html(root)]} {
					# Root already exists.
	if {[$PAD tree getchildren [html_GetItem $PAD $_html(root)]] == ""} {
					# If no pages loaded, then reposition root
					# to current view so new pages appear there.
	    $PAD ic $_html(root) -place $view
	}
    } else {
	set _html(root) [$PAD tree createroot]
	$PAD ic $_html(root) -place $view

					# Want to know when HTML root is deleted
					# so we free up resources.
	$PAD bind $_html(root) <Delete> {
	    unset _html(root)
	    set _html(focus) -1
	    set _html(focus1) -1
	}
    }
}

#
# This gets called when the window changes size.
#   Relayout buttons.
#   Fix portal if exists
#
proc Pad_ConfigureHTML {PAD} {
    global _html

					# First do buttons
    set rect [$PAD find withtag htmlbuttonbgnd]
    set back [$PAD find withtag htmlback]
    set forward [$PAD find withtag htmlforward]
    set showall [$PAD find withtag htmlshowall]
    set close [$PAD find withtag htmlclose]

    if {($rect == "") || ($close == "") || ($back == "") || ($forward == "") || ($showall == "")} {
	return
    }

    $PAD layout position 0 1.0 -ref 1 0 1 $rect
    $PAD layout position 0.01 0.9 -ref $rect 0 1 $back
    $PAD layout position 1.05 1 -ref $back 0 1 $forward
    $PAD layout position 1.05 1 -ref $forward 0 1 $showall
    $PAD layout position 1.05 1 -ref $showall 0 1 $close

					# Then do portals
    html_PositionCameraPortal $PAD
    html_CameraFocus $PAD $_html(focus)
}

#
# Create forward/back buttons for use in navigating through HTML pages
#
proc html_CreateButtons {PAD} {
    global _html _pad

					# Delete any existing buttons
    $PAD delete htmlback
    $PAD delete htmlforward
    $PAD delete htmlshowall
    $PAD delete htmlclose
    $PAD delete htmlbuttonbgnd

					# Erase history
    set _html(history) ""
    set _html(history_index) -1

					# Create new buttons
    set zoom [$PAD getzoom]
    set back [$PAD create button -text "Back" -command "html_Back $PAD" -layer status \
		  -z [expr 1.2 / $zoom] -font "Times-10" -sticky 1]
    set forward [$PAD create button -text "Forward" -command "html_Forward $PAD" -layer status \
		     -z [expr 1.2 / $zoom] -font "Times-10" -sticky 1]
    set showall [$PAD create button -text "Show All" -command "html_ShowAll $PAD" -layer status \
		     -z [expr 1.2 / $zoom] -font "Times-10" -sticky 1]
    set close [$PAD create button -text "Close" -command "html_DeleteButtons $PAD" -layer status \
		  -z [expr 1.2 / $zoom] -font "Times-10" -sticky 1]

					# Add tags with 'addtag' so default tags aren't replaced
    $PAD addtag htmlback $back
    $PAD addtag htmlforward $forward
    $PAD addtag htmlshowall $showall
    $PAD addtag htmlclose $close
    $PAD addtag htmlbutton $back
    $PAD addtag htmlbutton $forward
    $PAD addtag htmlbutton $showall
    $PAD addtag htmlbutton $close

					# Position them in the upper-left corner of the window
    $PAD layout position 0 1 -ref 1 0 1 $back
    $PAD layout position 1 1 -ref $back 0 1 $forward
    $PAD layout position 1 1 -ref $forward 0 1 $showall
    $PAD layout position 1 1 -ref $showall 0 1 $close

    set rect [eval $PAD create rectangle [$PAD bbox htmlbutton] -fill white \
		  -penwidth 0 -pen black -layer status -sticky 1]
    $PAD addtag htmlbuttonbgnd $rect
    $PAD addtag htmlbutton $rect

    $PAD raise $back
    $PAD raise $forward
    $PAD raise $showall
    $PAD raise $close

    set coords [$PAD coords $rect]
    set x0 [lindex $coords 0]
    set y0 [lindex $coords 1]
    set x1 [lindex $coords 2]
    set y1 [lindex $coords 3]
    $PAD coords $rect $x0 [expr $y0 - 0.1*($y1-$y0)] [expr $x1 + 0.06*($x1-$x0)] [expr $y1 + 0.1*($y1-$y0)]

    Pad_ConfigureHTML $PAD

					# Make <Delete> event bindings to reset flag when buttons are deleted
    $PAD bind htmlbutton <Delete> {set _html(buttons) 0}

    set _html(buttons) 1
}

proc html_DeleteButtons {PAD} {
    global _html

    set _html(buttons_deleted) 1
    pad_fade_delete $PAD "htmlbutton htmlbuttonbgnd"
}

#
# Routine to execute to go to the parent of this page in the hierarchy
# There are two cases here.
#   * The parent is the previous node.  In this case, we call previous
#     so we change the index to that node.
#   * The parent is not the previous node.  In this case, we
#     add the parent to the history list so it is like a new node.
#
proc html_Parent {PAD} {
    global _html

    set parent [html_GetHTML $PAD [$PAD tree getparent [html_GetItem $PAD $_html(focus)]]]
    set previous ""
    if {$_html(history_index) >= 1} {
	set index [expr $_html(history_index) - 1]
	set previous [lindex $_html(history) $index]
    }
    if {$parent == $previous} {
	html_Back $PAD
    } else {
	html_AddToHistory $PAD $parent
	html_SetFocus $PAD $parent
    }
}

#
# Routine to execute to go to the back page visited
# Change the index to the page that we go to.
#
proc html_Back {PAD} {
    global _html

    if {$_html(history_index) >= 1} {
	set index [expr $_html(history_index) - 1]
	set html [lindex $_html(history) $index]
	set _html(history_index) $index
	
	html_SetFocus $PAD $html
    }
}

#
# Routine to execute to go forward one page
# Change the index to the page that we go to.
#
proc html_Forward {PAD} {
    global _html

    set len [llength $_html(history)]
    if {$_html(history_index) < [expr $len - 1]} {
	set index [expr $_html(history_index) + 1]
	set html [lindex $_html(history) $index]
	set _html(history_index) $index
	
	html_SetFocus $PAD $html
    }
}

#
# Routine to execute to change view so all html pages are visible
#
proc html_ShowAll {PAD} {
    global _html

    eval $PAD centerbbox [eval $PAD bbox [$PAD find withtag treenode]] $_html(animation_time) .5 .5 .85
}

#
# Add the specified page to the history of page traversal.
# This history can be used by other navigation mechanism
# (such as the forward/back buttons).
#
proc html_AddToHistory {PAD html} {
    global _html

					# Add page to the end of the history,
					# whether it is already in the history or not.
					# as long as we don't put the same page on twice in a row
    set index $_html(history_index)
    set _html(history) [lrange $_html(history) 0 $index]
    if {$html != [lindex $_html(history) end]} {
	lappend _html(history) $html
    }
    set _html(history_index) [expr [llength $_html(history)] - 1]
}

#
# Callback when more of the HTML page has loaded
# Layout node when first update comes (but not for ensuing images)
#
proc html_UrlUpdate {PAD html} {
    global _html

    if {$_html($html.updated) == 0} {
					# Page contents came in, so layout the tree
					# with new info, but don't lay it out again
					# until all of the images have been loaded.
	set _html($html.updated) 1
	html_Layout $PAD $html update
    }
}


#
# Callback when HTML page has finished loading
# Do a final tree layout
#
proc html_UrlDone {PAD donescript html} {
    global _html

    if {$_html(scrollbar)} {
	set group [$PAD getgroup $html]
	set gbbox [$PAD bbox $group]
	$PAD removeg -notransform $html
	$PAD ic $html -place "0 $_html(page_size) 1"
	set bbox [$PAD bbox $html]
	set width [bbwidth $bbox]
	set height [bbheight $bbox]

					# If page is too big, then create scrollbars
	if {($height > $_html(page_size))} {
	    set sb [$PAD create scrollbar -orientation vertical -width 20 -height [expr 8 * $_html(page_size)] \
			-from $_html(page_size) -to $height -value $height -anchor sw -place "0 0 0.125" \
			-pagesize [expr ($height - $_html(page_size)) * $_html(page_size) / $height]]
	    
	    set panel [$PAD create panel -width $width -height $_html(page_size)]
	    $PAD ic $sb -command "html_ScrollCallback $PAD $sb $html"
	    $PAD layout position 1 0 -ref $panel 0 0 $sb
	    $PAD addg $html $panel
	    $PAD addg -notransform $panel $group
	    $PAD addg -notransform $sb $group
	} else {
	    $PAD addg -notransform $html $group
	}
	$PAD layout position 0 1 -bbox $gbbox 0 1 $group
    }

    html_Layout $PAD $html 
    unset _html($html.updated)
    if {$donescript != ""} {
	uplevel #0 "$donescript $html"
    }
}


#
# Callback if there is an error fetching HTML page.
#
proc html_UrlError {PAD html} {
    global _html

    set url [$PAD ic $html -url]
    updateStatusMsg $PAD "Link not available: $url"

    set item [html_GetItem $PAD $html]
    $PAD tree delete $item
    $PAD tree layout [html_GetItem $PAD $_html(root)]
}

#
# Clicked on an image map. 
# Image maps actually return an intermediary html file that
# contains the actual destination.  This extra step is necessary
# in order for relative pathnames to work properly.
#
# So, fetch the intermediary file, and when it arrives, parse it,
# and then get the real one.
#
proc html_FollowImageMap {PAD html anchor url} {
    $PAD urlfetch $url \
	    -updatescript "html_ImageMapUpdate $PAD $html $anchor" \
	    -donescript   "html_ImageMapDone $PAD $html $anchor $url" \
            -errorscript  "html_ImageMapError $PAD $html $anchor $url"

    return ""
}

#
# Image map coming in.  Accumulate data
#
proc html_ImageMapUpdate {PAD html anchor token} {
    global _html

    append _html($anchor.imagemap) [$PAD urlfetch $token]
}

#
# Image map received.
# Parse file to find resulting anchor and follow link
#
proc html_ImageMapDone {PAD html anchor requested_url token} {
    global _html
    
    if {[info exists _html($anchor.imagemap)]} {
	set result [regexp \".*\" $_html($anchor.imagemap) url]
    } else {
	set result 0
    }
    if {$result} {
	set url [string trim $url "\""]
	updateStatusText $PAD $url 1
	html_FollowLink $PAD $html $anchor $url ""
    } else {
	updateStatusMsg $PAD "Link not available: $requested_url"
    }
			# May not have been defined if there was error loading it
    catch {unset _html($anchor.imagemap)}
}

proc html_ImageMapError {PAD html anchor url token} {
    updateStatusMsg $PAD "Link not available: $url"
}

#
# Load in a new page as a result of following a link,
# and add the page to the tree of pages as a descendent of <html>.
# If the page is already loaded, go to that page instead.
#
proc html_FollowLink {PAD html anchor url index {donescript ""}} {
    global _html

					# If URL already loaded, then go to it.
    if {[info exists _html(url.$url)]} {
	html_ActivatePage $PAD $_html(url.$url) $index
	return 
    }

    $PAD itemconfig $anchor -state visited
    set new_html [html_ImportChild $PAD $url $donescript]
    if {$new_html == ""} {
	updateStatusMsg $PAD "Can't load url: $url"
	return
    }
    set item [html_GetItem $PAD $html]
    set new_item [html_GetItem $PAD $new_html]
					# Position new page by anchor it came from
    set anchor_bbox [$PAD bbox $anchor]
    set x [lindex $anchor_bbox 0]
    set y [expr 0.5 * ([lindex $anchor_bbox 1] + [lindex $anchor_bbox 3])]
    set z $_html(scale)
    $PAD ic $new_item -place "$x $y $z"
    
    $PAD tree setscale $new_item $_html(scale)
    $PAD tree addnode $new_item $item 
    html_FixLinkOrder $PAD $new_html
    html_ActivatePage $PAD $new_html $index

    return $new_html
}

#
# Just added <new_html> to be a child of <html>.
# However, we'd like the layout of the child within the tree
# to be in the same relative order as the links within
# the parent - so move it if necessary.
#
proc html_FixLinkOrder {PAD new_html} {
    global _html

    set new_item [html_GetItem $PAD $new_html]
    set item [$PAD tree getparent $new_item]
    set html [html_GetHTML $PAD $item]
        # Set up some useful variables
    set new_url [$PAD ic $new_html -url]

        # First go through the parents anchors, and
        # find the anchor just before this one
    set anchors [$PAD ic $html -htmlanchors]
    set prev_anchors ""
    foreach anchor $anchors {
	set url [$PAD ic $anchor -url]
	if {$url == $new_url} {
	    break
	}
	set prev_anchors "$anchor $prev_anchors"
    }

	# Then back up through the anchors, and find
	# the first one that was followed.
	# Make sure link anchor goes to is actually a child
        # of this page.  It might not be if the page was
        # accessed from somewhere else.
    set prev_html ""
    set prev_item ""
    set found 0
    set children [$PAD tree getchildren $item]
    foreach anchor $prev_anchors {
	set url [$PAD ic $anchor -url]
	if {[info exists _html(url.$url)]} {
	    
	    set prev_html $_html(url.$url)
	    set prev_item [html_GetItem $PAD $prev_html]
	    if {[lmember $children $prev_item]} {
		set found 1
		break
	    }
	}
    }
    if {$found == 0} {
	set prev_item ""
    }

	# Finally, repair link order by moving new page
	# just after the previous link that was followed.
    if {$prev_item == ""} {
	$PAD tree lower $new_item
    } else {
	$PAD tree raise $new_item $prev_item
    }
}

#
# Create a camera for the HTML pages, and associated portal
#
proc html_CreateCamera {PAD} {
    global env _html _color

					# Change the view to see all the pages
    if {[$PAD find withtag html] != ""} {
	set bbox [$PAD bbox html]
	set ctr [bbcenter $bbox]
	set window_dim [max [winfo width $PAD] [winfo height $PAD]]
	set html_dim [max [bbwidth $bbox] [bbheight $bbox]]
	set zoom [expr 0.3 * $window_dim / $html_dim]
	set xview [expr [lindex $ctr 0] - (0.3 * $window_dim / $zoom)]
	set yview [lindex $ctr 1]
	$PAD moveto $xview $yview $zoom $_html(animation_time)
    }

					# Create the camera portal
    set zoom [$PAD getzoom]
    $PAD create portal 0 0 0 0 -title "Web Camera" -tags "camera_portal" \
	-visiblelayers "all - camera - status" \
	-fill $_color(htmlcamera) -font "Times-10"
    $PAD ic camera_portal -borderwidth [expr 7.0 / $zoom]
    html_PositionCameraPortal $PAD

					# Create the camera
    set bgnd "\#505050"
    set fgnd "\#707070"
    set Pad_ObjectList ""
    $PAD create polygon -960.354 -311.303 -938.597 -303.9 -936.372 -307.104 -934.194 -312.019 -955.605 -325.328 -970.172 -327.346  -events 1 -layer main -lock 0 -place "-970.99 -262.168 1.25669" -tags "item camera_comp" -fill $bgnd -pen $bgnd -penwidth 6.21325
    $PAD create polygon -954.036 -333.793 -942.516 -354.372 -932.311 -355.877 -897.282 -333.793 -894.621 -331.534 -891.074 -321.747 -900.607 -299.41 -911.336 -296.191 -914.574 -296.65 -950.266 -320.994  -events 1 -layer main -lock 0 -place "-987.579 -272.841 1" -tags "item camera_comp" -fill $bgnd -pen $bgnd -penwidth 3.31396
    $PAD create polygon -955.959 -345.566 -943.609 -344.523 -924.019 -334.093 -911.362 -324.358 -907.445 -317.752 -903.85 -323.85 -913.472 -331.311 -921.608 -337.222 -935.471 -344.175 -948.039 -347.091  -events 1 -layer main -lock 0 -place "-986.766 -272.889 1" -tags "item camera_comp" -fill $fgnd -pen $fgnd -penwidth 6.21324
    $PAD create polygon -998.05 -245.016 -993.771 -242.054 -992.126 -244.687 -996.405 -247.649  -events 1 -layer main -lock 0 -place "-977.643 -257.094 1" -tags "item camera_comp" -fill $fgnd -pen $fgnd -penwidth 3.31396
    $PAD create polygon -998.05 -245.016 -993.771 -242.054 -992.126 -244.687 -996.405 -247.649  -events 1 -layer main -lock 0 -place "-988.176 -263.678 1" -tags "item camera_comp" -fill $fgnd -pen $fgnd -penwidth 3.31396
    $PAD create polygon -998.05 -245.016 -993.771 -242.054 -992.126 -244.687 -996.405 -247.649  -events 1 -layer main -lock 0 -place "-997.909 -269.984 1" -tags "item camera_comp" -fill $fgnd -pen $fgnd -penwidth 3.31396
    $PAD create spline -963.509 -246.663 -958.46 -250.227 -955.424 -250.957 -954.176 -257.896 -953.685 -260.627 -954.637 -263.427 -954.868 -266.192  -layer main -lock 0 -place "-958.843 -256.427 1" -tags "item camera_comp" -arrow none -arrowshape "1.382600 1.728250 0.518476" -capstyle round -joinstyle miter -pen $fgnd -penwidth 0.864126
    set camera [$PAD create group -members [$PAD find withtag camera_comp] -tags "camera" -layer camera -anchor ne]
    html_CameraFocus $PAD $_html(focus)

					# Make event bindings on camera to drag it in Run mode
    $PAD bind camera_comp <Run-ButtonPress-1> {
	break
    }
    $PAD bind camera_comp <Run-B1-Motion> {
	set z [expr 1.0 / [%W getzoom]]
	%W ic camera -place "%i %j $z"
	%W raise camera
	break
    }
    $PAD bind camera_comp <Run-ButtonRelease-1> {
	break
    }

					# Track motion of camera in portal
    $PAD bind camera <Modify> {html_CameraPortalFollow %W}
}

#
# Position and size the camera portal to fill up the left
# side of the window.
#
proc html_PositionCameraPortal {PAD} {
    if {[$PAD find withtag camera_portal] == ""} {
	return
    }

    set zoom [$PAD getzoom]

    set bbox [$PAD bbox 1]
    set x1 [lindex $bbox 0]
    set y1 [expr [lindex $bbox 1] + 20.0 / $zoom]
    set x2 [lindex $bbox 2]
    set y2 [lindex $bbox 3]
    set x2 [expr $x1 + 0.6 * ($x2 - $x1)]

    $PAD ic camera_portal -sticky 0
    $PAD coords camera_portal $x1 $y1 $x2 $y2
    $PAD ic camera_portal -sticky 1
}

#
# Update the view of the camera portal to follow the camera
#
proc html_CameraPortalFollow {PAD} {
    set place [$PAD ic camera -place]
    set x [lindex $place 0]
    set y [lindex $place 1]
    set z [lindex [$PAD ic camera_portal -view] 2]
    $PAD ic camera_portal -view "$x $y $z"
}

#
# Change the focus of the camera to the specified page
#
proc html_CameraFocus {PAD html} {
    global _html

				# Can't focus if no camera
    if {[$PAD find withtag camera] == ""} {
	return
    }
				# Can't focus if no html pages
    if {[$PAD find withtag html] == ""} {
	return
    }

    set camera_portal [$PAD find withtag camera_portal]
    set view [html_CalculateBBoxViewPortal $PAD $html $camera_portal]
    eval $PAD moveto $view $_html(animation_time) $camera_portal
    set zoom [$PAD getzoom]
    $PAD ic camera -place "[lindex $view 0] [lindex $view 1] [expr 1.0 / $zoom]"
    $PAD raise camera
}

#
# Set the focus to the specified html page.
# Keep a history of n foci (specified by _html(foci))
# Pass index on to html_Layout.
#
proc html_SetFocus {PAD html {index ""}} {
    global _html

    set item [html_GetItem $PAD $html]
					# Make sure node is valid
    if {[$PAD tree isnode $item] == 0} {
	return
    }
    if {[$PAD tree getroot $item] == $item} {
	return
    }
					# Deal with zero focus as a special case
    if {$_html(foci) == 0} {
	set _html(focus) $html
	html_Layout $PAD $html "" $index
	return
    }
			                # Order is important because of focus propogation
					# Catch error because _html(focus) may not be defined
					# on first call.
    set foci [expr $_html(foci) - 1]
    if {$foci == 0} {
	set foci ""
    }
    catch {$PAD tree setfocus [html_GetItem $PAD $_html(focus$foci)] 0.0 $_html(focus_prop)}
    for {set i $foci} {$i > 0} {incr i -1} {
	set i1 [expr $i - 1]
	if {$i1 == 0} {
	    set i1 ""
	}
	if {[catch {set _html(focus$i) $_html(focus$i1)}]} {
	    set _html(focus$i) -1
	}
	if {$i == 1} {
	    set focus_value 0.7
	} else {
	    set focus_value [expr 1.0 / $i]
	}
	catch {$PAD tree setfocus [html_GetItem $PAD $_html(focus$i)] $focus_value $_html(focus_prop)}
    }
    set _html(focus1) $_html(focus)
    set _html(focus) $html
    $PAD tree setfocus $item 1.0 $_html(focus_prop)
    
    html_Layout $PAD $html "" $index
}

#
# Layout the HTML subtree starting at the node specified by <html>
# Use the current viewmode to determine how to change the view while
# layout out the tree.
#
# If <status> is "update", then this call is the result of a page being
# updated, rather than being loaded in initially.  If <status> is "done",
# then this call is the result of a page being finished loading.
#
# If index is specified, then go to the specified index
# within the page (if it exists).
#
proc html_Layout {PAD html {status ""} {index ""}} {
    global _html

					# Check to be sure there is a current focus
    if {$_html(focus) == -1} {
	return
    }
    set item [html_GetItem $PAD $html]
    set root $_html(root) 
    set root_item [html_GetItem $PAD $root]
    switch -exact $_html(viewmode) {
	"camera" {
	    $PAD tree layout $root_item
	    html_CameraFocus $PAD $html
	}
	"noviewchange" {
					# View does not change while pages are layed out
	    $PAD tree layout $root_item
	}
	"indirectviewchange" {
					# View zooms out, pages re-layout, and then view zooms in
	    if {$status == ""} {
		set bbox [$PAD bbox $html]
		set focus1 [$PAD find withtag $_html(focus1)]
		if {$focus1 != ""} {
		    set bbox [bbunion $bbox [$PAD bbox $focus1]]
		    eval $PAD centerbbox $bbox $_html(animation_time)
		}
	    }
	    $PAD tree layout $root_item
	    if {$status != "done"} {
		set new_view [html_CalculateBBoxView $PAD $html [$PAD bbox $html] $index]
		eval $PAD moveto $new_view $_html(animation_time)
	    }
	}
	"directviewchange" {
					# View changes as pages are layed out
	    $PAD tree computelayout $root_item
	    set bbox [$PAD tree getlayoutbbox $item]
	    set view [html_CalculateBBoxView $PAD $html $bbox $index]
	    $PAD tree animatelayout $root_item -view $view
	}
    }
}

#
# Calculate the viewpoint so that the specifed object will
# be positioned nicely on the screen when it fills the
# future bbox that is specified.  The definition of "nice fit" is
#   * The object is viewed with a magnification of _html(focus_size)
#   * The object is offset from the top left corner of the window by _html(focus_offset)
#   * If there is an index, than that portion of the page goes to the top-left corner
#
proc html_CalculateBBoxView {PAD html fbbox {index ""}} {
    global _html

    set item [html_GetItem $PAD $html]
    set hbbox [$PAD bbox $item]
    set hbbox_width [bbwidth $hbbox]

    if {$index != ""} {
	foreach anchor [$PAD ic $html -htmlanchors] {
	    set anchorindex [$PAD info htmlanchor getname $anchor]
	    if {$anchorindex == $index} {
		set abbox [$PAD bbox $anchor]
		set ymin [lindex $hbbox 1]
		set ymax [lindex $hbbox 3]
		set y    [lindex $abbox 3]
		set yfrac [expr ($y - $ymin) / ($ymax - $ymin)]

		set fxmin [lindex $fbbox 0]
		set fymin [lindex $fbbox 1]
		set fxmax [lindex $fbbox 2]
		set fymax [lindex $fbbox 3]
		set new_fymax [expr $yfrac * ($fymax - $fymin) + $fymin]
		set fbbox "$fxmin $fymin $fxmax $new_fymax"
		break
	    }
	}
    }

    set fbbox_width [bbwidth $fbbox]
    set hscale [expr [$PAD ic $item -z] * ($fbbox_width / $hbbox_width)]
    set zoom [expr $_html(focus_size) / $hscale]

					# If html buttons are visible, don't overlap them.
    if {$_html(buttons)} {
	set button_bbox [$PAD bbox htmlbutton]
	set button_height [expr 2 * [bbheight $button_bbox] * $zoom]
    } else {
	set button_height 0.0
    }

    set window_width [winfo width $PAD]
    set window_height [expr [winfo height $PAD] - $button_height]
    set offset [expr $_html(focus_offset) / $zoom]

					# This code puts page at the top center
    set xview [expr 0.5 * ([lindex $fbbox 0] + [lindex $fbbox 2])]
    set yview [expr [lindex $fbbox 3] + (-0.5 * $window_height) / $zoom + $offset]

    return "$xview $yview $zoom"
}

#
# Calculate the viewpoint so that the specifed object will
# be positioned nicely on within the portal.  The definition of "nice fit" is
#   * The object is viewed with a magnification of _html(focus_size)
#
proc html_CalculateBBoxViewPortal {PAD html portal} {
    global _html
    
    set item [html_GetItem $PAD $html]
    set hbbox [$PAD bbox $item]
    set hbbox_width [bbwidth $hbbox]

    set pbbox [$PAD coords -objectcoords $portal]
    set size [lindex [$PAD ic $portal -place] 2]

    set hscale [$PAD ic $item -z]
    set zoom [expr $_html(focus_size) / ($hscale * $size)]

    set zoom [expr $zoom / $size]
    set portal_width [bbwidth $pbbox]
    set portal_height [bbheight $pbbox]
    set yoffset 16        ;# Counter portal title

    set xview [expr [lindex $hbbox 0] + (0.5 * $portal_width) / $zoom]
    set yview [expr [lindex $hbbox 3] + (-0.5 * $portal_height + $yoffset) / $zoom]

    return "$xview $yview $zoom"
}

#
# Follow all links from the specified page, avoiding loops.
# If <recursive> is true, then follow links recursievely.
#

proc html_FollowAllLinks {PAD html {recursive 0} {recursion_level 0}} {
    global _html

    if {$recursive} {
	set recurse_cmd "html_FollowAllLinks $PAD"
    } else {
	set recurse_cmd ""
    }

    if {$recursion_level == 0} {
	set min_size $_html(min_size)
	set _html(min_size) 0
    }

    foreach anchor [$PAD ic $html -htmlanchors] {
	set orig_url [$PAD ic $anchor -url]
	set url_index [html_ParseURL $orig_url]
	set url [lindex $url_index 0]
	set index [lindex $url_index 1]
	
	if {$_html(follow_all_links) == 0} {
	    break
	}

	if {![info exists _html(url.$url)] && ![$PAD ic $anchor -ismap]} {
	    set new_html [html_ActivateAnchor $PAD $anchor 0 0 0 "" $recurse_cmd]
	    if {$new_html != ""} {
		html_FollowAllLinks $PAD $new_html [expr $recursion_level + 1]
	    }
	    html_SetFocus $PAD $html
	}
	update
    }

    if {$recursion_level == 0} {
	set _html(min_size) $min_size
    }
}
