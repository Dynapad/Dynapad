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

#
# Create Event Debugger window
#
proc make.debugevent {PAD} {
    global _color _pad geometry _font
    
    set de .debugevent

    if {[winfo exists $de]} {
        raise $de
	return
    }

    toplevel $de -bg $_color(toolbg)

					# Tag List
    label   $de.static_tags_label -text "Tag List" \
	-font $_font(tools) \
	    -bg $_color(toolbg)
    listbox $de.static_tags_list -width 12 -height 5 \
	-font $_font(tools) \
	-bg $_color(toolbg) -selectbackground $_color(toolselbg) \
	-highlightbackground $_color(toolbg)
    scrollbar $de.static_tags_scrollver -orient vertical -command de_ScrollVerTags \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)

    bind $de.static_tags_list <ButtonPress-1> "de_UpdateEvents $PAD \[%W get \[%W curselection\]\]"
    bindtags $de.static_tags_list "Listbox $de.static_tags_list .debugevent all"

					# Event List
    label   $de.static_events_label -text "Event List" \
	-font $_font(tools) \
	-bg $_color(toolbg)
    label   $de.static_events_filterlabel -text "tagOrId: " \
	-font $_font(tools) \
	-bg $_color(toolbg)
    entry   $de.static_events_filter \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
    listbox $de.static_events_list -width 25 -height 5 \
	-font $_font(tools) \
	-bg $_color(toolbg) -selectbackground $_color(toolselbg) \
	-highlightbackground $_color(toolbg)
    scrollbar $de.static_events_scrollver -orient vertical -command de_ScrollVerEvents \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)

    bind $de.static_events_filter <KeyPress-Return> "de_UpdateEvents $PAD \[%W get\]"
    bind $de.static_events_list <ButtonPress-1> "de_DisplayBody $PAD %W"
    bindtags $de.static_events_list "Listbox $de.static_events_list .debugevent all"

					# Event Body
    label   $de.static_body_label -text "Event Body" \
	-font $_font(tools) \
	-bg $_color(toolbg)
    text    $de.static_body_text -width 0 -height 0 -wrap none \
	-font $_font(tools) \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg)
    scrollbar $de.static_body_scrollhor -orient horizontal -command de_ScrollHorBody \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    scrollbar $de.static_body_scrollver -orient vertical -command de_ScrollVerBody \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)

					# Event Trace
    label     $de.dynamic_label -text "Event Handler Trace" -anchor w \
	-font $_font(tools) \
	-bg $_color(toolbg)
    checkbutton $de.dynamic_first -text "Unique Events Only" \
	-variable _pad(debugEventFirstOnly) \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    set _pad(debugEventFirstOnly) 1
    listbox   $de.dynamic_trace -height 5 \
	-font $_font(tools) \
	-bg $_color(toolbg) -selectbackground $_color(toolselbg) \
	-highlightbackground $_color(toolbg)
    scrollbar $de.dynamic_scrollver -orient vertical -command de_ScrollTrace \
	-bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	-troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)

    bind $de.dynamic_trace <ButtonPress-1> "de_DisplayBody $PAD %W"
    bindtags $de.dynamic_trace "Listbox $de.dynamic_trace .debugevent all"

					# Buttons
    button $de.buttons_update -text "Update"  -command "de_Update $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)
    button $de.buttons_close -text "Close"  -command "de_Close $PAD" \
	-font $_font(tools) \
	-bg $_color(toolbg) -activebackground $_color(toolactivebg) \
	-highlightbackground $_color(toolbg)

    grid $de.static_tags_label     -row 0 -column 0               -sticky ew
    grid $de.static_tags_list      -row 1 -column 0 -rowspan 2    -sticky nsew
    grid $de.static_tags_scrollver -row 1 -column 1 -rowspan 2    -sticky ns
    grid $de.static_events_label   -row 0 -column 2 -columnspan 3 -sticky ew
    grid $de.static_events_filterlabel  -row 1 -column 2          -sticky ew
    grid $de.static_events_filter  -row 1 -column 3 -columnspan 1 -sticky ew
    grid $de.static_events_list    -row 2 -column 2 -columnspan 2 -sticky nsew
    grid $de.static_events_scrollver -row 2 -column 4             -sticky ns
    grid $de.static_body_label     -row 0 -column 5 -columnspan 2 -sticky ew
    grid $de.static_body_text      -row 1 -column 5 -rowspan 4    -sticky nsew
    grid $de.static_body_scrollhor -row 5 -column 5               -sticky ew
    grid $de.static_body_scrollver -row 1 -column 6 -rowspan 4    -sticky ns
    grid $de.dynamic_label         -row 3 -column 0 -columnspan 3 -sticky ew   -padx 20
    grid $de.dynamic_first         -row 3 -column 3 -columnspan 2 -sticky ew   -padx 20
    grid $de.dynamic_trace         -row 4 -column 0 -columnspan 4 -sticky nsew
    grid $de.dynamic_scrollver     -row 4 -column 4               -sticky ns
    grid $de.buttons_update        -row 6 -column 2
    grid $de.buttons_close         -row 6 -column 5

    grid rowconfigure $de 2 -weight 1
    grid rowconfigure $de 4 -weight 2

    grid columnconfigure $de 0 -weight 1
    grid columnconfigure $de 2 -weight 1
    grid columnconfigure $de 3 -weight 1
    grid columnconfigure $de 5 -weight 5

    if {![info exists geometry($de)]} {
	wm geometry $de 700x450
    }
    pad_ManageWindow $de
    wm title $de "Pad++ Event Debugger"

    de_Update $PAD
}

#
# Update the Event Debugger data with current info
# about event bindings, etc., and turn on
# event trapping.
#
proc de_Update {PAD} {
    global _pad

					# Start trapping events
    set _pad(debugEvents) 1
    $PAD config -debugEvent 1
    $PAD config -debugOut _pad(debugOut)
    set _pad(debugOut) ""
    trace variable _pad(debugOut) w de_UpdateTrace
    .debugevent.dynamic_trace delete 0 end
    de_ScrollTrace
    .debugevent.static_body_text delete 0.1 end
    de_ScrollHorBody
    de_ScrollVerBody
    .debugevent.static_events_filter delete 0 end

    de_UpdateTags $PAD
    de_UpdateEvents $PAD
}

#
# Update the list of all tags in pad
#
proc de_UpdateTags {PAD} {
    set taglist "all"
    foreach item "1 [$PAD find -groupmembers all]" {
	set tags [$PAD gettags $item]
	eval lnewappend taglist $tags
    }

    .debugevent.static_tags_list delete 0 end
    set taglist [lsort $taglist]
    foreach tag $taglist {
	.debugevent.static_tags_list insert end $tag
    }

    de_ScrollVerTags
}

#
# Update the list of events.  If tagorid is specified, then
# show just those events on tagorid.  Otherwise, show all events.
#
proc de_UpdateEvents {PAD {tagorid ""}} {
    .debugevent.static_events_filter delete 0 end
    .debugevent.static_events_filter insert 0 $tagorid
    .debugevent.static_events_list delete 0 end
    set event_list ""
    if {$tagorid != ""} {
	set events [$PAD bind $tagorid]
	foreach event $events {
	    lappend event_list "$event $tagorid"
	}
	set event_list [lsort $event_list]
    } else {
					# Then find events on tags
	set tags [.debugevent.static_tags_list size]
	for {set i 0} {$i < $tags} {incr i} {
	    set tag [.debugevent.static_tags_list get $i]
	    set events [$PAD bind $tag]
	    if {$events != ""} {
		set events [lsort $events]
		foreach event $events {
		    lappend event_list "$event $tag"
		}
	    }
	}
					# Then find events on ids
	foreach item "1 [$PAD find -groupmembers all]" {
	    set events [$PAD bind $item]
	    if {$events != ""} {
		set events [lsort $events]
		foreach event $events {
		    lappend event_list "$event $item"
		}
	    }
	}
    }

    foreach event $event_list {
	.debugevent.static_events_list insert end $event
    }

    de_ScrollVerEvents
}

#
# Close the Event Debugger, and turn off event trapping
#
proc de_Close {PAD} {
    global _pad

    if {[winfo exists $PAD]} {
	$PAD config -debugEvent {}
	$PAD config -debugOut {}
    }
    set _pad(debugEvents) {}
    trace vdelete _pad(debugOut) w de_UpdateTrace
    destroy .debugevent
}

#
# Update the Trace list box because some new event(s) were fired
#
proc de_UpdateTrace {name1 name2 op} {
    global _pad

    set lines [split $_pad(debugOut) "\n"]
    foreach line $lines {
	if {$line != ""} {
	    set add_line 1
	    if {$_pad(debugEventFirstOnly)} {
		set size [.debugevent.dynamic_trace size]
		set prev_line [.debugevent.dynamic_trace get [expr $size - 1]]
		if {$line == $prev_line} {
		    set add_line 0
		}
	    }
	    if {$add_line} {
		.debugevent.dynamic_trace insert end $line
	    }
	}
    }
    set _pad(debugOut) ""


    .debugevent.dynamic_trace yview moveto 1.0
    de_ScrollTrace
}

proc de_UpdateTrace {name1 name2 op} {
    global _pad

    set break 0

    set lines [split $_pad(debugOut) "\n"]
    foreach line $lines {
        if {$line != ""} {
            set add_line 1
            if {$_pad(debugEventFirstOnly)} {
                set size [.debugevent.dynamic_trace size]
                set prev_line [.debugevent.dynamic_trace get [expr $size - 1]]

                # fix to ignore previous lines that are breaks...
                set tmp_size [expr $size - 2]

                # This is for breaks that cancel other bindings.
                while {" " == [string range $prev_line 0 1]} {
                    set prev_line [.debugevent.dynamic_trace get $tmp_size]
                    incr tmp_size -1
                }
                if {"break" == [string range $prev_line 0 4]} {
                    set prev_line [.debugevent.dynamic_trace get $tmp_size]

                    # record the fact that the previous line was a break
                    set break 1
                } 

                if {$line == $prev_line} {
                    set add_line 0
                } 
                    
            }
            if {$add_line} {
                # if the previous line was not a break, or the current
                # line is not a break, than append the line to the trace
                if {($break == 0) ||
                    ([string range $line 0 4] != "break")} {
                    .debugevent.dynamic_trace insert end $line
                    set break 0
                }
            }
        }
    }
    set _pad(debugOut) ""


    .debugevent.dynamic_trace yview moveto 1.0
    de_ScrollTrace
}

#
# Call this to synchronize listbox and scrollbar for Trace
#
proc de_ScrollTrace {args} {
    set lb .debugevent.dynamic_trace
    set sb .debugevent.dynamic_scrollver

    eval $lb yview $args
    eval $sb set [$lb yview]
}

#
# Call this to synchronize listbox and horizontal scrollbar for Body
#
proc de_ScrollHorBody {args} {
    set text .debugevent.static_body_text
    set sb   .debugevent.static_body_scrollhor

    eval $text xview $args
    eval $sb set [$text xview]
}

#
# Call this to synchronize listbox and vertical scrollbar for Body
#
proc de_ScrollVerBody {args} {
    set text .debugevent.static_body_text
    set sb   .debugevent.static_body_scrollver

    eval $text yview $args
    eval $sb set [$text yview]
}

#
# Call this to synchronize listbox and vertical scrollbar for Tags
#
proc de_ScrollVerTags {args} {
    set lb .debugevent.static_tags_list
    set sb   .debugevent.static_tags_scrollver

    eval $lb yview $args
    eval $sb set [$lb yview]
}

#
# Call this to synchronize listbox and vertical scrollbar for Events
#
proc de_ScrollVerEvents {args} {
    set lb .debugevent.static_events_list
    set sb   .debugevent.static_events_scrollver

    eval $lb yview $args
    eval $sb set [$lb yview]
}

#
# Show the body of the event selected in the specified listbox.
# If called from a listbox other than the event listbox, then
# load the event listbox with events of the current tagorid,
# and put the current event at the top of the list.
#
proc de_DisplayBody {PAD listbox} {
    set selection [$listbox curselection]
    if {$selection == ""} {
	return
    }

    set line [$listbox get $selection]
    if {([string range $line 0 4] == "break") ||
        ([string range $line 0 4] == "error")} {
	return
    }

					# Show the event body
    set sequence [lindex $line 0]
    set id [lindex $line 1]
    set tag [lindex $line 2]
    if {$tag != ""} {
	set tagorid $tag
    } else {
	set tagorid $id
    }
    set body [$PAD bind $tagorid $sequence]

    .debugevent.static_body_text delete 0.1 end
    .debugevent.static_body_text insert 0.1 $body

    de_ScrollHorBody
    de_ScrollVerBody

					# If not Event List, then update it
    if {$listbox != ".debugevent.static_events_list"} {
	de_UpdateEvents $PAD $tagorid
	set size [.debugevent.static_events_list size]
	set this_event "$sequence $tagorid"
	set found 0
	for {set i 0} {$i < $size} {incr i} {
	    set event [.debugevent.static_events_list get $i]
	    if {$event == $this_event} {
		set found 1
		break
	    }
	}
	if {$found == 0} {
	    set i 0
	}
	.debugevent.static_events_list yview $i
	.debugevent.static_events_list selection set $i
    }
}
