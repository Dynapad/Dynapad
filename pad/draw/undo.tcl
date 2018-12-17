

#
# PadDraw undo mechanism
# 

# internal function - sets the label of the Undo menu option.
proc _pad_undo_label {label isredo} {
   if {$isredo} {
      .menubar.edit.m entryconfigure 1 -label "Redo $label"
   } else { 
      .menubar.edit.m entryconfigure 1 -label "Undo $label"
   } 
}

# internal function - sets the sensitivity of the Undo menu option.
proc _pad_undo_state {value} {
    if {$value} {
        .menubar.edit.m entryconfigure 1 -state normal
    } else {
        .menubar.edit.m entryconfigure 1 -state disabled
    }
}

# should be in widget.tcl - or something more general
proc pad_unset_prop {widget prop} {
    global $widget
    unset [set widget]($prop)
}

#
# Called by Undo menu option to undo a change 
#
proc pad_undo_it {} {
   if {[pad_undo_available]} {
       set name [pad_get_prop _padundo name]
       uplevel #0 "eval [pad_get_prop _padundo action]"
       if {[pad_undo_available] == 0} {
	  pad_set_prop _padundo name
       }
       if {[pad_undo_is_redo]} {
          pad_set_prop _padundo isredo 0
       } else {
          pad_set_prop _padundo isredo 1 
       }
       _pad_undo_label $name [pad_get_prop _padundo isredo] 
   }
}

#
# Calls the undo lose callback to discard any information. For delete
# undos, this method should actually delete the item.
#
proc _pad_undo_lost {} {
    if {[pad_test_prop _padundo lose]} {
        set lose [pad_get_prop _padundo lose]
        if {$lose != ""} {
            uplevel #0 "eval $lose"
        }
    }
}

#
# Main routine to setup an undo
# 
proc pad_undo_setup {PAD name undo_action lose_action} {
    _pad_undo_lost
    pad_set_prop _padundo PAD $PAD
    pad_set_prop _padundo action $undo_action
    pad_set_prop _padundo lose $lose_action
    pad_set_prop _padundo name $name
    pad_set_prop _padundo isredo 0

    # Set the Edit menu option
    _pad_undo_label $name 0
    _pad_undo_state 1
}

#
# Returns true if there is undo information.
#
proc pad_undo_available {} {
    return [pad_test_prop _padundo name]
}

#
# Returns the registered name of the undo information, or "" if none.
#
proc pad_undo_type {} {
    if {[pad_undo_available]} {
	return [pad_get_prop _padundo name]
    } else {
	return ""
    }
}

#
# Used to change the type of undo
#
proc pad_set_undo_type {name} {
    if {[pad_undo_available]} {
	pad_set_prop _padundo name $name
        _pad_undo_label $name [pad_get_prop _padundo isredo]
    } 
}

#
# Returns 1 if the current undo action is a redo, otherwise 0. 
#
proc pad_undo_is_redo {} {
    if {[pad_undo_available]} {
        return [pad_get_prop _padundo isredo]
    } else {
        return 0
    }
}

#
# Throw away any undo information
#
proc pad_undo_cancel {} {
    if {[pad_undo_available]} {
      _pad_undo_lost 
      pad_unset_prop _padundo name
      _pad_undo_label "" 0
      _pad_undo_state 0
    }
}

#
# Simple API to undo interface - used for itemconfigure of items on the
# Pad.
#
proc _pad_undo_itemconfigures {} {
    set PAD [pad_get_prop _padundo PAD]
    set list [pad_get_prop _padundo iclist]
    set newlist ""
    foreach item $list {
         set obj [lindex $item 0]
         set prop [lindex $item 1]
         set val [lindex $item 2]
         set old_val [$PAD ic $obj $prop]
         $PAD ic $obj $prop $val
	 lappend newlist [list $obj $prop $old_val]
         if {[lmember [$PAD gettags $obj] "selected"]} {
            pad_select $PAD $obj
	 }
    }
    pad_set_prop _padundo iclist $newlist
}

proc pad_prepare_undo {PAD name ids prop} {
    set list ""
    foreach id $ids {
        set val [$PAD ic $id $prop]
        set l [list $id $prop $val]
        lappend list $l
    }
    pad_undo_setup $PAD $name _pad_undo_itemconfigures ""
    pad_set_prop _padundo iclist $list
}
