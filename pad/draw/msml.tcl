proc msml_load_pad {url {size 0.9} {donescript ""}} {
    global _pad _html Pad_CurrentHTMLPad Pad_CurrentHTML

    set html $Pad_CurrentHTML
    if {![info exists _html(items.$html)]} {
	set _html(items.$html) 0
    }
    incr _html(items.$html)
    set PAD $Pad_CurrentHTMLPad
    set group [$PAD create group -tags "_html html" -divisible 0 -maxsize -1]
    set _pad($group.upselect) 0
    if {$donescript == ""} {
        set donescript "default_done_pad $PAD $html $size $group"
    }
    $PAD urlfetch $url -var "_msml$group" -donescript $donescript
}

proc msml_load_tcl {url tagname {size 0.9} {donescript ""}} {
    global _pad _html Pad_CurrentHTMLPad Pad_CurrentHTML

    set PAD $Pad_CurrentHTMLPad
    set html $Pad_CurrentHTML
    if {![info exists _html(items.$html)]} {
	set _html(items.$html) 0
    }
    incr _html(items.$html)
    set group [$PAD create group -tags "_html html" -divisible 0 -maxsize -1]
    set _pad($group.upselect) 0
    if {$donescript == ""} {
        set donescript "default_done_tcl $PAD $html $tagname $size $group"
    }

    $PAD urlfetch $url -var "_msml$group" -donescript "$donescript"
}

proc default_done_pad {PAD html size group token} {
    global _msml$group _html Pad_ObjectList PADLOAD

    set PADLOAD $PAD
    set view [$PAD getview]	;# Save view because pad files change it
    uplevel #0 [set _msml$group]
    unset _msml$group
    eval $PAD moveto $view

    set groupmembers [set Pad_ObjectList]
    add_list_to_html $PAD $groupmembers $size $group

    incr _html(items.$html) -1
    if {$_html(items.$html) == 0} {
	html_Layout $PAD $html
    }
}

proc default_done_tcl {PAD html tagname size group token} {
    global _msml$group _html

    uplevel #0 [set _msml$group]
    unset _msml$group

    set groupmembers [$PAD find -groupmembers withtag $tagname]
    add_list_to_html $PAD $groupmembers $size $group

    incr _html(items.$html) -1
    if {$_html(items.$html) == 0} {
	html_Layout $PAD $html
    }
}

proc add_list_to_html {PAD groupmembers size group} {
    global  Pad_CurrentHTML 

    set bbox [eval $PAD bbox $groupmembers]
    $PAD ic $group -members $groupmembers
    
    set width [expr [lindex $bbox 2] - [lindex $bbox 0]]
    set height [expr [lindex $bbox 3] - [lindex $bbox 1]]
    if {$width > $height} { 
       set maxdim $width
    } else {
       set maxdim $height
    }

    set width [$PAD ic $Pad_CurrentHTML -width]
    set html_size [$PAD ic $Pad_CurrentHTML -z]
    set size [expr $size * $width / ($html_size * $maxdim)]

    $PAD ic $group -z $size

			    # Hack to force html to recompute display list
    $PAD ic $Pad_CurrentHTML -width [$PAD ic $Pad_CurrentHTML -width]
}

 
