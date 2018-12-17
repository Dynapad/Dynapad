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
# Desktop
#
# Globals: _desktop
# Every item (file or directory) has the following entries in _desktop
# where "$id" is the id of the item.
#   _desktop($id.type)   - Type of item: "file" or "dir"
#   _desktop($id.name)   - Name of item
#   _desktop($id.loaded) - Boolean.  True if contents loaded
#   _desktop($id.parent) - Id of parent, or "" if root
#   _desktop($id.data)   - List of data ids
#

#
# ToDo
#
#   * Make folders have user-definable shapes
#   * Allow copy, delete, rename
#   * Remember layout
#   * Drag and drop for re-structuring
#

proc td {} {
    .pad resetcoordframe
    .pad moveto 0 0 1
    .pad delete all
    import .pad /local/bederson/pad_devo/tmp
    update
}

set _desktop(initial_dir_size) 100
set _desktop(dir_load_size)    150
set _desktop(dir_minsize)      10
set _desktop(file_minsize)     10
set _desktop(data_minsize)     25
set _desktop(label_transition) 150

proc bindDesktop {PAD} {
				# Highlight items when mouse is over them
    $PAD bind desktop <Run-Enter> {
	catch {%P ic %O -pen red3}
	break
    }

    $PAD bind desktop <Run-Leave> {
	catch {%P ic %O -pen black}
	break
    }

				# ButtonPress centers a file, and loads it
    $PAD bind desktop <Run-ButtonPress-1> {
	panEvent %P %W "press" %O %s %x %y %i %j %l
	break
    }

    $PAD bind desktop <Run-B1-Motion> {
	panEvent %P %W "motion" %O %s %x %y %i %j %l
	break
    }

    $PAD bind desktop <Run-ButtonRelease-1> {
	panEvent %P %W "release" %O %s %x %y %i %j %l
	if {$_pan(started) == 0} {
	    set group [%P getgroup %O]
	    %P center $group $_pad(AnimationSpeed) 0.5 0.5 0.9
	    if {$_desktop($group.type) == "dir"} {
		_dt_load_dir %P $group
	    } else {
		_dt_load_file %P $group
	    }
	}
	break
    }

				# Double ButtonPress executes a file if it is executable.
				# If it is a directory, then the contents of *all*
				# the members are loaded
    $PAD bind desktop <Run-Double-ButtonPress-1> {
	panEvent %P %W "press" %O %s %x %y %i %j %l
	break
    }

    $PAD bind desktop <Run-Double-B1-Motion> {
	panEvent %P %W "motion" %O %s %x %y %i %j %l
	break
    }

    $PAD bind desktop <Run-Double-ButtonRelease-1> {
	panEvent %P %W "release" %O %s %x %y %i %j %l
	if {$_pan(started) == 0} {
	    set group [%P getgroup %O]
	    %P center $group $_pad(AnimationSpeed) 0.5 0.5 0.9
	    if {$_desktop($group.type) == "dir"} {
		_dt_load_dir %P $group
		foreach id $_desktop($group.data) {
		    _dt_load %P $id
		    %P update	;# Show some progress
		}
	    } else {
		if {[file executable $_desktop($group.name)]} {
		    updateStatusMsg %P "executing $_desktop($group.name)"
		    exec $_desktop($group.name) &
		}
	    }
	}
	break
    }

				# Shift-ButtonPress unloads a file, and centers the parent
    $PAD bind desktop <Run-Shift-ButtonPress-1> {
	panEvent %P %W "press" %O %s %x %y %i %j %l
	break
    }

    $PAD bind desktop <Run-Shift-B1-Motion> {
	panEvent %P %W "motion" %O %s %x %y %i %j %l
	break
    }

    $PAD bind desktop <Run-Shift-ButtonRelease-1> {
	panEvent %P %W "release" %O %s %x %y %i %j %l
	if {$_pan(started) == 0} {
	    set group [%P getgroup %O]
	    if {$_desktop($group.type) == "dir"} {
		_dt_unload_dir %P $group
	    } else {
		_dt_unload_file %P $group
	    }
	    if {$_desktop($group.parent) != ""} {
		%P center $_desktop($group.parent) $_pad(AnimationSpeed) 0.5 0.5 0.9
	    }
	}
	break
    }

				# Don't let desktop components get written out
    $PAD bind desktop <Write> {
	set Pad_Write 0
	break
    }
}

proc pad_AddDesktopItem {PAD filename} {
    global _desktop

    set view [$PAD getview]
    set xview [lindex $view 0]
    set yview [lindex $view 1]
    set zoom [lindex $view 2]
    set item ""
    if {[file readable $filename]} {
	if {[file isdirectory $filename]} {
	    set item [_dt_create_directory $PAD $filename $filename]
	} else {
	    set item [_dt_create_file $PAD $filename]
	}
	$PAD ic $item -place "$xview $yview [expr $_desktop(initial_dir_size) / $zoom]"
    }

    return $item
}

proc _dt_create_label {PAD label tags} {
    global _desktop _pad

    set fontheight1 0.15
    set font1 "Helvetica-$fontheight1"

    set width [bbwidth [$PAD font bbox $label $font1]]
    set ids ""
    if {$width > 1.0} {
	set length [string length $label]
	set index [expr round(0.5 + $length / $width) - 2]
	set label1 [string range $label 0 $index]
	set fontheight2 [expr 0.9 * $fontheight1 / $width]
	set font2 "Helvetica-$fontheight2"
	lappend ids [$PAD create text -text $label1 -place "0 0 1" -anchor sw \
		-font $font1 -maxsize [expr 1.2 * $_desktop(label_transition)] -tags $tags]
	lappend ids [$PAD create text -text $label -place "0 0 1" -anchor sw \
		-font $font2 -minsize [expr 0.8 * $_desktop(label_transition)] -tags $tags]
    } else {
	lappend ids [$PAD create text -text $label -place "0 0 1" -anchor sw \
		-font $font1 -tags $tags]
    }

    return $ids
}

proc _dt_create_directory {PAD dirname {dirlabel ""}} {
    global _desktop _pad

    set dirname [string trimright $dirname "/"]
    if {$dirlabel == ""} {
	set dirlabel [file tail $dirname]
    }
				# Create graphical items
    set folder [$PAD create polygon 0 0 1 0 1.0 0.7 0.5 0.7 0.4 0.8 0.1 0.8 0.0 0.7 0 0 \
	    -fill white -pen black -penwidth 0.01 -tags "desktop dir item"]
    set label [_dt_create_label $PAD $dirlabel "desktop dir item"]
    set id [$PAD create group -members "$folder $label" -anchor sw \
	    -tags "desktop item" -minsize $_desktop(dir_minsize) -maxsize -1]
    set _pad($folder.upselect) 2
    foreach labelid $label {
	set _pad($labelid.upselect) 2
    }
    set container [$PAD create group -members $id -anchor sw -tags "desktop item" -maxsize -1]
    set _pad($container.autoraise) 1

    set _desktop($container.type) "container"
    set _desktop($container.name) $dirname
    set _desktop($container.loaded) 0
    set _desktop($container.parent) ""
    set _desktop($container.data) $id

    set _desktop($id.type) "dir"
    set _desktop($id.name) $dirname
    set _desktop($id.loaded) 0
    set _desktop($id.parent) ""
    set _desktop($id.data) ""
 
   return $container
}

proc _dt_create_file {PAD filename} {
    global _desktop _pad

    set bgnd [$PAD create rectangle 0 0 1.0 1.0 \
	    -fill gray70 -pen black -penwidth 0.01 -tags "desktop file item"]
    set label [_dt_create_label $PAD [file tail $filename] "desktop file item"]
    set id [$PAD create group -members "$bgnd $label" -anchor sw \
	    -tags "desktop file item" -minsize $_desktop(file_minsize) -maxsize -1]
    set _pad($id.autoraise) 1
    set _pad($bgnd.upselect) 1
    foreach labelid $label {
	set _pad($labelid.upselect) 1
    }

    set _desktop($id.type) "file"
    set _desktop($id.name) $filename
    set _desktop($id.loaded) 0
    set _desktop($id.parent) ""
    set _desktop($id.data) ""

    return $id
}

proc _dt_load {PAD item} {
    global _desktop

    if {$_desktop($item.type) == "dir"} {
	_dt_load_dir $PAD $item
    } elseif {$_desktop($item.type) == "file"} {
	_dt_load_file $PAD $item
    } else {
				# A container, load contents
	foreach item $_desktop($item.data) {
	    _dt_load $PAD $item
	}
    }
}

#
# Load the contents of a directory, and layout each file and
# folder in alphabetized order.
#
proc _dt_load_dir {PAD dir} {
    global _desktop

    if {$_desktop($dir.loaded) == 0} {
	set _desktop($dir.loaded) 1
	if {[catch {set files [glob $_desktop($dir.name)/*]}]} {
				# No members
	    return
	}
	set container [$PAD getgroup $dir]
	set files [lsort $files]
	set num_files [llength $files]
	set n [expr int(1.0 + sqrt($num_files / 0.5))]
	if {[expr int(0.75 * $n) * $n] < $num_files} {    ;# Fix infrequent badness
	    incr n
        }
	set size [expr 1.0 / ((1.1 * $n) + 1)]
	set bbox [$PAD bbox $container]
	set xmin [lindex $bbox 0]
	set ymin [lindex $bbox 1]
	set mag 1.0
	set c $container
	while 1 {
	    set mag [expr $mag * [$PAD ic $c -z]]
	    set c [$PAD getgroup $c]
	    if {$c == ""} {
		break
	    }
	}
	set i 0
	foreach filename $files {
	    if {[file readable $filename]} {
		if {[file isdirectory $filename]} {
		    set fileid [_dt_create_directory $PAD $filename]
		} else {
		    set fileid [_dt_create_file $PAD $filename]
		}
		set _desktop($fileid.parent) $dir
		lappend _desktop($dir.data) $fileid
		set x [expr $mag * (0.0125 + ($i % $n) / $n.0) + $xmin]
		set y [expr $mag * (0.65 - ($i / $n) / $n.0 - $size) + $ymin]
		$PAD ic $fileid -place "$x $y [expr $mag * $size]"
		$PAD addgroupmember $fileid $container
		incr i
	    }
	}
    }
}

#
# Load the contents of a file, and put it within the file.
#
proc _dt_load_file {PAD file} {
    global _desktop _pad

    if {$_desktop($file.loaded) == 0} {
	set _desktop($file.loaded) 1

	set view [$PAD getview]
	set bgnd [lindex [$PAD config -bg] 4]
	if {[catch {set obj [import $PAD $_desktop($file.name)]}]} {
	    updateStatusMsg $PAD "Can't load: $_desktop($file.name)"
	    return		   ;# Can't load object
	}
	eval $PAD moveto $view	   ;# Importing an object could change the view
	$PAD config -bg $bgnd      ;# Restore background because files store background
	if {$obj != ""} {
				   # Loaded in several objects, so group them
	    if {[llength $obj] > 1} {
		set obj [$PAD create group -members $obj -divisible 0 -tags "desktop file item" -maxsize -1]
	    }
	    $PAD addtag desktop $obj
	    set _pad($obj.upselect) 1
	    set _desktop($file.data) $obj
	    set bbox [$PAD bbox $file]
	    set width [bbwidth $bbox]
	    set height [bbheight $bbox]
	    set x1 [expr [lindex $bbox 0] + ($width * 0.1)]
	    set y1 [expr [lindex $bbox 1] + ($height * 0.3)]
	    set x2 [expr [lindex $bbox 2] - ($width * 0.1)]
	    set y2 [expr [lindex $bbox 3] - ($height * 0.1)]
	    pad_position_obj_in_bbox $PAD $obj $x1 $y1 $x2 $y2
	    $PAD ic $obj -minsize $_desktop(data_minsize)]
	    $PAD addgroupmember $obj $file
        			# Let contents get large
	    $PAD ic $file -maxsize -1
	    foreach member [$PAD ic $file -members] {
				# Don't change label's maxsize since they may be faders
		if {[$PAD type $file] == "polygon"} {
		    $PAD ic $member -maxsize -1
		}
	    }
	}
    }
}

proc _dt_unload_dir {PAD dir} {
    global _desktop

    if {$_desktop($dir.loaded) == 1} {
	set _desktop($dir.loaded) 0
	foreach id $_desktop($dir.data) {
	    if {$_desktop($id.type) == "dir"} {
		_dt_delete_dir $PAD $id
	    } else {
		_dt_delete_file $PAD $id
	    }
	}
	set _desktop($dir.data) ""
    }
}

proc _dt_unload_file {PAD file} {
    global _desktop

    if {$_desktop($file.loaded) == 1} {
	set _desktop($file.loaded) 0
	$PAD delete $_desktop($file.data)
	set _desktop($file.data) ""
    }
}

proc _dt_delete_dir {PAD dir} {
    global _desktop

    _dt_unload_dir $PAD $dir
    $PAD delete $dir
    unset _desktop($dir.type)
    unset _desktop($dir.name)
    unset _desktop($dir.loaded)
    unset _desktop($dir.parent)
    unset _desktop($dir.data)
}

proc _dt_delete_file {PAD file} {
    global _desktop

    _dt_unload_file $PAD $file
    $PAD delete $file
    unset _desktop($file.type)
    unset _desktop($file.name)
    unset _desktop($file.loaded)
    unset _desktop($file.parent)
    unset _desktop($file.data)
}

