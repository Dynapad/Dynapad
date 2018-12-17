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
# File Selection Dialog Box
#
# Calling 'make.file' makes a modal file selection box that
# returns the file or directory selected, or an empty string
# if no file is selected (i.e., cancel is pressed).
# If no file is specified, it starts with the most recent file selected.
# Entries are alphabetized first by directories and then files.
#
# File box interactions include:
#   - Filename completion with Tab or Escape key
#   - Select file with return key or double-clicking
#   - Select file in listbox with mouse
#   - Absolute and relative pathnames and tilde-expansions supported
#   - Simple emacs bindings in entry box
#
# Global variables used:
#   _file:    (internal state)
#   _color:   Colors to use
#   geometry: Stores geometry of window
#

proc make.file {{msg "Select file:"} {file ""} {type "open"}} {
    global env _file _color _font
    global tcl_platform

    if {[winfo exists .file]} {
        raise .file
	return
    }

    toplevel .file \
	    -bg $_color(toolbg)
    wm resizable .file 0 0

    label .file.command -text $msg \
	    -bg $_color(toolbg)
    label .file.file_label -text "File:" \
	    -font $_font(tools) \
	    -bg $_color(toolbg)
    entry .file.file -relief sunken -width 30 \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -selectbackground $_color(toolselbg) \
	    -highlightbackground $_color(toolbg)
    frame .file.files \
	    -bg $_color(toolbg)
    listbox .file.files.files -relief sunken \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -selectbackground $_color(toolselbg) \
	    -highlightbackground $_color(toolbg)
    scrollbar .file.files.scroll -orient vertical -command "fileScroll" \
	    -bg $_color(toolbg) -highlightbackground $_color(toolbg) \
	    -troughcolor $_color(toolbg) -activebackground $_color(toolactivebg)
    label .file.dir \
	    -font $_font(tools) \
	    -font $_font(tools) \
	    -bg $_color(toolbg)
    label .file.stats \
	    -font $_font(tools) \
	    -bg $_color(toolbg)
    frame .file.sep1 -height 2 -bg $_color(toolselbg)
    frame .file.buttons \
	    -bg $_color(toolbg)
    button .file.buttons.ok -text "Ok" -command "fileOk" \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -activebackground $_color(menuactivebg) \
	    -highlightbackground $_color(toolbg)
    button .file.buttons.cancel -text "Cancel" -command "fileCancel" \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -activebackground $_color(menuactivebg) \
	    -highlightbackground $_color(toolbg)

    pack .file.command -anchor w

    pack .file.file_label -anchor w
    pack .file.file -fill x
    pack .file.files -expand t -fill both
    pack .file.files.files -expand t -fill both -side left
    pack .file.files.scroll -fill y -side left
    pack .file.dir -anchor w
    pack .file.stats -anchor w

			# If <type> is "open" or "import", include widgets
			# to control whether image rgb data should be loaded,
			# and whether file data (images and textfiles) should
			# copied or referenced.  If it is "import", allow imported
			# items to be automatically selected.
    if {($type == "open") || ($type == "import")} {
	frame .file.sep2 -height 2 -bg $_color(toolselbg)
	pack .file.sep2 -fill x -padx 10 -pady 5

	if {$type == "import"} {
	    checkbutton .file.select -text "Select imported items" -command "" \
		-font $_font(tools) \
		-variable _file(SelectItems) -anchor w \
		-bg $_color(toolbg) -activebackground $_color(menuactivebg) \
		-highlightbackground $_color(toolbg)
	    pack .file.select -fill x -anchor w
	}

	checkbutton .file.rgb -text "Load image RGB data" -command "" \
	    -font $_font(tools) \
	    -variable _file(RGB) -anchor w \
	    -bg $_color(toolbg) -activebackground $_color(menuactivebg) \
	    -highlightbackground $_color(toolbg)
	pack .file.rgb -fill x -anchor w

	set fm [frame .file.import_frame \
	    -bg $_color(toolbg)]
	label $fm.import_label -text "Import by: " \
	    -font $_font(tools) \
	    -bg $_color(toolbg) 
	tk_optionMenu $fm.import_menu _file(WriteFormat) "reference" "copy"
	$fm.import_menu config -bg $_color(toolbg) -activebackground $_color(menuactivebg) \
	    -font $_font(tools) \
	    -highlightbackground $_color(toolbg)
	pack $fm.import_label -side left
	pack $fm.import_menu -fill x
	pack $fm -anchor w -fill x
    } else {
			# If <type> is "write", then include widgets to control
			# file format, and whether to use relative pathnames
	frame .file.sep2 -height 2 -bg $_color(toolselbg)
	pack .file.sep2 -fill x -padx 10 -pady 5

	checkbutton .file.relative -text "Relative pathnames" -command "" \
	    -font $_font(tools) \
	    -variable _file(RelativePathnames) \
	    -bg $_color(toolbg) -activebackground $_color(menuactivebg) \
	    -highlightbackground $_color(toolbg)
	pack .file.relative -fill x -anchor w

	if {$tcl_platform(platform) == "unix" } {
	  set cpu [exec uname -m | tr / -]
	  set os  [exec uname -s | tr / -]
	  if {$os == "AIX"} {
	      set cpu $env(HOSTTYPE)
	  }
	  if {$os == "IRIX"} {
	      set cpu "MIPS"
 	  }
      } else {
	  set cpu "win95"
      }

	set fm [frame .file.format_frame -bg $_color(toolbg)]
	label $fm.format_label -text "Format: " \
	    -font $_font(tools) \
	    -bg $_color(toolbg) 
	tk_optionMenu $fm.format_menu _file(Format) "Text" "Binary Interchange"
	$fm.format_menu config -bg $_color(toolbg) -activebackground $_color(menuactivebg) \
	    -font $_font(tools) \
	    -highlightbackground $_color(toolbg)
	pack $fm.format_label -side left
	pack $fm.format_menu -fill x
	pack $fm -anchor w -fill x
    }

    pack .file.sep1 -fill x -padx 10 -pady 5
    pack .file.buttons -fill x
    pack .file.buttons.ok -expand t -padx 10 -side left
    pack .file.buttons.cancel -expand t -padx 10 -side left

    pad_ManageWindow .file 0 0
    wm title .file "Pad++ File Tool"
    update

    fileInit $file
    fileUpdate $file

					# Fix broken default backspace/delete bindings
    bind .file.file <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
    bind .file.file <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}
					# File-completion and selection bindings
    bind .file.file <KeyPress-Escape> {fileComplete [%W get]}
    bind .file.file <KeyPress-Return> {fileSelect [%W get]}
    bind .file.file <Double-ButtonPress-1> {fileSelect [%W get]; break}

					# Make proper bindings for up/down arrows and pageup/pagedown keys
					# Make scrollbar match listbox
    bind .file.files.files <Key-Up> {
	tkListboxUpDown %W -1
	fileScroll
	break
    }
    bind .file.files.files <Key-Down> {
	tkListboxUpDown %W 1
	fileScroll
	break
    }
    bind .file.files.files <Key-Prior> {
	%W yview scroll -1 pages
	%W activate @0,0
	fileScroll
	break
    }
    bind .file.files.files <Key-Next> {
	%W yview scroll 1 pages
	%W activate @0,0
	fileScroll
	break
    }

    bind .file.files.files <ButtonPress-1> {
	%W selection clear 0 end
        %W selection set [%W nearest %y]
	set _file(select) 1
	set _file(index) [%W curselection]
	break
    }
    bind .file.files.files <Double-ButtonPress-1> {
	set index [.file.files.files curselection]
	if {$index == ""} {
		# Some weird bug where sometimes the double-buttonpress isn't
		# called, and select isn't reset.  So, in this case, get the
		# index from the previous value.
	    set index $_file(index)
	}
	set file [string trimright [%W get $index] "/"]
	fileSelect $file
	set _file(select) 0
	break
    }
    bind .file.files.files <B1-Motion> {
	if {$_file(select) == 1} {
	    %W selection clear 0 end
	    %W selection set [%W nearest %y]
	}
	break
    }
    bind .file.files.files <B1-ButtonRelease> {
	if {$_file(select) == 1} {
	    set index [%W curselection]
	    if {$index == ""} {
		# Some weird bug where sometimes the double-buttonpress isn't
		# called, and select isn't reset.  So, in this case, get the
		# index from the previous value.
		set index $_file(index)
	    }
	    set file [%W get $index]
	    .file.file delete 0 end
	    .file.file insert 0 $file
	}
	break
    }

    focus .file.file
    update idletask
    grab .file
    tkwait window .file

    return $_file(result)
}

#
# Given a filename, compute the relative filename
# from the current working directory.
#
proc fileComputeRelativePathname {file} {
    set cwd [pwd]

    set shared $cwd
    set levels 0
    while {1} {
	set len [string length $shared]
	if {$shared == [string range $file 0 [expr $len - 1]]} {
	    set remainder [string range $file $len end]
	    break
	}
	set shared [string range $shared 0 [expr [string wordstart $shared 1000] - 2]]
	if {$shared == ""} {
	    break
	}
	set levels [incr levels]
    }

    if {$shared != ""} {
	set relativefile ""
	for {set i 0} {$i < $levels} {incr i} {
	    append relativefile "../"
	}
	set len [string length $shared]
	append relativefile [string range $file [expr $len + 1] end]
	set file $relativefile
	if {$file == ""} {
	    set file "."
	}
    }

    return $file
}

#
# Complete filename.
# If there are multiple completions, complete as far as shared base.
# Change listbox view so first comleted file is at top of listbox.
#
proc fileComplete {name} {
    set file [fileParse $name]

    if {$file == ""} {
	return
    }
    if {([string index $name 0] == "/") ||
        ([string index $name 0] == "~")} {
	set relative 0
    } else {
	set relative 1
    }
    set len [expr [string length $file] - 1]
    set hits 0
    set files ""
    catch {set files [glob $file*]}
    foreach item $files {
	if {$file == [string range $item 0 $len]} {
	    incr hits
	    if {$hits == 1} {
		set completed_file $item
	    } else {
		set len1 [string length $completed_file]
		set len2 [string length $item]
		if {$len1 < $len2} {
		    set min_len $len1
		} else {
		    set min_len $len2
		}
		set new_file ""
		for {set j 0} {$j < $min_len} {incr j} {
		    if {[string index $completed_file $j] == [string index $item $j]} {
			set new_file [append new_file [string index $item $j]]
		    }
		}
		set completed_file $new_file
	    }
	}
    }
    if {$hits > 0} {
	if {$relative} {
	    set completed_file [file tail $completed_file]
	    set lb .file.files.files
	    set size [$lb size]
	    for {set i 0} {$i < $size} {incr i} {
		if {[string first $completed_file [$lb get $i]] == 0} {
		    .file.files.files see $i
		    $lb selection clear 0 end
		    $lb selection set $i
		    break
		}
	    }
	}
	.file.file delete 0 end
	.file.file insert 0 $completed_file
    }
}

#
# Select file from file name box.
# Allow WWW addresses as is.
# Enter specified directories.
# Allow absolute or relative pathnames.
# Allow ~-expansion.
#
proc fileSelect {name} {
    global _file

				# Give some immediate feedback
    .file.files.files delete 0 end
    .file.files.files insert 0 "Searching ..."
    update

    if {[string range $name 0 4] == "http:"} {
	.file.buttons.ok invoke
    } else {
	set file [fileParse $name]

	if {[file isdirectory $file]} {
	    fileUpdate $file
	    .file.file delete 0 end
	} else {
	    .file.buttons.ok invoke
	}
    }
}

#
# Parse specified filename, and set global _file(dir) appropriately.
# Handle absolute and relative pathnames with ~-expansion.
#
proc fileParse {name} {
    global _file env

    if {[string trimright $name /] == ".."} {
	if {$_file(dir) == "/"} {
	    set file "/"
	} else {
	    set file [file dirname [string trimright $_file(dir) /]]
	    set _file(dir) $file
	}
    } elseif {[string index $name 0] == "/"} {
	set file $name
	if {[file isdirectory $file]} {
	    set _file(dir) $file
	} else {
	    set _file(dir) [file dirname $file]
	}
    } elseif {[string index $name 0] == "~"} {
	if {$name == "~"} {
	    set file "[file dirname ~]/$env(USER)"
	    set _file(dir) $file
	} else {
	    if {[string index $name 1] == "/"} {
		set file "[file dirname ~]/$env(USER)"
		append file [string range $name 1 end]
	    } else {
		regexp {(~[^/]*)(.*)} $name dummy1 topdir bottomdir
		set file "[file dirname $topdir]/[string range $topdir 1 end]$bottomdir"
	    }
	    if {[file isdirectory $file]} {
		set _file(dir) $file
	    } else {
		set _file(dir) [file dirname $file]
	    }
	}
    } else {
	set file "[string trimright $_file(dir) /]/$name"
	if {[file isdirectory $file]} {
	    set _file(dir) $file
	}
    }

    return $file
}

#
# Pick first filename.
# If it is specified, than use that.
# Otherwise, if there has been one before, use that.
# Otherwise, use the current directory.
#
proc fileInit {file} {
    global _file

    if {$file == ""} {
	if {[info exists _file(name)]} {
	    if {[string range $_file(name) 0 4] == "http:"} {
		.file.file delete 0 end
		.file.file insert 0 $_file(name)
		return
	    }
	} else {
	    set _file(dir) [pwd]
	    set _file(name) ""
	}
    } else {
	if {[file isdirectory $file]} {
	    set _file(dir) $file
	    set _file(name) ""
	} else {
	    set _file(dir) [file dirname $file]
	    set _file(name) [file tail $file]
	}
    }
    .file.file delete 0 end
    .file.file insert 0 $_file(name)
}

#
# Update files shown in listbox based on specified file.
# Also update current directory and stats.
#
proc fileUpdate {file} {
    global _file

    set lb .file.files.files
    set sb .file.files.scroll

    if {($file == ".") || ($file == "")} {
    } elseif {($file == "..") || ($file == "../")} {
	set _file(dir) [file dirname [string trimright $_file(dir) "/"]]
    } elseif {[file isdirectory "$_file(dir)/$file"]} {
	set _file(dir) "[string trimright $_file(dir) "/"]/[string trimright $file /]"
    }
				# Update listbox
    $lb delete 0 end
    if {[catch {set items [glob $_file(dir)/*]}]} {
	set files ".."
    } else {
	set dirs ""
	set files ""
	foreach item $items {
	    if {[file isfile $item]} {
		lappend files $item
	    } else {
		lappend dirs $item
	    }
	}
	set files ".. [lsort $dirs] [lsort $files]"
    }
    set num_files 0
    set num_dirs 0
    foreach item $files {
	if {[file isfile $item]} {
	    incr num_files
	    $lb insert end "[file tail $item]"
	} else {
	    incr num_dirs
	    $lb insert end "[file tail $item]/"
	}
    }
				# Update dir and stats
    .file.dir config -text $_file(dir)
    .file.stats config -text "$num_dirs directories, $num_files files"

    $lb see 0
    eval $sb set [$lb yview]
}

proc fileOk {} {
    global _file

    set file [.file.file get]
    if {[string range $file 0 4] == "http:"} {
	set result $file
	set _file(dir) ""
	set _file(name) $file
    } else {
	set result [fileParse $file]
	set _file(name) [file tail $result]
    }
    set _file(result) $result
    destroy .file
    update			;# Make sure there is some immediate feedback
}

proc fileCancel {} {
    global _file

    set _file(result) ""
    destroy .file
    update			;# Make sure there is some immediate feedback
}

proc fileScroll {args} {
    set lb .file.files.files
    set sb .file.files.scroll

    eval $lb yview $args
    eval $sb set [$lb yview]
}
