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

proc makeMenuBar {PAD} {
    global env _pad
    global showDrawingPalette 
    global menu
    global _pad _color _font
    global _grid
    global tcl_platform

			# Compute menubar name
    set mb [menubarGetName $PAD]
    frame $mb -bg $_color(menubg)

    #
    # File submenu
    #
    menubutton $mb.file \
	-menu "$mb.file.m" \
	-text {File } \
	-underline {0} \
	-font $_font(menu) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    menu $mb.file.m \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.file.m add command \
	-label {New Pad} \
	-command "menubarNewPad $PAD"
    $mb.file.m add command \
	-label {Open Pad File...} \
	-command "pad_open $PAD"
    $mb.file.m add command \
	-label {Save} \
	-command "set _pad(PadFileName$PAD) \[padSave $PAD \$_pad(PadFileName$PAD)\]"
    $mb.file.m add command \
	-label {Save As...} \
	-command "set _pad(PadFileName$PAD) \[padSaveAs $PAD \$_pad(PadFileName$PAD)\]"
    $mb.file.m add separator
    $mb.file.m add command \
	-label {Import...} \
	-command "pad_import $PAD"
    $mb.file.m add command \
	-label {Import WWW Page...} \
	-command "make.url $PAD"
    $mb.file.m add separator

    #
    # Start submenu
    #
    $mb.file.m add cascade \
	-label {Starting view} \
	-menu $mb.file.m.start

    menu $mb.file.m.start \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.file.m.start add command \
        -label {Set Start View} \
	-command "pad_set_start_view $PAD"
    $mb.file.m.start add command \
        -label {Goto Start View} \
	-command "pad_goto_start_view $PAD"
    $mb.file.m.start add command \
        -label {Clear Start View} \
	-command "pad_clear_start_view $PAD"

    $mb.file.m add separator
    if {[winfo toplevel $PAD] != "."} {
	$mb.file.m add command \
	    -label {Close} \
	    -command "padCloseAck $PAD"
    }
    $mb.file.m add command \
	-label {Exit} \
	-accelerator {Ctl+Q} \
	-command "padExitAck $PAD"

    #
    # Edit submenu
    #
    menubutton $mb.edit \
	-menu "$mb.edit.m" \
	-text {Edit } \
	-underline {0} \
	-font $_font(menu) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    menu $mb.edit.m \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)

    $mb.edit.m add command \
	-label {Undo} \
	-state disabled \
	-command "pad_undo_it"
    $mb.edit.m add separator
    $mb.edit.m add command \
	-label {Cut} \
	-accelerator "Ctl+X" \
	-command "pad_cut $PAD"
    $mb.edit.m add command \
	-label {Copy} \
	-accelerator "Ctl+C" \
	-command "pad_copy $PAD"
    $mb.edit.m add command \
	-label {Paste} \
	-accelerator "Ctl+V" \
	-command "pad_paste $PAD"
    $mb.edit.m add command \
        -label {Delete} \
	-accelerator "Ctl+D" \
        -command "pad_delete $PAD"
    $mb.edit.m add command \
	-label {Make Alias} \
	-command "pad_make_alias $PAD"
    $mb.edit.m add separator
    $mb.edit.m add command \
        -label {Select All} \
	-accelerator "Ctl+A" \
	-command "pad_select $PAD all
                  pad_unselect $PAD status
                  pad_unselect $PAD statusbg
                  pad_unselect $PAD debugstatus
                  pad_unselect $PAD logo"
    $mb.edit.m add command \
	-label {Delete All} \
	-command "padClearAck $PAD"

    #
    # Object submenu
    #
    menubutton $mb.object \
	-menu "$mb.object.m" \
	-text {Object } \
	-underline {0} \
	-font $_font(menu) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    menu $mb.object.m \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg) \
	-postcommand "foreach item \[pad_sel $PAD\] {
                          if {\[$PAD type \$item\] == \"portal\"} {
			      if {\[$PAD ic \$item -title\] == \"\"} {
				  set foobar 0
			      } else {
				  set foobar 1
			      }
		          }
                      }"

    $mb.object.m add command \
	-label {Resize} \
	-command "
            makeResizeHandles $PAD
	 "
    $mb.object.m add command \
	-label {Reshape} \
	-accelerator "Ctl+R" \
	-command "
            makeReshaperHandles $PAD
	 "
    $mb.object.m add command \
	-label {Rotate} \
        -command "
	     if {\[pad_sel $PAD\] != \"\"} {
	         makeRotateHandles $PAD
	     }"
    $mb.object.m add command \
	-label {Flip Vertical} \
	-command "flipVertical $PAD"
    $mb.object.m add command \
	-label {Flip Horizontal} \
        -command "flipHorizontal $PAD"
    $mb.object.m add command \
	-label {Smooth} \
        -command "pad_smooth $PAD \[pad_sel $PAD\]"
    $mb.object.m add command \
	-label {Unsmooth} \
        -command "pad_unsmooth $PAD \[pad_sel $PAD\]"
    $mb.object.m add cascade \
        -label {Fade} \
        -menu $mb.object.m.fade
    #
    # Fade submenu
    #
    menu $mb.object.m.fade \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.object.m.fade add command \
        -label {None} \
        -command "foreach item \[pad_sel $PAD\] {
                      pad_fadenone $PAD \$item
        }
        changesMade $PAD true"
    $mb.object.m.fade add command \
        -label {Default} \
        -command "foreach item \[pad_sel $PAD\] {
                      pad_fadedefault $PAD \$item
        }
        changesMade $PAD true"
    $mb.object.m.fade add command \
        -label {When smaller} \
        -command "foreach item \[pad_sel $PAD\] {
                      pad_fadein $PAD \$item
        }
        changesMade $PAD true"
    $mb.object.m.fade add command \
        -label {When larger} \
        -command "foreach item \[pad_sel $PAD\] {
                      pad_fadeout $PAD \$item
        }
        changesMade $PAD true"
    $mb.object.m.fade add command \
        -label {Cross} \
        -command "pad_cross_fade $PAD
                  changesMade $PAD true"


    $mb.object.m add separator

    $mb.object.m add cascade \
	-label {Sticky} \
	-menu $mb.object.m.sticky
    #
    # Sticky submenu
    #
    menu $mb.object.m.sticky \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.object.m.sticky add command \
        -label {UnSticky} \
	-command "$PAD ic selected -sticky 0
	          changesMade $PAD true"
    $mb.object.m.sticky add command \
        -label {Sticky} \
	-command "$PAD ic selected -sticky 1
	          changesMade $PAD true"
    $mb.object.m.sticky add command \
        -label {Sticky X} \
	-command "$PAD ic selected -sticky x
	          changesMade $PAD true"
    $mb.object.m.sticky add command \
        -label {Sticky Y} \
	-command "$PAD ic selected -sticky y
	          changesMade $PAD true"
    $mb.object.m.sticky add command \
        -label {Sticky Z} \
	-command "$PAD ic selected -sticky z
	          changesMade $PAD true"
    $mb.object.m.sticky add command \
        -label {Sticky View} \
	-command "$PAD ic selected -sticky view
	          changesMade $PAD true"

    $mb.object.m add command \
	-label {Center} \
	-command "foreach id \[pad_sel $PAD\] {
                      pad_center_object $PAD \$id 0.95
                  }
		  pad_select $PAD \[pad_sel $PAD\]
	          changesMade $PAD true"
    $mb.object.m add command \
	-label {Make same size} \
	-command "pad_same_size $PAD \[pad_sel $PAD\]
	          changesMade $PAD true"
    $mb.object.m add separator

    $mb.object.m add command \
	-label {Properties...} \
	-accelerator {Ctl+P} \
	-command "snoopSelected $PAD"
    $mb.object.m add command \
	-label {Line Props...} \
	-command "prop_MakeLinePropWindow $PAD"
    $mb.object.m add command \
	-label {Hyperlink Props...} \
	-command "make.link $PAD"
    $mb.object.m add separator
    $mb.object.m add cascade \
	    -label {Portal} \
	    -menu $mb.object.m.portal

    #
    # Portals subsubmenu
    #
    menu $mb.object.m.portal \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.object.m.portal add command \
        -label {Layers...} \
        -command "layer_MakePortalLayerProps $PAD \[pad_sel $PAD\]"
    $mb.object.m.portal add command \
        -label {Polygon to Portal} \
        -command "pad_PolygonToPortal $PAD \[pad_sel $PAD\]"
    $mb.object.m.portal add command \
	-label {Drag in Pan mode} \
	-command "foreach item \[pad_sel $PAD\] {
                      if {\[$PAD type \$item\] == \"portal\"} {
			  $PAD addtag drag \$item
		      }
                  }
	          changesMade $PAD true"
    $mb.object.m.portal add command \
	-label {Don't drag in Pan mode} \
	-command "foreach item \[pad_sel $PAD\] {
                      if {\[$PAD type \$item\] == \"portal\"} {
			  $PAD deletetag drag \$item
		      }
                  }
	          changesMade $PAD true"
    $mb.object.m.portal add command \
	-label {Track View} \
	-command "foreach item \[pad_sel $PAD\] {
                      if {\[$PAD type \$item\] == \"portal\"} {
			  $PAD addtag trackView \$item
			  trackView $PAD \$item
		      }
                  }
	          changesMade $PAD true"
    $mb.object.m.portal add command \
	-label {Don't Track View} \
	-command "foreach item \[pad_sel $PAD\] {
                      if {\[$PAD type \$item\] == \"portal\"} {
			  $PAD deletetag trackView \$item
		      }
                  }
	          changesMade $PAD true"
    $mb.object.m.portal add command \
	-label {Filter} \
	-command "filterPortal $PAD"
    $mb.object.m.portal add command \
	-label {UnFilter} \
	-command "unfilterPortal $PAD"
    $mb.object.m.portal add checkbutton \
	-label {TitleBar} \
	-variable foobar \
	-command "foreach item \[pad_sel $PAD\] {
                      if {\[$PAD type \$item\] == \"portal\"} {
			  if {\$foobar} {
			      $PAD ic \$item -title Title
			  } else {
			      $PAD ic \$item -title {}
			  }
		      }
                  }"

    proc filterPortal {PAD} {
	set portal ""
	set items ""
	foreach item [pad_sel $PAD] {
	    set type [$PAD type $item]
	    if {($type == "portal") && ($portal == "")} {
		set portal $item
		$PAD addtag "trackView" $portal
				# Make sure "filter" tag comes before "drag" tag on portal 
		$PAD deletetag "drag" $portal
		$PAD addtag "filter" $portal
		$PAD addtag "drag" $portal
		$PAD ic $portal -info "open [list [$PAD bbox $portal]]"
		trackView $PAD $portal
		$PAD ic $portal -visiblelayers "filter"
		set viewlayers [$PAD ic 1 -visiblelayers]
		$PAD ic 1 -visiblelayers "$viewlayers -filter"
	    } else {
		lappend items $item
		pad_unselect $PAD $item
	    }
	}
	foreach item $items {
	    $PAD ic $item -layer "filter"
	}
    }

    proc unfilterPortal {PAD} {
	foreach item [pad_sel $PAD] {
	    set type [$PAD type $item]
	    if {$type == "portal"} {
		$PAD ic $item -visiblelayers "all - status"
		$PAD deletetag trackView $item
		$PAD deletetag filter $item
	    } else {
		$PAD ic $item -layer "main"
	    }
	}
    }

    #
    # Arrange submenu
    #
    menubutton $mb.arrange \
	-menu "$mb.arrange.m" \
	-text {Arrange } \
	-underline {0} \
	-font $_font(menu) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    menu $mb.arrange.m \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)

    $mb.arrange.m add command \
	-label {Bring to Front   } \
	-accelerator {Ctl+F} \
	-command "$PAD raise selected
	          changesMade $PAD true"
    $mb.arrange.m add command \
	-label {Send to Back} \
	-accelerator {Ctl+B} \
	-command "$PAD lower selected
	          changesMade $PAD true"
    $mb.arrange.m add command \
	-label {Bring Closer} \
	-command "$PAD raise -one selected
	          changesMade $PAD true"
    $mb.arrange.m add command \
	-label {Send Further} \
	-command "$PAD lower -one selected
	          changesMade $PAD true"

    $mb.arrange.m add separator

    $mb.arrange.m add command \
	-label {Group} \
	-accelerator "Ctl+G" \
	-command "set items \[pad_sel $PAD\]
		  eval pad_unselect $PAD \$items
		  set group \[pad_group $PAD \$items\]
                  if {\$group != \"\"} {pad_select $PAD \$group}"
    $mb.arrange.m add command \
	-label {UnGroup} \
	-accelerator "Ctl+U" \
	-command "set items \[pad_sel $PAD\]
		  eval pad_unselect $PAD \$items
		  set items \[pad_ungroup $PAD \$items\]
                  if {\$items != \"\"} {eval pad_select $PAD \$items}"

    $mb.arrange.m add separator

    $mb.arrange.m add command \
	-label {Lock} \
	-command "foreach id \[pad_sel $PAD\] {
                      $PAD ic \$id -lock 1
                  }
                  pad_unselect $PAD all
	          changesMade $PAD true"
    $mb.arrange.m add command \
	-label {UnLock All} \
	-command "$PAD ic all -lock 0
                  $PAD ic logo -lock 1
                  $PAD ic status -lock 1
                  $PAD ic statusbg -lock 1
                  $PAD ic debugstatus -lock 1
	          changesMade $PAD true"
    $mb.arrange.m add command \
	-label {Hide} \
	-command "foreach id \[pad_sel $PAD\] {
                      $PAD ic \$id -events 0 -transparency 0
                      $PAD addtag hide \$id
                  }
                  pad_unselect $PAD all
	          changesMade $PAD true"
    $mb.arrange.m add command \
	-label {Show All} \
	-command "$PAD ic all -events 1 -transparency 1
                  $PAD deletetag hide all
	          changesMade $PAD true"

    #
    # Font submenu
    #
    menubutton $mb.font \
	-menu "$mb.font.m" \
	-text {Font } \
	-underline {0} \
	-font $_font(menu) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    menu $mb.font.m \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.font.m add cascade \
	    -label "Size" \
	    -menu $mb.font.m.size
    $mb.font.m add cascade \
	    -label "Name" \
	    -menu $mb.font.m.name
    $mb.font.m add cascade \
	    -label "Style" \
	    -menu $mb.font.m.style

				# Create Font size submenu
    menu $mb.font.m.size \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    foreach size {6 8 10 12 14 18 24 38 42 50 60 72 144 288} {
        $mb.font.m.size add radiobutton \
	    -variable _pad(TextSize) \
            -label $size \
            -command "pad_set_font_size $PAD $size"
    }

				# Create Font name submenu
    menu $mb.font.m.name \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)

    set fontnames "System"
    foreach font [lsort [$PAD font names]] {
	lnewappend fontnames [pad_font_name $font]
    }
    foreach font $fontnames {
        $mb.font.m.name add radiobutton \
	    -variable _pad(FontNameMenu) \
            -label $font \
            -command "pad_set_font $PAD $font $mb"
    }
    set _pad(FontNameMenu) $_pad(Font)
    set _pad(FontName) $_pad(Font)

				# Create Font style submenu
    menu $mb.font.m.style \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg) \
	-postcommand "pad_create_font_style_menu $PAD $mb"
    set _pad(FontStyle) "Normal"

    proc pad_create_font_style_menu {PAD mb} {
	global Fonts _pad

	$mb.font.m.style delete 0 end
	set styles ""
	foreach font [$PAD font names] {
	    if {[pad_font_name $font] == $_pad(FontName)} {
		set style ""
		if {[pad_font_bold $font]} {
		    set style "Bold"
		}
		if {[pad_font_italic $font]} {
		    set tmp "Italic"
		    set style "$style$tmp"
		}
		if {$style == ""} {
		    set style "Plain"
		}
		lnewappend styles $style
	    }
	}
	foreach style [lsort $styles] {
	    $mb.font.m.style add radiobutton \
		-variable _pad(FontStyle) \
		-label $style \
		-command "pad_set_font_style $PAD $style $mb"
	}
    }

    proc pad_set_font_style {PAD style mb} {
	global _pad

	set _pad(FontStyle) $style
	pad_set_font $PAD $_pad(FontName) $mb
    }

    proc pad_set_font_size {PAD size} {
	global _pad

	set _pad(TextSize) $size
	set zoom [$PAD getzoom]
	set selection [pad_sel $PAD]
	foreach item "$selection [$PAD focus]" {
	    set type [$PAD type $item]
	    if {($type == "text") || ($type == "textfile")} {
		set place [$PAD ic $item -place]
		set z [lindex $place 2]
		set scale [expr double($_pad(TextSize)) / ($z * $zoom)]
		$PAD scale $item $scale
		if {[lmember $selection $item]} {
		    pad_unselect $PAD $item
		    pad_select $PAD $item
		}
	    }
	}
    }

    proc pad_set_font {PAD font_name mb} {
	global _pad Fonts

				# When switching fonts, go back to "Normal" style
	if {$font_name != $_pad(FontName)} {
	    set _pad(FontName) $font_name
	    pad_create_font_style_menu $PAD $mb
	    set _pad(FontStyle) [$mb.font.m.style entrycget 1 -label]
	}

				# First try requested style
	set style $_pad(FontStyle)
	if {$style == "Normal"} {
	    set font "$font_name-1"
	} else {
	    set style [string tolower $style]
	    set font "$font_name-$style-1"
	}
				# Now update each item to use the new font
	set _pad(Font) $font
	set selection [pad_sel $PAD]
	foreach item "$selection [$PAD focus]" {
	    catch {$PAD ic $item -font $font}
	    if {[lmember $selection $item]} {
		pad_unselect $PAD $item
		pad_select $PAD $item
	    }
	    if {[$PAD type $item] == "group"} {
		pad_set_group_option $PAD $item "-font" $font
	    }
	}
    }

    #
    # Tools submenu
    #

    menubutton $mb.tools \
	-menu "$mb.tools.m" \
	-text {Tools } \
	-underline {0} \
	-font $_font(menu) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    menu $mb.tools.m \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.tools.m add command \
	-label {Drawing...} \
	-accelerator {Ctl+O} \
	-command "set menu(.draw) 1; controlWindow $PAD .draw"
    $mb.tools.m add command \
	-label {Colors...} \
	-accelerator {Ctl+L} \
	-command "set menu(.ce) 1; controlWindow $PAD .ce"
    $mb.tools.m add command \
        -label {Layers...} \
        -command "set menu(.layer) 1; controlWindow $PAD .layer"
    $mb.tools.m add command \
        -label {Selection...} \
        -command "set menu(.selection) 1; controlWindow $PAD .selection"
    $mb.tools.m add command \
        -label {Properties...} \
        -command "make.snoopLens $PAD"
    $mb.tools.m add cascade \
	-label {Layout} \
	-menu $mb.tools.m.layout
    #
    # Layout submenu
    #
    menu $mb.tools.m.layout \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.tools.m.layout add command \
        -label {Align...} \
	-command "set menu(.align) 1; controlWindow $PAD .align"
    $mb.tools.m.layout add command \
        -label {Distribute...} \
	-command "set menu(.distribute) 1; controlWindow $PAD .distribute"

    $mb.tools.m add command \
	-label {Search...} \
	-accelerator {Ctl+S} \
	-command "set menu(.search) 1; controlWindow $PAD .search"
    $mb.tools.m add command \
	-label {Animation...} \
	-command "set menu(.animate) 1; controlWindow $PAD .animate"
    $mb.tools.m add command \
	-label {Navigation...} \
	-command "set menu(.bookmark) 1; controlWindow $PAD .bookmark"
    $mb.tools.m add command \
        -label {Web Pages...} \
        -command "set menu(.html) 1; controlWindow $PAD .html"
    $mb.tools.m add separator
    $mb.tools.m add command \
	-label {Preferences...} \
	-command "set menu(.pref) 1; controlWindow $PAD .pref"

    if {$tcl_platform(platform) == "windows"} "
        $mb.tools.m add command \
	    -label {Tcl Console} \
	    -command {console show}
    "

    #
    # Debug submenu
    #

    global renderBoundingBoxes debugRegion
    global fastPan
    global tcl_platform

    set renderBoundingBoxes 0
    set _pad(debugEvents) 0
    set debugRegion 0

    if { "$tcl_platform(platform)" == "unix" } {
      set fastPan 1
    } else {
      set fastPan 0
    }

    menubutton $mb.debug \
	-menu "$mb.debug.m" \
	-text {Debug } \
	-underline {2} \
	-font $_font(menu) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    menu $mb.debug.m \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.debug.m add command \
	-label {Event Debugger...} \
	-command "set menu(.debugevent) 1; controlWindow $PAD .debugevent"
    $mb.debug.m add separator
    $mb.debug.m add command \
	-label {Print Info} \
	-command "
	    $PAD printinfo
            set pw \[winfo width $PAD\]
            set ph \[winfo height $PAD\]
            puts \"Pad Dimensions = (\$pw x \$ph)\"
            puts \"Tk focus=\[focus\], Pad focus=\[$PAD focus\]\"
	"
    $mb.debug.m add command \
	-label {Print Tree} \
	-command "$PAD printtree"
    $mb.debug.m add command \
	-label {Render Scene Once} \
	-command "$PAD damage"
    $mb.debug.m add separator
    $mb.debug.m add checkbutton \
	-label {Render Bounding Boxes} \
	-variable renderBoundingBoxes \
	-command "$PAD config -debugBB \$renderBoundingBoxes"
    $mb.debug.m add checkbutton \
	-label {Fast Pan} \
	-variable fastPan \
	-command "$PAD config -fastPan \$fastPan"
    $PAD config -fastPan $fastPan
    $mb.debug.m add checkbutton \
	-label {Debug Events} \
	-variable _pad(debugEvents) \
	-command "if {\$_pad(debugEvents)} {
                      $PAD config -debugEvent 1
                  } else {
                      $PAD config -debugEvent {}
                  }"
    $mb.debug.m add checkbutton \
	-label {Debug Status Line} \
	-variable _pad(DebugStatusLine) \
	-command "pad_update_debug_status $PAD"
    $mb.debug.m add checkbutton \
	-label {Debug Region Management} \
	-variable debugRegion \
	-command "$PAD config -debugRegion \$debugRegion"
    $mb.debug.m add checkbutton \
	-label {Double Buffer} \
	-variable _pad(DoubleBuffer) \
	-command "$PAD config -doubleBuffer \$_pad(DoubleBuffer)"
    $mb.debug.m add checkbutton \
	-label {X Synchronize} \
	-variable _pad(XSync) \
	-command "$PAD config -sync \$_pad(XSync)"

    #
    # Demo submenu
    #
    menubutton $mb.demo \
	-menu "$mb.demo.m" \
	-text {Demo } \
	-underline {0} \
	-font $_font(menu) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    menu $mb.demo.m \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)

    #
    # HTML subsubmenu
    #
    menu $mb.demo.m.html \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.demo.m.html add command \
	-label {Ben (local)} \
	-command "import $PAD $env(PADHOME)/html/home-page.html"
    $mb.demo.m.html add command \
	-label {Pad++} \
	-command "import $PAD http://www.cs.unm.edu/pad++/"
    $mb.demo.m.html add command \
	-label {UNM CS Dept.} \
	-command "import $PAD http://www.cs.unm.edu/"
    $mb.demo.m.html add command \
	-label {NYU MRL} \
	-command "import $PAD http://www.mrl.nyu.edu/"
    $mb.demo.m.html add command \
	-label {Yahoo} \
	-command "import $PAD http://www.yahoo.com/"
    $mb.demo.m add cascade \
	    -label {Web Pages} \
	    -menu $mb.demo.m.html

    #
    # Poetry subsubmenu
    #
    menu $mb.demo.m.poetry \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.demo.m.poetry add command \
	-label {The Search (v1)} \
	-command "import_poetry $PAD $env(PADHOME)/draw/the_search_v1.pad"
    $mb.demo.m.poetry add command \
	-label {The Search (v2)} \
	-command "import_poetry $PAD $env(PADHOME)/draw/the_search_v2.pad"
    $mb.demo.m.poetry add command \
	-label {The Search (v3)} \
	-command "import_poetry $PAD $env(PADHOME)/draw/the_search_v3.pad"
    $mb.demo.m.poetry add command \
	-label {The Red Wheelbarrow} \
	-command "import_poetry $PAD $env(PADHOME)/draw/red_wheelbarrow.pad"
    $mb.demo.m.poetry add command \
	-label {About Poetry} \
	-command "import_poetry $PAD $env(PADHOME)/draw/about_poetry.pad"
    $mb.demo.m add cascade \
	    -label {Poetry} \
	    -menu $mb.demo.m.poetry

		# The poetry demos are deep, so make sure we aren't
		# too far zoomed in before we import them.
    proc import_poetry {PAD filename} {
	set view [$PAD getview]
	set zoom [lindex $view 2]
	if {$zoom > 1.0} {
	    set zoom 1.0
	}
	$PAD moveto [lindex $view 0] [lindex $view 1] $zoom 1000
	import $PAD $filename 0
    }

    $mb.demo.m add command \
	-label {Full Screen} \
	-accelerator {Ctl-T} \
	-command "pad_toggle_full_screen $PAD"

    # Telnet - only under unix
    if {$tcl_platform(platform) == "unix"} {
	$mb.demo.m add command \
	    -label {Telnet} \
	    -command "ptelnet_quickdialog $PAD"
    }

    $mb.demo.m add command \
	-label {Outline Pages} \
	-command "import $PAD \$env(PADHOME)/draw/pad.outline 0"

    $mb.demo.m add command \
	-label {Sample picture} \
	-command "import $PAD \$env(PADHOME)/draw/demo.pad 0"

    $mb.demo.m add checkbutton \
	    -label {Grid} \
	    -command "toggleGrid $PAD" \
            -variable _grid(status)
    set _grid(status) 0

    #
    # Window Shape, only supported in unix
    #
    if {$tcl_platform(platform) == "unix"} {
       $mb.demo.m add cascade \
          -label {Window Shape} \
          -menu $mb.demo.m.shape

      menu $mb.demo.m.shape \
	-font $_font(tools) \
        -bg $_color(menubg) \
        -activebackground $_color(menuactivebg)

      foreach i {card card1 card2 card3 cloud pad tv horiz_page petal zig} {
        $mb.demo.m.shape add command \
          -label $i \
          -command "source $env(PADHOME)/draw/window_shapes/$i.tcl ; \
                 $PAD windowshape \$inner_coords \$outer_coords"
      }
      $mb.demo.m.shape add command \
        -label {None} -command "$PAD windowshape {} {}"

      $mb.demo.m.shape add command \
        -label {From Selection} \
        -command "set c \[$PAD coords selected\] ; $PAD windowshape \$c \$c"
    }


    $mb.demo.m add cascade \
	    -label {Widgets} \
	    -menu $mb.demo.m.widgets

    #
    # Widgets subsubmenu
    #
    menu $mb.demo.m.widgets \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.demo.m.widgets add command \
	-label {Button} \
	-command "pad_pos_demo_obj $PAD \[$PAD create button -fill lightblue3 -text Hello\] 2.0"
    $mb.demo.m.widgets add command \
	-label {Check Button} \
	-command "pad_pos_demo_obj $PAD \[$PAD create checkbutton -fill lightblue3 -text Hello\] 2.0"
    $mb.demo.m.widgets add command \
	-label {Scale} \
	-command "pad_pos_demo_obj $PAD \[$PAD create scale -fill lightblue3\] 3.0"
    $mb.demo.m.widgets add command \
	-label {Graph} \
	-command "pad_pos_demo_obj $PAD \[$PAD create graph -fill lightblue3\] 3.0"
#    $mb.demo.m.widgets add command \
#	-label {Choice} \
#	-command "pad_pos_demo_obj $PAD \[$PAD create choice -fill lightblue3 \
#	-choices {First Second Third Fourth} -value First \] 1.0"
#    $mb.demo.m.widgets add command \
#	-label {Frame} \
#	-command "set frame \[pad_pos_demo_obj $PAD \[$PAD create frame -fill lightblue3\]\]
#	          pad_demo_frame $PAD \$frame"

    proc pad_demo_frame {PAD frame} {
	$PAD bind $frame <ButtonPress-1> {
	    set relief [%W ic %O -relief]
	    switch -exact $relief {
		raised  {%W ic %O -relief flat}
		flat    {%W ic %O -relief sunken}
		sunken  {%W ic %O -relief groove}
		groove  {%W ic %O -relief ridge}
		ridge   {%W ic %O -relief raised}
	    }
	} Widget
    }

    proc pad_pos_demo_obj {PAD id {mag 1.0}} {
	set view [$PAD getview]
	set x [lindex $view 0]
	set y [lindex $view 1]
	set zoom [lindex $view 2]
	$PAD ic $id -place "$x $y [expr $mag / $zoom]"
	return $id
    }
    
    $mb.demo.m add separator

    $mb.demo.m add command \
	-label {Chart Lens} \
	-command "
	    makeChartTest $PAD
	    changesMade $PAD true
	"
    $mb.demo.m add command \
	-label {Number Lens} \
	-command "
	    makeNumberTest $PAD
	    changesMade $PAD true
	"
    $mb.demo.m add command \
	-label {Magnifying Lens} \
	-command "
	    makeMagnifyTest $PAD
	    changesMade $PAD true
	"
    $mb.demo.m add command \
	-label {Clock Lens} \
	-command "
	    makeClockTest $PAD
	    changesMade $PAD true
	"
    
    $mb.demo.m add separator
    $mb.demo.m add command \
	-label {Animated Rat} \
	-command "
	    makebob $PAD
	    changesMade $PAD true
	"
    $mb.demo.m add command \
	-label {Make Cheese} \
	-command "
	    makecheese $PAD
	    changesMade $PAD true
	"
#    $mb.demo.m add command \
#	-label {Time Line} \
#	-command {
#	    if {[winfo exists .t]} {raise .t; return}
#	    toplevel .t
#	    wm minsize .t 10 10
#	    wm title .t "Pad++ Timeline Demo"
#	    pad .t.pad
#	    button .t.b -text "Close"  -command {destroy .t}
#	    pack .t.pad -expand t -fill both
#	    pack .t.b -fill x
#	    bindPan .t.pad
#	    bindZoom .t.pad
#	    bindTimeline .t.pad
#	    update
#	    .t.pad read $env(PADHOME)/draw/history.pad
#	    .t.pad ic all -maxsize -1
#           import .t.p $env(PADHOME)/draw/history.timeline
#	    bindTimeline .t.pad
#	}

    #
    # Help submenu
    #
    menubutton $mb.help \
	-menu "$mb.help.m" \
	-text {Help} \
	-underline {0} \
	-font $_font(menu) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    menu $mb.help.m \
	-font $_font(tools) \
	-bg $_color(menubg) \
	-activebackground $_color(menuactivebg)
    $mb.help.m add command \
	-label {Pad++ Help...} \
	-command {
	    if {[winfo exists .help]} {
		raise .help
		return
	    }
	
	    toplevel .help
	    pad .help.pad -bg $_color(padbg) -gamma $_pad(Gamma) -defaultEventHandlers 0
	    button .help.dismiss -text "Close" -command "destroy .help" \
		-font $_font(tools) \
	        -bg $_color(toolbg) -activebackground $_color(toolactivebg) \
                -highlightbackground $_color(toolbg)
	
	    pack .help.pad -expand true -fill both
	    pack .help.dismiss -fill x

	    .help.pad create text -text "Loading..." -place "0 0 1" -anchor center \
		    -font "Times-20" -tags "loading"
	    set x [winfo x .]
	    set y [winfo y .]
	    wm geometry .help +[expr $x + 100]+[expr $y + 100]
	    wm title .help "Pad++ Tour"
	    wm minsize .help 50 50
	    update

	    .help.pad modifier create Run
	    .help.pad modifier set Run
	    bindPan .help.pad
	    bindZoom .help.pad

	    pad_help .help.pad "Pad++ Tour" $env(PADHOME)/draw/help.txt
	    eval .help.pad centerbbox [.help.pad bbox all]
	
	    .help.pad delete loading
	    .help.pad update
	}
    $mb.help.m add command \
	-label {PadDraw Help...} \
	-command {
	    if {[winfo exists .help]} {
		raise .help
		return
	    }
	
	    toplevel .help
	    pad .help.pad -bg $_color(padbg) -gamma $_pad(Gamma) -defaultEventHandlers 0
	    button .help.dismiss -text "Close" -command "destroy .help" \
		-font $_font(tools) \
	        -bg $_color(toolbg) -activebackground $_color(toolactivebg) \
                -highlightbackground $_color(toolbg)
	
	    pack .help.pad -expand true -fill both
	    pack .help.dismiss -fill x

	    .help.pad create text -text "Loading..." -place "0 0 1" \
		    -font "Times-20" -tags "loading" -anchor center
	    set x [winfo x .]
	    set y [winfo y .]
	    wm geometry .help +[expr $x + 100]+[expr $y + 100]
	    wm title .help "PadDraw Help"
	    wm minsize .help 50 50
	    update

	    pad_help .help.pad "PadDraw Help" $env(PADHOME)/draw/paddraw_help.txt
	    eval .help.pad centerbbox [.help.pad bbox all]
	    bindPan .help.pad
	    bindZoom .help.pad
	    .help.pad modifier create Run
	    .help.pad modifier set Run
	    .help.pad delete loading
	    after 0 {.help.pad delete statusbg}
	}
    $mb.help.m add command \
	-label {About...} \
	-command {
	    if {[winfo exists .about]} {
		raise .about
		return
	    }
	
	    toplevel .about
	    pad .about.pad -bg $_color(padbg) -gamma $_pad(Gamma) -defaultEventHandlers 0
	    pad_InitOptions .about.pad
	    button .about.dismiss -text "Close" -command "destroy .about" \
		-font $_font(tools) \
	        -bg $_color(toolbg) -activebackground $_color(toolactivebg) \
                -highlightbackground $_color(toolbg)
	
	    pack .about.pad -expand true -fill both
	    pack .about.dismiss -fill x

	    .about.pad create text -text "Loading..." -place "0 0 1" \
		    -font "Times-20" -tags "loading" -anchor center

	    bindPan .about.pad
	    bindZoom .about.pad
	    bindLink .about.pad
	    .about.pad modifier create Run
	    .about.pad modifier set Run
	
	    set x [winfo x .]
	    set y [winfo y .]
	    wm geometry .about +[expr $x + 100]+[expr $y + 100]
	    wm title .about "About Pad++"
	    wm minsize .about 50 50
	    source $env(PADHOME)/draw/window_shapes/pad.tcl
            .about.pad windowshape $inner_coords $outer_coords
	    update
	    .about.pad read $env(PADHOME)/draw/startup.pad
	    link_ConvertFormat .about.pad
	    .about.pad delete loading
	}

    #
    # pack Menubar
    #
    pack $mb -side top -before $PAD -fill x
    pack $mb.file -side left
    pack $mb.edit -side left
    pack $mb.arrange -side left
    pack $mb.object -side left
    pack $mb.font -side left
    pack $mb.tools -side left
    pack $mb.debug -side left
    pack $mb.help -side right
    pack $mb.demo -side right
}

#
# Make a new pad drawing window
#
proc menubarNewPad {PAD} {
    global _pad

    if {![info exists _pad(padnum)]} {
	set _pad(padnum) 0
    } else {
	incr _pad(padnum)
    }
    set toplevel .pad$_pad(padnum)
    toplevel $toplevel
    startPadDraw $toplevel.pad
}

#
# Compute menubar name
#
proc menubarGetName {PAD} {
    set toplevel [winfo toplevel $PAD]
    if {$toplevel == "."} {
	set mb .menubar
    } else {
	set mb $toplevel.menubar
    }
    return $mb
}

#
# make.url - makes a window which allows users to type in a URL
#            for importing onto the Pad surface. Obtained by choosing
#            the 'Open Location...' option on the file menu [next to the
#            'Import' option
#
proc make.url {PAD} {
    global _html _color _font

    if {[winfo exists .url]} { 
        wm deiconify .url
        raise .url
        return
    }

    # make new toplevel window
    toplevel .url 
    wm resizable .url 0 0
    wm title .url "Pad++: Open Location"

    # Make frame with the text entry area
    frame .url.entry -bg $_color(toolbg)
    label .url.entry.label -text "Location (URL):" \
	-font $_font(tools) \
	-bg $_color(toolbg)
    entry .url.entry.text -relief sunken -width 40 \
	-font $_font(tools) \
	-bg $_color(toolbg) \
	-highlightbackground $_color(toolbg)
    if {[info exists _html(url)]} {
	.url.entry.text insert 0 $_html(url)
    }
					# Fix broken default backspace/delete bindings
    bind .url.entry.text <KeyPress-BackSpace> {%W delete [expr [%W index insert] - 1]; break}
    bind .url.entry.text <KeyPress-Delete> {%W delete [expr [%W index insert] - 1]; break}
					# Bind return key to load URL
    bind .url.entry.text <Key-Return> "
        set temp \[.url.entry.text get\]
        set _html(url) \$temp
        destroy .url
        import $PAD \$temp
    "
    pack .url.entry.label .url.entry.text -side left -fill x -expand t

    # make frame with the OK, Clear, Close buttons
    frame .url.buttons -bg $_color(toolbg)

    button .url.buttons.ok -text "OK" \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -activebackground $_color(menuactivebg) \
	    -highlightbackground $_color(toolbg) \
	    -command "
                set temp \[.url.entry.text get\]
                set _html(url) \$temp
                destroy .url
                import $PAD \$temp
	    "
    button .url.buttons.clear -text "Clear" \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -activebackground $_color(menuactivebg) \
	    -highlightbackground $_color(toolbg) \
	    -command ".url.entry.text delete 0 10000"
    button .url.buttons.cancel -text "Close" \
	    -font $_font(tools) \
	    -bg $_color(toolbg) -activebackground $_color(menuactivebg) \
	    -highlightbackground $_color(toolbg) \
	    -command "wm withdraw .url"

    # pack the buttons
    pack .url.buttons.ok .url.buttons.clear .url.buttons.cancel \
	    -side left -fill x -expand t

    # pack frames
    pack .url.entry -ipady 30 -ipadx 10
    pack .url.buttons -fill x -side bottom
}

#
# Save the specified Pad++ surface into the specified filename.
# If filename is blank, ask for one.
#
proc padSave {PAD filename} {
    global _file _pad Pad_Version

    if {$filename == ""} {
	set filename [make.file "Select a file to write" $filename "write"]
    }
    if {$filename != ""} {
	switch $_file(Format) {
	    "Text"               {set format "-format text"}
	    "Binary Interchange" {set format "-format binary-interchange"}
	    default              {set format "-format binary-native"}
	}
	if {$_file(RelativePathnames)} {
	    set location "-relative"
	} else {
	    set location ""
	}
	catch {exec mv -f $filename "$filename~"}
	eval $PAD write $format $location $filename
	set _pad(PadTitle$PAD) "Pad++ v$Pad_Version ($filename)"
	wm title [winfo toplevel $PAD] $_pad(PadTitle$PAD)

	changesMade $PAD false
    }

    return $filename
}

#
# Ask for a filename, and save the Pad++ surface into it.
#
proc padSaveAs {PAD filename} {
    global _pad Pad_Version _file

    set filename [make.file "Select a file to write" $filename "write"]
    if {$filename != ""} {
	padSave $PAD $filename
    }

    return $filename
}

#
# Main window has been iconified,
# so unmap all subsidary windows
#

proc iconifyPadWindow {PAD} {
    global menu

    foreach window [array name menu] {
	if {$menu($window) == 1} {
	    wm withdraw $window
	}
    }
}

#
# Main window has been deiconified,
# so restore all subsidary windows
#
proc uniconifyPadWindow {PAD} {
    global menu

    foreach window [array name menu] {
	if {$menu($window) == 1} {
	    wm deiconify $window
	}
    }
}

#
# Open tools windows as specified in the global menu() variable.
# For every entry in menu(), call controlWindow on that entry.
#
proc controlToolWindows {PAD} {
    global menu

    foreach window [array name menu] {
	controlWindow $PAD $window
    }
}

#
# Create window, raise or remove it, depending on menu($window)
#   If menu($window) is false, destroy window
#   If menu($window) is true and window exists, raise window
#   If menu($window) is true and window doesn't exist, create window
#
proc controlWindow {PAD window} {
    global menu

    if {$menu($window)} {
	if {[winfo exists $window]} {
	    raise $window
	} else {
	    make$window $PAD
	}
    } else {
	if {[winfo exists $window]} {
	    destroy $window
	}
    }
}

#
# Call to update whether the pad buffer has been
# changed relative to the file.
#
proc changesMade {PAD value {keepUndo 0}} {
    global _pad

    set _pad(ChangesMade$PAD) $value

    set mb [menubarGetName $PAD]
    if {![winfo exists $mb]} {
	return
    }
    if {$_pad(ChangesMade$PAD) == "true"} {
	$mb.file.m entryconfigure "Save*" -label "Save (needed)"
        if {$keepUndo == 0} {
            pad_undo_cancel
        }
    } else {
	$mb.file.m entryconfigure "Save*" -label "Save"
    }
}

proc toggleGrid {PAD} {
    global _grid
    
    set obj ""
    if {[info exists _grid(obj)]} {
	set obj $_grid(obj)
    }

    if {$obj == ""} {
	makeGrid $PAD
    } else {
	$PAD delete $obj
    }
}

proc setRenderLevel {PAD value} {
    $PAD config -defaultRenderLevel $value
}

proc setRefinementDelay {PAD value} {
    $PAD config -refinementDelay [expr 200 * $value]
}

proc setDissolveSpeed {PAD value} {
    global _pad

    # configure dissolve speed for refinements
    $PAD config -dissolveSpeed $value

    # configure dissolve speed for PadDraw uses
    set _pad(DissolveSpeed) $value
}

proc padClearAck {PAD} {
    global _color

    set response [pad_message \
"OK to clear the
Pad++ surface?" \
OK Cancel]

    if {$response == "OK"} {
	pad_fade_delete $PAD all
	changesMade $PAD true
    }
}

#
# Manage visibility and geometry of window.  By default, remember
# whether window should be appear or not, and position and size of it.
# This information gets stored in ~/.padsetup when PadDraw exits
# (see padExitCleanup).
#
# If manage_dimensions is 0, then the visibility and position of the window is saved,
# but the dimensions are not.  This is appropriate for windows such as
# the layers window which has a different geometry every time it is
# used.  If manage_window is 0, then the visibility of the window is
# not saved, and it never automatically appears when PadDraw is started.
#
proc pad_ManageWindow {window {manage_dimensions 1} {manage_window 1}} {
    global geometry menu

    if {[info exists geometry($window)]} {
	wm geometry $window $geometry($window)
    } else {
	set x [winfo x .]
	set y [winfo y .]
	wm geometry $window +[expr $x + 100]+[expr $y + 100]
    }

    if {$manage_dimensions == 0} {
	set geometry($window.dimensions) 0
    }

    bind $window <Destroy> "pad_ManageWindowDestruction %W $window $manage_dimensions $manage_window"
}

proc pad_ManageWindowDestruction {window orig_window manage_dimensions manage_window} {
    global menu geometry

    if {"$window" == "$orig_window"} {
			# Tcl7.5p1/Tk4.1p1 had a bug where in a <Destroy> event
			# handler for a window, the window was no longer available,
			# so the geometry won't be remembered.  In this case, just
			# catch the error, and ignore the geometry.
	if {[catch {set geo [wm geometry $window]}]} {
	    catch {unset geometry($window)}
	    set menu($window) 0
	    return
	}
	if {$manage_dimensions} {
	    set geometry($window) $geo
	} else {
	    regsub {.*x[0-9]*} $geo "" geometry($window)
	}
	if {$manage_window} {
	    set menu($window) 0
	}
    }
}

#
# Check if we should confirm before closing
#    
proc padCloseAck {PAD} {
    global _pad _draw

    if {[llength $_pad(Pads)] == 1} {
	padExitAck $PAD
	return
    }

    if {$_pad(ChangesMade$PAD) == "true"} {
	set response [pad_message "OK to close Pad++ window?" OK Cancel]
    } else {
	set response "OK"
    }

    if {$response == "OK"} {
	set toplevel [winfo toplevel $PAD]
	destroy $toplevel
	set _pad(Pads) [lremove $_pad(Pads) $PAD]
	if {$_draw(PAD) == $PAD} {
	    destroy .draw
	}
    }
}

#
# Confirm before exiting
#
proc padExitAck {PAD} {
    set response [pad_message "OK to exit Pad++?" OK Cancel]

    if {$response == "OK"} {
	destroy .
    }
}

proc padExitCleanup {PAD} {
    global env menu geometry Pad_Version _pad

    # Only store geometry information about primary pad
    if {$PAD != [lindex $_pad(Pads) 0]} {
	return
    }

    #
    # Write out setup file that contains window positions, etc.
    #
    set fid [open "$env(HOME)/.padsetup" "w"]
    puts $fid "# $Pad_Version"
    puts $fid "global menu geometry"


    #
    # First write out description of whether window is visible or not
    #
    foreach window [array name menu] {
	# Don't include file and bookmark windows
	if {[string first "file" $window] != -1 || \
	    [string first "bookmark" $window] != -1} {
	    continue	
	}

	puts $fid "set menu($window) $menu($window)"
    }

    #
    # Special case for main Pad++ window
    #
    set position [string range [wm geometry .] [string first + [wm geometry .]] end]
    puts $fid "set geometry($PAD) [winfo width $PAD]x[winfo height $PAD]$position"

    #
    # Write out geometry of all windows that have it
    #
    foreach window [array name menu] {
	if {[winfo exists $window]} {
	    set geo [wm geometry $window]
	    if {[info exists geometry($window.dimensions)] && ($geometry($window.dimensions) == 0)} {
		regsub {.*x[0-9]*} $geo "" geo
	    }
	    puts $fid "set geometry($window) $geo"
	} elseif {[info exists geometry($window)]} {
	    puts $fid "set geometry($window) $geometry($window)"
	}
    }

    #
    # Write out general preferences
    #
    puts $fid "set _pad(AnimationSpeed) $_pad(AnimationSpeed)"
    puts $fid "set _pad(ZoomSpeed) $_pad(ZoomSpeed)"
    puts $fid "set _pad(ZoomStyle) $_pad(ZoomStyle)"
    puts $fid "set _pad(DesiredFrameRate) $_pad(DesiredFrameRate)"
    puts $fid "set _pad(SmallObjSize) $_pad(SmallObjSize)"
    puts $fid "set _pad(MediumObjSize) $_pad(MediumObjSize)"
    puts $fid "set _pad(Help) $_pad(Help)"
    puts $fid "set _pad(StartupTips) $_pad(StartupTips)"
    puts $fid "set _pad(8bitWarning) $_pad(8bitWarning)"
    puts $fid "set _pad(StatusLine) $_pad(StatusLine)"
    puts $fid "set _pad(TextBuffer) $_pad(TextBuffer)"
    puts $fid "set _pad(ShowCoords) $_pad(ShowCoords)"
    puts $fid "set _pad(ShowLinks) $_pad(ShowLinks)"
    puts $fid "set _pad(Selection) $_pad(Selection)"
    puts $fid "set _pad(SelectGroupMembers) $_pad(SelectGroupMembers)"
    puts $fid "set _pad(SelectLargeItems) $_pad(SelectLargeItems)"
    puts $fid "set _pad(SelectSmallItems) $_pad(SelectSmallItems)"
    puts $fid "set _pad(SelectTransparentItems) $_pad(SelectTransparentItems)"
    puts $fid "set _pad(SmallObject) $_pad(SmallObject)"
    puts $fid "set _pad(Gamma) $_pad(Gamma)"
    puts $fid "set _pad(cacheSize) $_pad(cacheSize)"
    puts $fid "set _pad(cacheDir) \"$_pad(cacheDir)\""
    puts $fid "set _pad(cacheDelay) $_pad(cacheDelay)"

    close $fid
}

#
# Import the specified file using the file type defined by the file name.
# Position the object so it is centered and fills most of the screen.
# This function supports the following file types:
#   - HTML (file or URL)
#   - GIF image
#   - JPEG image
#   - TIFF image
#   - Illustrator files
#   - Directories are loaded with desktop application demo
#   - .pad data file
#   - .tcl source code
#   - .outline outline files
#   - .history history files
#
# If <select> is 1, then select the newly imported items.
#
proc import {PAD filename {select 0}} {
    global _color _pad _file Pad_ObjectList _animate

    # puts "import: filename=$filename"

    set objid ""
    set Pad_ObjectList ""
    if {$filename == ""} {return}
    set header [string tolower [string range $filename 0 4]]
    if {($header == "http:") || ($header == "file:")} {
	set objid [importHtml $PAD $filename]
	return $objid
    }
    if {[file exists $filename]} {
	updateStatusText $PAD "Importing $filename ..." 1
	$PAD update
	set view [$PAD getview]
	catch {unset _pad(StartView)}
	if {[file isdirectory $filename]} {
            set objid [pad_AddDesktopItem $PAD $filename]
	    after 0 "updateStatusMsg $PAD \"Click on folder to load contents\" 3"
	} else {
	    set dotPos [string last "." $filename]
	    if {$dotPos == -1} {
                if {![catch {set objid [$PAD create textfile -file $filename -font $_pad(Font) \
					    -pen $_pad(PenColor) -writeformat $_file(WriteFormat)]}]} {
		    $PAD ic $objid -place center
		    $PAD addtag text $objid
		    $PAD addtag item $objid
		}
	    } else {
		set len [string length $filename]
		set ext [string tolower [string range $filename [expr $dotPos + 1] $len]]
		switch -exact $ext {
		    bmp -
		    pbm -
		    pgm -
		    ppm -
		    jpg -
		    jpeg -
		    tif -
		    tiff -
		    gif {
			if {$_file(RGB)} {
			    set dither "1"
			} else {
			    set dither "0"
			}
			if {![catch {set image [$PAD image alloc $filename -rgb $dither]}]} {
			    set objid [$PAD create image -image $image -writeformat $_file(WriteFormat)]
			    $PAD ic $objid -place center
			    $PAD addtag image_item $objid
			    $PAD addtag item $objid
			}
		    }
		    html {
			set objid [importHtml $PAD $filename]
		    }
		    outline {
			set objid [importOutline $PAD $filename]
		    }
		    timeline {
			set objid [importTimeline $PAD $filename]
		    }
		    ill {
			set objid [importIllustrator $PAD $filename]
		    }
		    pad {
			set _pad(importing) 1
			set _animate(import) 1
			$PAD read $filename
			set _pad(importing) 0
			link_CleanupImport $PAD $Pad_ObjectList
			link_ConvertFormat $PAD
			set objid $Pad_ObjectList
		    }
		    tcl {
			uplevel #0 "source $filename"
		    }
            default {
                        if {![catch {set objid [$PAD create textfile -file $filename -font $_pad(Font) \
						   -pen $_pad(PenColor) -writeformat $_file(WriteFormat)]}]} {
			    $PAD ic $objid -place center
			    $PAD addtag text $objid
			    $PAD addtag item $objid
			}
		    }
		}
	    }
	}
	changesMade $PAD true
	updateStatusText $PAD "" 0

				# Now, center imported objects, and select them (if requested), 
                                # and set view back to starting point of new items (if one)
	if {($objid != "")} {
	    eval $PAD moveto $view
	    set group [$PAD create group -members $objid]
	    set obbox [$PAD bbox $group]
	    $PAD ic $group -place center
	    set nbbox [$PAD bbox $group]

	    $PAD ic $group -members ""
	    $PAD delete $group
	    if {$select == 1} {
		selectMode $PAD "Select"
		eval pad_select $PAD $objid
	    }
			# Now, let applications know that objects have been moved
	    foreach obj $objid {
		catch {link_SourceMoved $PAD $obj $obbox $nbbox}
	    }
			# Finally, update the view - but transform the start view
			# by the same amount the objects were moved.
	    if {[info exists _pad(StartView)]} {
		set newview [pad_calculate_transformed_view_bbox $_pad(StartView) $obbox $nbbox]
		eval $PAD centerbbox $newview $_pad(AnimationSpeed) .5 .5 1
	    }

			# This stuff is to set up an animation's start and end point
	    foreach id $objid {
		if {[$PAD hastag "animate" $id]} { 		
		
		    set _animate(ObjPath.$id.xR) \
			[lindex [$PAD ic $id -place] 0]
		    set _animate(ObjPath.$id.yR) \
			[lindex [$PAD ic $id -place] 1]
		    set zoom [$PAD getzoom]
		    set _animate(ObjPath.$id.xF) \
			[expr [lindex [$PAD ic $id -place] 0] + \
			     [expr $_animate(ObjPath.$id.dx) / $zoom] ]
		    set _animate(ObjPath.$id.yF) \
			[expr [lindex [$PAD ic $id -place] 1] + \
			     [expr $_animate(ObjPath.$id.dy) / $zoom]]
		}
	    }
	    
	}
    }
	   # remove this after fixing the bugs of dithered colors
    global tcl_platform
    if {$tcl_platform(platform) == "windows"} {
	update
	$PAD damage
    }
    
    return $objid
}

proc bindMenu {PAD} {
    bind $PAD <Alt-KeyPress> {
				# Don't allow short-cuts when object
				# is being edited
	if {[%W focus] == 1} {
	    tkTraverseToMenu %W %A
	}
	break
    }
}
