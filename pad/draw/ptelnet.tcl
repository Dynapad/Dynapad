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

# Telnet terminal emulator

# By: Daniel Kristjansson
# 0.04a (modified by JM: Jazzed up comments, merged into a single file)

# Bugs & | bizare features:
#
# - Null character closes the connection
# - Does not handle invalid hostnames very well
# - Doesn't have attributes for characters
# - Assumes escape commands always arrive whole
# - Terminal emulation is not complete, only implements ANSI subset of
#   VT100 terminal (plus "r," reversed argument goto)
# - SLOW, especially VT100 terminal emulation

proc readtel name {
  global $name
  set socket [set ${name}(socket)]
  set p [set ${name}(pad)]
  set txt [set ${name}(text)]

  set what [read $socket]
  if {$what == ""} {
    set ${name}(keepalive) 0
    ptelnet_end $name
  }
  while {$what != ""} {
    set where "-1 -1"
 
    if [expr \$${name}(emulation)] {
      set what [[expr \$${name}(emulator)] $name $what]
    } elseif {[regexp -indices -- \[x08x1Bx0C\t\] $what where]} {
        set place [lindex $where 0]
        ptelnetWRITE $name [string range $what 0 [expr $place-1]]
        switch -- [string range $what $place $place] {
          \x08 {ptelnetMOVE $name -1 0}
          \x0C {ptelnetCLRSCR $name 2
                ptelnetGOTO $name 0 0}
          \t   {set x [lindex [ptelnetLOC $name] 0]
                set s [expr 8 - ($x % 8)]
                set spaces ""
                for {set k 0} {$k < $s} {incr k} {set spaces "$spaces "}
                ptelnetWRITE $name "$spaces"
               }
	  \x1B {set ${name}(emulation) 1}
        }
        set what [string range $what [expr $place+1] end]
    } else {
       ptelnetWRITE $name $what
       set what ""
    }
  }

  # keep only last histbuf lines
  catch {
    $p text $txt delete 0.0 "end linestart - [expr [set ${name}(histbuf)] -1]l"
  }
}

proc ptelnet_scale_txt {name} {
  global $name
  set p [set ${name}(pad)]
  set txt [set ${name}(text)]

  set zoom [lindex [$p getview] 2]
  set place [$p ic $txt -place]
  set z [lindex $place 2]
  set scale [expr 1.0 / ($zoom)]
  $p scale $txt $scale
}

proc ptelnet_size {name {fontsize "0"}} {
  global $name
  set p [set ${name}(pad)]
  set item [set ${name}(group)]
  set place [set ${name}(view)]

  if {$fontsize > 0} {
    set size $fontsize
  } else { set size [set ${name}(fontsize)] }

  $p ic $item -z [expr double($size)]
}

proc ptelnet_move {name {x center} {y 1}} {
  global $name
  set p [set ${name}(pad)]
  set group [set ${name}(group)]
  if {$x == "center"} { 
    set view [$p getview]
    set x [lindex $view 0]
    set y [lindex $view 1]
    $p ic $group -anchor center -x $x -y $y
  } else {
    set coords [$p padxy $x $y]
    $p ic $group -anchor nw -x [lindex $coords 0] -y [lindex $coords 1]
  }
}

proc ptelnet_view_center {name} {
  global $name
  set p [set ${name}(pad)]
  set it [set ${name}(group)]
  set view [$p getview]
  $p moveto -twostep [$p ic $it -x] [$p ic $it -y] [lindex $view 2]
}


proc ptelnet_general_cfg {name {scope ""}} {
  global $name
  if {$scope != ""} {set scope "$scope,"}
  set histbuf [ptelnet_getoption $scope$name histbuf]
  set mode [ptelnet_getoption $scope$name mode]
  if {$histbuf != ""} {set ${name}(histbuf) $histbuf}
  if {$mode != ""} {set ${name}(mode) $mode}

  set frame [set ${name}(frame)]
  set txt [set ${name}(text)]
  set bgcolor [set ${name}(bgcolor)]
  set fgcolor [set ${name}(fgcolor)]
  .pad ic $frame -fill $bgcolor
  .pad ic $txt -pen $fgcolor
}

proc telnet {name {port 23}} {
  set PAD .pad
  set it [ptelnet $PAD $name $port]
  global $it
  set item [expr \$${it}(text)]
  set txt [expr \$${it}(text)]

  # Resize, using font size of one
  ptelnet_scale_txt $it

  # Add some frames and buttons
  $PAD pushcoordframe $txt
  set frame [$PAD create rectangle 1.0 1.0 0.0 0.0 -penwidth 0 \
    -tags "item ptelnet $it"]
  set ${it}(frame) $frame
  $PAD popcoordframe
  uplevel #0 "$PAD addtag ptelnet $frame"

  $PAD pushcoordframe $frame
  set min [$PAD create rectangle 0 1 -.025 [expr 23.0/24] \
	  -fill #7f7f30 -penwidth 0 -tags "item ptelnet $it"]
  set ns [$PAD create rectangle 0 [expr 23.0/24] -.025 [expr 19.0/24] \
	  -fill #30307f -penwidth 0 -tags "item ptelnet $it"]
  set conf [$PAD create rectangle 0 [expr 9.0/24] -.025 [expr 19.0/24] \
	  -fill #307f30 -penwidth 0 -tags "item ptelnet $it"]
  set quit  [$PAD create rectangle 0 [expr 9.0/24] -.025 0 \
	  -fill #7f3030 -penwidth 0 -tags "item ptelnet $it"]
  set min_t [$PAD create text -font "Helvetica-1" -text "-" \
	  -pen #ffff7f -place "-.025 1 .05" -anchor nw \
	   -tags "item ptelnet $it"]
  set ns_t [$PAD create text -font "Helvetica-1" -text "N\ne\nw\n" \
	  -pen #ffff7f -place "-.025 [expr 23.0/24] .05" -anchor nw \
	   -tags "item ptelnet $it"]
  set conf_t [$PAD create text -font "Helvetica-1" -text "C\no\nn\nf\ni\ng\nu\nr\ne" \
	  -pen #ff7f7f -place "-.025 [expr 19.0/24] .05" -anchor nw \
	   -tags "item ptelnet $it"]
  set quit_t [$PAD create text -font "Helvetica-1" -text "C\n\nL\n\nO\n\nS\n\nE" \
	  -pen #ffffff -place "-.025 [expr 9.0/24] .05" -anchor nw \
	  -tags "item ptelnet $it"]


  set len [string length [set ${it}(host)]]
  set len [expr {(17 < $len) ? 17 : $len}]
  for {set x 0;set line "C\nl\no\nn\ne\n \n"} {$x < $len} {incr x} {
    set line "$line[string index [set ${it}(host)] $x]\n"
  }

  set hostn [$PAD create rectangle 1 [expr 23.0/24] 1.025 0 \
	  -fill #408040 -penwidth 0 -tags "item ptelnet $it"]
  set hostn_t [$PAD create text -font "Helvetica-1" -text "$line" \
	  -pen #ff7f7f -place "1.0 [expr 23.0/24] .05" -anchor nw \
	  -tags "item ptelnet $it"]


  set labelbg [$PAD create rectangle 1.0 1.0 0.0 0.0 -fill #ffffff \
	  -tags "item ptelnet $it"]
  set label [$PAD create text -font "Helvetica-1" -text "$name" -place "0.5 0.5 0.3" \
	  -tags "item ptelnet $it"]
  $PAD popcoordframe

  $PAD bind $min_t <Text-Button-1> "break"
  $PAD bind $ns_t <Text-Button-1> "break"
  $PAD bind $conf_t <Text-Button-1> "break"
  $PAD bind $quit_t <Text-Button-1> "break"
  $PAD bind $hostn_t <Text-Button-1> "break"
  $PAD bind $hostn <Text-Button-1> "break"

  $PAD bind $min_t <B1-ButtonRelease> "break"
  $PAD bind $ns_t <B1-ButtonRelease> "break"
  $PAD bind $conf_t <B1-ButtonRelease> "break"
  $PAD bind $quit_t <B1-ButtonRelease> "break"
  $PAD bind $hostn_t <B1-ButtonRelease> "break"
  $PAD bind $hostn <B1-ButtonRelease> "break"

  $PAD bind $ns_t <Button-1> \
	"after 10 \"ptelnet_quickdialog $PAD\";break"
  $PAD bind $conf_t <Button-1> \
	"after 10 \"ptelnet_setupdialog $PAD local $it\";break"
  $PAD bind $quit_t <Button-1> \
	"after 10 \"ptelnet_end $it\";break"
  $PAD bind $hostn_t <Button-1> \
	"after 10 \"telnet [set ${it}(host)]\";break"
  $PAD bind $hostn <Button-1> \
	"after 10 \"telnet [set ${it}(host)]\";break"


  # Group

  set group2  [$PAD create group -anchor nw \
	-members "$labelbg $label" -tags "item ptelnet $it"]
  set group1 [$PAD create group -anchor nw \
	-members "$frame $txt $min $min_t $ns $ns_t $conf $conf_t \
                  $quit $quit_t $hostn $hostn_t" -tags "item ptelnet $it"]
  set place [$PAD getview]
  set ${it}(view) $place
  $PAD zoom 2 \
      [lindex $place 0] [lindex $place 1]
  ptelnet_stack $PAD A $group1 $group2
  set group [$PAD create group -anchor nw \
	-members "$group1 $group2" -tags "item ptelnet $it"]
  $PAD zoom .5 [lindex $place 0] [lindex $place 1]
  set ${it}(group) $group

  # binding that depends on group
  $PAD bind $min_t <Button-1> \
	"%P ic $group -z 1 ;break"

  #bind group2
    $PAD bind $label <Text-Button-1> "
    eval %P moveto $place
    ptelnet_size $it 
    ptelnet_view_center $it
    %P raise $group
    break
  "
  $PAD bind $label <Button-1> "
    eval %P moveto $place
    ptelnet_size $it 
    ptelnet_view_center $it
    %P raise $group
    break
  "
  $PAD bind $labelbg <Button-1> "
    eval %P moveto $place
    ptelnet_size $it 
    ptelnet_view_center $it
    %P raise $group
    break
  "

  # Place
  ptelnet_move $it center

  # Final Configs
  # Resize, using default font size
  ptelnet_size $it
  ptelnet_general_cfg $it

  # make sure we have focus
  $PAD focus $txt

  # done
  return $it
}

# This creates a terminal with no fluff.  The "telnet" procedure above
# makes the actual User Interface. Basically this should be left
# alone unless there is a problem. 
# Please mail me if you make improvements here:  danielk@play.nyu.edu

proc ptelnet {PAD name {port 23}} {
  global padterminst
  if [info exists padterminst] {incr padterminst
  } else {set padterminst 1}
  set inst $padterminst
  global ptelnet$inst

  set xname ptelnet$inst
  set ptelnet${inst}(pad) $PAD
  
  # Some defaults
  global ptelnet_option

  # find this
  if ![info exist ptelnet_option] {
    set ptelnet_option(global,mode) VT100
    set ptelnet_option(global,histbuf) 24 ;#keep only the last 24 lines
    ptelnet_restore_options /
  }

  set ptelnet${inst}(secure) 0 ;#false, Not Implemented
  set ptelnet${inst}(spos) {1 1} ;#if restore pos. is called before save pos...
  set ptelnet${inst}(emulation) 0 ;#false
  set ptelnet${inst}(emulator) ptelnetANSI
  set ptelnet${inst}(width) 80
  set ptelnet${inst}(height) 24
  set ptelnet${inst}(zwidth) [expr  \$ptelnet${inst}(width) -1]
  set ptelnet${inst}(zheight) [expr \$ptelnet${inst}(height) -1]
  set ptelnet${inst}(wrap) 1 ;#true
  set ptelnet${inst}(fontsize) 9
  set ptelnet${inst}(host) $name
  set ptelnet${inst}(bgcolor) "#eeeeee"
  set ptelnet${inst}(fgcolor) "#000000"
  set ptelnet${inst}(inscolor) "#ffffff" ;#Not Implemented

  set ptelnet${inst}(mode) [ptelnet_getoption $name mode]
  set ptelnet${inst}(histbuf) [ptelnet_getoption $name histbuf]

  # Text Widget Stuff
  set txt [$PAD create text -font "Line-1" -anchor sw -tags "item ptelnet $xname"]
  set lines [expr \$ptelnet${inst}(zheight)]
  set columns [expr \$ptelnet${inst}(zwidth)]
  for {set i 0} {$i < $lines} {incr i} {$PAD text $txt insert point "\n"}
  set ptelnet${inst}(text) $txt
  ptelnetGOTO ptelnet${inst} $columns 0
  ptelnetGOTO ptelnet${inst} 0 0
  $PAD focus $txt

  # Bindings
  $PAD bind $txt <Text-Button-1> "
    global ptelnet$inst
    %P focus %O
    %P raise \[set ptelnet${inst}(group)\]
    break
  "

  $PAD bind $txt <Button-1> "
    global ptelnet$inst
    %P focus %O
    %P raise \[set ptelnet${inst}(group)\]
  "

  set socket [open "|telnet $name $port" r+]
  $PAD bind $txt <Any-KeyPress> "
    puts -nonewline $socket %A
    if {\"%A\" ==\"\x0D\"} \"flush $socket\"
    break
  "
  $PAD bind $txt <Up> "
    puts -nonewline $socket {\x1B\[A}
    flush $socket
    break
  "
  $PAD bind $txt <Down> "
    puts -nonewline $socket {\x1B\[B}
    flush $socket
    break
  "
  $PAD bind $txt <Right> "
    puts -nonewline $socket {\x1B\[C}
    flush $socket
    break
  "
  $PAD bind $txt <Left> "
    puts -nonewline $socket {\x1B\[D}
    flush $socket
    break
  "

  # Pipe related
  fconfigure $socket -blocking false -translation binary

  fileevent $socket readable "readtel $xname"
  set ptelnet${inst}(socket) $socket

  set ptelnet${inst}(keepalive) 1
  flushsock ptelnet$inst

  # done
  return ptelnet$inst
}

proc ptelnet_end {name} {
  global $name

  if ![info exists $name] {
    return 1
  }

  set p [expr \$${name}(pad)]
  set socket [expr \$${name}(socket)]
  set group [expr \$${name}(group)]

  foreach handle [$p find withtag handle$group] {$p delete $handle}

  catch {close $socket}
  $p delete $group

  after 120000 "
    global $name
    catch \"unset $name\"
  "
  return ""
}

proc flushsock {name} {
  global $name
  catch {
    if [expr \$${name}(keepalive)] {
      catch {flush [expr \$${name}(socket)]}
      after 100 "flushsock $name"
    }
  }
}

proc ptelnet_quickdialog {PAD} {
  if [winfo exists .ptel] {
    raise .ptel
    return ""
  }
  toplevel .ptel
  wm title .ptel "Pad Telnet Dialog"
  frame .ptel.f -border 4
  label .ptel.f.label -text "Where do you want to go today?"
  frame .ptel.f.m -height .3i
  frame .ptel.f.b
  frame .ptel.f.b.buf -width .3i
  entry .ptel.f.b.entry
  bind .ptel.f.b.entry <Return> {
    set it [.ptel.f.b.entry get]
    if {$it != ""} {
      telnet [.ptel.f.b.entry get]
      destroy .ptel}
  }
  button .ptel.f.b.go -text "Go!" -command {
    set it [.ptel.f.b.entry get]
    if {$it != ""} {
      telnet [.ptel.f.b.entry get]
      destroy .ptel}
  }

  button .ptel.f.cancel -text "Cancel" -command {destroy .ptel}

  pack .ptel.f
  pack .ptel.f.label .ptel.f.m .ptel.f.b -side top
  pack .ptel.f.b.entry .ptel.f.b.buf .ptel.f.b.go -side left 
  pack .ptel.f.cancel -side bottom -fill x
  focus .ptel.f.b.entry
}

proc ptelnet_setup_name {slider host value} {
  switch $value {
    0 {$slider configure -label "to this terminal"}
    1 {$slider configure -label "to $host only"}
    2 {$slider configure -label "from now on"}
    3 {$slider configure -label "to everything"}
  }
}

proc ptelnet_setup {dialog name} {
  global $name
  set scope [$dialog.scope_c get]
  set hist [$dialog.history_c get]

  switch $scope {
    0 {
        set scope terminal,$name
        set range $name
      }
    1 {
        set scope host,[set ${name}(host)]
        set range $name ;#not yet completelly implemented
      }
    2 {
        set scope global
        set range ""
      }
    3 {
        set scopt global
        set range [info global ptelnet?]
        set range "$range [info global ptelnet??]"
      }
  }
  ptelnet_putoption $scope histbuf $hist

  foreach item $range {
    ptelnet_general_cfg $item $scope
  }
  destroy $dialog
  ptelnet_save_options
}

proc ptelnet_setupdialog {PAD {scope local} {name ""}} {
  set i 0
  while {[winfo exists .ptelconf$i]} {incr i}
  toplevel .ptelconf$i
  if {$name == ""} {
    wm title .ptelconf$i "Pad Telnet Setup Dialog"
  } else {
    global $name
    set host [set ${name}(host)]
    wm title .ptelconf$i "$host setup"
  }

  if {$name != ""} {set x 0} else {set x 1}
  label .ptelconf${i}.scope_l -text "Applies..."
  scale .ptelconf${i}.scope_c -orient horizontal -from $x -to 3 \
	  -showvalue false -label " " \
          -command "ptelnet_setup_name .ptelconf${i}.scope_c $host"

  label .ptelconf${i}.history_l -text "History"
  scale .ptelconf${i}.history_c -orient horizontal -from 24 -to 1000

  frame .ptelconf${i}.b
  button .ptelconf${i}.b.ok -text "Apply" -width 6 \
	-command "ptelnet_setup .ptelconf$i \"$name\""
  
  button .ptelconf${i}.b.cancel -text "Cancel" -width 6 \
	-command "destroy .ptelconf$i"

  frame .ptelconf${i}.buf

  # arrange items
  pack .ptelconf${i}.scope_l -fill x
  pack .ptelconf${i}.scope_c -fill x
  pack .ptelconf${i}.history_l -fill x
  pack .ptelconf${i}.history_c -fill x
  pack .ptelconf${i}.b.ok .ptelconf${i}.b.cancel -side left
  pack .ptelconf${i}.b

  # set defaults
  switch $scope {
    global  {set scope 3}
    future  {set scope 2}
    host    {set scope 1}
    default {set scope 0}
  }
  .ptelconf${i}.scope_c set $scope
  .ptelconf${i}.history_c set [ptelnet_getoption $name histbuf]

  return ""
}

proc ptelnet_setupdialog_old {PAD {scope local} {name ""}} {
  set i 0
  while {[winfo exists .ptelconf$i]} {incr i}
  toplevel .ptelconf$i
  if {$name == ""} {
    wm title .ptelconf$i "Pad Telnet Setup Dialog"
  } else {
    global $name
    set n [set ${name}(host)]
    wm title .ptelconf$i "$n setup"
  }

  label .ptelconf${i}.scope -text "Scope"
  label .ptelconf${i}.emulator -text "Emulator"
  label .ptelconf${i}.col_row -text "Columns x Rows"
  label .ptelconf${i}.history -text "History"

  checkbutton .ptelconf${i}.global -text "global"
  listbox .ptelconf${i}.emulat -height 1 -width 5
  .ptelconf${i}.emulat insert end ANSI

  listbox .ptelconf${i}.hist -height 1 -width 5
  .ptelconf${i}.hist insert end 0 100 200 400 800 inf.

  checkbutton .ptelconf${i}.future -text "from now on"
  frame .ptelconf${i}.mode
    label .ptelconf${i}.mode.label -text "Mode"
    listbox .ptelconf${i}.mode.list -height 1 -width 5
    .ptelconf${i}.mode.list insert end VT100 ANSI
    pack .ptelconf${i}.mode.label -side left
    pack .ptelconf${i}.mode.list -padx 5 -side left
  frame .ptelconf${i}.dim
    entry .ptelconf${i}.dim.cols -text "80" -width 2
    label .ptelconf${i}.dim.x -text "x"
    entry .ptelconf${i}.dim.rows -text "24" -width 2
    pack .ptelconf${i}.dim.cols .ptelconf${i}.dim.x \
	 .ptelconf${i}.dim.rows -side left
  button .ptelconf${i}.ok -text "OK-NOP" -width 6 \
	-command "destroy .ptelconf$i"
  
  button .ptelconf${i}.cancel -text "Cancel" -width 6 \
	-command "destroy .ptelconf$i"

  frame .ptelconf${i}.buf

  # arrange items
  

  grid .ptelconf${i}.scope -sticky w -padx 5 -pady 5
  grid .ptelconf${i}.global -row 0 -column 1 -sticky w -padx 5 -pady 5
  grid .ptelconf${i}.future -row 0 -column 2 -sticky w -padx 5 -pady 5
  grid .ptelconf${i}.emulator -row 1 -column 0 -sticky w -padx 5 -pady 5
  grid .ptelconf${i}.emulat -row 1 -column 1 -sticky w -padx 5 -pady 5
  grid .ptelconf${i}.mode -row 1 -column 2 -sticky w -padx 5 -pady 5
  grid .ptelconf${i}.col_row -row 2 -column 0 -columnspan 2 -sticky w \
	  -padx 5 -pady 5
  grid .ptelconf${i}.dim -row 2 -column 2 -sticky w -padx 5 -pady 5
  grid .ptelconf${i}.history -row 3 -column 0 -sticky w -padx 5 -pady 5
  grid .ptelconf${i}.hist -row 3 -column 1 -sticky w -padx 5 -pady 5
  grid .ptelconf${i}.ok -row 3 -column 2 -padx 5 -pady 10
  grid .ptelconf${i}.cancel -row 4 -column 2 -padx 5 -pady 0
  grid .ptelconf${i}.buf -row 5 -pady 5

  # set defaults
  if {$scope == "global"} {
    .ptelconf${i}.global select
    .ptelconf${i}.future deselect
  } elseif {$scope == "future"} { ;#for current and future sessions
    .ptelconf${i}.global deselect
    .ptelconf${i}.future select
  } else {   ;#only for current session
    .ptelconf${i}.global deselect
    .ptelconf${i}.future deselect
  }
  return ""
}


# low level configuration routines

proc ptelnet_save_options {} {
  if ![catch {set out [open ~/.ptelnetrc w]}] {
    global ptelnet_option
    if [info exists ptelnet_option] {
      foreach option [array names ptelnet_option] {
        if ![regexp ptelnet $option] {
	  puts $out "set ptelnet_option($option) [set ptelnet_option($option)]"
        }
      }
    } else {puts "ptelnet option array doesn't exist"}
    close $out
  } else {puts "can't open .ptelnetrc for writing"}
  return ""
}

proc ptelnet_restore_options {defaultpath} {
  if [file exists ${defaultpath}/ptelnetrc] {
    source ${defaultpath}/ptelnetrc
  } elseif [file exists ~/.ptelnetrc] {
    source ~/.ptelnetrc
  }
  return ""
}

proc ptelnet_putoption {dest option value} {
  global ptelnet_option
  set ptelnet_option($dest,$option) $value
}

proc ptelnet_getoption {source option} {
  global ptelnet_option
  if [info exists ptelnet_option($source,$option)] {
    return [set ptelnet_option($source,$option)]
  } elseif [info exists ptelnet_option(terminal,$source,$option)] {
    return [set ptelnet_option(terminal,$source,$option)]
  } elseif [info exists ptelnet_option(host,$source,$option)] {
    return [set ptelnet_option(host,$source,$option)]
  } else {
    return [set ptelnet_option(global,$option)]
  }
}

# stack
#
# Takes a minimum of four arguments                 
# 1)The pad the items can be found                  
# 2)The x/y ratio to mangle the items into (if "A"  
#     it uses the x/y ratio of the first item)      
# Then a minimum of two items must be listed by id  
#
# The optional arguments are -d, and -r they must   
# be placed between two items in the list.         
# (i.e. after the 3rd arguement)                    
# "-d #" sets the default distance, on the z-axis,  
#        to place between two items in the list,    
#        it applies to all remaining items unless   
#        overriden by a "-r".                       
# "-r #" like "-d" except it only applies locally   
# (replace the # with a float, it is plugged into   
# the formula 2^# when calculating z-axis distance) 
#                                                   
# You can also set the global dflt_crss_fade_ratio  
# which changes the distance used when neither a    
# "-d" or "-r" applies.                             

proc ptelnet_stack {PAD xyratio firstitem args} {
  if {$args == ""} {
    return "wrong # args:\nshould be \"stack PAD xyratio firstitem ?-r faderatio? seconditem ?arg...?\""
  }

  global dflt_crss_fade_ratio
  if ![info exist dflt_crss_fade_ratio] {set dflt_crss_fade_ratio 1}
  set default $dflt_crss_fade_ratio
  set override $dflt_crss_fade_ratio

  set h [$PAD ic $firstitem -height]
  set w [$PAD ic $firstitem -width]

  if {$xyratio == "A"} {set xyratio [expr $w/$h]}
  if {[expr $w / $h] > $xyratio} {
    $PAD ic $firstitem -height [expr $w / $xyratio]
  } else {$PAD ic $firstitem -width [expr $h * $xyratio]}
  set fheight [$PAD ic $firstitem -height]
  set fplace  [$PAD ic $firstitem -place]
  set fz      [$PAD ic $firstitem -z]
  set fanchor [$PAD ic $firstitem -anchor]
  set fsize [expr [$PAD getsize $firstitem] * 0.8]

  set groupz [$PAD create group -members $firstitem]
  
  set lastitem $firstitem
  set level 1
  set len [llength $args]
  for {set c 0} {$c < $len} {incr c} {
    set item [lindex $args $c]
    switch -- $item {
      -d {
           incr c
           set default [expr [lindex $args $c] * 1.0]
         }
      -r {
           incr c
           set override [expr [lindex $args $c] * 1.0]
         }
      default {;##this is the real meat...
        if {[$PAD ic $item] == ""} break;
        
	##Get rid of selection stuff
	if {[$PAD hastag "selected" $item]} {
          $PAD deletetag "selected" $item
          foreach handle [$PAD find withtag handle$item] {
            $PAD delete $handle}

        }

        ##put it in the right place
        $PAD ic $item -place $fplace -anchor $fanchor


        ##fix the x:y ratio
        set h [$PAD ic $item -height]
        set w [$PAD ic $item -width]
        if {[expr $w / $h] > $xyratio} {
          $PAD ic $item -height [expr $w / $xyratio]
        } else {$PAD ic $item -width [expr $h * $xyratio]}

        ##resize to fit under first item
        set ratio [expr [$PAD ic $item -height] / $fheight]
        $PAD ic $item -z [expr $fz / $ratio]

        ##
        #the formula should be something like (2^override)*level*fsize

        $PAD ic $item -maxsize [
          expr pow(pow(.5,$override),$level) * 2.4 * $fsize/$level]
        $PAD ic $lastitem -minsize [
          expr pow(pow(.5,$override),$level) * 1.6 * $fsize/$level]
	incr level

        $PAD addgroupmember $item $groupz
        set override $default
        set lastitem $item
      }
    }
  }
}

#
# terminal emulator code
#

proc ptelnetCLREOL {name {what 2}} {
  global $name
  set p [expr \$${name}(pad)]
  set txt [expr \$${name}(text)]
  switch -- $what {
    0 {$p text $txt delete point "point lineend"}
    1 {set ploc [ptelnetLOC $name]
       $p text $txt delete "point linestart" point
       set width [lindex $ploc 0]
       for {set i 0} {$i < $width} {incr i} {
	 $p text $txt insert point " "}
      }
    2 {set ploc [ptelnetLOC $name]
       $p text $txt delete "point linestart" "point lineend"
       ptelnetGOTO $name [lindex $ploc 0] [lindex $ploc 1]
      }
  }
}

proc ptelnetCLRSCR {name {what 2}} {
  global $name
  set p [expr \$${name}(pad)]
  set txt [expr \$${name}(text)]
  set zheight [expr \$${name}(zheight)]
  switch -- $what {
    2 {set ploc [ptelnetLOC $name]
       ptelnetGOTO $name 0 0
       $p text $txt delete point end
       for {set i 0} {$i < $zheight} {incr i} {
	 $p text $txt insert point "\n"}
       ptelnetGOTO $name [lindex $ploc 0] [lindex $ploc 1]
      }
    0 {set ploc [ptelnetLOC $name]
       $p text $txt delete point end
       for {set i [lindex $ploc 1]} {$i < $zheight} {incr i} {
	 $p text $txt insert point "\n"}
       ptelnetGOTO $name [lindex $ploc 0] [lindex $ploc 1]
      }
    1 {set ploc [ptelnetLOC $name]
       set nploc [$p text $txt index point]
       ptelnetGOTO $name 0 0
       $p text $txt delete point $nploc
       set height [lindex $ploc 1]
       for {set i 0} {$i < $height} {incr i} {
	 $p text $txt insert point "\n"}
       set width [lindex $ploc 0]
       for {set i 0} {$i < $width} {incr i} {
	 $p text $txt insert point " "}
      }
  }
}


proc ptelnetWRITE {name what} {
  # this is a destructive write needed because of terminal emulation
  global $name
  set p [expr \$${name}(pad)]
  set txt [expr \$${name}(text)]
  set wrap [expr \$${name}(wrap)]
  set zwidth [expr \$${name}(zwidth)]
  set histbuf [expr \$${name}(histbuf)]
  set ploc [$p text $txt index point] ;#returns y.x coords
  set eloc [$p text $txt index end]

  regsub -all x0D $what {} what

    while {"$what" != ""} {
      if {[set where [string first "\x0A" "$what"]] >= 0} {
        if {$where == "0"} {
	  set peloc [$p text $txt index "point lineend"]
          if {$eloc == $ploc} {$p text $txt insert point "\x0A"
          } elseif {$eloc == $peloc} {
            $p text $txt mark set point "point lineend"
            $p text $txt insert point "\x0A"
          } else {$p text $txt mark set point "point linestart + 1 line"}
          set what [string range "$what" 1 end]
          continue
        } else {set where [expr $where -1]}
      } else {set where [expr [string length "$what"] -1]}

      set line [string range $what 0 $where]
      catch {set what [string range "$what" [expr $where +1] end]}
      set xloc [lindex [split [$p text $txt index point] .] 1]
      set xend [lindex [split [$p text $txt index "point lineend"] .] 1]
      set strlen [string length $line]

      if {$xloc == $xend} {
        $p text $txt insert point $line
      } elseif {[expr $strlen + $xloc] < $xend} {
        $p text $txt delete "point linestart + $xloc char" \
                            "point linestart + [expr $xloc + $strlen] char"
        $p text $txt insert point $line
      } else {
        $p text $txt delete "point linestart + $xloc char" "point lineend"
        $p text $txt insert point $line
      }
    }
  $p text $txt delete 0 "end linestart - $histbuf lines"
  return ""
}

proc ptelnetAGOTO {name {y 0} {x 0}} {
  ptelnetGOTO $name [expr $x -1] [expr $y -1]
}

proc ptelnetGOTO {name {x -1} {y -1}} {
  global $name
  set p [expr \$${name}(pad)]
  set txt [expr \$${name}(text)]
  set zheight [expr \$${name}(zheight)]
  if {$x == -1} {
    set x [lindex [ptelnetLOC $name] 0]
  } 

  $p text $txt mark set point "point linestart"
  
  if {$y >= 0} {
    if {$y >= $zheight} {
      $p text $txt mark set point "end"
    } else {
      $p text $txt mark set point "end [expr + $y - $zheight] lines"
    }
  }

  $p text $txt mark set point "point lineend"
  set rx [lindex [ptelnetLOC $name] 0]

  if {$rx < $x} {
    set i 0
    set j [expr $x - $rx]
    for {} {$i < $j} {incr i} {
      $p text $txt insert point " "
    }
  } else {$p text $txt mark set point "point linestart + $x chars"}
}

proc ptelnetMOVE {name {x 0} {y 0}} {
  set loc [ptelnetLOC $name]
  ptelnetGOTO $name [expr [lindex $loc 0] + $x] [expr [lindex $loc 1] + $y]
}

proc ptelnetALOC {name} {
  global $name
  set height [expr \$${name}(height)]
  set p [expr \$${name}(pad)]
  set txt [expr \$${name}(text)]
  set loc [split [$p text $txt index point] .]
  set endy [lindex [split [$p text $txt index end] .] 0]
  return "\{[expr [lindex $loc 0] - $endy + $height]\} \{[expr [lindex $loc 1]+1]\}"
}

proc ptelnetLOC {name} {
  global $name
  set zheight [expr \$${name}(zheight)]
  set p [expr \$${name}(pad)]
  set txt [expr \$${name}(text)]
  set loc [split [$p text $txt index point] .]
  set endy [lindex [split [$p text $txt index end] .] 0]
  return "\{[lindex $loc 1]\} \{[expr [lindex $loc 0] - $endy + $zheight]\}"
}


proc ptelnetANSI {name what} {
  global $name
  set p [expr \$${name}(pad)]
  set txt [expr \$${name}(text)]
  set where {0 1}
  set mode "VT100"

  ##do we have a (whole) command?
  if {[string range "$what" 0 0] != "\["} {
    if {[string range "$what" 0 1] != "(B"} {
      set ${name}(emulation) 0
      return "$what"
    } else {
      set ${name}(emulation) 0
    }
  }
  if {![regexp -indices -- {[ABCDHJKfhlmsugr]} "$what" where]} {
    set ${name}(emulation) 0
    return ""
    #we don't have enough of the command to process it yet
    #!!!this won't work, we need to buffer the data!!!
  }

  ##do something with the comand
  set ${name}(emulation) 0 ;#let normal rutine take next character

  set cmd [string range $what [lindex $where 0] [lindex $where 0]]

  set num1 "";set num2 "";set num3 "";set num4 "";set num5 ""
  catch {scan "$what" "\[%d;%d;%d;%d;%d" num1 num2 num3 num4 num5}

  if {$num1==""} {
    switch -- $cmd {
      J -
      K {
          if {($mode == "VT100")} {set num1 0	;#VT100 default
          } else {set num1 2}			;#ANSI default
        }
      default {set num1 1}
    }
  }

  if {$num2==""} {
    switch -- $cmd {
      J -
      K {
          if {($mode == "VT100")} {set num2 0	;#VT100 default
          } else {set num2 2}			;#ANSI default
        }
      default {set num2 1}
    }
  }

  if {$cmd >= "a"} {
    switch -- $cmd {
      f {ptelnetAGOTO $name $num1 $num2
        }
      h -
      l {puts "change terminal mode to $num1"}
      m {
        }
      g {puts "got the mysterious g command"}
      s {set ${name}(spos) [ptelnetALOC $name] }
      u {catch{ptelnetAGOTO $name [lindex [expr \$${name}(spos)] 1] \
                                  [lindex [expr \$${name}(spos)] 0]}}
      r {ptelnetAGOTO $name $num2 $num1
        }
    }
  } else {
    switch -- $cmd {
      C {ptelnetMOVE $name $num1  0
        }
      H {ptelnetAGOTO $name $num1 $num2
        }
      J {ptelnetCLRSCR $name $num1
        }
      K {ptelnetCLREOL $name $num1
        }
      A {ptelnetMOVE $name 0 -$num1
        }
      B {ptelnetMOVE $name  0 $num1
        }
      D {ptelnetMOVE $name -$num1  0
        }
    }
  }

  return [string range "$what" [expr [lindex $where 1]+1] end]
}










