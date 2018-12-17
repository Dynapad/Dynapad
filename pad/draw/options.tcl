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

proc pad_InitOptions {PAD} {
    global _pad

    set _pad(noisepos) 1

    $PAD addoption line -roughness pad_Roughness 0.0
    $PAD addoption line -undulate  pad_Undulation 0
    $PAD addoption all  -linkto    link_Define ""
    $PAD addoption all  -linkstyle link_Style "box"
    $PAD addoption all  -linkcolor link_Color "red"
}

#
# Specify roughness (noisiness) of line with
# a single floating point number between 0.0 and 1.0
#
proc pad_Roughness args {
    global _pad

    set PAD [lindex $args 0]
    set item [lindex $args 1]
    if {[llength $args] == 3} {
	set value [lindex $args 2]
    }

				# Set new value if specified
    if {[info exists value]} {
	if {$value == 0} {
	    $PAD ic $item -noisedata ""
	} else {
	    set ndata [$PAD ic $item -noisedata]
	    if {$ndata == ""} {
				# Turning on roughness, get a new noise position
		set _pad(noisepos) [expr $_pad(noisepos) + 123.12345]
		set noisepos $_pad(noisepos)
	    } else {
				# Roughness already on, just increment noise position
		set noisepos [expr [lindex $ndata 0] + 0.1]
	    }
	    set ndata "$noisepos 0.3 $value 10"
	    
	    $PAD ic $item -noisedata $ndata
	}
    }

				# Return current value
    set ndata [$PAD ic $item -noisedata]
    if {$ndata == ""} {
	set roughness 0.0
    } else {
	set roughness [lindex $ndata 2]
    }
    return $roughness
}

#
# Make line item start or stop undulating, based
# on the value of flag (true or false)
#
proc pad_Undulation args {
    global _pad

    set PAD [lindex $args 0]
    set item [lindex $args 1]
    if {[llength $args] == 3} {
	set value [lindex $args 2]
    }

				# Set new value if specified
    if {[info exists value]} {
	set _pad($item.Undulate) $value
	if {$value} {
	    set roughness [$PAD ic $item -roughness]
	    $PAD ic $item -timerrate 100
	    $PAD ic $item -timerscript "$PAD ic $item -roughness $roughness"
	} else {
	    $PAD ic $item -timerrate 0
	    $PAD ic $item -timerscript {}
	}
    }

				# Return current value
    if {[info exists _pad($item.Undulate)] && $_pad($item.Undulate)} {
	return 1
    } else {
	return 0
    }
}
