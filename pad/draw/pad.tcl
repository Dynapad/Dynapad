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

if {![info exists env(PADHOME)]} {
    puts "PADHOME environment variable not set,"
    puts "  set it to the main PAD directory"
    exit
}
if {![file exists $env(PADHOME)]} {
    puts "PADHOME environment variable not set correctly,"
    puts "  set it to the main PAD directory"
    exit
}

set auto_path [linsert $auto_path 0 $env(PADHOME)/draw]

# 
# Some machine dependent things
#
#set OS [exec uname -s]
#if {$OS == "AIX"} {
#    set CPU $env(HOSTNAME)
#} else {
#    set CPU [exec uname -m]
#}
#if {($CPU == "IP7") || ($CPU == "IP12") || ($CPU == "IP20") || ($CPU == "IP22") || ($CPU == "IP26")} {
#    set CPU "MIPS"
#}
#set PADBIN $env(PADHOME)/bin-$CPU-$OS
#set PADBIN $env(PADHOME)/bin
#
# load user prefs
# Need to do this outside procedure, or they won't be globals.
#
if {[file readable "$env(PADHOME)/draw/paddefaults"]} {
    source "$env(PADHOME)/draw/paddefaults"
}
set VersionString ""
if {[file readable "$env(HOME)/.padsetup"]} {
    set fid [open "$env(HOME)/.padsetup" "r"]
    gets $fid VersionString
    close $fid
				# Make sure VersionString from ~/.padsetup file is ok
    if {([string index $VersionString 0] == "#") &&
        ([lindex $VersionString 1] == $Pad_Version)} {
	source "$env(HOME)/.padsetup"
    }
}
if {[file readable "$env(HOME)/.paddefaults"]} {
    source "$env(HOME)/.paddefaults"
}

    #
    # load option db
    #
if {[file readable "$env(HOME)/.padrc"]} {
    option readfile "$env(HOME)/.padrc" 65
}

startPadDraw .pad
