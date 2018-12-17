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

# Debugging procedure - reparse all kpl code
proc rp {} {
    kpl eval 'draw/widget.kpl source
    kpl eval 'draw/scale.kpl source
    kpl eval 'draw/chkbutn.kpl source
    kpl eval 'draw/graph.kpl source
    kpl eval 'draw/choice.kpl source
    kpl eval 'draw/frame.kpl source
    .pad damage
}

#
# widget_init
#
#   General widget initialization
#   Set defaults.
#   Initialize individual widgets.
#   This must be called before any widgets can be created.
#
proc widget_init {PAD} {
    global _widget
    global env

    kpl eval '$env(PADHOME)/draw/widget.kpl source

				# Widget default colors
    set _widget(fill)       #4c92aa
    set _widget(activefill) #7cc2ea
    set _widget(font)       "Times-12"

    pad_CheckbuttonInit $PAD
    pad_ScaleInit $PAD
    pad_GraphInit $PAD
    choice_init $PAD
    frame_init $PAD

    return ""
}

#
# Accessors for widget properties
#

proc pad_get_prop {widget prop} {
    global $widget

    return [set [set widget]($prop)]
}

proc pad_set_prop {widget prop value} {
    global $widget

    set [set widget]($prop) $value
}

proc pad_test_prop {widget prop} {
    global $widget

    return [info exists [set widget]($prop)]
}
#
# pad_widget_resize
#
#   Called when a widgets width or height property has changed.
#   Resets the widgets bbox.
#
proc pad_widget_resize {widget} {
    set PAD [pad_get_prop $widget PAD]
    set id [pad_get_prop $widget id]
    set width [pad_get_prop $widget width]
    set height [pad_get_prop $widget height]
    $PAD ic $id -bb "0:0 $width:$height"
    $PAD damage $id
}

#
# pad_widget_reborder
#
#   Called when a widgets fill color has changed.
#   Re-allocs the widgets border
#
proc pad_widget_reborder {widget {fill fill}} {
    set PAD [pad_get_prop $widget PAD]
    set id [pad_get_prop $widget id]

    set fill_border $fill
    append fill_border _border
    if {[pad_test_prop $widget $fill_border]} {
	$PAD border free [pad_get_prop $widget $fill_border]
    }
    set fill_color [pad_get_prop $widget $fill]
    pad_set_prop $widget $fill_border [$PAD border alloc $fill_color]

    $PAD damage $id
}

#
# pad_widget_rename
#
#   Called when a widgets name has been changed.
#   Resets the item's tag list.
#
proc pad_widget_rename {widget new_name} {
    set PAD [pad_get_prop $widget PAD]
    set id [pad_get_prop $widget id]
    set old_name [pad_get_prop $widget name]
    if {$old_name != ""} {
        $PAD deletetag $old_name $id
    }
    $PAD addtag $new_name $id
    pad_set_prop $widget name $new_name
}
