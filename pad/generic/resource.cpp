/*
"(c) Copyright 1993-1997 Pad++ Consortium {University of New Mexico (UNM),
and New York University (NYU)}, All Rights Reserved."  
Licensee can not remove or obscure any of the
copyright or trademark notices in this software.

IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.

See the file "License" for general information on usage and
redistribution, and the file "LicenseTerms" for the specific license
agreement on usage and redistribution of this file, and the Pad++
software in general.
*/


#include "defs.h"
#include "resource.h"
#include "display.h"

// Pad_Resource -
//
// Per-display resource management for device independent resources
// (fonts, colors, ...)
//
//
// Renderers call resource->Find(dpy) to find the device-specific value for a resource.
// Find() looks for the given display in a linked-list. If it is there, the value for
// that display is returned (as a void*). If it is not there, the virtual member
// function Alloc() is called to allocate the resource for the display, the
// display is added to the linked list, and the new value is returned.
//
// When a Pad_Resource is freed, the linked-list is iterated 
// over and the virtual member function Free(dpy, value) is called
// to free the resource information for each display.
//

typedef struct _PerDisplay {
    Pad_Display *dpy;                  // device 
    void *value;                       // device-specific value for this resource
    struct _PerDisplay *next;          // next device
} PerDisplay;

Pad_Resource::~Pad_Resource()
{
}

// called by subclass destructors
void
Pad_Resource::Delete_per_display()
{
    // iterate over list of displays, freeing resource in each display

    PerDisplay *pd = _perdisplay;
    while (pd) {
	PerDisplay *next = pd->next;
	Free(pd->dpy, pd->value);
	delete pd;
	pd = next;
    }

    _perdisplay = NULL;
}


//
// Public method for finding device-dependent data for
// a given display
// 

void *
Pad_Resource::Find(Pad_Display *dpy)
{
				// test if the display is the front of the list
    if (_perdisplay && _perdisplay->dpy == dpy)
      return _perdisplay->value;

    //
    // not there - search the list. If the display is found, move it to the
    // front of the list.
    //
    PerDisplay *pd = (_perdisplay ? _perdisplay->next : (PerDisplay *)NULL);
    PerDisplay *prev = _perdisplay;

    while (pd) {
	if (pd->dpy == dpy) {	// found the display.
				// move this display to the front of the list
	    prev->next = pd->next;
	    pd->next = _perdisplay;
	    _perdisplay = pd;
				// return the value
	    return pd->value;
	}
	prev = pd;
	pd = pd->next;
    }
				// not in list - allocate new resource
    pd = new PerDisplay;
    pd->dpy = dpy;
    pd->value = Alloc(dpy);
    pd->next = _perdisplay;
    _perdisplay = pd;

    return pd->value;
}










