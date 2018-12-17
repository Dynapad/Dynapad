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


#ifndef _PAD_RESOURCE_H
#define _PAD_RESOURCE_H

//
// Class structure for Pad_Resource.
//

class Pad_Display;

class Pad_Resource {
  public:
    Pad_Resource() : _perdisplay(0), _count(0) { };
    virtual ~Pad_Resource();

    void Delete_per_display();

    // used by renderers to find device-specific value for a resource,
    // allocating it if necessary
    void *Find(Pad_Display *dpy);

    // used by subclasses of Pad_Resource to reference count a resource
    inline void Addref() { _count++; }
    inline int  Deref()  { return(--_count == 0); }

    // called by Find() to allocate a resource if necessary
    virtual void *Alloc(Pad_Display *dpy) = 0;

    // called by ~Pad_Resource() to free resources
    virtual void Free(Pad_Display *dpy, void *value) = 0;

  private:
    struct _PerDisplay *_perdisplay;
    unsigned long _count;
};

#endif /* _PAD_RESOURCE_H */
