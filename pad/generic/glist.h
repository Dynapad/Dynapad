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

/*
   glist.h

   Generic array lists.
   See glist.i for documentation and instantiation.
*/

#ifndef NULL
#  define NULL   0
#endif

#ifndef MAX
#  define MAX(a,b)	(((a)>(b))?(a):(b))	
#endif

#ifndef LIST_PTR
#  define LIST_PTR
#endif

/*
#define DOSLIST(i, list, type, element) \
    for (i=0; element = (type)list.Nth(i), i<list.Length(); i++)
*/

class PAD_LIST_CLASS
{
 protected:
  int _length;
  int _space;
  LIST_TYPE *_data;
  void extend(int n);

 public:
  void Make_empty();
  int Is_empty();

  void Push(LIST_TYPE LIST_PTR obj);
  void Push_last(LIST_TYPE LIST_PTR obj);
  void Set(int n, LIST_TYPE LIST_PTR obj);
  int Push_new(LIST_TYPE LIST_PTR obj);
  int Push_new_last(LIST_TYPE LIST_PTR obj);
  int Member(LIST_TYPE LIST_PTR obj);
  int Remove(LIST_TYPE LIST_PTR obj);

  LIST_TYPE LIST_PTR Pop(void);
  LIST_TYPE LIST_PTR Pop_last(void);
  LIST_TYPE LIST_PTR First(void);
  LIST_TYPE LIST_PTR Last(void);
  LIST_TYPE LIST_PTR Nth(int n);
  LIST_TYPE *Pointer(void);
  LIST_TYPE *Pointer(int n);

  int Length(void);

  ~PAD_LIST_CLASS();
  PAD_LIST_CLASS();
};

#undef LIST_PTR
