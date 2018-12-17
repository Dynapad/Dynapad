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

#ifndef LIST_INCL
#define LIST_INCL

#include <sys/types.h>

				// Iterate through a list
				//   iter is an instance of a Pad_Iterator
				//   list is an instance of a Pad_List
				//   type is the type element points to
				//   element is a pointer to type
#define DOLIST(iter, list, type, element) \
    for(iter.Init(list); \
	iter.Next(), element = (type *) iter.e, iter.more;)

				// Same as DOLIST, but use exact type
				// instead of pointer to type.
#define DOLISTI(iter, list, type, element) \
    for(iter.Init(list); \
	iter.Next(), element = (type) iter.e, iter.more;)

				// This is similar to DOLIST, but uses two
				// iterators so that the current element can
				// be deleted during the iteration.
#define DOLIST2(citer, niter, list, type, element) \
    for(citer.Init(list), niter.Init(list), citer.Next(), niter.Next(); \
	niter.Next(), element = (type *) citer.e, citer.more; \
	citer.Init(niter))

				// This is similar to DOLIST2, but uses three
				// iterators so that the current element can
				// be deleted during the iteration using the
				// more efficeint remove_next member function
				// of the first (previous pointer) iterator.
#define DOLIST3(piter, citer, niter, list, type, element) \
    for(piter.Init(list), citer.Init(list), niter.Init(list), citer.Next(), niter.Next(); \
	niter.Next(), element = (type *) citer.e, citer.more; \
	piter.Init(citer), citer.Init(niter))

#define DORESTLIST(iter, iter_ref, type, element) \
    for(iter.Init(iter_ref); \
	iter.Next(), \
	element = (type *) iter.e, \
	iter.more;)

#ifndef NULL
#define NULL   0
#endif

class Pad_Cons
{
 public:
    void *car;
    Pad_Cons *cdr;
    
    Pad_Cons() {
	car = 0;
	cdr = 0;
    };

    void * operator new(size_t size);
    void   operator delete(void *item);

  // no destructor - not our job to destroy things we point at.
};

class Pad_Iterator;

class Pad_List
{
    friend class Pad_Iterator;
  
protected:
    Pad_Cons *head, *tail;   
    void Remove_next(Pad_Cons *cons);
 
public:

    inline int Is_empty() const {
	return (head == NULL);
    }
  
    int  Index(const void *obj) const;
    virtual int  Insert_before(void *obj, void *ref);
    virtual int  Insert_after(void *obj, void *ref);
            void Join(Pad_List &l);
    virtual void Make_empty(void);
            int  Member(const void *obj) const;
            void *Nth(int) const;  
            void *Pop(void);
    virtual void Push(void *obj);
    virtual void Push_last(void *obj);
    virtual int  Push_new(void *obj);
    virtual int  Push_new_last(void *obj);
    virtual int  Remove(void *obj);
            int  Remove_last(void);
            void Reverse(void);
  
    inline void *First(void) const {
	if (!head)
	    return NULL;
	else
	    return head->car;
    }
    inline void *Last(void) const {
	if (!tail)
	    return NULL;
	else 
	    return tail->car;
    }

    int Length() const;
  
    Pad_List& operator=(Pad_List *list);
    Pad_List& operator=(Pad_List &list);

    virtual ~Pad_List();
    Pad_List();
    Pad_List(Pad_List &list);
};
 
class Pad_Iterator
{
 public:  
  unsigned char first, more, del;
  
  void *e;
  Pad_Cons *ilist;
  const Pad_List *list;

  void Init(const Pad_List &l);
  void Init(Pad_Iterator &i);
  
  int Next();			// Get next element
  void Remove_next();		// Remove next element

  Pad_Iterator();
};

#endif




