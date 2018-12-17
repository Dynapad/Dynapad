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
#include "list.h"
#include <assert.h>
#include <stdlib.h>

//
// Alternative memory allocation for list cons cells -
//

static Pad_Cons *freelist = NULL;

void *
Pad_Cons::operator new(size_t size)
{
#define BLOCK_SIZE 512

    Pad_Cons *result;

    static int nused = BLOCK_SIZE;
    static Pad_Cons *freeblock = NULL;

    if (freelist) {
	// If there is a cons cell on the free list, use it
	result = freelist;
	freelist = freelist->cdr;
    } else {
	// Otherwise, if we don't have a block of cons cells available, alloc up a
	// new block of them
	if (nused >= BLOCK_SIZE) {
	    freeblock = (Pad_Cons *)malloc(BLOCK_SIZE * size);
	    nused = 0;
	}
	// Take a cons cell off the block
	result = &freeblock[nused++];
    }

    return result;

#undef BLOCK_SIZE
}

void
Pad_Cons::operator delete(void *item)
{
    Pad_Cons *cons = (Pad_Cons *)item;

    cons->cdr = freelist;
    freelist = cons;
}


Pad_List::~Pad_List()
{
    Make_empty();
}

Pad_List::Pad_List() 
{
    head = NULL;
    tail = NULL;
}

Pad_List::Pad_List(Pad_List &list)
{
    Pad_Iterator li;
    void *item;

    head = NULL;
    tail = NULL;

    DOLIST(li, list, void, item) {
	Push_last(item);
    }
}

Pad_List& 
Pad_List::operator=(Pad_List *list)
{
    *this = *list;
    return (*this);
}

Pad_List& 
Pad_List::operator=(Pad_List &list)
{
    Pad_Iterator li;
    void *item;

    Make_empty();
    DOLIST(li, list, void, item) {
	Push_last(item);
    }
    return (*this);
}

void
Pad_List::Make_empty(void)
{
    Pad_Cons *e, *tmp;
    
    e = head;
    while (e) {
	tmp = e;
	e = e->cdr;
	delete tmp;
    }
    
    head = NULL;
    tail = NULL;
}

void 
Pad_List::Remove_next(Pad_Cons *cons)
{
    if (!cons) {
	Pop();
	return;
    }
      
    if (cons->cdr == NULL) {
				// Can't remove element after last one
	return;
    } else if (cons->cdr == tail) {
				// Remove element after 2nd to last one
	cons->cdr = NULL;	  
	tail = cons;
    } else {
				// Remove other
	cons->cdr = cons->cdr->cdr;
    }
}

int 
Pad_List::Remove(void *e)
{
    Pad_Cons *prev = head, *cur, *fnd = NULL;
  
    if (!head) {
	return 0;
    }

				// If removing first item
    if (e == head->car) {
	Pop();
	return 1;
    }
				// Already checked first one.
    cur = head;
    while (cur) {
	if (e == cur->car) {
	    fnd = cur;
	    break;
	} else {
	    prev = cur;
	    cur = cur->cdr;
	}
    }
  
    if (fnd) {
	if (fnd == tail) {
				// If removing last item
	    prev->cdr = NULL;	  
	    tail = prev;
	} else {
	    prev->cdr = fnd->cdr;
	}
	delete fnd;
	return 1;
    } else {
	return 0;
    }
}

int 
Pad_List::Remove_last(void)
{
    return(Remove(tail->car));
}

int 
Pad_List::Index(const void *o) const
{
    int index = -1, i = 0;
    Pad_Iterator ei;
    void *e;
  
    DOLIST(ei, *this, void, e) {
	if (e == o) {
	    index = i;
	    break;
	} else {
	    i++;
	}
    }
  
    return(index);
}

int 
Pad_List::Member(const void *o) const
{
    int found = 0;
    Pad_Iterator i;
    void *e;
  
    DOLIST(i, *this, void, e) {
	if (e == o) {
	    found = 1;
	    break;
	}
    }
  
    return(found);
}

int 
Pad_List::Insert_before(void *obj, void *ref)
{
    int found = 0;
    Pad_Cons *e, *prev;
  
    e = head;
    prev = e;
    while (e) {
	if (e->car == ref) {
	    found = 1;
	    break;
	}
	prev = e;
	e = e->cdr;
    }

    if (found) {
	Pad_Cons *c = new Pad_Cons;

	c->car = obj;
	c->cdr = e;
  
	if (head == e) {
	    head = c;
	} else {
	    prev->cdr = c;
	}
    }
    return(found);
}

int 
Pad_List::Insert_after(void *obj, void *ref)
{
    int found = 0;
    Pad_Cons *e;
  
    e = head;
    while (e) {
	if (e->car == ref) {
	    found = 1;
	    break;
	}
	e = e->cdr;
    }

    if (found) {
	Pad_Cons *c = new Pad_Cons;

	c->car = obj;
	c->cdr = e->cdr;
  
	e->cdr = c;
	if (tail == e) {
	    tail = c;
	}
    }
    return(found);
}

//
// Noncopying append of self with l
//
void 
Pad_List::Join(Pad_List &l)
{
    if (head) {
        tail->cdr = l.head;
    } else {
        head = l.head;
    }
    tail = l.tail;

    l.head = NULL;
    l.tail = NULL;
}

int 
Pad_List::Push_new(void *e) 
{
    int found = Member(e);

    if (!found) {
	Push(e);
    }
  
    return(found);
}

int 
Pad_List::Push_new_last(void *e) 
{
    int found = Member(e);

    if (!found) {
	Push_last(e);
    }
  
    return(found);
}

void* 
Pad_List::Nth(int pos) const
{
    assert(pos >= 0);
  
    Pad_Cons *tmp = head;
  
    while (pos--) {
	if (!tmp) {
	    return NULL;
	}
	tmp = tmp->cdr;
    }

    return(tmp->car);
}

int
Pad_List::Length(void) const
{
    int len = 0;
    Pad_Cons *tmp = head;

    while (tmp) {
	len++;
	tmp = tmp->cdr;
    }

    return(len);
}

void* 
Pad_List::Pop()
{
    Pad_Cons *tmp = head;
    void *val;
  
    if (!head) {
	return NULL;
    }
  
    val = head->car;
  
    head = head->cdr;
    if (!head) {		// Check if deleted last element
	tail = NULL;
    }
    delete tmp;

    return val;
}

void 
Pad_List::Push(void *e)
{
    Pad_Cons *c = new Pad_Cons;

    c->car = e;
    c->cdr = head;
  
    if (head) {
	head = c;
    } else {
	head = c;
	tail = c;
    }
}

void 
Pad_List::Push_last(void *e)
{
    Pad_Cons *c = new Pad_Cons;

    c->car = e;
    c->cdr = NULL;
  
    if (head) {
	tail->cdr = c;
	tail = c;
    } else {
	head = c;
	tail = c;
    }
}

void 
Pad_List::Reverse() 
{
				// At least two long
    if ((head != NULL) && (head->cdr != NULL)) {
	Pad_Cons *prev = head, *cur = head->cdr, *next;
      
	cur = prev->cdr;
	prev->cdr = NULL;
      
	while (1) {
	    next = cur->cdr;
	    cur->cdr = prev;
	  
	    if (!next)
		break;
	  
	    prev = cur;
	    cur = next;
	}
	head = cur;
    }
}

//////////////////////////////////////////////////////
//    Pad_Iterator code
//////////////////////////////////////////////////////

Pad_Iterator::Pad_Iterator()
{
    list = NULL;
    ilist = NULL;
    first = '\0';
    e = NULL;
    more = 0;
    del = 0;
}

void 
Pad_Iterator::Init(const Pad_List &l)
{
    list = &l;
    ilist = l.head;
    first = 1;
    e = NULL;
    more = 0;
    del = 0;
}

void 
Pad_Iterator::Init(Pad_Iterator &i)
{
    if (!del) {
	list = i.list;
	ilist = i.ilist;
	first = '\0';
	e = i.e;
	more = i.more;
    }
    del = 0;
}

int 
Pad_Iterator::Next()
{
    if (first)
      {
	  first = '\0';
	  if (ilist)
	    {
		e = ilist->car;	      
		more = 1;
	    }
	  else
	    {
		more = 0;
	    }
      }
    else
      {
	  if (!del) {
	      if (ilist) {
		  ilist = ilist->cdr;
	      }
	  }
	  if (ilist)
	    {
		e = ilist->car;
		more = 1;	  
	    }
	  else
	    {
		more = 0;
	    }
      }
    del = 0;

    return(more);
}
