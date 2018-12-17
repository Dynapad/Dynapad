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


#ifndef _PAD_HASHTABLE_H
#define _PAD_HASHTABLE_H

#include <db.h>

#define PAD_STRING_TABLE     0
#define PAD_VOID_TABLE       1

typedef DBC *Pad_HashSearch;

#define DOTABLE(iter, table, type, element) \
    for (element = (type *) iter.Init(table); element ; \
	 element = (type *) iter.Next() )

#ifndef NULL
#define NULL 0
#endif

class Pad_HashTable {
  public:

    Pad_HashTable(int type);
    Pad_HashTable();
    ~Pad_HashTable();

    // Set():
    //     adds an entry to table.
    //     Returns 1 if key was found in table beforehand, 0 otherwise.
    int Set(void *key, void *val);

    // Remove();
    //    removes an entry from  table.
    //    returns 1 if the entry was found and removed, 0 otherwise.	
    int Remove(void *key);

    // Get():
    //     Retrieves an entry from the table. Returns NULL if the entry is not
    //     in the table.
    void *Get(void *key);

    // Returns the number of entries in the table
    int Length() const;

    // Used by Pad_HashTableIterator (maybe these should be protected friends?)
    void *Init(Pad_HashSearch &ptr, voidPtr &key);
    void *Next(Pad_HashSearch ptr, voidPtr &key);

  private:
    void Init();
    inline int keysize(void *key);
    DB *dbp;
    int type;
};


class Pad_HashTableIterator {
  public:
    DBC *dbcp;
    voidPtr key; // last key found
    Pad_HashTable *table;

    Pad_HashTableIterator();
    void *Init(Pad_HashTable &t);
    void *Next();
};

#endif



