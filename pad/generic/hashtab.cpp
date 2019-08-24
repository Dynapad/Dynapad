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
#include "hashtab.h"
#include <string.h>
#include <stdlib.h>

int
Pad_HashTable::keysize(void *key)
{
    if (type == PAD_STRING_TABLE)
	return strlen((char *)key) + 1;
    else
	return 4 * type;
}

void
Pad_HashTable::Init()
{
    int ret;

    if ((ret = db_create(&dbp, NULL, 0)) != 0) {
        fprintf(stderr, "db_create: %s\n", db_strerror(ret));
        exit(1);
    }
    if ((ret = dbp->open(dbp, NULL, NULL, NULL, DB_HASH, DB_CREATE, 0)) != 0) {
        dbp->err(dbp, ret, "dbp->open");
        exit(1);
    }
}

Pad_HashTable::Pad_HashTable(int t)
{
    type = t;
    Init();
}

Pad_HashTable::Pad_HashTable()
{
    type = PAD_VOID_TABLE;
    Init();
}

// caller must Remove entries and free data
Pad_HashTable::~Pad_HashTable()
{
    int ret;

    if ((ret = dbp->close(dbp, 0)))
	fprintf(stderr, "dbp->close %d\n", ret);
}

int
Pad_HashTable::Remove(void *rawkey)
{
    int ret;
    DBT key;

    memset(&key, 0, sizeof(key));

    if (type == PAD_VOID_TABLE)
	key.data = &rawkey;
    else
        key.data = rawkey;
    key.size = keysize(key.data);

    if ((ret = dbp->del(dbp, NULL, &key, 0)) == 0)
	return 1;
    else if (ret == DB_NOTFOUND)
	return 0;
    else {
	dbp->err(dbp, ret, "DB->del");
	exit(1);
    }
}

int
Pad_HashTable::Set(void *rawkey, void *rawval)
{
    int ret;
    DBT key, data;

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    if (type == PAD_VOID_TABLE)
	key.data = &rawkey;
    else
        key.data = rawkey;
    key.size = keysize(key.data);

    data.data = &rawval;
    data.size = sizeof(void *);

    if ((ret = dbp->put(dbp, NULL, &key, &data, 0)) == 0)
        return 1;
    else if (ret == DB_KEYEXIST)
	return 0;
    else {
        dbp->err(dbp, ret, "DB->put");
        exit(1);
    }
}

int
Pad_HashTable::Length() const
{
    int ret;
    DB_HASH_STAT *dbstatp;
    int ndata;

    if ((ret = dbp->stat(dbp, NULL, &dbstatp, 0)) == 0) {
        ndata = dbstatp->hash_ndata;
        free(dbstatp);
        return ndata;
    } else {
        dbp->err(dbp, ret, "DB->stat");
        exit(1);
    }
}


void *
Pad_HashTable::Get(void *rawkey)
{
    int ret;
    DBT key, data;

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    if (type == PAD_VOID_TABLE)
	key.data = &rawkey;
    else
        key.data = rawkey;
    key.size = keysize(key.data);
    if ((ret = dbp->get(dbp, NULL, &key, &data, 0)) == 0)
	return *(void **)data.data;
    else if (ret == DB_NOTFOUND)
	return NULL;
    else {
	dbp->err(dbp, ret, "DB->get");
	exit(1);
    }
}

// Used by Pad_HashTableIterator
void *
Pad_HashTable::Init(Pad_HashSearch &dbcp, void *rawkey)
{
    int ret;
    DBT key, data;

    if ((ret = dbp->cursor(dbp, NULL, &dbcp, 0)) != 0) {
	dbp->err(dbp, ret, "DB->cursor");
	exit(1);
    }

    return Next(dbcp, rawkey);
}

void *
Pad_HashTable::Next(Pad_HashSearch dbcp, void *rawkey)
{
    int ret;
    DBT key, data;

    memset(&key, 0, sizeof(key));
    memset(&data, 0, sizeof(data));

    if ((ret = dbcp->c_get(dbcp, &key, &data, DB_NEXT)) == 0) {
	rawkey = key.data;
        return *(void **)data.data;
    }
    if (ret != DB_NOTFOUND) {
	dbp->err(dbp, ret, "DBcursor->get");
	exit(1);
    }
    rawkey = NULL;
    return NULL;
}

Pad_HashTableIterator::Pad_HashTableIterator()
{
    key = NULL;
    table = NULL;
}

void *
Pad_HashTableIterator::Init(Pad_HashTable &t)
{
    table = &t;
    return table->Init(dbcp, key);
}

void *
Pad_HashTableIterator::Next()
{
    return table ? table->Next(dbcp, key) : NULL;
}
