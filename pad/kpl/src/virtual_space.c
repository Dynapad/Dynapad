
/*
virtual_space() is a routine that simulates arbitrarily large contiguous arrays.

There are two ways to initialize a virtual space:

(1)	The simplest way is:

		char *client = NULL;

		...

		virtual_space(&client, ...);

	This results in a default page size of 1024 bytes.

(2)	Alternatively, you can set a particular page size by:

		char *client = new_virtual_space(yourpagesize);

		...

		virtual_space(&client, ...);

	The pagesize will be rounded down to the nearest sizeof(long) multiple.

Algorithm:

	Each virtual space is stored as a tree.
	Each node contains 2N long words, where N = pagesize/sizeof(long).
	A node consists of two parts:

		The first N words are pointers to more nodes.
		The last N words contain the data for this node.

	The pages of the virtual space are organized as follows:

		Page 0 is in the first node.
		Pages 1...N-1 are in the nodes pointed to by the first node.
		Pages N...N*N-1 are in the nodes pointed to by these nodes, etc.

	Note that the first word of the first node is never used in the above
	scheme.  We use this word to store the page size of the virtual space.

	A virtual byte B resides on page (B / pagesize), at byte (B mod pagesize).

	When a data segment is fetched or stored, just those pages containing
	it are accessed, and created if necessary.
*/

#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#ifndef PAD_WIN
#  define min(a,b) ( (a) < (b) ? (a) : (b) )
#  define max(a,b) ( (a) > (b) ? (a) : (b) )
#endif

static int space_used = 0;

int virtual_space_used()	/* a very useful diagnostic */
{
	return space_used;
}

static char *new_page(int pagesize)
{
	space_used += 2 * pagesize;
	return calloc(2 * pagesize, sizeof(char));
}

char *virtual_page(char *client, int page)
{
	int path[100], k, n, pagesize;
	char *node, **ptr;
	
	pagesize = *(long *)client;
	n = pagesize / sizeof(char *);

	for (k = 0 ; page > 0 ; page /= n)
		path[k++] = page % n;
	for (node = client ; k-- ; node = *ptr)
		if ( *(ptr = (char **)node + path[k]) == NULL)
			*ptr = new_page(pagesize);
	return node + pagesize;
}

char *new_virtual_space(int pagesize)
{
	char *node;

	pagesize -= pagesize % sizeof(char *);
	node = new_page(pagesize);
	*(long *)node = pagesize;
	return node;
}

static void delete_nodes(char *node, int lo, int hi)
{
	char **ptr, **ptr0;

	if (node) {
		ptr0 = (char **)node;
		for (ptr = ptr0 + lo ; ptr - ptr0 < hi ; ptr++)
			delete_nodes(*ptr, 0, hi);
		free(node);
	}
}

void delete_virtual_space(char *client)
{
/* GENERAL ALGORITHM:
	Go through pointer part of first page.
	For every non-null pointer, recursively call delete routine,
	then free the page itself.

*/
	int n;

	if (client) {
		n = *(long *)client / sizeof(char *);
		delete_nodes(client, 1, n);
	}
}

void virtual_space(char **client, int mode, long addr, long size, char *buffer)
{
	int page, first_page, last_page, lo, hi, pagesize;
	char *buf, *spc;

	if (*client == NULL)
		*client = new_virtual_space(1024);

	pagesize = *(long *)(*client);

	first_page = addr / pagesize;
	last_page  = (addr + size - 1) / pagesize;

	for (page = first_page ; page <= last_page ; page++) {
		lo = max(addr, page * pagesize);
		hi = min(addr + size, (page+1) * pagesize);
		spc = virtual_page(*client, page) + (lo % pagesize);
		buf = buffer + lo - addr;
		if (mode == 0)
			memcpy(buf, spc, hi - lo);	/* READ */
		else
			memcpy(spc, buf, hi - lo);	/* WRITE */
	}
}

