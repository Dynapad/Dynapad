/* hashing routines */
#include <string.h>
#include "hash.h"

#ifdef PAD_WIN
#  include <malloc.h>
#endif

/*
Synopsis
--------
char **get_hashtab_entry(char * name)
    Returns a pointer to a location which is keyed on name (a null-
    terminated string). Multiple calls with the same name return the same
    location. You can store an arbritrary pointer value in that location -
    the initial value is NULL.

int free_hashtab_entry(char *name)
    If name is in the table, storage for it is freed and it is removed
    from the table. Returns 1 if the name is was found in the table,
    0 if it was not.
*/

#ifndef NULL
#define NULL 0
#endif

typedef struct node {
    char *client_data;
    char *name;
    unsigned int hash;
    struct node *next;
} Node;

#define HASH_SIZE 1001 /* MUST BE AN ODD NUMBER ... */

static Node *table[HASH_SIZE];

/* this is a special node which marks the last bucket in a chain */
static Node sentinel = {NULL, NULL,  0xffffffff, NULL};

static int init_needed = 1;

/* PJ Weinberger's hashing function.

   Takes a string. Returns an integer, of which 28 bits are significant
   (though all may be used). Also returns the length of a string as a
   side effect.
*/
static unsigned int compute_hash(char *name, int *len /* RETURN */)
{
    unsigned int hash = 0, top_four;
    char c;
    int l = 0;

    while (c = *name++) {
        hash = (hash << 4) + c;
        if (top_four = hash & 0xf0000000) {
            hash ^= top_four | (top_four >> 24);
        }
        l++;
    }

    *len = l;
    return hash;
}

char **get_hashtab_entry(char *name)
{
    Node *node, *new, *prev = NULL;
    int len;

    /* compute the hash */
    unsigned int hash = compute_hash(name, &len);
    int index = hash % HASH_SIZE;

    if (init_needed) {
        int i;
        for (i = 0; i < HASH_SIZE; i++) {
            /* all entries in table set to the sentinel node */
            table[i] = &sentinel;
        }
        init_needed = 0;
    }

    /* all nodes in this node list are kept sorted by their hash values,
       which are also stored in the table. Hence we can skip entries with
       too small a hash value.
    */
    for (node = table[index]; node->hash < hash; node = node->next)
        prev = node;

    while (node->hash == hash) {
        if (!strcmp(node->name, name)) {
            return (char **)&node->client_data;
        } else {
            prev = node;
            node = node->next;
        }
    }

    /* gone past point in table where node should be */

    /* allocate new node */
    new = (Node*)malloc(sizeof(Node));
    new->hash = hash;
    new->client_data = NULL;
    memcpy(new->name = (char*)malloc(len + 1), name, len + 1);

    /* link it into table */
    new->next = node;
    if (prev)
        prev->next = new;
    else
        table[index] = new;

    return (char **)&new->client_data;
}

int free_hashtab_entry(char *name)
{
    Node *node, *prev = NULL;
    int len;

    /* compute the hash */
    unsigned int hash = compute_hash(name, &len);
    int index = hash % HASH_SIZE;

    if (init_needed) /* table must be empty ? */
        return 0;

    for (node = table[index]; node->hash < hash; node = node->next)
        prev = node;

    while (node->hash == hash) {
        if (!strcmp(node->name, name)) {
            free(node->name);
            if (prev)
                prev->next = node->next;
            else
                table[index] = node->next;
            free(node);
            return 1;
        } else {
            prev = node;
            node = node->next;
        }
    }

    /* if we get here we are freeing an entry that is not in the table. */
    return 0;
}
