/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* List processor code */

#define  import_spp
#define  import_libc
#define  import_stdio
#include <iraf.h>

#include "defines.h"
#include "datum.h"

#define NUMNODES 1000		/* Number of nodes in list structure */
static DATUM *nodes = NULL;     /* array of nodes */
static DATUMPTR nextfree=NULL;  /* pointer to next free node */
static DATUMPTR prevfree=NULL;  /* pointer to  last free node */
static ArrayBlockPtr ArrayBlockList = NULL;


char *MemAlloc();  

marklist(k)  /* make all active list structure (recursive ) starting at k */
DATUMPTR k;
{
	while(k)     /* start at k */
	{
		if(k->car)  /* if vector stuff exists */
			markarray(k->car);  /* mark it */
		if(k->name >= 0)  /* if node not marked, mark it */
			k->name = -1 - k->name; /* by setting name < 0 */
		k = k->next;  /* next k */
	}
}

markarray(a)
ArrayBlockPtr a;
{
	int j, k;
	long i, nelems;
	a->mark = -1;	/* mark array block proper */
	for (i=0, nelems=1; i<a->dim[4]; ++i) nelems *= a->dim[i];
	for (i=0; i<nelems; ++i) {
		if (a->elem[i] != NULL) marklist(a->elem[i]);
	}
}

markactive()  /* mark all active list structure */
{
	marklist(prevfree);/* mark starting at previous free node (may still be active ) */
	markdeclare(); /* mark declared variables */
	markvalue();  /* mark all stacks */
}

collectactive()  /* collect the unmarked nodes */	
{
	int i;
	ArrayBlockPtr a, b, *c;
	for(i=0;i<NUMNODES;i++) { /* sweep through entire node array */	
		if(nodes[i].name < 0) { /* Active cell */ /* if it was marked it's active */
			nodes[i].name = -1 - nodes[i].name; /* unmark it */
		} else { /* Inactive Cell */
			nodes[i].next = nextfree;/* add to free node list */
			nextfree = &nodes[i];			
		}
	}
	c = &ArrayBlockList;
	for (a = ArrayBlockList; a != NULL; a = b) { /* free array blocks */
		b = a->next;
		if (a->mark == 0) {
			free(a);	/* free it */
			*c = b;		/* relink around it */
		} else {
			a->mark = 0;	/* unmark it */
			c = &a->next;	/* keep track of prev block */
		}
	}
}

initliststruct()  /* initialize the list structure */
{
	int i;

	/* allocate NUMNODES nodes */
	nodes = (DATUM *)MemAlloc("LIST",(long)NUMNODES*sizeof(DATUM));
	for(i=0;i<NUMNODES;i++) /* add them all to the free list */
	{
		nodes[i].next = nextfree;
		nextfree = &nodes[i];
	}
}

DATUMPTR nextfreenode()  /* get the next free node */
{
	DATUMPTR k;

	if(nextfree) {  /* if there is a node */
		k = prevfree = nextfree;  /* get it and */
		nextfree = k->next;   /* initialize all its fields */
		k->next = NULL;
		k->car  = NULL;
		k->name = 0;
		k->index[0] = 0;
		k->index[1] = 0;
		k->index[2] = 0;
		k->index[3] = 0;
		k->index[4] = 0;
		k->type = -1;
		k->value = 0.0;
		return k;
	}
	else
		return NULL; /* otherwise return Null */
}

DATUMPTR getnode() /* get a free node */
{
	DATUMPTR k;

	if(k = nextfreenode())  /* if there is a free node */
		return k;   /* return it */
	else
	{
		markactive(); /* mark all active structure */

		collectactive(); /* collect them */
		if(k = nextfreenode()) /* return free node if it exists */
			return k;
		else
			fatalerror("Out of Memory\n","");/* otherwise we've run out of memory */
	}
}

DATUMPTR copynode(i)  /* make a copy of the current node */
DATUMPTR i;
{
	DATUMPTR k;

	k = getnode(); /* get a new node */
	k->value = i->value; /* and copy the old one into the new one */
	k->name  = i->name;
	k->next = i->next;
	k->car   = i->car;
	k->index[0] = i->index[0];
	k->index[1] = i->index[1];
	k->index[2] = i->index[2];
	k->index[3] = i->index[3];
	k->index[4] = i->index[4];
	k->type  = i->type;
	return k;
}

double getdatumvalue(i)  /* get the value pointed to by a pointer */
DATUMPTR i;
{
	if(i==NULL)
		return 0.0; /* It's zero, if pointer is null */
	else
		return i->value;  /* otherwise it's the value field */
}


ArrayBlockPtr arrayalloc(dim)
int dim[5];
{
	ArrayBlockPtr a;
	long size, nelems;
	int i;

	for (i=0, nelems=1; i<dim[4]; ++i) nelems *= dim[i];
	size = sizeof(ArrayBlock) + (nelems - 1) * sizeof(DATUMPTR);
	a = (ArrayBlockPtr)MemAlloc("ArrayBlock", size);
	if (a == NULL) {
		markactive(); /* mark all active structure */
		collectactive(); /* collect them */
		a = (ArrayBlockPtr)MemAlloc("ArrayBlock", size);
		if (a == NULL) {
			fatalerror("Out of Memory\n",""); 
		}
	}
	a->mark = 0;
	a->dim[0] = dim[0];
	a->dim[1] = dim[1];
	a->dim[2] = dim[2];
	a->dim[3] = dim[3];
	a->dim[4] = dim[4];
	for (i=0; i<nelems; ++i) a->elem[i] = NULL;
	a->next = ArrayBlockList;	/* link into master list */
	ArrayBlockList = a;
	return(a);
}
