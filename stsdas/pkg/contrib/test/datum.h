/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/



#ifdef THINK_C
#include <MacTypes.h>
#endif

#define MATSIZE 4			/* Max order of matrices for Weights */

struct aBlock;              /* for forward reference */

typedef struct datum        /* basic node for all data */

{
	double value;        /* value of node's coefficient */
	struct datum  *next; /* pointer to next node */
	struct aBlock *car;  /* pointer to an array block */
	int name;            /* name of variable (pointer to name table) */
	int index[5];        /* subscripts ; index[4] = num of dims */
	int type;            /* type of variable (Observation, global parameter,	
	                               subscripted parameter ) */
} DATUM, *DATUMPTR;


typedef struct declaration  /* entry on declaration stack */
{
	DATUMPTR value;     /* structure */
	int last;           /* previous entry with same name */
	int name;           /* pointer to name */
	int ext;
} DECLARATION;

typedef struct aBlock {      /* array implementation */
	int dim[5];				 /* size of each dimension */
	int mark;				 /* garbage collection tag */
	struct aBlock *next;     /* next array Block */
	DATUMPTR elem[1];        /* 1 is a dummy, actual size is variable */
} ArrayBlock, *ArrayBlockPtr;

ArrayBlockPtr arrayalloc();	/* allocate an array block */
DATUMPTR getnode();         /* get a new node */
DATUMPTR copynode();        /* get a new node and copy contents of argument
                              node */
                              
DATUMPTR popval();          /* pop top of data stack */
DATUMPTR popitem();         /* push argument on top of data stack */
double getdatumvalue();     /* get the value of a DATUM */
