/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_math
#include <iraf.h>

#include "defines.h"
#include "datum.h"
#include "house.h"
#include "files.h"
#include "machine.h"

#define DSTACKLENGTH 100	/* Number of declared variables */
#define MARKLENGTH 10		/* Size of Mark Stack */
#define NoType (-1)

extern SYMBOL *symtable;
/*static*/
DECLARATION declarestack[DSTACKLENGTH]; /* stack to store declared variables */
static int markstack[MARKLENGTH]; /* stack to save stack position of previous stack frame */
static int markptr = 0; /* pointer to mark stack */
static int declareptr = 0;  /* pointer to declare stack */
static int declarelimit = 0; /* last declared variable */
static int globlimit = 0; /* last declared global variable */
int traceflag = 0;
int lastindex = -1;
int indexlist[4];
int numdims = 0;

extern FILE *fp;

static traceset(name,k)  /* trace a value */
char *name;
DATUMPTR k;
{
	if(traceflag)
		printvalue(name,k);
}

label0(pc) /* label0 function */
int *pc;
{
	trace2(*pc); /* trace instruction */
	if(markptr >= MARKLENGTH) /* check for overflow */
		fatalerror("Mark Stack Overflow\n","");
	markstack[markptr] = declareptr; /* insert declare pointer into mark stack */
	markptr++;
	*pc += 2; /* increment p.c. */
}

markstackdecrement() /* decrement mark stack */
{
	int mark;

	if((mark = markstack[markptr-1]) < 0) /* if top of stack < 0, error */
		fatalerror("Garbage in Mark Stack\n","");
	for(;declareptr>mark;) /* remove items from declare stack down to saved level */
	{
		--declareptr;
		putvalueptr(declarestack[declareptr].name, /*adjust pointers */
		declarestack[declareptr].last);
	}
}

label1(pc) /* label 1 function */
int *pc;
{
	trace2(*pc);
	markstackdecrement(); /*decrement mark stack */
	*pc += 2; /* increment p.c. */

}

label2(pc)  /* label 2 function */
int *pc;
{
	trace2(*pc);
	markstackdecrement(); /*decrement mark stack */
	--markptr; /* decrement markstack pointer */
	if(markptr < 0) /* check for underflow */
		fatalerror("Mark Stack Underflow\n","");
	*pc += 2; /* increment p.c. */

}

reset()  /* reset declare stack */
{
#ifdef DEBUG4D
fprintf(stderr, "reset declare stack\n");
#endif
	for(;declareptr>0;)  /* go backwards thorugh declare stack */
	{
		--declareptr;
		putvalueptr(declarestack[declareptr].name, /* adjust pointers */
		declarestack[declareptr].last);
	}
	declareptr=0;
	declarelimit=0;
	globlimit=0;
	traceflag = 0;
}

markdeclare()  /* mark the declare stack */
{
	int i;
#ifdef DEBUG4D
fprintf(stderr, "mark declare stack\n");
#endif

	for(i=0;i<declareptr;i++)  /* sweep thru declare stack */
		marklist(declarestack[i].value);  /* mark each entry */
}

trace1(pc)  /* trace 1 argument */
int pc;
{
	if(traceflag >= 2)
	{
		fprintf(fp,"%3d  %s\n",pc,getnam(getcore(pc)));	
		fflush(fp);
	}
}

trace2(pc)  /* trace 2 arguments */
int pc;
{
	if(traceflag >= 2)
	{
		fprintf(fp,"%3d  %s  %s\n",pc,getnam(getcore(pc)),
		getnam(getcore(pc+1)));	
		fflush(fp);
	}
}

settrace0(pc)  /* set trace to 0 */
int *pc;
{
	traceflag = 0;
	trace1(*pc);
	*pc += 1; /* increment p.c. */

}

settrace1(pc)  /* set trace to 1 */
int *pc;
{
	traceflag = 1;
	trace1(*pc);
	*pc += 1; /* increment p.c. */

}

settrace2(pc)  /* set trace to 2 */
int *pc;
{
	traceflag = 2;
	trace1(*pc);
	*pc += 1; /* increment p.c. */

}

settrace3(pc)  /* set trace to 3 */
int *pc;
{
	traceflag = 3;
	trace1(*pc);
	*pc += 1; /* increment p.c. */

}

pushcon(pc)  /* push a constant onto the stack */
int *pc;
{
	DATUMPTR x;

	trace2(*pc);
	x = getnode();  /* get a node */
	/* translate the ASCII into a value */
	/* sscanf(getnam(getcore(*pc+1)),"%lf",&(x->value)); */
	/* get constant stored in index space at compile time */
	x->value = symtable[getcore(*pc+1)].x.c.cnst;
	pushval(x); /* push onto stack */
	tracevalue(); /* trace value */
	*pc += 2; /* increment p.c. */

}


swap()  /* Routine to fix indexing bug */
{
	DATUMPTR k,l;

	k = popitem();
	l = popitem();
	pushval(k);
	pushval(l);
}



stovec(pc)   /* store a value in a vector variable */
int *pc;
{
	int i, j, index, dims, subscript;
	DATUMPTR d1, m;
	ArrayBlockPtr a;

	trace2(*pc);
	
	i = getvalueptr(getcore(*pc+1)); /* get name of variable */
	if(i<declarelimit && i>=globlimit) { 
		fatalerror("Value name = %s for vector storage has not been declared\n",
			getnam(getcore(*pc+1)));
	}
	if(declarestack[i].ext >= 0) { 
		fatalerror("Attempt to change datum value in vector storage,value to be stored is a parameter or observation.\n","");
	}
	m = declarestack[i].value; /* get the variable node */
	if(!(m && m->car)) { /* error if it's not a vector */
		fatalerror("Variable %s has not been declared a vector\n",
			getnam(getcore(*pc+1)));
	}
	a = m->car;
	dims = a->dim[4];
	index = 0;
	for (j = dims-1; j >= 0; --j) {
		swap();
		d1 = popval();
		subscript = d1->value;
		if (subscript < 0 || subscript >= a->dim[j]) {
			fatalerror("Array index out of bounds.\n","");
		}
		index = index * a->dim[j] + subscript;
	}
	a->elem[index] = popval(); /* put val in array */
	if(traceflag) {
		char str[256];
		sprintf(str,"%s[%d]",getnam(declarestack[i].name), index);
		traceset(str,m->car->elem[index]); 
	}
	*pc += 2; /* increment p.c. */

	unsave();  /* drop intermediate values */

}

DATUMPTR copyvec(k) /* copy a vector */
DATUMPTR k;
{
	int i, j, h, nelems;
	if(!k)  /* if nothing, return null */
		return NULL;
	if(k->car)  /* if a vector */
	{
		DATUMPTR m, n;
		ArrayBlockPtr a, b;

		a = k->car;
		b = arrayalloc(a->dim);
		nelems = a->dim[0] * a->dim[1] * a->dim[2] * a->dim[3];
		for (h=0; h<nelems; ++h) {
			b->elem[h] = a->elem[h]; 
			/*(b->elem[h] = copyvec(a->elem[h]);*/
		}
		m = getnode();  /* get a node */
		m->car = b; /* make it point to the copied vector */
		return m;
	} else {
		k = copynode(k);
		/* experimental : */
		/*if (k->next) k->next = copyvec(k->next);*/
		/*
		if (k->next && k->next->name == 0) {
			fprintf(stderr, "stop!\n");
			sleep(15);
		}
		*/
		return(k);
	}
}

sto(pc)    /* store a value */
int *pc;
{
	DATUMPTR k,m;

	int i;
	trace2(*pc);  /* trace input */
	i = getvalueptr(getcore(*pc+1));  /* get the variable name */
	if(i<declarelimit && i>=globlimit) /* error if not declared */
		fatalerror("Value name = %s for non-vector storage has not been declared\n",getnam(getcore(*pc+1)));
	if(declarestack[i].ext >= 0) /* error if observation or parameter */
		fatalerror("Attempt to change datum value, value to be stored is a parameter or observation.\n","");
	k = popitem(); /* pop stack */
	m = declarestack[i].value; /* get variable pointer */
	if(m->car && !k->car || k->car && !m->car) /* look for type conflict */
		fatalerror("Scalar/vector type conflict with variable %s\n",
		getnam(getcore(*pc+1)));
	declarestack[i].value = copyvec(k); /* if none, copy the item, set variable to new value */
#ifdef DEBUG4D
	printdatum(declarestack[i].value);
#endif
	traceset(getnam(declarestack[i].name),declarestack[i].value); /* trace the result */
	*pc += 2; /* increment p.c. */

	unsave();  /* drop intermediate values */
}

drop(pc) /* drop top of stack */
int *pc;
{
	trace1(*pc); /* trace */
	popitem(); /* pop stack */
	*pc += 1; /* increment p.c. */

	unsave();  /* drop intermediate values */
}

dupl(pc)  /* duplicate top of stack */
int *pc;
{
	DATUMPTR k;

	trace1(*pc); /* trace */
	k = popitem(); /* pop stack */
	pushval(k);
	pushval(k); /* push  it twice */
	*pc += 1; /* increment p.c. */

	unsave();  /* drop intermediate values */
}

pushpar(i,k,number) /* push parameter on stack */
int i,number[5];
DATUMPTR k;
{
	int theType;
	int symInx;
	int j;

	theType = declarestack[i].ext; /* get the type of the variable */
	symInx = declarestack[i].name;
	k = copynode(k);  /* copy the vector */  /* was copyvec, but k->car==NIL*/
	if (k->next) k->next = copynode(k->next); /* hmmm ... */
	pushval(k); /* push onto stack */
	if (theType != IndexedType) /* check for type conflict */
		fatalerror("Attempt to fetch subscripted variable from scalar\n","");
	else {
		/* get subscripted variable */
		k->value = getxparval(symInx, number);
		if(k->next) {
			/* set index in the variable on the stack */
			for (j=0; j<5; ++j) 
				k->next->index[j] = number[j];
#ifdef DEBUG4D
			fprintf(stderr, "pushpar : %d %s [%d,%d,%d,%d](%d) = %f\n",
				symInx, getnam(symInx),
				number[0],number[1],number[2],number[3],number[4],
				k->value);
#endif
		}
	}
}

pushvec(pc) /* push a vector on the stack */	
int *pc;
{
	int i, dims, index, subscript;
	int j, symInx;
	ArrayBlockPtr a;
	DATUMPTR k, d1, d2;

	trace2(*pc); /* trace */
	i = getvalueptr(getcore(*pc+1)); /* get name of variable */

	if(i<declarelimit && i>=globlimit) /* check that it is declared */
		fatalerror("Vector name = %s for push onto stack has not been declared.\n",getnam(getcore(*pc+1)));
	k = declarestack[i].value; /* get the value */
	if(!(k->car)) { /* if its scalar type */
		int inx[5];
		symInx = declarestack[i].name;
		inx[0] = inx[1] = inx[2] = inx[3] = 0;
		inx[4] = dims = symtable[symInx].x.xvars[4];
		for (j=0; j<dims; ++j) {
			d1 = popval();
			inx[j] = d1->value;
		}
		pushpar(i,k,inx); /* push it as a vector */
		tracevalue(); /* trace value */
		*pc += 2; /* increment p.c. */
	} else {
		a = k->car;
		dims = a->dim[4];
		index = 0;
		for (j=dims-1; j>=0; --j) {
			d1 = popval();
			subscript = d1->value;
			if (subscript < 0 || subscript >= a->dim[j]) {
				fatalerror("Array index out of bounds.\n","");
			}
			index = index * a->dim[j] + subscript;
		}
		if (a->elem[index]) pushval(a->elem[index]);
		else pushfalse();
		tracevalue(); /* trace value */
		*pc += 2; /* increment p.c. */
	}
}

push(pc)  /* push a scalar onto stack */
int *pc;
{
	int i, m;
	DATUMPTR k;
	int number[5];
	int symInx;
	char *theName;
	int theType;

	trace2(*pc); /* trace */
	i = getvalueptr(getcore(*pc+1)); /* get name of variable */
	if(i<declarelimit && i>=globlimit) /* check that it's declared */
		fatalerror("Scalar name = %s for puch onto stack has not been declared.\n",getnam(getcore(*pc+1)));
	/* get type of variable. Is it external data ? */
	if((theType = declarestack[i].ext) >= 0) {
		/* Yes; get string for variable */
		symInx = declarestack[i].name;
		theName = getnam(symInx);
		/* copy the variable to be pushed on stack */
		k = copyvec(declarestack[i].value);
		k = copynode(k); /* copy the node to bet a new node */
		pushval(k); /* push onto stack */
		switch(theType) {   /* get the value from files */
			case GlobalType:
				k->value = getparamval(symInx);
				break;
			case DataType:
				k->value = getdataval(theName);
				break;
			case ObsType:
				k->value = gettheobs(theName);
				break;
			default:
				/* indexed variable no declared? */
				number[0] = number[1] = number[2] = number[3] = 0;
				number[4] = symtable[symInx].x.xvars[4];
				for (m=0; m<symtable[symInx].x.xvars[4]; ++m) {
					if((number[m] 
						= getdataint(getnam(symtable[symInx].x.xvars[m]))) < 0) { 
						fatalerror(
							"Indexed variable %s is not currently defined\n",
							theName);
					}
				}
				/* get indexed variable valeu */
				k->value = getxparval(symInx, number);
				if(k->next) {
					/* set index in the variable on the stack */
					for (m=0; m<5; ++m) 
						k->next->index[m] = number[m];
#ifdef DEBUG4D
			fprintf(stderr, "push : %d %d %s [%d,%d,%d,%d](%d)\n",
				symInx, k->name, theName,
				number[0],number[1],number[2],number[3],number[4]);
#endif
				}
				break;
		}
	} else {
		pushval(declarestack[i].value); /* it's a regular variable - push it */
	}
	tracevalue(); /* trace value */
	*pc += 2; /* increment p.c. */

}

int defarg(pc)  /* define a function argument on the declare stack */
int *pc;
{
	int corename;

	trace2(*pc); /* trace args*/
	corename = getcore(*pc+1); /* get the name of the argument */
	/* pop stack (could be a vector) -> new argument */
	declarestack[declareptr].value = popitem(); 
#ifdef DEBUG4D
	printdatum(declarestack[declareptr].value);
#endif
	/* location of previous instance of the variable */
	declarestack[declareptr].last = getvalueptr(corename); 
	declarestack[declareptr].name = corename; /* name of variable */
	declarestack[declareptr].ext = NoType; /* type of variable is NoType*/

	if(declareptr<DSTACKLENGTH) {
		/* increment declare stack pointer if not full */
		putvalueptr(getcore(*pc+1),declareptr);
		declareptr++;
	} else {
		fatalerror("Out of declare stack space for function argument.\n","");
	}
	*pc += 2; /* increment p.c. */
	unsave();  /* drop intermediate values */
}

DATUMPTR def1(pc,type,corename) /* define a non-derivative variable of given type */
int *pc;
int type;
int *corename;
{
	DATUMPTR j;

	*corename = getcore(*pc+1); /* get name */
	j = getnode();  /* get a node */
	declarestack[declareptr].value = j; /* insert empty node into declare stack */
#ifdef DEBUG4D
	printdatum(declarestack[declareptr].value);
#endif
	declarestack[declareptr].last = getvalueptr(*corename);
	declarestack[declareptr].name = *corename;
	declarestack[declareptr].ext = type;
	if(declareptr<DSTACKLENGTH)                /* error if full */
	{
		putvalueptr(getcore(*pc+1),declareptr); /* increment stack pointer */
		declareptr++;
	}
	else
		fatalerror("Out of declare stack space for non-derivative variable of a given type.\n","");
	if (type == IndexedType) {
		symtable[*corename].x.xvars[0] = (lastindex >= 0) ? indexlist[0] : -1;
		symtable[*corename].x.xvars[1] = (lastindex >= 1) ? indexlist[1] : -1;
		symtable[*corename].x.xvars[2] = (lastindex >= 2) ? indexlist[2] : -1;
		symtable[*corename].x.xvars[3] = (lastindex >= 3) ? indexlist[3] : -1;
		symtable[*corename].x.xvars[4] = lastindex + 1;
	}
	*pc += 2; /* increment p.c. */

	return j; /* return node */
}

int def2(pc,type) /* define a parameter or observation of given type that has a derivative */
int *pc;
int type;
{
	DATUMPTR j,k;
	int corename;

	j = def1(pc,type,&corename); /* define the local variable */
	k = getnode();  /* get a node for the derivative */
	j->next = k; /* make j point to it */
	k->value = 1.0; /* set the derivative to 1.0 */
	k->name  = corename; /* put name */
	k->type  = type; /* and type into derivative */
	/*
	if (type == IndexedType) {
		symtable[corename].x.xvars[0] = (lastindex >= 0) ? indexlist[0] : -1;
		symtable[corename].x.xvars[1] = (lastindex >= 1) ? indexlist[1] : -1;
		symtable[corename].x.xvars[2] = (lastindex >= 2) ? indexlist[2] : -1;
		symtable[corename].x.xvars[3] = (lastindex >= 3) ? indexlist[3] : -1;
		symtable[corename].x.xvars[4] = lastindex + 1;
	}
	*/
}

int defindex(pc) /* define index variable */
int *pc;
{
	int corename;
	int name;

	trace2(*pc); /* trace */
	corename = getcore(*pc+1); /* get name of variable */

	/* index is saved in lastindex for fetching the subscripted parameter */
	if (++lastindex > 3) {  /* more than 4 dimensions */
		fatalerror("Too many dimensions in array.");
	}
	indexlist[lastindex] = corename;  
	*pc += 2; /* increment p.c. */

}

int defvec(pc) /* define a vector variable */
int *pc;
{
	DATUMPTR j, d1;
	int i, corename, dim[5];

	trace2(*pc); /* trace */
	j = def1(pc,NoType,&corename); /* define the variable */
	dim[0] = dim[1] = dim[2] = dim[3] = 1;
	dim[4] = numdims;
	for (i=0; i<numdims; ++i) { /* I think I'll need to reverse this */
		d1 = popval();
		dim[i] = d1->value;
	}
	j->car = arrayalloc(dim);
#ifdef DEBUG4D
fprintf(stderr, "defvec : %1d %2d %2d %2d %2d \n",dim[4],dim[0],dim[1],dim[2],
	dim[3]);
#endif
	numdims = 0;
}
	
	
int defvar(pc) /* define a variable */
int *pc;
{
	DATUMPTR j;
	int corename;

	trace2(*pc); /* trace */
	j = def1(pc,NoType,&corename); /* define the variable */
}

int defdat(pc) /* define a data type quantity */
	int *pc;
{
	DATUMPTR j;
	int corename;

	trace2(*pc); /* trace */
	j = def1(pc,DataType,&corename); /* define the variable */
}

int defconst(pc) /* define a cnstant (data fetched from parameter file ) */
	int *pc;
{
	DATUMPTR j;
	int corename;

	trace2(*pc); /* trace */
	j = def1(pc,GlobalType,&corename); /* define the variable */
}


int defxcon(pc) /* define indexed constant (data in parameter file ) */
	int *pc;
{
	DATUMPTR j;
	int corename;

	trace2(*pc); /* trace */
	if(lastindex<0) /* be sure index has been defined */
		fatalerror("No indexed constant for data in paramter file has been defined\n","");
	/* if it has,define the variable */
	j = def1(pc,IndexedType,&corename); 
	lastindex = -1; /* redefine lastindex to -1 */
}


int defpar(pc) /* define a parameter */
	int *pc;
{
	DATUMPTR j,k;
	int corename;

	trace2(*pc); /* trace */
	def2(pc,GlobalType); /* define the variable */
}

int defxpar(pc) /* define indexed parameter */
	int *pc;
{
	DATUMPTR j,k;
	int corename;

	trace2(*pc); /* trace */
	if(lastindex<0) /* check that index is defined */
		fatalerror("No indexed parameter has been defined\n","");
	def2(pc,IndexedType); /* define the parameter */

	lastindex = -1; /* undefine the index */
}

int defobs(pc)  /* define an observation variable */
	int *pc;
{
	DATUMPTR j,k;
	int corename;

	trace2(*pc); /* trace */
	def2(pc,ObsType); /* define the variable */
}

fcall(pc) /* call a function */
	int *pc;
{
	int save,savedeclare;

	trace2(*pc); /* trace */
	save = *pc + 1; /* save p.c. */
	savedeclare = declarelimit; /* save declare limit */
	declarelimit = declareptr; /* save declare pointer (tells where local vairables are )*/
	interpret(getcore(save)); /* interpret starting from name of function called */
	declarelimit = savedeclare; /* restore declare limit */
	tracevalue(); /* trace result */
	*pc = save+1; /* restore p.c. */
}

finis(pc)  /* end statement */
	int *pc;
{
	trace1(*pc); /* trace */
	for(;declareptr>declarelimit;) /* decrement declare stack */
	{
		--declareptr;
		putvalueptr(declarestack[declareptr].name, /* restore old definition of variables */
		declarestack[declareptr].last);
	}
	*pc += 1; /* increment p.c. */

}

finisglob(pc) /* end of global definitions */
	int *pc;
{
	trace1(*pc); /* trace */
	globlimit = declareptr; /* set global limit to current declare pointer */
	*pc += 1; /* increment p.c. */

}

nop(pc) /* no-op (one argument */
	int *pc;
{
	trace1(*pc); /* trace */
	*pc += 1; /* increment p.c. */

}

nop2(pc)  /* no-op (2 arguments) */
	int *pc;
{
	trace2(*pc); /* trace */
	*pc += 2; /* increment p.c. */

}


pushdim(pc)  /* push a dimension onto the stack */
int *pc;
{
	DATUMPTR x;

	trace2(*pc);
	x = getnode();  /* get a node */
	/* translate the ASCII into a value */
	/* sscanf(getnam(getcore(*pc+1)),"%lf",&(x->value)); */
	/* get constant stored in index space at compile time */
	x->value = symtable[getcore(*pc+1)].x.c.cnst;
	pushval(x); /* push onto stack */
	tracevalue(); /* trace value */
	++ numdims;
	*pc += 2; /* increment p.c. */

}


#ifdef DEBUG4D
printdatum(k)
DATUMPTR k;
{
	fprintf("datum:(ntvcnx) %d %d  %f  %d %d  [%d,%d,%d,%d](%d)\n",
	k->name, k->type, k->value, k->car, k->next,
	k->index[0],  k->index[1],  k->index[2],  k->index[3],  k->index[4]); 
}
#endif
