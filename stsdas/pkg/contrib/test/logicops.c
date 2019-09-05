/*
	GaussFit - A System for Least Squares and Robust Estimation
	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/
/* logical operations of virtual machine */

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_math
#include <iraf.h>

#include "defines.h"
#include "datum.h"
pushtrue() /* push a true value on the stack */
{
	DATUMPTR x;
	x = getnode(); /* get a node */
	x->value = 1.0; /* make it equal to one */
	pushval(x); /* push it on the stack */
}
pushfalse() /* push a false value on the stack */
{
	DATUMPTR x;
	x = getnode(); /* get a node */
	x->value = 0.0; /* make it zero */
	pushval(x); /* push it on the stack */
}
int andfn(pc) /* logical and of two top of stack items -> top of stack */
int *pc;
{
	DATUMPTR i,j;
	trace1(*pc); /* trace the input */
	j = popval(); /* pop arguments */
	i = popval();
	if(getdatumvalue(i)==0.0 || getdatumvalue(j)==0.0) /* if both are false */
		pushfalse(); /* result is false */
	else
		pushtrue(); /* otherwise result is true */
	*pc += 1; /* increment p.c. */
	tracevalue(); /* trace result */
	unsave(); /* discard intermediate values */
}
int orfn(pc) /* logical or of two top stack items -> top of stack */
int *pc;
{
	DATUMPTR i,j;
	trace1(*pc); /* trace the input */
	j = popval(); /* pop arguments */
	i = popval();
	if(getdatumvalue(i)==0.0 && getdatumvalue(j)==0.0) /* if both are false */
		pushfalse(); /* result is false */
	else
		pushtrue(); /* otherwise result is true */
	*pc += 1; /* increment p.c. */
	tracevalue(); /* trace result */
	unsave(); /* discard intermediate values */
}
int notfn(pc) /* ~ top of stack -> top of stack */
int *pc;
{
	DATUMPTR i,j;
	trace1(*pc); /* trace the input */
	i = popval(); /* pop arguments */
	if(getdatumvalue(i)==0.0) /* if its false */
		pushtrue(); /*  result is true */
	else
		pushfalse(); /* result is false */
	*pc += 1; /* increment p.c. */
	tracevalue(); /* trace result */
	unsave(); /* discard intermediate values */
}
iffn(pc) /* branch depending on top of stack true */
int *pc;
{
	trace1(*pc); /* trace the input */
	if(getdatumvalue(popval()) != 0.0) /* if top of stack is true */
		*pc += 1; /*  execute next instruction */
	else
		*pc += 3; /*  otherwise skip next instruciton */
	unsave(); /* discard saved values */
}
ifnot(pc) /* branch of top of stack false */
int *pc;
{
	trace1(*pc); /* trace the input */
	if(getdatumvalue(popval()) == 0.0) /* if top of stack is false */
		*pc += 1; /*  execute next instruction */
	else
		*pc += 3; /*  else skip next instruction */
	unsave(); /* discard  saved values */
}
