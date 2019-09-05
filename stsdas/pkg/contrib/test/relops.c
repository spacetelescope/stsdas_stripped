/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* Relational operators */

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_math
#include <iraf.h>

#include "defines.h"
#include "datum.h"


int greaterp(pc) /* top of stack -1 ? > ? top of stack -> top of stack */
int *pc;
{
	DATUMPTR i,j;
	trace1(*pc);  /*trace */
	j = popval(); /* pop arguments */
	i = popval();
	if(getdatumvalue(i)>getdatumvalue(j)) /*true if i>j */
		pushtrue();
	else
		pushfalse(); /* otherwise false */
	*pc += 1; /* increment p.c. */
	tracevalue(); /* trace result */
	unsave(); /* drop intermediates */
}

int lessp(pc) /* top of stack -1  < ? top of stack -> top of stack */
int *pc;
{
	DATUMPTR i,j;
	trace1(*pc);  /*trace */
	j = popval(); /* pop arguments */
	i = popval();
	if(getdatumvalue(i)<getdatumvalue(j)) /*true if i<j */
		pushtrue();
	else
		pushfalse(); /* otherwise false */
	*pc += 1; /* increment p.c. */
	tracevalue(); /* trace result */
	unsave(); /* drop intermediates */
}

int leqp(pc) /* top of stack -1  <= ? top of stack -> top of stack */
int *pc;
{
	DATUMPTR i,j;
	trace1(*pc);  /*trace */
	j = popval(); /* pop arguments */
	i = popval();
	if(getdatumvalue(i)<=getdatumvalue(j)) /*true if i<=j */
		pushtrue();
	else
		pushfalse(); /* otherwise false */
	*pc += 1; /* increment p.c. */
	tracevalue(); /* trace result */
	unsave(); /* drop intermediates */
}

int geqp(pc) /* top of stack -1  >= ? top of stack -> top of stack */
int *pc;
{
	DATUMPTR i,j;
	trace1(*pc);  /*trace */
	j = popval(); /* pop arguments */
	i = popval();
	if(getdatumvalue(i)>=getdatumvalue(j)) /*true if i>=j */
		pushtrue();
	else
		pushfalse(); /* otherwise false */
	tracevalue(); /* trace result */
	*pc += 1; /* increment p.c. */
}

int differp(pc) /* top of stack -1  != ? top of stack -> top of stack */
int *pc;
{
	DATUMPTR i,j;
	trace1(*pc);  /*trace */
	j = popval(); /* pop arguments */
	i = popval();
	if(getdatumvalue(i)!=getdatumvalue(j)) /*true if i!=j */
		pushtrue();
	else
		pushfalse(); /* otherwise false */
	*pc += 1; /* increment p.c. */
	tracevalue(); /* trace result */
	unsave(); /* drop intermediates */
}

int equalp(pc) /* top of stack -1  == ? top of stack -> top of stack */
int *pc;
{
	DATUMPTR i,j;
	trace1(*pc);  /*trace */
	j = popval(); /* pop arguments */
	i = popval();
	if(getdatumvalue(i)==getdatumvalue(j)) /*true if i==j */
		pushtrue();
	else
		pushfalse(); /* otherwise false */
	*pc += 1; /* increment p.c. */
	tracevalue(); /* trace result */
	unsave(); /* drop intermediates */
}
