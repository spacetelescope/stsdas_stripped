
/*
	GaussFit - A System for Least Squares and Robust Estimation
	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/
/* Basic arithmetic operations performed by abstract machine.
   Each item is sum of body(value) + soul (derivatives) */

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_math
#include <iraf.h>

#include "defines.h"
#include "datum.h"

long comparenodes(i,j)  /* compare variables in two nodes; return -1,0,1
                           for i<j, i=j, i>j respectively*/
DATUMPTR i,j;
{
	long x;
	int m;
	/* if names are different, return difference of same pointers,
	           otherwise return difference of indexes */
	if(x = (long)i->name - (long)j->name)   return x;
	else {
		for (m=0; m<i->index[4]; ++m) {
		   if (i->index[m] < j->index[m]) return -1;
		   else if (i->index[m] > j->index[m]) return 1;
		}
		return 0;
	}
}

add(pc) 
/* add two items on top of stack, value returned on top of stack */
int *pc;
{
	DATUMPTR i,j,k;
	long compare;
	trace1(*pc);    /* trace input */
	j = popval();   /* pop two items on top of stack */
	i = popval();  
	k = getnode();  /* get node to contain result */
	k->value = getdatumvalue(i) + getdatumvalue(j); /* value of body */
	pushval(k);     /* push on top of stack */
	i = i->next;    /* get derivatives */
	j = j->next;    
	while(i || j) {  /* any derivatives left ? */
		if(i && j) {   /* if both have derivatives */
			/* see if variables are the same */
			compare = comparenodes(i,j);       
			if(compare<0) {      /* i < j */           
				/* copy derivative of node i */
				k = (k->next = copynode(i));
				i = i->next;      /* next i */
			} else if(compare == 0) {                                     
				k = (k->next = copynode(i));
				/* sum derivatives of i and j */			      
				k->value = i->value + j->value; 
				/* next i and j */
				i = i->next;
				j = j->next;
			} else {              /* i > j */
				/* copy derivative of node j */
				k = (k->next = copynode(j));   
				/* next j */
				j = j->next;
			}
		} else if(i) {        /* only i left */
			/* copy derivative of i */
			k = (k->next = copynode(i)); 
			/* next i */
			i = i->next;
		} else {               /* only j left */
			k = (k->next = copynode(j)); 	/* copy derivative of node j */
			j = j->next; 					/* next j */
		}
	}
	tracevalue();   /* trace result */
	*pc += 1;       /* increment program counter */
	unsave();       /* discard intermediate results */
}

chsfn(pc)   /* change sign of top of stack */
int *pc;
{
	DATUMPTR j,k;
	trace1(*pc);          /* trace input */
	j = popval();         /* pop top of stack */
	k = getnode();        /* get node for result */
	k->value = -getdatumvalue(j);    /* change sign of body */
	pushval(k);           /* push result on stack */
	j = j->next;          /* get first derivative */
	while(j)              /* do as long as there are derivatives */
	{
		k = (k->next = copynode(j));    /* copy the derivative */
		k->value = -k->value;           /* change its sign */
		j = j->next;                    /* next node */
	}
	tracevalue();         /* trace result */
	*pc += 1;             /* increment p.c. */
	unsave();             /* discard intermediate results */
}

sub(pc)   /* subtract top of stack from next stack items result to stack */
int *pc;
{
	DATUMPTR i,j,k;
	long compare;
	trace1(*pc);     /* trace input */
	j = popval();    /* pop two items on top of stack */
	i = popval();    
	k = getnode();   /* get node for result */
	/* difference of the two bodies */
	k->value = getdatumvalue(i) - getdatumvalue(j);
	pushval(k);     /* push result on stack */
	i = i->next;    /* fetch first derivatives */
	j = j->next;    /* any derviatives left ? */
	while(i || j)
	{
		if(i && j)    /* if both have derivatives */
		{
			compare = comparenodes(i,j); /* same variables?*/
			if(compare<0)    /* i < j */
			{                           
				/* copy derivative of node i */
				k = (k->next = copynode(i));   
				/* next i */
				i = i->next;
			}
			else if(compare == 0)  /* i = j */
			{
				k = (k->next = copynode(i));
				/* difference of two derivatives */
				k->value = i->value - j->value;    
				/* next i and j */
				i = i->next;
				j = j->next;
			}
			else                   /* i > j */
			{                                        
				/* copy derivative of node j */  
				k = (k->next = copynode(j));
				k->value = - k->value; /* change its sign
				
				*/
				
								j = j->next;  /* next j */
			}
		}
		else if(i)      /* only i has derivatives left */
		{                                                
			/* copy derivative of node i */   
			k = (k->next = copynode(i));
			i = i->next;  /* next i */
		}
		else            /* only j has derivatives left */
		{                                                 
			/* copy derivative of node j */   
			k = (k->next = copynode(j));
			k->value = - k->value; /* change its sign
			
			*/
			
							j = j->next;    /* next j */
		}
	}
	tracevalue();  /* trace result */
	*pc += 1;      /* increment p.c. */
	unsave();      /* discard intermediate results */
}
mpy(pc)  /* multiply two top items on stack, result to stack */
int *pc;
{
	DATUMPTR i,j,k;
	long compare;
	double ival,jval;
	trace1(*pc);                  /* trace input */
	j = popval();                 /* pop the arguments */
	i = popval();
	ival = getdatumvalue(i);      /* get the bodies of the arguments*/
	jval = getdatumvalue(j);
	k = getnode();                /* get a node for result */
	pushval(k);                   /* push it on stack */
	k->value = ival * jval;       /* body of result */
	i = i->next;                  /* get first derivatives */
	j = j->next;
	while(i || j)                 /* done */
	{
		if(i && j)            /* if both have derivatives */
		{
			compare = comparenodes(i,j); /* i = j ? */
			if(compare<0)                /* i < 0 */
			{                                       
				/* get node for i derivative */
				k = (k->next = copynode(i));   
				/* product rule for derivatives */
				k->value = jval*i->value;         
				i = i->next; /*next i */
			}
			else if(compare == 0)        /* i = j */
			{                                   
				/* get node for derivative */
				k = (k->next = copynode(i));
				/* product rule for derivatives */
				k->value = jval*i->value + ival*j->value;
				i = i->next;  /*next i and j */
				j = j->next;
			}
			else                         /* i > j */
			{                                    
				/* get node for derivative */
				k = (k->next = copynode(j));
				/* product rule for derivatives */
				k->value = ival*j->value;
				j = j->next;       /*next j */
			}
		}
		else if(i)    /* only i derivatives left ? */
		{         
			/* get node */
			k = (k->next = copynode(i));
			/* product rule for derivatives */
			k->value = jval*i->value;
			i = i->next;         /*next i */
		}
		else           /* only j derivatives left ? */
		{                             
			/* get node */
			k = (k->next = copynode(j)); 
			/* product rule for derivatives */
			k->value = ival*j->value;
			j = j->next;        /*next j */
		}
	}
	tracevalue();  /* trace result */
	*pc += 1;      /* increment p.c. */
	unsave();      /* discard intermediate values */
}
div(pc)  /* Divide top of stack into second stack entry, result to stack */
int *pc;
{
	DATUMPTR i,j,k;
	double ival,jval,factor;
	long compare;
	trace1(*pc);               /* trace arguments */
	j = popval();              /* pop arguments */
	i = popval();
	ival = getdatumvalue(i);   /* get bodies of arguments */
	jval = getdatumvalue(j);
	if(notzero(jval))    /* can't divide if divisor of body is zero*/
	{
		jval = 1.0/jval;        /* reciprocal of divisor */
		k = getnode();          /* get node for result */
		k->value = factor = ival*jval;  /* body of result */
		factor = - factor*jval;  /* coefficient for quotient rule */
		pushval(k);
		i = i->next;     /* get souls of arguments */
		j = j->next;
		while(i || j)    /* no more soul? */
		{
			if(i && j) /* do both args still have soul? */
			{
				compare = comparenodes(i,j); /* compare
				
				i and j */
				if(compare<0)  /*i , j */
				{                        
					/* copy node for result */
					k = (k->next = copynode(i));
					/* quotient rule for derivatives
					
					*/
					
										k->value = jval*i->value;
					
										i = i->next;  /* next i */
				}
				else if(compare == 0) /* i = j */
				{
					/* copy node for result */
					k = (k->next = copynode(i));
					/* quotient rule for derivatives
					
					*/					k->value = jval*i->value + factor*j->value;
					
										i = i->next; /* next i and j */
					j = j->next;
				}
				else                    /* i > j */
				{
					/* copy node for result */
					k = (k->next = copynode(j));
					/* quotient rule for derivatives
					
					*/					k->value = factor*j->value;
					
										j = j->next;  /* next j */
				}
			}
			else if(i)     /* only i still has body */
			{
				/* copy node for result */
				k = (k->next = copynode(i));
				/* quotient rule for derivatives
				
				*/					k->value = jval*i->value;
				
									i = i->next;   /* next i */
			}
			else           /* only j still has body */
			{
				/* copy node for result */
				k = (k->next = copynode(j));
				/* quotient rule for derivatives
				
				*/					k->value = factor*j->value;
				
									j = j->next;   /* next j */
			}
		}
	}
	else                                         
	/* error if divide by body of zero */
	fatalerror("Attempted division by zero in model\n","");
	tracevalue();    /* trace result */
	*pc += 1;        /* increment p.c. */
	unsave();        /* discard intermediate values */
}
static double npow(x,n)   /* raise (double)x to (int)n */
double x;
int n;
{
	double y;
	if(n<0)           /* raise to negative power */
	{
		if(x==0.0)  /* error of x = 0.0 */
			fatalerror("Attempt to raise 0 to negative power in model\n","");
		x = 1.0/x;  /* reciprocal of x */
		n = -n;     /* change sign of n */
	}                                         
	/* binary decomposition of n gives efficient exponenetiation */
	y = 1.0;         /* X 0 = 1.0 */
	while(n>0)       /* if n > 0 */
	{
		if(n & 0x0001)   /* if n is odd */
			y *= x;  /* multiply y by x */
		x *= x;                  /* square x */
		n /= 2;                  /* divide n by 2 */
	}
	return y;
}
/* Note: should check upper values of n also */
pwr(pc) 
/* raise second from top of stack to power of top of stack.
    Result to top of stack.
    Top of stack must be an integer */  
int *pc;
{
	DATUMPTR i,j,k;
	double ival, jval;
	trace1(*pc);         /* trace arguments */
	j = popval();        /* pop arguments */
	i = popval();
	ival = getdatumvalue(i);     /* get body of arguments */
	jval = getdatumvalue(j);	
	if(((int)jval) != jval)      /* only integer exponents allowed */
		fatalerror("Fractional powers not allowed in model\n","");
	k = getnode();               /* get node for result */
	k->value = npow(ival,(int)jval);   /* body of reslt */
	pushval(k);                        /* push result on stack */
	i = i->next;                 /* get soul of first argument */
	while(i)                     /* as long as soul remains */
	{
		k = (k->next = copynode(i));/* copy node for result */
		/* derivative of k j(wedge)= j*k wedge(j-1) */
		k->value = (int)jval * npow(ival,(int)jval - 1) * k->value;
		i = i->next;   /* next i */
	}
	tracevalue();    /* trace result */
	*pc += 1;        /* increment p.c. */
	unsave();        /* discard intermediate values */
}
