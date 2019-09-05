/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* Special functions recognized by abstract machine */

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_math
#include <iraf.h>

#include "defines.h"
#include "datum.h"
#include "house.h"
#include "files.h"
#include "robust.h"

extern DATUMPTR exportstack[MATSIZE];  /* stack to save simultaneous equations
                                          of condition */
extern int exportptr; /* pointer to stack of simultaneous equations of condition*/

asinfn(code)  /* arcsine of top of stack -> top of stack */
{
	DATUMPTR i,k;
	double x;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */
	k = getnode(); /* get node */
	x = getdatumvalue(i);  /* body of argument */
	k->value = asin(x);   /* body of result */
	pushval(k);  /* push result on stack */

	i = i->next;  /* get soul of argument */
	if(i)     /* remaining soul */
	{
		if(fabs(x)==1)  /* error if body = 1 */
			fatalerror("Derivative of asin(1) is infinite\n","");
		factor = 1.0/sqrt(1 - sqr(x)); /* derviative of soul */
		while(i)  /* multiply each derivative by derivative of soul */
		{
			k = (k->next = copynode(i)); /* get node for result */
			k->value = factor*k->value;  /* derivative of body x term i */
			i = i->next;  /* next i */
		}
	}
	unsave(); /* discard intermediate results */
}

acosfn(code)  /* arccos of top of stack -> top of stack */
{
	DATUMPTR i,k;
	double x;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval(); /* pop argument */
	k = getnode(); /* node for result */
	x = getdatumvalue(i);   /* body of argument */
	k->value = acos(x);    /* body of result */
	pushval(k);   /* push result onto stack */

	i = i->next;    /* soul */
	if(i)    /* is there soul remaining */
	{
		if(fabs(x)==1)  /* error if body of x = 1 */
			fatalerror("Derivative of acos(1) is infinite\n","");
		factor = -1.0/sqrt(1 - sqr(x)); /* derviative of body */
		while(i)  /* multiply each derivative by derivative of body*/ 

		{
			k = (k->next = copynode(i)); /* node for result */
			k->value = factor*k->value; /* derivative of body x term i */
			i = i->next;  /* next i */
		}
	}


	unsave();  /* discard intermediate results */
}

tanfn(code)  /* tangent of top of stack -> top of stack */
{
	DATUMPTR i,k;
	double x;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */

	k = getnode();  /* node for result */

	x = getdatumvalue(i);  /* body of argument */
	factor = cos(x);  /* check that tangent isn't infinite */


	if(factor==0)
		fatalerror("tan x is infinite\n","");
	k->value = (factor = sin(x)/factor); /* tangent = sin/cos -> body of result */
	factor = 1.0 + sqr(factor);  /* derivative of body */
	pushval(k);   /* push result onto stack */

	i = i->next; /* get soul */
	while(i)  /* multiply derivative of body by soul */
	{
		k = (k->next = copynode(i)); /* get node */
		k->value = factor*k->value;  /* term of derivative */
		i = i->next;   /* next i */
	}


	unsave();  /* discard intermediate results */
}

expfn(code)  /* exponential of top of stack -> top of stack */
{
	DATUMPTR i,k;
	double x;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */
	k = getnode(); /* get node for result */
	x = getdatumvalue(i); /* body of argument */
	factor = exp(x); /* body of result = derivative of body */
	k->value = factor;/* body of result */
	pushval(k); /* push result on stack */

	i = i->next;  /* derivative of bocy x soul */
	while(i)   /* get next term of soul */
	{
		k = (k->next = copynode(i)); /* node for term */
		k->value = factor*k->value;  /* derviative term */
		i = i->next;  /* next i */
	}


	unsave();  /* discard intermediate results */
}

log10fn(code)  /* log to base 10 of top of stack -> top of stack */
{
	DATUMPTR i,k;
	double x;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */
	k = getnode(); /* node for result */
	x = getdatumvalue(i); /* body of argument */
	if(x<=0.0) /* body of argument must be >0 */
		fatalerror("Base 10 log of non-positive number, body of argument is <= 0\n","");
	factor = 1.0/log(10.0); /* log10(x) = log(x)/log(10) */
	k->value = log(x)*factor;/* body of result */
	factor = factor/x;  /* derivative of body */
	pushval(k);   /* push result onto stack */

	i = i->next; /* derivative of body x soul */
	while(i)  /* get next term of soul */	
	{
		k = (k->next = copynode(i));/*get node for next term */
		k->value = factor*k->value; /* derivative term */
		i = i->next;/* next i */
	}


	unsave(); /* discard intermediate results */
}

absfn(code)  /* absolute value of top of stack -> top of stack */
{
	DATUMPTR i,k;
	double x;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */
	k = getnode(); /* node for result */
	x = getdatumvalue(i); /* body of argument */
	k->value = fabs(x);/* body of result */
	pushval(k);   /* push result onto stack */

	i = i->next; /* get soul */
	if(i)   /* is there a soul? */
	{
		factor = (x<0?-1:1);  /* derivative of body */
		if(x==0.0)  /* derivative of |x| does not exist at x = 0 */
			factor = 0.0;
		/* changed by WHJ 4-4-88 */
		/*	fatalerror("Derivative of abs of 0.0 not defined\n","");*/	
		while(i)  /*derivative of bocy x soul */
		{
			k = (k->next = copynode(i)); /* next node for soul */
			k->value = 1.0/factor*k->value; /* next term of soul */
			i = i->next; /*next i */
		}
	}


	unsave(); /* discard intermediate results */
}


stripfn(code)
{
	DATUMPTR i,k;
	double x;

	i = popval();
	k = getnode();
	x = getdatumvalue(i);
	k->value = x;
	pushval(k);
	unsave();
}
sqrtfn(code)   /* sqrt from top of stack -> top of stack */
{
	DATUMPTR i,k;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */
	k = getnode(); /* node for result */
	factor = getdatumvalue(i); /* body of argument */
	if(factor<0.0)  /* error if body = 0 */
		fatalerror("Square root of negative number attempted in model\n","");
	k->value = (factor = sqrt(factor));/* body of result */
	pushval(k);   /* push result onto stack */

	i = i->next; /* get soul */
	if(i)  /* derviative of body x soul */
	{
		if(factor == 0.0)  /* error if body = 0 */
			fatalerror("Derivative of sqrt(0) is infinite\n","");
		factor = 0.5/factor; /* derivative of body */
		while(i)
		{
			k = (k->next = copynode(i)); /* next node */
			k->value = factor*k->value; /* next term of soul */
			i = i->next; /*next i */
		}
	}


	unsave(); /* discard intermediate results */
}


truncfn(code)   /* trunc from top of stack -> top of stack */
/* the function trunc truncs a double down and returns a double */
{
	DATUMPTR i,k;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */
	k = getnode(); /* node for result */
	factor = getdatumvalue(i); /* body of argument */
	k->value = (factor = (double)((int)factor));/* body of result */
	pushval(k);   /* push result onto stack */

	i = i->next; /* get soul */
	if(i)  /* derviative of body x soul */
	{
		factor = 1.00; /* derivative of body */
		while(i)
		{
			k = (k->next = copynode(i)); /* next node */
			k->value = factor*k->value; /* next term of soul */
			i = i->next; /*next i */
		}
	}


	unsave(); /* discard intermediate results */
}

sinfn(code)   /* sin of top of stack -> top of stack */
{
	DATUMPTR i,k;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */
	k = getnode(); /* node for result */
	factor = getdatumvalue(i); /* body of argument */
	k->value = sin(factor);/* body of result */
	factor = cos(factor);  /* derivative of body */
	pushval(k);   /* push result onto stack */

	i = i->next; /* get soul */
	while(i)  /* derivative of body x soul */
	{
		k = (k->next = copynode(i)); /* next node for result */
		k->value = factor*k->value;  /* next term of soul */
		i = i->next; /*next i */
	}


	unsave(); /* discard intermediate results */
}

cosfn(code)  /* cos of top of stack -> top of stack */
{
	DATUMPTR i,k;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */
	k = getnode(); /* node for result */
	factor = getdatumvalue(i); /* body of argument */
	k->value = cos(factor);/* body of result */
	factor = -sin(factor); /* derivative of body */
	pushval(k);   /* push result onto stack */

	i = i->next; /* get soul */
	while(i)  /* derivative of body x soul */
	{
		k = (k->next = copynode(i));/* next nodef or result */
		k->value = factor*k->value; /* next term of soul */
		i = i->next; /*next i */
	}


	unsave(); /* discard intermediate results */
}

atanfn(code)  /* tangent of top of stack -> top of stack */
{
	DATUMPTR i,k;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */
	k = getnode(); /* node for result */
	factor = getdatumvalue(i); /* body of argument */
	k->value = atan(factor);/* body of result */
	factor = 1/(1 + sqr(factor));  /* derivative of body */
	pushval(k);   /* push result onto stack */
	i = i->next;  /* get soul */
	while(i)  /* derivative of body x soul */
	{
		k = (k->next = copynode(i)); /* next node for result */
		k->value = factor*k->value;  /* next term of soul */
		i = i->next; /*next i */
	}


	unsave(); /* discard intermediate results */
}


logfn(code)   /* log of top of stack -> top of stack */
{
	DATUMPTR i,k;
	double x;
	double factor;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	i = popval();  /* pop argument */
	k = getnode(); /* node for result */
	x = getdatumvalue(i); /* body of argument */
	if(x<=0.0)  /* error of body <= 0 */
		fatalerror("Natural log(base e) of non-positive number\n","");
	k->value = log(x);/* body of result */
	pushval(k);   /* push result onto stack */

	factor = 1.0/x; /* derivative of body */
	i = i->next; /* get soul */
	while(i)  /* derivative of body x soul */
	{
		k = (k->next = copynode(i)); /* next node for result */
		k->value = factor*k->value;  /* next term of soul */
		i = i->next; /*next i */
	}


	unsave(); /* discard intermediate results */
}

doexport()   /* export pending equations of condition */
{
	if(exportptr>0)  /* only export if there are pending equations */
	{
		exportn(exportptr,exportstack); /* export all items in export stack */
		unsaveexport(); /* discard items from stack */
		popval();  /* pop top of stack */
	}
}

export(code)  /* export equation of condition found on top of stack */
int code;
{
	DATUMPTR k[MATSIZE];

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	saveexport(popval()); /* pop top of stak and save on export stack */
	pushtrue(); /* push true as result of export */
}

export2(code) /* export top two equations of condition.  
                 To be DISCARDED eventually */
int code;
{
	DATUMPTR k[MATSIZE];

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	saveexport(popval());
	saveexport(popval());
	pushtrue();
}

exportn(m,k) /* actually send pending equations of condition to least squares routine */
int m;
DATUMPTR k[MATSIZE];
{
	DATUMPTR kk;
	double rhs[MATSIZE];
	double wt[MATSIZE][MATSIZE];
	int i,j,iz[5];

	iz[0] = iz[1] = iz[2] = iz[3] = iz[4] = 0;
	pushval(k[0]); /* push something on top of stack (to be popped later) */
	computewt(m,k,rhs,wt); /* compute weight matrix and right hand side vector*/
	for(i=0;i<m;i++) /* add m equations of condition to matrix */
	{
		SumInit(Condition);/* initialize new row of matrix as condition */
		for(j=0;j<=i;j++) /* combine first i equations with weight matrix */
		{
			/* weight righhand side and export it */
			SumAdd(RightHandType,"_RHS",iz,wt[i][j]*rhs[j]);
			kk = k[j];  /* get unweighted equation of condition */
			while(kk = kk->next) /* do for each derivative in equation */
			{
				char *str;

				str = getnam(kk->name); /* get the name of the variable */
				switch(kk->type) /* get the type of the variable */
				{
				case GlobalType:  /* global paramter */
						SumAdd(GlobalType,str,iz,wt[i][j]*(kk->value));
					/* weight derivative and add in */
#ifdef DEBUG
					printf("Type = %d  D(%s) = %lf\n",
					GlobalType,str,(kk->value));
#endif
					break;
				default:  /* all other derivatives */
					if(kk->type > ObsType) /* only do it for paramters */
					{
						/* subscripted paramter */
						SumAdd(kk->type,str,kk->index,wt[i][j]*(kk->value)); 
						/* weight derivative and add in */
#ifdef DEBUG
						printf("Type = %d  D(%s[%d,%d,%d,%d (%d)]) = %lf\n",
						kk->type,str,
						kk->index[0], kk->index[1], kk->index[2],
						kk->index[3], kk->index[4], (kk->value));
#endif
					}
					break;
				}
			}
		}
	}

	unsave(); /* discard intermediate results */
}

exportconstraint(code) /* export a constraint */
{
	DATUMPTR k;

#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	SumInit(Constraint); /* initialize matrix to receive a constraint */

	sumconstraint(code); /* add in the constraint */

}

sumconstraint(code)
{
	DATUMPTR kk;
	int iz[5];

	iz[0] = iz[1] = iz[2] = iz[3] = iz[4] = 0;
#ifdef DEBUG
	printf("Executing %s\n",getnam(code));
#endif
	kk = popval(); /* pop top of stack */
	pushval(kk); /* push a result onto stack */
	SumAdd(RightHandType,"_RHS",iz,-(kk->value));/* add right hand side
	           to matrix */
#ifdef DEBUG
	printf("Type = %d  %s  %lf\n",RightHandType,"_RHS",-(kk->value));
#endif
	while(kk = kk->next) /* add each derivative term to matrix */
	{
		char *str;

		str = getnam(kk->name); /* get name of parameter */
		switch(kk->type)  /* get type of paramter */
		{
		case GlobalType:  /* global paramter */	
			SumAdd(GlobalType,str,iz,kk->value);/* add into matrix */
#ifdef DEBUG
			printf("Type = %d  D(%s) = %lf\n",
			GlobalType,str,(kk->value));
#endif
			break;
		default:  /* indexed paramter */
			if(kk->type > ObsType) /* only do it if it is a paramter */
			{
				/* add derivative to matrix */
				SumAdd(kk->type,str,kk->index,kk->value);
#ifdef DEBUG
				printf("Type = %d  D(%s[%d,%d,%d,%d (%d)]) = %lf\n",
				kk->type,str,
				kk->index[0], kk->index[1], kk->index[2],
				kk->index[3], kk->index[4], (kk->value));
#endif
			}
			break; /* discard intermdiate results */
		}
	}

	unsave(); /* discard intermediate results */
}
