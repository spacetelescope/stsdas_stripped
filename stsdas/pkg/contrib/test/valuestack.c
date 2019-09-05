/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#include <iraf.h>

#include "defines.h"
#include "files.h"
#include "datum.h"

#define NUMVALS 100			/* Depth of Value Stack */
#define NUMSAVES 100		/* Depth of Save Stack of Values */

extern int traceflag;    /* trace if this falg is on */
extern FILE *fp;         /* result file */

static DATUMPTR valuestack[NUMVALS];   /* value stack ("the stack") */
static int valueptr=0;                /* pinter to value stack */
static DATUMPTR savestack[NUMSAVES];  /* stack to save values pooped from valuestack until they are no longer needed */
static int saveptr=0;  /* save stack pointer */
DATUMPTR exportstack[MATSIZE];  /* stack to save values pending export */
int exportptr=0;  /* export stack pointer */

tracevalue()
{
	if(traceflag >= 2 && valueptr>0) /* print values on top of stack if flag >= 2 */
	{
		fprintf(fp,"          Stackptr = %d,",valueptr-1);
		printvalue("_TOS",valuestack[valueptr-1]);/* print the top of stack */

	}
}

markvalue() /* mark all active data structures in stacks */
{
	int i;

	for(i=0;i<valueptr;i++) /* mark all values in stack */
		marklist(valuestack[i]);
	for(i=0;i<saveptr;i++) /* mark all values in save stack */
		marklist(savestack[i]);
	for(i=0;i<exportptr;i++) /* mark all values in export stack */
		marklist(exportstack[i]);
}

DATUMPTR popval()  /* pop top of stack (error if it is not a scalar) */
{                  
	int dummyptr;                  
	if(valueptr>0)  /* see if something is there */
	{                                       
		--valueptr;  /* decrement pointer */
		savestack[saveptr] = valuestack[valueptr]; /* protect object against garbage colleciton */
		if(savestack[saveptr]->car) /* see if object popped was a vector */
			fatalerror("Popped a vector for a value\n",""); /* if so, error */
		if(saveptr<NUMSAVES) /* see if room is left in save stack */    
		{
			dummyptr = saveptr; /* increment stack pointer and return the pointer popped */
			saveptr++;
			return savestack[dummyptr]; 
		}
		else
			fatalerror("Save stack overflow in attempt to pop scalar top of stack.\n",""); /* overflow of save stack */
	}
	else
		fatalerror("Stack underflow in attempt to pop scalar top of stack.\n",""); /* underflow of stack */
}

DATUMPTR popitem() /* pop top of stack */
{                                        
	int dummyptr;
	if(valueptr>0)  /* check that something is there */
	{                                       
		--valueptr;  /* decrement pointer */
		savestack[saveptr] = valuestack[valueptr]; /* protect object against garbage collection */

		if(saveptr<NUMSAVES) /* room left on save stack */
		{
			dummyptr = saveptr; /* increment stack pointer and return the pointer popped */
			saveptr++;
			return savestack[dummyptr];
		}
		else
			fatalerror("Save stack overflow in attempt to pop top of save stack\n",""); /* overflow of save stack */
	}
	else
		fatalerror("Stack underflow in attempt to pop top of stack\n",""); /* underflow of stack */
}

saveexport(x)  /* save item on export stack */
DATUMPTR x;
{	
	if(exportptr<MATSIZE)  /* see if there is room */
	{
		exportstack[exportptr] = x; /* yew, put on stack and increment pointer */
		exportptr++;
		return 0;
	}
	else
		fatalerror("Export Stack Overflow\n",""); /* stack overflow */
}

unsave()  /* drop everything from save stack */
{
	saveptr = 0;
}

unsaveexport()  /* drop everything from export stack */
{
	exportptr = 0;
}

pushval(x)   /* push item onto stack */
DATUMPTR x;
{
	if(valueptr<NUMVALS)  /* check to see that there is room */
	{
		valuestack[valueptr] = x; /* put item on stack and */
		valueptr++;  /* increment stack pointer */

	}
	else
		fatalerror("Stack overflow in attempt to push item onto the stack.\n",""); /* error if stack is full */

}

printonevalue(name,k)  /* print a vector */
char *name;
DATUMPTR k;
{
	int insz;

	if(k) { /* only print non-null */
		fprintf(fp,"     %4s = %lf\n",name,k->value); /* print the body */
		if(traceflag >= 3) { /* print derviatives if tracefalg >= 3 */
			while(k = k->next) { /* get next derivative */
				/*
				if (k->name == 0) {
					fprintf(stderr, "stop!\n");
					sleep(15);
				}
				*/
				if(k->index[4]) { /* print indexed paramter */
					int i;
					fprintf(fp,"             Deriv[%4s", getnam(k->name));
					insz = getindexsz();
					prIndex(fp, k->index,insz);	/* print subscripts */
					/*
					for (i=0; i<k->index[4]; ++i) {
						if (i!=0) fprintf(fp,", ");
						fprintf(fp,"%2d",k->index[i]);
					}
					*/
					fprintf(fp,"] = %lf\n", k->value);
				} else {    /* print global paramter */
					fprintf(fp,"             Deriv[%4s    ] = %lf\n",
						getnam(k->name),k->value);
				}
			}
		}
	} else fprintf(fp,"     %4s = %ld\n",name,k);
}

printvalue(name,k)  /* print a scalar or a vector */
char *name;
DATUMPTR k;
{
	if(k)  {  /* only print non-null */
		if(k->car)  {  /* if vector */
			int h, i, j, dims, nelems;
			int div[5];
			ArrayBlockPtr a;
			a = k->car;
			dims = a->dim[4];
			for (i=0,nelems=1; i<dims; ++i) nelems*=a->dim[i];
			div[0] = div[1] = div[2] = div[3] = 1;
			for (i=1; i<dims; ++i) { /* calc nelems per dimension */
				for (j=0; j<i; ++j) div[i] *= a->dim[j];
			}
			for(h=0;h<nelems;h++) { /* print each component of vector */
				char str[256], str2[64];
				sprintf(str,"     %4s[\0",name);
				for (j=0; j<dims; ++j) {
					if (j!=0) strcat(str, ",\0"); 
					sprintf(str2, "%d\0", h/div[j]);
					strcat(str, str2);
				}
				strcat(str, "]\0");
				printonevalue(str,a->elem[h]); /* points to actual value */
			}
		} else  printonevalue(name,k);  /* it's a scalar */
	} else printonevalue(name,k);
}
