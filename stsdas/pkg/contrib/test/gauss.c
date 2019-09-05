/*	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* main loop */

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_math
#include <iraf.h>

#include "defines.h"
#include "datum.h"
#include "house.h"
#include "simpledefs.h"
#include "files.h"
#ifdef THINK_C
#include <MemoryMgr.h>
#endif

extern FILE *fp;  /* result file piinter */
double Get_Sigma();  /* sigma value from SumIt */
extern int SumIt();   /* sum routine from house.c */

double tolerance;  /* tolerance */
double fit,minimum,fairmeth;
double lambda, factor;
int MARQ = 0;
int maxiters;
int space;
int limit;
double *OldPrm = NULL;

double UpdateValues()  /* update all parameters */
{
	int i;
	int type;
	double dahat;
	char *name;
	int index[5];
	int symInx,indsz,prmsz;
	double error;
	extern double DeltaV;
	double ahat;

    /* cycle through all paramters */
	error = DeltaV; /* error starts with value returned by phi calculation */
	i = 0;

	indsz = getindexsz();
	prmsz = getmaxprmname();
	/* get data on next parameter */
	while(GetDeltaValue(i,&name,index,&type,&dahat))
	{                    
		i++;
		symInx = findsymbol(name);
		switch(type) {
		case GlobalType:   /* its a global parameter */
			ahat = getparamval(symInx); /*get current value of parameter */
			putparamval(symInx,ahat + dahat); /* increment value and update parameter file */
			break;
		default: /* it's an indexed parameter */
			ahat = getxparval(symInx,index);/* get current value of paramter */
			/* increment value and update paramter file */
			putxparval(symInx,index,ahat + dahat);
			break;
		}
		/* print results in results file and to screen */
		if ((index[4] == 0) && (indsz == 0)) {
			printf("%*s = ",prmsz,name);
			fprintf(fp,"%*s = ",prmsz,name);
		}else if ((index[4] == 0) && (indsz != 0)) {
			printf("%*s%*s = ",prmsz,name,indsz," ");
			fprintf(fp,"%*s = ",prmsz+indsz,name);
		} else {
			printf("%*s",prmsz, name);
			prIndex(stdout,index,-indsz);
			printf(" = ");
			fprintf(fp,"%*s", prmsz,name);
			prIndex(fp,index,-indsz);
			fprintf(fp, " = ");
		}
		printdouble(stdout,ahat+dahat);
		printf(" delta = ");
		printdouble(stdout,dahat);
		printf("\n");

		printdouble(fp,ahat+dahat);
		fprintf(fp," delta = ");
		printdouble(fp,dahat);
		fprintf(fp,"\n");

		/* update convergence error */
		error = maxval(fabs(dahat)/(tolerance +fabs(ahat)),error);
	}
	i++;  /* ????? */
	printf("\n");
	fprintf(fp,"\n");
	return error;
}

gaussmain(t,s)  /* main loop routine */
char *t,*s;
{
	int i;
	static double  oldSigma,Curr_Sigma;;

	prolog(); /* do any initializations */
	printcopyright(stdout);
	mycompile(t); /* compile file "t" */

	getenvvars(s);

	oldSigma = -1.0;
	space = 0;

	for(i=0;i<maxiters;i++) /* iterate at most maxite times */
	{
		/* inform user of progress */
		printf("\nStart of Iteration %d...\n\n",i+1);

		/* initialize Householder routines */
		InitHouse("results", getenvint("prmat"), 10.0);

		fprintf(fp,"\nStart of Iteration %d...\n\n",i+1);

		Forming_Eqns(); /* form the equations of condition */

		/*if double  iteration method  or Marq thnn proceed with second */
		if ((getenvval("double")) || (MARQ))
		{
			InitHouse("results", getenvint("prmat"), 10.0);
			Forming_Eqns(); /* form the equations of condition */
		}

		if (MARQ)
		{
			if (!space)
				Allocate_Param_Space();
			
			Curr_Sigma=Get_Sigma();


			if (oldSigma < 0.00 || Curr_Sigma < oldSigma)
			{
				oldSigma = Curr_Sigma;
				Current_into_Old_Params();
				SolveMatrix(); /* solve the estimation problem */
				ShowResults(i+1);/* show results and update parameters */
				lambda = lambda * factor;
			}
			else
			{
				Old_into_Current_Params();
				lambda = lambda/(sqr(factor));
				oldSigma = -1.0;
			}
			fprintf(fp,"lambda = %28.17lf\n",lambda);
			printold();
		}
		else
		{
			SolveMatrix(); /* solve the estimation problem */
			ShowResults(i+1);/* show results and update parameters */
		}
		if(fit<tolerance)   /* quit if converged */
			break;
	}
	if(tolerance < fit)   /* if reached iters without convergence */
	{
		itlimit(stdout);
		itlimit(fp);
	}
	if (getenvval("prvar") && (fit < tolerance))
		CovarianceMtx();

	epilog();  /* clean up */
}

Forming_Eqns()
{
		printf("\nForming Eqs...\n\n");
		/* define global variables */
		interpret(findsymbol("_GLOBS"));
		/* execute user's model starting at main */
		interpret(findsymbol("main"));
		popitem(); /* pop top of stack */
		reset(); /* discard all defined variables */
		SumIt(); /* sum sigmas */
}

ShowResults(count) 
int count;
{

	PrintResults(); /* print results */
	fit = UpdateValues(); /* update the paramters */
	/* print information for user */
	printf("fit =\t");
	fprintf(fp,"fit =\t");
	printdouble(fp,fit);
	printdouble(stdout,fit);
	printf(", tol =\t");
	fprintf(fp,", tol =\t");
	printdouble(stdout,tolerance);
	printdouble(fp,tolerance);
	printf("\n");
	fprintf(fp,"\n");
	printf("\nEnd of Iteration %d...\n\n",count);
	fprintf(fp,"\nEnd of Iteration %d...\n\n",count);
}

itlimit(fpp)
FILE *fpp;
{
	fprintf(fpp,"\n***********************************");
	fprintf(fpp,"***********************************\n");
	fprintf(fpp,"\nIteration limit reached.  Convergence has not occurred.\n");
	fprintf(fpp,"\n***********************************");
	fprintf(fpp,"***********************************\n\n");
}

prolog()
{
#ifdef THINK_C
	SetApplLimit(CurStackBase - 32768L);
	MaxApplZone(); /* MAC requires large heap for space allocation */
#endif
}

getenvvars(s) /* get values from environment file and test them */
char *s;
{
	openenvtable(s); /* open environment table "s" */
	tolerance = getenvval("tol"); /* get tolerance from environment */
	if ((tolerance < 1.0e-13)   && (tolerance > -1.0e-13))   
		tolerance = 0.000001;
	minimum = getenvval("minsum");
	fairmeth = getenvval("fair");
	if ((notzero(minimum)) && (notzero(fairmeth)))
		fatalerror("Both minsum and fair have been set to non-zero in the environment file.\n"," ");
	lambda = getenvval("lambda");
	factor = getenvval("factor");
	if (notzero(lambda))
		MARQ = 1.0;
	if ((notzero(minimum)) && (MARQ))
		fatalerror("Both minsum and lambda have been set to non-zero in the environment file.\nLevenburg-Marquardt cannot be used with minsum.\n"," ");
	if (((lambda < .0000000000001)||(lambda > 100.0)) && (MARQ)) 
	{
		lambda = .0001;
		warningerror("Keyword lambda missing or less than or equal to zero in the environment file.\n"," ",0,0);
	}
	if (((factor < .0000000000001)||(factor >100.0)) && (MARQ)) 
	{
		factor = 0.1;
		warningerror("Keyword factor missing or less than or equal to zero in the environment file.\n"," ",0,0);
	}
	maxiters = getenvint("iters"); /* get max iterations from environment */
	if ((maxiters < 1) || (maxiters >1000))
	{
		maxiters = 10;
		warningerror("Keyword iters missing or less than zero in environment file.\n"," ",0,0);
	}
}


Allocate_Param_Space()
{
	int i;

	limit = Get_LastCol();
	space++;
	OldPrm = (double*)MemAlloc("OldPrm",(long)(limit+1)*sizeof(double));
	for (i=0;i<=limit;i++)
	{
		*(OldPrm+i) = 0.0;
	}
}

Old_into_Current_Params()
{
	int i, type;
	double dahat,ahat;
	char *name;
	int index[5];
	int symInx;


 	i = 0;   /* cycle through all paramters */
	fprintf(fp,"\n");
 	while(GetDeltaParams(i,&name,index,&type,&dahat)) /* get data on next parame ter */
 	{
		 i++;
		symInx = findsymbol(name);
	 	switch(type)
 		{
	 		case GlobalType:   /* its a global parameter */
 				ahat = getparamval(symInx); /*get current value of parameter */
 				putparamval(symInx,OldPrm[i-1]);
				/*fprintf(fp,"PutParam %s = %28.17lf\n",name,OldPrm[i-1]);*/
 				break;
			case RightHandType:
				break;
 			default: /* it's an indexed parameter */
 				ahat = getxparval(symInx,index);/* get current value of paramter */
 				putxparval(symInx,index,OldPrm[i-1]);/* increment value and update paramter file */
 				break;
 		}
	}
	fprintf(fp,"\n");
}


Current_into_Old_Params()
{
	int i, type;
	double dahat;
	char *name;
	int index[5];
	int symInx;

	i = 0;   /* cycle through all paramters */
	fprintf(fp,"\n");
	while(GetDeltaParams(i,&name,index,&type,&dahat)) /* get data on next parameter */
	{                    
		i++;
		symInx = findsymbol(name);
		switch(type)
		{
		case GlobalType:   /* its a global parameter */
			OldPrm[i-1] = getparamval(symInx); /*get current value of parameter */
/*				fprintf(fp,"GetParam %s = %28.17lf\n",name,OldPrm[i-1]);*/
			break;
		case RightHandType:
			break;
		default: /* it's an indexed parameter */
			OldPrm[i-1] = getxparval(symInx,index);/* get current value of paramter */
			break;
		}
	}

	fprintf(fp,"\n");
}

epilog()
{
	savexpar(); /* save paramter file */
	saveenv(); /* save environment file */
}
printold()
{
	int i;
	fprintf(fp,"\n");
	for (i=0;i<=limit;i++)
	{
		fprintf(fp,"OldPrm[%d] = %28.17lf\n",i,OldPrm[i]);
	}
	fprintf(fp,"\n");
}
