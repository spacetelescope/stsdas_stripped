/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* Matrix operations to calculate weights and phi vector */

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

extern FILE *fp;
extern double tolerance;           /* number of residuals */

typedef double MATRIX[MATSIZE][MATSIZE];   /* MATRIX typedef */
typedef double VECTOR[MATSIZE];            /* VECTOR typedef */

int iterno=1;

matxpose(A,B,p,q)   /* transpose p x q matrix A into B */
MATRIX A,B;
int p;
int q;
{
	int i,j;

	for(i=0;i<p;i++)
		for(j=0;j<q;j++)
			B[j][i] = A[i][j];
}

matcopy(A,B,p,q)  /* copy p x q matrix A into B */
MATRIX A,B;
int p;
int q;
{
	int i,j;

	for(i=0;i<p;i++)
		for(j=0;j<q;j++)
			B[i][j] = A[i][j];
}

matmpy(A,B,C,p,r,q)     /* multiply p x q matrix A by z x r matrix B into C */
MATRIX A,B,C;
int p;
int q;
int r;
{
	int i,j,k;
	double sum;

	for(i=0;i<p;i++)
		for(j=0;j<q;j++)
		{
			sum = 0.0;
			for(k=0;k<r;k++)
				sum += A[i][k]*B[k][j];
			C[i][j] = sum;
		}
}

matvecmpy(A,B,C,p,q)  /* multiply p x q matrix A by q-vector B into vector C */
MATRIX A;
VECTOR B,C;
int p;
int q;                              
{
	int i,j;
	double sum;

	for(i=0;i<p;i++)
	{
		sum = 0.0;
		for(j=0;j<q;j++)
			sum += A[i][j]*B[j];
		C[i] = sum;
	}
}

cholesky(A,p)	/* Calculates Upper Triangular Cholesky Factor of symmetric
                   p x p matrix A, Result to A */
MATRIX A;
int p;
{
	int i,j,k;
	double sum,beta;

	if(p>MATSIZE)          /* max size is MATSIZE */
		fatalerror("Matrix Square Root, Order too high\n","");
	if(A[0][0] <= 0.0)     /* can't do it if diagonal element is <= 0 */
		fatalerror("Matrix Square Root Error 0\n","");
	A[0][0] = sqrt(A[0][0]);  /* 1 x 1 is just square root */
	for(i=1;i<p;i++)  /* generate i x i from (i-1)x(i-1) */
	{
		beta = A[i][i];   /* pivot */
		for(j=0;j<i;j++)
		{
			sum = A[j][i];
			A[i][j] = 0.0;
			for(k=0;k<j;k++)
			{
				sum -= A[k][j]*A[k][i];
			}
			if(A[j][j] <= 0.0)
				fatalerror("Cholesky: Matrix not pos def\n","");
			A[j][i] = sum = sum/A[j][j];
			beta -= sqr(sum);
		}
		if(beta < 0.0)
			fatalerror("Cholesky: Negative diagonal term\n","");
		A[i][i] = sqrt(beta);
	}
}


mat_inv(A,p) /* Inverse of matrix */
MATRIX A;
int p;
{
	MATRIX L,U;

	matcopy(A,U,p,p);
	cholesky(U,p);
	ut_inv(U,p);
	matxpose(U,L,p,p);
	matmpy(U,L,A,p,p,p);
}



ut_inv(A,p) /* Inverse of Upper-triangularp x p Matrix A */
MATRIX A;
int p;
{
	int i,j,k;
	double sum;

	if(p>MATSIZE)
		fatalerror("Matrix Inverses Order Too Great\n","");
	for(i=p-1;i>=0;i--)
	{
		for(j=i;j>=0;j--)
		{
			sum = deltafn(i,j);  /* right hand side is unti matrix */
			for(k=j+1;k<p;k++) /* back substitute */
				sum -= A[j][k]*A[k][i];
			if(A[j][j] == 0.0)   /* if point is zero, error */
				fatalerror("Weight Matrix is Singular!\n","");
			A[j][i] = sum/A[j][j];  /* compute inverse */
		}
	}
}


matprint(A,p,q)  /* print out a p x q matrix A (for debug) */
MATRIX A;
int p,q;
{
	int i,j;

	fprintf(fp,"\n");
	for(i=0;i<p;i++)
	{
		for(j=0;j<q;j++)
			fprintf(fp,"%lf ",A[i][j]);
		fprintf(fp,"\n");
	}
	fprintf(fp,"\n");
}

matfprint(A,p,q)  /* print out result file a p x q matrix A */
MATRIX A;
int p,q;
{
	int i,j;

	for(i=0;i<p;i++)
	{
		for(j=0;j<q;j++)
			fprintf(fp,"%lf ",A[i][j]);
		fprintf(fp,"\n");
	}
	fprintf(fp,"\n");
}

search(n,names,str)  /* look for an observational datum name */
char *str;
char *names[MATSIZE];
int *n;
{
	int i;

	for(i=0;i<*n;i++)  /* look through what currently exists */
	{
		if(str==names[i])   /* if found, return the index */
			return i;
	}
	if(*n >= MATSIZE)  /* if not found and no more room */
		fatalerror("No More Room In Matrix Name Table\n","");/*fatal error */
	names[*n] = str;  /* add name to table */
	return (*n)++; /* and return its value */
}

covarname(x,y,name)  /* concatenate name(x) and name(y) with underscores */
char *x, *y, *name;
{
	strcpy(name,x);  /* result is 8 characters long */
	strcat(name,"_");
	strcat(name,y);
}


double getsigma(x,y)
char *x, *y;
{
	char name[64];
	double cov;


	covarname(x,y,name);  /* make the name for the covariance */
	if((cov = getdataval(name)) != 0.0)  /* look for it in datafile */
	{
		return cov;  /* if nonzero, return it */
	}
	if(x!=y)  /* if not found, if x!= y then */
	{
		covarname(y,x,name); /* its a covariance, look for it with
		                                        names in reverse order */
		if((cov = getdataval(name)) != 0.0);
			return cov;
	}
	if (iterno == 1)
	{
		banner();
		iterno = iterno + 1;
	}
	return 1.0;  /* otherwise its a variance, return 1.0 */
}

banner()
{
	fprintf(fp,"\n***********************************");
	fprintf(fp,"***********************************\n");
	fprintf(fp,"\nNo value given for variance in data table.\n");
	fprintf(fp,"Variance assumed equal to 1.0.\n");
	fprintf(fp,"Covariance assumed equal to 0.0.\n");
	fprintf(fp,"\n***********************************");
	fprintf(fp,"***********************************\n\n");
	printf("\n***********************************");
	printf("***********************************\n");
	printf("\nNo value given for variance in data table.\n");
	printf("Variance assumed equal to 1.0.\n");
	printf("Covariance assumed equal to 0.0.\n");
	printf("\n***********************************");
	printf("***********************************\n\n");
}


computewt(m,k,rhs,L) /* compute weight and phi vector */
int m;    /* number of conditional equations */
DATUMPTR k[MATSIZE];  /* pointer to equations of condition */
VECTOR rhs;   /* phi vector */
MATRIX L;   /* weight matrix */
{
	int i,j;
	MATRIX sigma,fx,sigfxt,U,W,fxt;
	VECTOR uhat,vhat,temp1,temp2,psizero,duhat,dvhat;
	MATRIX Q,Qt,R,Rt,D,A,At,B,Bt,C,Ct;
	char *names[MATSIZE];
	int n;
	DATUMPTR kk;
	extern double SumRho, SumPsi, SumPsiP, SumPsiSq, SumPsiPSq;
	extern double DeltaV, ScaleFac;

	n = 0;
	for(i=0;i<m;i++) /* do for all eqs of condition in this batch */
	{
		for(j=0;j<MATSIZE;j++)  /* zero out fx matrix row */
			fx[i][j] = 0.0;
		kk = k[i];  /* get equation of condition from vector */
		rhs[i] = - kk->value; /* get the righthand side */
		while(kk = kk->next)  /* look at derivative term */
			if(kk->type == ObsType) /* if its an observation type */
			{
				j = search(&n,names,getnam(kk->name)); /* get the position
				                           of the variable in the matrix */
				fx[i][j] = kk->value; /* enter derivative into fx matrix */
			}
	}
	for(i=0;i<n;i++)			/* Get Sigma */
	{
		vhat[i] = getresidual(names[i]); /* get residual */
		for(j=i;j<n;j++)
		{
			sigma[j][i] = sigma[i][j]  /* fill covariance matrix */
			= sqr(ScaleFac==0.0?1.0:ScaleFac)
				*getsigma(names[i],names[j]);  /* use names entered 
			                       into name vector */
		}
	}

	matcopy(sigma,Rt,n,n);
	cholesky(Rt,n);  /* square root of covariance matrix */
	matxpose(Rt,R,n,n);  /* square root of covariance matrix */
	matcopy(Rt,Qt,n,n);  /* sigma (-0.5) */
	ut_inv(Qt,n);
	matxpose(Qt,Q,n,n);
	matvecmpy(Q,vhat,uhat,n,n);   /* normalized residual vector */
	for(i=0;i<n;i++)
	{
		double temp;
		double M,N;

		N = n;
		M = m; /* M = m ? 1? */
		if (getenvval("minsum")) {
			insertresidual(uhat[i]); 
		} 
		SumRho += rhofn(uhat,i,n)*M/N; /* compute rho(u) */
		psizero[i] = temp = psifn(uhat,i,n);  /* compute psr(u) */
		SumPsi += temp*M/N;  /* add into sum */
		SumPsiSq += sqr(temp);  /* add into sum squared */
		temp = psipfn(uhat,i,i,n);  /* psi'(u) */
		SumPsiP += temp*M/N;  /* add into sum */
		SumPsiPSq += sqr(temp)*M/N;  /* add into sum squared */

		for(j=0;j<n;j++)   /* compute D matrix */	
		if(getenvval("irls"))  /* value depend on IRLS or Newton's method */
		{
			if(i==j)
				D[i][j] = Weightfn(uhat,i,n);
			else
				D[i][j] = 0.0;
		}
		else
			D[i][j] = psipfn(uhat,i,j,n);
	}
	mat_inv(D,n);
	matmpy(fx,R,A,m,n,n);/* compute product of matrices: A = f(x)*signam 0.5 */
	matxpose(A,At,m,n);
	matmpy(A,D,C,m,n,n);/* C = A . D */
	matxpose(C,Ct,m,n);  
	matmpy(C,At,U,m,n,m);  /* U = A . D . A' */
	cholesky(U,m,m);       /* (ADA')  0.5 */
	ut_inv(U,m);           /* (ADA')  (-0.5) */

	matxpose(U,L,m,m);
	matmpy(U,L,W,m,m,m);   /* (ADA')  (-1) */

	if(!getenvval("irls"))
		for(i=0;i<m;i++)
		{
			for(j=0;j<n;j++)
				rhs[i] += C[i][j]*psizero[j]; /* rhs = ADpsf */
		}
	else
		for(i=0;i<m;i++)
		{
			for(j=0;j<n;j++)
				rhs[i] += A[i][j]*uhat[j]; 
			/* rhs = A. sigma  (-0.5) . vhat */
		}
	matvecmpy(W,rhs,temp1,m,m);  /* W . rhs */
	matvecmpy(At,temp1,temp2,n,m);  /* A' . W . rhs */
	if(!getenvval("irls"))
		for(i=0;i<n;i++)
			temp2[i] -= psizero[i]; /* subtract psfzero */
	matvecmpy(D,temp2,duhat,n,n);  /* duhat */
	if(!getenvval("irls"))
		for(i=0;i<n;i++)
		{
			double du;

/*			if((du = fabs(duhat[i])) > 1.0)*/  /* don't step too far */
/*				duhat[i] = duhat[i]/du; commented out vis Dick French's problem*/
		}
	else
		for(i=0;i<n;i++)
		{
			duhat[i] = duhat[i] - uhat[i];
		}
	matvecmpy(R,duhat,dvhat,n,n);
	for(i=0;i<n;i++)
	{
		/* compare delta V to largest so far */
		DeltaV = maxval(DeltaV,fabs(duhat[i])*Weightfn(duhat,i,n));
		putresidual(names[i],vhat[i]+dvhat[i]);/* put residual into datafile */
	}
}
