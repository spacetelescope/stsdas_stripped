/*
	GAUSS - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/*
	This code is a modification of the FORTRAN subroutine "CL1",
	by I. Barrodale and F. D. K. Roberts, for calculating an L1 
	solution to a k x n system of linear equations
		ax = b
	subject to l linear equality constraints
		cx = d
	and m linear inequality constraints
		ex <= f
		
	It was modified from their original algorithm (published in
	Report #DM-104-IR from the Mathematics Department  of the
	University of Victoria, B.C., Canada, (1977).
	
	The FORTRAN algorithm was converted into RATFOR by a UNIX
	utility; this was then hand-converted into Pascal and into C
	by W. H. Jefferys, University of Texas. This process resulted
	in code that more clearly displays the logical structure of the
	algorithm than does the original FORTRAN code. Further 
	modifications were made to improve compatibility
	with the Gauss program.
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_math
#include <iraf.h>

#include "defines.h"
#include "house.h"

#ifdef VAX
typedef int Boolean;
#define TRUE 1
#define FALSE 0
#endif

static int	k,			/*	Number of rows of matrix a (k>=1) */
	l,			/*	Number of rows of matrix c (l>=0) */
	m,			/*	Number of rows of matrix e (m>=0) */
	n;			/*	Number of columns of the matrices
			a, c, e (n>=1) */
static double **q = NULL;		/*	(Array of pointers to a) two
			dimensional double array with Rows
			rows and Cols columns.
			
			On entry the matrices a,c and e, and
			the vectors b, d and f must be stored
			in the first k+l+m rows and n+1 
			columns of (the array pointed to
			by) q as follows:
			
				a b
			q =	c d
				e f
			
			These values are destroyed by the
			subroutine.
			
			Note: the columns and rows of q
			are addressed indirectly through
			the arrays rowptr and colptr. In
			particular, b, d and f are stored
			in the (logical) n+1st column of
			q, rather than the actual column.
				*/
static int kode;		/*
			A code used on entry to, and exit
			from, the subroutine.
			
			On entry, this should normally be
			set to 0. However, if certain
			nonnegativity constraints are to 
		be included implicitly, rather
		than explicitly in the constraints
		ex <= f, then kode should be set
		to 1, and the nonnegativity 
		constraints included in the arrays
		x and res (see below).
		
		On exit, kode has one of the
		following values:
		0-	Optimal solution found.
		1-	No feasable solution to the
		constraints.
		2-	Calculations terminated
		prematurely due to rounding
		errors.
		3-	Maximum number of iterations
		reached.
		
		Corresponds to kodedum in cl1();
		*/
static double error;	/*	On exit, this gives the minimum 
		sum of absolute values of the
		residuals. Corresponds to the
		argument errordum of cl1().
		*/
static double toler = 1.0e-10;	/*	A small positive tolerance.
		Empirical evidence suggests
		toler = 10^(-d*2/3) where d
		represents the number of decimal
		digits of accuracy available.
		Essentially, the subroutine cannot
		distinguish between zero and any 
		quantity whose magnitude does not
		exceed toler, in particular, it will
		not pivot on any number whose 
		magnitude does not exceed toler.
		
		Corresponds to input tolerdum
		to cl1();
		*/
static int iter;	/*	On entry iter must contain an
		upper bound on the maximum number of
		iterations allowed. A suggested 
		value is 10*(k+l+m). On exit
		iter gives the number of simplex
		iterations.
		
		Corresponds to input iterdum
		to cl1();
		*/
static double *x = NULL;	/*	(Pointer to a) one dimensional array
		of size Cols. On exit this array
		contains a solution to the L1
		problem. If kode=1 on entry, this
		array is also used to include simple
		nonnegativity constraints on the
		variables. The values -1, 0, or 1
		for x[j] indicate that the j-th
		variable is restricted to be <=0,
		unrestricted, or >= 0 respectively.
		
		Addressed indirectly through ColPTR.
		Corresponds to input xdum to cl1();
		*/
static double *res = NULL;	/*	(Pointer to) one dimensional array
		of size Rows. On exit this contains
		the residuals b-ax in the first k
		components, d-cx in the next l
		components (these will be =0), and
		f-ex in the next m components. If
		kode=1 on entry, this array is also
		used to include simple nonnegativity
		constraints on the residuals b-ax.
		The values -1, 0, or 1 for res[i]
		indicate that the i-th residual
		(1 <=i <= k) is restricted to be 
		<=0, unrestricted, of >=0
		respectively.
		
		Addressed indirectly through RowPTR.
		Corresponds to input resdum to cl1();
		*/
static double **cu = NULL;	/*	(Array of pointers to) a two
		dimensional double array with two 
		rows and Rows columns used for 
		workspace.
		*/
static int **iu = NULL;	/*	(Array of pointers to) a two
		dimensional integer array with two 
		rows and Rows columns used for 
		workspace.
		*/
static int *s = NULL;	/*	(Pointers to) an integer array with 
		of dimension Rows used for workspace.
		*/

static double *cucol = NULL;	/*(Pointer to) actual array referred to by cu */
static int *iucol = NULL;	/*(Pointer to) actual array referred to by iu */

static int *RowPTR = NULL;	/*(Pointer to) input arrays used for row operations on arrays. */
static int *ColPTR = NULL;	/*(Pointer to) input arrays used for column operations on arrays. */
static int *rowptr = NULL;	/*row operations array for q matrix. */
static int *colptr = NULL;	/*column operations array for q matrix.*/

static int Cols = 10;	/* Input parameters defining matrix sizes.*/
static int Rows = 30;

extern FILE *fp;


/*	
	The following macros provide indirect access to the elements
	of the arrays through the pointer arrays RowPTR and ColPTR.
*/

#define Q(i,j) q[rowptr[i]][colptr[j]]
		/*
		Barrodale and Roberts' algorithm
		performs row and column operations
		on q which are independent of those
		on the other matrices. Therefore
		a separate pair of arrays is
		provided.
		*/
#define X(j) x[ColPTR[j]]
#define Res(i) res[RowPTR[i]]
#define S(i) s[i]
#define Cu(u,i) cu[u][i]
#define Iu(u,i) iu[u][i]

static double	/* Private variables shared by subroutines. */
	sum,
	z,
	sn,
	zu,
	zv,
	cuv,
	xmax,
	xmin,
	pivot,
	tpivot;

static int
	i,
	j,
	ia,
	ii,
	inn,
	js,
	kk,
	nk,
	n1,
	n2,
	jmn,
	jpn,
	klm,
	nkl,
	iout,
	klm1,
	klm2,
	nklm,
	maxit,
	phase,
	kforce,
	iineg;

cl1(	/*	Inputs/outputs described below */
	Rowdum,	/*	Number of rows in input matrix >= k+l+m+2 */
	Coldum,	/*	Number of columns in input matrix >= n+2*/
	kdum,	/*	kdum = k, number of rows in a */
	ldum,	/*	ldum = l, number of rows in c */
	mdum,	/*	mdum = m, number of rows in e */
	ndum,	/*	ndum = l, number of columns in a, c, e */
	qdum,	/*	pointer array to matrix containing a, c, e */
	xdum,	/*	pointer to result vector */
	resdum,	/*	pointer to residual vector */
	RowPtrDum,	/*	pointer array for row exchanges */
	ColPtrDum,	/*	pointer array for column exchanges */
	kodedum,	/*	A code used on entry to, and exit from, the
		subroutine.
	*/
	tolerdum,	/*	input tolerance */
	errordum,	/*	output error */
	iterdum	/*	input/output iterations */
	)
int Rowdum,Coldum;
int kdum,ldum,mdum,ndum,*kodedum,*iterdum;
double **qdum;
double tolerdum,*errordum;
double *xdum,*resdum;
int *RowPtrDum,*ColPtrDum;
{
	Rows = Rowdum;
	Cols = Coldum;
	k = kdum;
	l = ldum;
	m = mdum;
	n = ndum;
	kode = *kodedum;
	toler = tolerdum;
	iter = *iterdum;
	q = qdum;
	x = xdum;
	res = resdum;
	RowPTR = RowPtrDum;
	ColPTR = ColPtrDum;
	initworkspace();
	initialize();
	simplex();
	results();
	*kodedum = kode;
	*errordum = error;
	*iterdum = iter;
}

initialize()
{
	maxit = iter;
	n1 = n+1;
	n2 = n+2;
	if(Cols < n2)
		fatalerror("Not enough columns for minsum\n","");
	nk = n+k;
	nkl = nk+l;
	klm = k+l+m;
	klm1 = klm+1;
	klm2 = klm+2;
	nklm = n+klm;
	if(Rows < maxval(nklm,klm2))
		fatalerror("Not enough rows for minsum\n","");
	kforce = 1;
	iter = 0;
	js = 0;
	ia = 0;
	
	for(j=0;j<n;j++)
	Q(klm1,j) = j+1;
	for(i=0;i<klm;i++)
	{
	Q(i,n1) = n+i+1;
	if(Q(i,n) < 0)
	for(j=0;j<n2;j++)
	Q(i,j) = -Q(i,j);
	}
}
	
phase1costs()
{
	phase = 2;
	for(j=0;j<nklm;j++)
	{
	Cu(0,j) = 0;
	Cu(1,j) = 0;
	Iu(0,j) = 0;
	Iu(1,j) = 0;
	}
	if(l != 0)
	{
	for(j=nk;j<nkl;j++)
	{
	Cu(0,j) = 1;
	Cu(1,j) = 1;
	Iu(0,j) = 1;
	Iu(1,j) = 1;
	}
	phase = 1;
	}
	if(m != 0)
	{
	for(j=nkl;j<nklm;j++)
	{
	Cu(1,j) = 1;
	Iu(1,j) = 1;
	jmn = j-n;
	if(Q(jmn,n1) < 0)
	phase = 1;
	}
	}
	if(kode != 0)
	{
	for(j=0;j<n;j++)
	if(X(j)<0)
	{
	Cu(0,j) = 1;
	Iu(0,j) = 1;
	}
	else if(X(j)>0)
	{
	Cu(1,j) = 1;
	Iu(1,j) = 1;
	}
	for(j=0;j<k;j++)
	{
	jpn = j+n;
	if(Res(j)<0)
	{
	Cu(0,jpn) = 1;
	Iu(0,jpn) = 1;
		if(Q(j,n1) > 0)
			phase = 1;
	}
	else if(Res(j)>0)
	{
		Cu(1,jpn) = 1;
		Iu(1,jpn) = 1;
		if(Q(j,n1)<0)
			phase = 1;
	}
		}
	}
}

xchrows(i,ia)
int i,ia;
{
	int k;
/*
	for(j=0;j<n2;j++)
	{
		z = Q(ia,j);
		Q(ia,j) = Q(i,j);
		Q(i,j) = z;
	}
*/
	k = rowptr[ia];
	rowptr[ia] = rowptr[i];
	rowptr[i] = k;
}

phase2costs()
{
	phase = 2;
	for(j=0;j<nklm;j++)
	{
		Cu(0,j) = 0;
		Cu(1,j) = 0;
	}
	for(j=n;j<nk;j++)
	{
		Cu(0,j) = 1;
		Cu(1,j) = 1;
	}
	for(i=0;i<klm;i++)
	{
		ii = trunc(Q(i,n1));
		if(ii <= 0)
		{
	if(Iu(1,-ii-1) != 0)
	{
		Cu(1,-ii-1) = 0;
		xchrows(i,ia);
		ia = ia+1;
	}
		}
		else
		{
	if(Iu(0,ii-1) != 0)
	{
		Cu(0,ii-1) = 0;
		xchrows(i,ia);
		ia = ia+1;
	}
		}
	}
}

computemarginalcosts()
{
	for(j=js;j<n1;j++)
	{
		sum = 0;
		for(i=0;i<klm;i++)
		{
	ii = trunc(Q(i,n1));
	if(ii >= 0)
		z = Cu(0,ii-1);
	else
		z = Cu(1,-ii-1);
	sum = sum + /*dble*/(Q(i,j))*/*dble*/(z);
		}
		Q(klm,j) = sum;
	}
	for(j=js;j<n;j++)
	{
		ii = trunc(Q(klm1,j));
		if(ii >= 0)
	z = Cu(0,ii-1);
		else
	z = Cu(1,-ii-1);
		Q(klm,j) = Q(klm,j) - z;
	}
}

enteringvector()
{
	for(j=js;j<n;j++)
	{
		zu = Q(klm,j);
		ii = trunc(Q(klm1,j));
		if(ii <= 0)
		{
	ii = -ii;
	zv = zu;
	zu = -zu - Cu(0,ii-1) - Cu(1,ii-1);
		}
		else
		{
	zv = -zu - Cu(0,ii-1) - Cu(1,ii-1);
		}
		if((kforce!=1) || (ii<=n))
		{
	if((Iu(0,ii-1)!=1) && (zu>xmax))
	{
		xmax = zu;
		inn = j;
	}
	if((Iu(1,ii-1)!=1) && (zv>xmax))
	{
		xmax = zv;
		inn = j;
	}
		}
	}
}

sub290()
{
	if(Q(klm,inn) != xmax)
	{
		for(i=0;i<klm2;i++)
	Q(i,inn) = -Q(i,inn);
		Q(klm,inn) = xmax;
	}
}

sub300()
{
	xmax = 0;
	for(i=0;i<ia;i++)
	{
		z = fabs(Q(i,inn));
		if(z>xmax)
		{
	xmax = z;
	iout = i;
		}
	}
}

sub320()
{
	ia = ia-1;
	xchrows(ia,iout);
	iout = ia;
	pivot = Q(iout,inn);
}

sub330()
{
	kk = -1;
	for(i=0;i<klm;i++)
	{
		z = Q(i,inn);
		if(z>toler)
		{
	kk = kk+1;
	Res(kk) = Q(i,n)/z;
	S(kk) = i+1;
		}
	}
}

sub360()
{
	xmin = Res(0);
	iout = S(0)-1;
	j = 0;
	if(kk != 0)
	{
		for(i=1;i<=kk;i++)
	if(Res(i) < xmin)
	{
		j = i;
		xmin = Res(i);
		iout = S(i)-1;
	}
		Res(j) = Res(kk);
		S(j) = S(kk);
	}
	kk = kk-1;
	pivot = Q(iout,inn);
	ii = trunc(Q(iout,n1));
}

bypassvertices()
{
	for(j=js;j<n1;j++)
	{
		z = Q(iout,j);
		Q(klm,j) = Q(klm,j) - z*cuv;
		Q(iout,j) = -z;
	}
	Q(iout,n1) = -Q(iout,n1);
}

xchcolumns()
{
	int k;
	
/*
	for(i=0;i<=klm1;i++)
	{
		z = Q(i,inn);
		Q(i,inn) = Q(i,js);
		Q(i,js) = z;
	}
*/
	k = colptr[inn];
	colptr[inn] = colptr[js];
	colptr[js] = k;
}

dogaussjordan()
{
	if (!notzero(pivot))
		fatalerror("Attempt to divide by zero in dogaussjordan.\n","");
	iter = iter+1;
	for(j=js;j<n1;j++)
		if(j!=inn)
		{
			Q(iout,j) = Q(iout,j)/pivot;
			z = Q(iout,j);
			for(i=0;i<klm1;i++)
				if(i!=iout)
					Q(i,j) = Q(i,j) - z*Q(i,inn);
		}
	tpivot = -1/pivot;
	for(i=0;i<klm1;i++)
		if(i!=iout)
	Q(i,inn) = Q(i,inn)*tpivot;
	Q(iout,inn) = -tpivot;
	z = Q(iout,n1);
	Q(iout,n1) = Q(klm1,inn);
	Q(klm1,inn) = z;
	ii = trunc(fabs(z));
	if((Iu(0,ii-1)!=0) && (Iu(1,ii-1)!=0))
	{
		xchcolumns();
		js = js+1;
	}
}

results()
{
	sum = 0;
	for(j=0;j<n;j++)
		X(j) = 0;
	for(i=0;i<klm;i++)
		Res(i) = 0;
	for(i=0;i<klm;i++)
	{
		ii = trunc(Q(i,n1));
		sn = 1;
		if(ii <= 0)
		{
	ii = -ii;
	sn = -1;
		}
		if(ii<=n)
	X(ii-1) = sn*Q(i,n);
		else
		{
/*	iimn = ii - n;*/
	Res(ii - n - 1) = sn*Q(i,n);
	if((ii>=n1) && (ii<=nk))
		sum = sum + /*dble*/(Q(i,n));
		}
	}
	error = sum;
}

leavingvector()
{
	Boolean done;
	
	done = FALSE;
	sub290();
	if((phase!=1) && (ia!=0))
	{
		sub300();
		if(xmax>toler)
		{
	sub320();
	done = TRUE;
		}
	}
	if(!done)
	{
		sub330();
		do
		{
	if(kk<0)
	{
		kode = 2;
		done = TRUE;
	}
	else
	{
		sub360();
		if(phase!=1)
		{
			if(ii>=0)
			{
		if(Iu(1,ii-1)==1)
			done = TRUE;
			}
			else
			{
		iineg = -ii;
		if(Iu(0,iineg-1)==1)
			done = TRUE;
			}
		}
		if(!done)
		{
			ii = abs(ii);
			cuv = Cu(0,ii-1) + Cu(1,ii-1);
			if(Q(klm,inn)-pivot*cuv>toler)
		bypassvertices();
			else
		done = TRUE;
		}
	}
		}
		while(!done);
	}
}

testoptimality()
{
	if(kforce!=0)
	{
		if((phase==1) && (Q(klm,n)<=toler))
		{
	phase2costs();
	computemarginalcosts();
		}
		else
	kforce = 0;
	}
	else
	{
		if(phase==1)
		{
	if(Q(klm,n) <= toler)
	{
		phase2costs();
		computemarginalcosts();
	}
	else
		kode = 1;
		}
		else
	kode = 0;
	}
}

simplex()
{
	phase1costs();
	kode = -1;
	if(phase==2)
		phase2costs();
	computemarginalcosts();
	do
	{
		xmax = 0;
		if(js>=n)
	testoptimality();
		else
		{
	enteringvector();
	if(xmax<=toler)
		testoptimality();
	else
	{
		leavingvector();
		if(kode<0)
			if(iter>=maxit)
		kode = 3;
			else
		dogaussjordan();
	}
		}
	}
	while(kode<0);
}

char *MemAlloc();

initworkspace()
{
	int i,j;

	if(s)
		return 0;
/*
	x = (double *)MemAlloc("MINSUM 0",(long)Cols*sizeof(double));
	res = (double *)MemAlloc("MINSUM 1",(long)Rows*sizeof(double));
*/
	s = (int *)MemAlloc("MINSUM 2",(long)Rows*sizeof(int));
	cu = (double **)MemAlloc("MINSUM 3",(long)2*sizeof(double *));
	cucol = 
		(double *)MemAlloc("MINSUM 4",(long)2*Rows*sizeof(double));
	iu = (int **)MemAlloc("MINSUM 5",(long)2*sizeof(int *));
	iucol = 
		(int *)MemAlloc("MINSUM 6",(long)2*Rows*sizeof(int));
	rowptr = (int *)MemAlloc("MINSUM 7",(long)Rows*sizeof(int));
	colptr = (int *)MemAlloc("MINSUM 8",(long)Cols*sizeof(int));
	
	
	for(i=0;i<2;i++)
	{
		cu[i] = &cucol[i*Rows];
		iu[i] = &iucol[i*Rows];
	}
	for(i=0;i<Rows;i++)
		rowptr[i] = RowPTR[i];
	for(i=0;i<Cols;i++)
		colptr[i] = ColPTR[i];
}
