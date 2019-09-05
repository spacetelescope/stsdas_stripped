
static int CountErrors = 0;
static int DontDump =0;

/*
 GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/******************************************/
/*Householder Reduction System Functions*/
/******************************************/

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_math
#include <iraf.h>

#include "defines.h"
#include "alloc.h"
#include "house.h"
#include "housemem.h"
#include "files.h"
#include "datum.h"
#include <time.h>
#include "simpledefs.h"

double MAD();			/* Median Absolute Deviation Function */

#ifndef VMS
FITS *getenvtabpntr();
#endif

#define Matrix(i,j)matrix[RowPtr[i]][ColPtr[j]] /* All arrays are alloc- */
#define RowType(i)rowtype[RowPtr[i]]/* ated dynamically and*/
#define ColType(i)coltype[ColPtr[i]]/* are accessed indirect-*/
#define Deltap(i) deltap[ColPtr[i]]/* ly through arrays*/
#define CondModulus(i)condmodulus[ColPtr[i]] /* ColPtr and RowPtr.*/
#define ConstModulus(i) constmodulus[ColPtr[i]]
#define ConstTest(i)consttest[ColPtr[i]]
#define CondTest(i)condtest[ColPtr[i]]
#define Number(i,j) number[ColPtr[i]][j]
#define NumberX(i) number[ColPtr[i]]
#define VarName(i)varname[ColPtr[i]]


extern double lambda;
extern int MARQ;

/* Allocate pointers for each matrix & vector */
static double **matrix = NULL;	/* Matrix for Eqns of Condition	 */

static int *RowPtr = NULL;	/* RowPtr matrix */
static int *ColPtr = NULL;	/* ColPtr matrix */
static int *rowtype = NULL;	/* Type of each row (condition, constraint) */
static int *coltype = NULL;	/* Type of each col (parameter,indexed parameter, right hand side) */
static int (*number)[5] = NULL;	/* index of a columns parameter */
static double *condmodulus = NULL;/* modulus of condition eqns in a column */
static double *constmodulus = NULL;/* modulus of condition eqns in a row */
static double *condtest = NULL;	
static double *consttest = NULL;
static double *deltap = NULL;	/* vector of parameter corrections */
static double *vector = NULL;	
static double *colvec = NULL;	
static double *sigvec = NULL;	

static int matcol=MINCOLS;
static int matrow=MINROWS;


double *resdum = NULL;/* For minsum routine */
int numresiduals;	/* For minsum routine */

static char **varname = NULL;

#ifdef THINK_C
#define WIDTH  60
#else
#define WIDTH  120
#endif

typedef int INDEX[3];	/* arrays for "sort" of columns by pivot ability*/
static INDEX *indexarray=NULL;

double *matstart,*matend;


static MEMSIZE matsize; /* pointer to current matrix size */

static int		LastNonzero;	/* Last possible pivot row */
static int		CurrentRow;	/* The row being eliminated */
static int		Recalculate;	/* Flag to recalculate moduli */
static int		LastCol;	/* Last column active in matrix */
static double	Tol;			/* Acceptable Tolerance */
static int		PrintFlag;	/* Print intermediate results */
static int		LastRow;	/* Last row active in matrix */
static int		FirstPivots;	/*	0 = pivot plates first;
									1 = pivot stars first;
								   -1 = let program choose pivots */
static int		Constraints;	/* Number of constraints entered */
static int		Touched = 0;	/* Whether more info has been added */
static int 		noindex = 0;   /* there are no indexed parameters */
static int 		lbl = 0;       /*  the indexes are large; put out line by line*/
static double	DOF;			/* Count of Degrees of Freedom */
static double	NEQN;			/* Count of Eqs of Condition */
double Sigma;				/* Current value of Sigma */
double Sigma1;				/* (extern) Current value of Sigma1 */
double Sigma1Sq;			/* (extern) Current value of Sigma1Sq */
double SumRho = 0.0;			/* (extern) Running Sum for Rho */
double SumPsi = 0.0;			/* (extern) Running Sum for Psi */
double SumPsiSq = 0.0;			/* (extern) Running Sum for PsiSq */
double SumPsiP = 0.0;			/* (extern) Running Sum for PsiP */
double SumPsiPSq = 0.0;			/* (extern) Running Sum for PsiPSq */
double ScaleFac = 1.0;			/* (extern) ScaleFac */
double DeltaV = 0.0;			/* (extern) Largest Residual Change */
static int		StartAt = 0;	/* Column to start pivoting */

FILE *fp;



double integral();

#ifdef BOUNDS
int rowptr(i)
int i;
{
	int k;

	if((i>=matsize.nrows)||(i<0))
		printf("Row Index Error %d, Rows = %d\n",i,matsize.nrows);
	k = RowPtr[i];
	if((k>=matsize.nrows)||(k<0))
		printf("Row Pointer Error %d, Rows = %d\n",k,matsize.nrows);
	return k;
}

int colptr(i)
int i;
{
	int k;

	if((i>=matsize.ncols)||(i<0))
		printf("Column Index Error %d, Cols = %d\n",i,matsize.ncols);
	k = ColPtr[i];
	if((k>=matsize.ncols)||(k<0))
		printf("Column Pointer Error %d, Cols = %d\n",k,matsize.ncols);
	return k;
}
#else
#define rowptr(i) RowPtr[i]
#define colptr(i) ColPtr[i]
#endif 

char *MemAlloc(s,size)/* Allocate 'size' bytes */
char *s;
long size;
{
	char *x;

#ifdef THINK_C
	if((x = (char *)mlalloc(size)) == NULL) /* Allocate the memory */
#else
	if((x = (char *)malloc((int)size)) == NULL) /* Allocate the memory */
#endif
			fatalerror("Memory Manager Error in Allocation--%s\n",s); /* error if not enough memory */
	return x;
}

char *Reallocate(s,size,spointer)/* use for both initial and reallocation */
char *s, *spointer;
long size;
{
	char *x;

#ifdef THINK_C
	if ((x = relalloc(spointer,size+8)) == NULL)/* reallocate rowarray */
#else
		if ((x = realloc(spointer,(int)size+8)) == NULL)/* reallocate rowarray */
#endif
			fatalerror("Memory Manager Error in Reallocation. Allocated pointer is NULL. --%s\n",s);
	return x;
}

double LSB()/*Computes the size of the LSB*/
/*of a word assuming the MSB is 1.0*/
{
	double x, y;

	x = 1.0;
	do
	    {
		x = 0.5 * x;	/* Divide by 2 repeatedly until no change is */
		y = 1.0 + x;	/* seen when result is added to 1	*/
	}
	while( y != 1.0);
	return 2.0 * x;
}

CountVars(type)		/* Count the number of variables of type 'type' */
int type;
{
	int k,count;

	count=0;	/* initialize */
	for(k=0;k<=LastCol;k++)	/* do for each columsn that has a variable */
		if(type==ColType(k)) /* if the desired type */
			count++;	/* increment count */
	return count;
}

GetDeltaValue(k,name,index,type,value) 	/* Get data which was computed for */
int k;									/* k'th parameter		 */
int (*index)[5];
char **name;
int *type;
double *value;
{
	if(k<LastCol)			/* done? */
	{
		*type = ColType(k);	/* No; return the data of parameter */
		*name = VarName(k);
		(*index)[0] = Number(k,0); 
		(*index)[1] = Number(k,1); 
		(*index)[2] = Number(k,2);
		(*index)[3] = Number(k,3);
		(*index)[4] = Number(k,4);
		*value = Deltap(k);
		return 1;		/* return TRUE */
	} else return 0;		/* all done; return FALSE */
}


GetDeltaParams(k,name,index,type,value) /* Get data which was computed for */
int k;									/* k'th parameter		 */
int (*index)[5];
char **name;
int *type;
double *value;
{
	if(k<=LastCol) {			/* done? */
		*type = ColType(k);	/* No; return the data of parameter */
		*name = VarName(k);
		(*index)[0] = Number(k,0); 
		(*index)[1] = Number(k,1); 
		(*index)[2] = Number(k,2);
		(*index)[3] = Number(k,3);
		(*index)[4] = Number(k,4);
		*value = Deltap(k);
		return 1;		/* return TRUE */
	} else return 0;		/* all done; return FALSE */
}


getcolumn(type,name,indices)	
/* Get the number of column corresponding to type; name[indices] */
int type, *indices;
char *name;
{
	int k;
	int CP, *NumPtr;

	for(k=0;k<=LastCol;k++) {		/* search all columns */
		CP = ColPtr[k];
		NumPtr = (int *)(number + CP);
		/* if all conditions satisfied */
		if( type==coltype[CP]
			&& NumPtr[0] == indices[0]
			&& NumPtr[1] == indices[1]
			&& NumPtr[2] == indices[2]
			&& NumPtr[3] == indices[3]
			&& NumPtr[4] == indices[4]
			&& strcmp(name, varname[CP]) == 0)
			return k;	/* return k */
	}
	if(++LastCol >= matsize.ncols) { /* column doesn't exist; add to table */
		matsize.lastcol = matsize.ncols;
		matsize.ncols = LastCol + 10;/*maxval(1.0,0.2*LastCol);*/
		colspace();
		matspace(); 
	}
	ColType(LastCol) = type; /* enter data into tables- coltype filled*/

	NumPtr = (int *)(number + ColPtr[LastCol]);
	NumPtr[0] = indices[0]; /* number filled */
	NumPtr[1] = indices[1]; /* number filled */
	NumPtr[2] = indices[2]; /* number filled */
	NumPtr[3] = indices[3]; /* number filled */
	NumPtr[4] = indices[4]; /* number filled */
	VarName(LastCol) = name; /* varname filled */
	/* Correct degrees of freedom if not RAS */
	if(/*type < RightHandType ||*/ type > ObsType)
		DOF--;
	/* 
		 DOF = n - k + r where
		n = # of eqs of cond
		k = # of parameters
		r = # of constraints
		*/
			return LastCol;	/* return pointer to new column */
}

double getdeltas(type,name,i)	/* gets parameter correction from array */
char *name;
int type, *i;
{
	int k;

	k = getcolumn(type,name,i);	/* get the column number */
	return Deltap(k);	/* fetch parameter correction */
}


PrintMatrix() 
{			/* Print current matrix of eqns of condition */
	int i, begin, width,inc;
	int colwid;

	colwid = getmaxprmname();
	if (colwid <6) colwid = 6;
	width = (WIDTH-colwid)/(colwid+2);

	if (PrintFlag /*&& Touched*/) { /* Only print if matrix has been altered */
				 /* and user wants it */
		begin = 0;
		inc = width;
		i = 1;
		while (inc < LastCol) {
			PrinttheMatrix(begin,inc,colwid);
			i++;
			begin = inc + 1;
			inc = i * width;
		}
		PrinttheMatrix(begin,LastCol,colwid);
	}
}

PrinttheMatrix(begin,end,colwid)
int begin, end,colwid;
{
	int i, j,ndim;

	fprintf(fp,"%*s  ",colwid," ");
	for (j = begin;j<=end;j++)	/* print column number */
		fprintf(fp,"%*d  ",colwid,colptr(j));	
	fprintf(fp,"\n");

	fprintf(fp,"%*s  ",colwid,"Type");		/* print name of row */
	for (j = begin;j<=end;j++)	/* print each item of vector */
		fprintf(fp,"%*d  ",colwid,coltype[colptr(j)]);
	fprintf(fp,"\n");		/* print carriage return */

	fprintf(fp,"%*s  ",colwid,"Name");	/* print name of each variable*/
	for (j = begin;j<=end;j++)
		fprintf(fp,"%*s  ",colwid,VarName(j));
	fprintf(fp,"\n");

	ndim = getdimnum();
	for (j = 0; j<ndim; j++) {
		if (j == 0)
			fprintf(fp,"%*s  ",colwid,"Index");	
		else
			fprintf(fp,"%*s  ",colwid," ");
		for(i=begin;i<end;i++)	{
			if(Number(i,4) != 0) {
				prSIndex(fp,NumberX(i),colwid,j);
				fprintf(fp, "  ");
			} 
			else{
				fprintf(fp,"%*s", colwid,"");
				fprintf(fp, "  ");
			}
		}
		fprintf(fp,"\n");
	}

	fprintf(fp,"\n");
	fprintf(fp,"\n");
	for (i = 0;i<=LastRow;i++)	/* print each row of matrix */
	{
		/* print row number and type */
		fprintf(fp,"%*d%*d  ",(colwid/2),rowptr(i), (colwid/2),RowType(i)); 
		for (j = begin;j<=end;j++) /* print normalized matrix entry */
			fprintf(fp,"%*d  ",colwid,trunc(100 * Matrix(i,j)));
		fprintf(fp,"\n");
	}
	fprintf(fp,"\n");
}

PrintResults()			/* Print results of iteration */
{
	int i;

	PrintMatrix();		/* Print the condition eqn matrix */
}

InitHouse(s,PrintVal,Tolerance)/* Initialize HouseHolder routines */
char *s;
int PrintVal;
double Tolerance;
{
	int i, j;

	if(!matrix)		/* If no space allocated, allocate it */
	{
		/* Allocate space for each array */
		matsize.ncols = MINCOLS;
		matsize.nrows = MINROWS;
		matsize.lastrow = 0;
		matsize.lastcol = 0;
		ReserveSpace();
		filecheck(s);
		printcopyright(fp);
		printdate(fp);
		printenvtoRes(fp);
	}
	Tol = Tolerance * LSB();/* Save tolerance for reduction */
	PrintFlag = PrintVal;	/* Should results be printed? */
	Constraints = 0;		/* Number of Constraints so far */
	LastRow = -1;			/* Number of Rows so far, -1 */
	LastCol = 0;			/* Number of Columns so far, -1 */
	Touched = 0;			/* Matrix is pristine */
	DOF = 0.0;				/* Start with no eqs read in */
	NEQN = 0.0;
	StartAt = 0;			/* Start looking for pivots in col 0 */
	for (i = 0;i<matsize.nrows;i++)/*Set up row headers */
		RowPtr[i] = i;
	for (j = 0;j<matsize.ncols;j++)/*Set up column headers*/
	{
		ColPtr[j] = j;
		ConstModulus(j) = 0.0;
		CondModulus(j) = 0.0;
	}

	VarName(LastCol) = "_RHS";/* Set up right hand side column varname filled*/
	ColType(LastCol) = RightHandType;/* coltype filled */
	Number(LastCol,0) = 0; /* number filled */
	Number(LastCol,1) = 0; /* number filled */
	Number(LastCol,2) = 0; /* number filled */
	Number(LastCol,3) = 0; /* number filled */
	Number(LastCol,4) = 0; /* number filled */
	/* take these next two lines out - they serve no purpose */
	/*Sigma1 = getenvval("sigma1");*/	/* find current sigma */
	/*Sigma1Sq = sqr(Sigma1);*/		/* get its square*/
	SumRho = 0.0;			/* Initialize all sums */
	SumPsi = 0.0;
	SumPsiSq = 0.0;
	SumPsiP = 0.0;
	SumPsiPSq = 0.0;
	DeltaV = 0.0;
	ScaleFac = getenvval("scale");	/* Get current scale factor */
	if (ScaleFac < 0.0001)
		ScaleFac = 1.0;
	fprintf(stdout,"Initial Scale= ");
	printdouble(stdout,ScaleFac);
	fprintf(stdout,"\n");
	numresiduals = 0;		/* number of residuals starts at zero */
}

ReserveSpace()
{
	rowspace();
	colspace();
	matspace();
}


rowspace()
{
	int i;
	long introw, doublerow;
	static int first=1;

	introw =(long)matsize.nrows*sizeof(int);
	doublerow =(long)matsize.nrows*sizeof(double);
	if (first == 1) { /*if first pass allocate space with malloc */
		RowPtr = (int *)MemAlloc("RowPtr",introw);
		rowtype = (int *)MemAlloc("rowtype",introw);
		indexarray = (INDEX*)MemAlloc("indexarray",(long)matsize.nrows*sizeof(INDEX));
		resdum = (double *)MemAlloc("resdum",MATSIZE*doublerow);
		first++;
	} else {  /* if subsequent pass reallocate space with realloc */
		RowPtr = (int *)Reallocate("RowPtr",introw,(char*)RowPtr);
		rowtype = (int *)Reallocate("rowtype",introw,(char*)rowtype);
		indexarray = (INDEX*)Reallocate("indexarray",(long)matsize.nrows*sizeof(INDEX),(char*)indexarray);
		resdum = (double *)Reallocate("resdum",MATSIZE*doublerow,(char*)resdum);
	}
	/* initialize any new rows */
	for (i = matsize.lastrow;i<matsize.nrows;i++)/*Set up row headers */
		RowPtr[i] = i;
}

colspace() {
	int j;
	long intcol, doublecol,charcol;
	static int firstcol = 1;

	intcol =(long)matsize.ncols*sizeof(int);
	doublecol =(long)matsize.ncols*sizeof(double);
	charcol =(long)matsize.ncols*sizeof(char*)*4;
	if (firstcol == 1) {
		ColPtr = (int *)MemAlloc("ColPtr",intcol);
		coltype = (int *)MemAlloc("coltype 5",intcol);
		number = (int (*)[5])MemAlloc("number 6",5*intcol);
		condmodulus = (double *)MemAlloc("comdmodulus",doublecol);
		constmodulus = (double *)MemAlloc("constmodulus",doublecol);
		condtest = (double *)MemAlloc("condtest",doublecol);
		consttest = (double *)MemAlloc("consttest",doublecol);
		deltap = (double *)MemAlloc("deltap",doublecol);
		vector = (double *)MemAlloc("vector",doublecol);
 		colvec = (double *)MemAlloc("colvec",doublecol);
 		sigvec = (double *)MemAlloc("sigvec",doublecol);
		varname = (char **)MemAlloc("varname",charcol);
		firstcol++;
	} else {
		ColPtr = (int *)Reallocate("ColPtr",intcol,(char*)ColPtr);
		coltype = (int *)Reallocate("coltype",intcol,(char*)coltype);
		number = (int (*)[5])Reallocate("number",5*intcol,(char*)number);
		condmodulus = (double *)Reallocate("condmodulus",doublecol,(char*)condmodulus);
		constmodulus = (double *)Reallocate("constmodulus",doublecol,(char*)constmodulus);
		condtest = (double *)Reallocate("condtest",doublecol,(char*)condtest);
		consttest = (double *)Reallocate("consttest",doublecol,(char*)consttest);
		deltap = (double *)Reallocate("deltap",doublecol,(char*)deltap);
		vector = (double *)Reallocate("vector",doublecol,(char*)vector);
 		colvec = (double *)Reallocate("colvec",doublecol,(char*)colvec);
 		sigvec = (double *)Reallocate("sigvec",doublecol,(char*)sigvec);
		varname = (char **)Reallocate("varname",charcol,(char*)varname);
	}
	for (j = matsize.lastcol;j<matsize.ncols;j++)/*Set up column headers*/
	{
		ColPtr[j] = j;
		ConstModulus(j) = 0.0;/* Moduli are zero to start */
		CondModulus(j) = 0.0;
	}
}

matspace() {
	int i,j;
	long doublerowcol,rowdptr;
	double *rowallocate(), *rowreallocate();
	static int firstmat= 1;

	rowdptr = (long)matsize.nrows*sizeof(double**);
	if (firstmat == 1) {  /* Matrix is set up as a double indirection array */
		matrix=(double **)MemAlloc("matrix",rowdptr);
		for (i=0;i <MINROWS; i++)
			matrix[i] = rowallocate();/* allocate space for double values */
		firstmat++;
	} else {
		if (matcol < matsize.ncols) /* column reallocate */
			for(i=0;i<matsize.nrows;i++) {
				matrix[i] = rowreallocate((char*)matrix[i]); /* allocate space for double values */
			}
		if (matrow < matsize.nrows) {
			matrix=(double **)Reallocate("matrix",rowdptr,(char*)matrix);
			for(i=matrow;i<matsize.nrows;i++)
				matrix[i] = rowallocate(); /* allocate space for double values */
		}
	}
	matcol = matsize.ncols;
	matrow = matsize.nrows;
} 

double *rowallocate() {
	double *x;
	int i;
	long rowsize;

	rowsize = (long)matsize.ncols*sizeof(double);
	x = (double*)MemAlloc("MATRIX ROW",rowsize);
	for (i=0;i<matsize.ncols;i++)
		x[i] = 0.0;
	return x;
}

double *rowreallocate(Pointr)
char *Pointr;
{
	double *x;
	long rowsize;
	int i;

	rowsize = (long)matsize.ncols*sizeof(double);
	x = (double*)Reallocate("MATRIX ROW",rowsize,Pointr);
	for (i = matcol;i<matsize.ncols;i++) /* zero out end of row */
		x[i] = 0.0;
	return x;
}

filecheck(s)
char *s;
{
	char *fname;

	/* check for result file name */
	fname = NULL;
	if(!(fname=getenvstr(s)))
		fatalerror("No Result File Specified.Use keyword * results * to specify file name in the environment file\n","");
	if(!(fp = fopen(fname,"w"))) /* open result file */
		fatalerror("Unable to open result file %s.\n",fname);
	
}


printcopyright(fp)
FILE *fp;
{
	fprintf(fp,"*********************************************");
	fprintf(fp,"***********************************\n\n");
	fprintf(fp,"GaussFit - A System for Least Squares and Robust Estimation\n\n");
	fprintf(fp,"Copyright (C) 1987,1988 by William H. Jefferys,\n");
	fprintf(fp,"Michael J. Fitzpatrick, Barbara E. McArthur and James McCartney\n");
	fprintf(fp,"All Rights Reserved. Version 1.00\n");
	fprintf(fp,"\n*********************************************");
	fprintf(fp,"***********************************\n\n");
}       

SumInit(eqtype)		/* Initialize condition or constraint equations now */
int eqtype;
{
	int i, j, k;

	NEQN++;			/* increment number of equations */
	DOF++;			/* 
						 DOF = n - k + r where
						n = # of eqs of cond
						k = # of parameters
						r = # of constraints
					*/
		LastRow = LastRow + 1;
		Touched = 1;		/* Matrix no longer pristine */
	if(LastRow >= matsize.nrows)	/* Check that row exists*/
	{
		matsize.lastrow = matsize.nrows;
		matsize.nrows = LastRow + 10;/*LastRow*maxval(1.0,0.2*LastRow);*/
		rowspace();
		matspace(); 
	}
	if(eqtype == Constraint)/* increment constraints if it's a constraint */
		Constraints += 1;
	RowType(LastRow) = eqtype;/* fix row type */
	for (i = 0;i<matsize.ncols;i++)	/* zero out entire row */
	{
		Matrix(LastRow,i) = 0.0;
	}
}

SumAdd(type,name,i,x)		/* Add term to matrix */
int type,*i;
char *name;
double x;
{
	int k;
#ifdef DEBUG	
	fprintf(fp,"Sumadd %s = %lf\n",name,x);
#endif
	k = getcolumn(type,name,i);	/* find column corresponding to var */
	Matrix(LastRow,k) += x;		/* ??????add in the value */
	StartAt = minval(StartAt,k);	/* Possible pivot column */
}

SwitchCol (i, j)	/* switch 2 columns of a matrix by switching pointers */
int i, j;
{
	int k;

	k = colptr(i);
	ColPtr[i] = colptr(j);
	ColPtr[j] = k;
}

int Modulus (Col, i)
int Col, i;
/*This has only 3 values: 
**	2 if the row is a potential constraint pivot; 
**	1 if it is a potential condition equation pivot 
**	zero otherwise (if its leading term in the pivot column is zero)
*/
{
	if (Matrix(i,Col) == 0.0)
		return 0;
	else
		return RowType(i);
}

SortPivotColumn (ThisCol)
int ThisCol;			/* only sort rows >= ThisCol */
/*This is an order(n) sort for the special case we have here.*/
{
	int n[3];		/* number of rows of each type */
	int i, j, k;

	for (i = 0;i<= 2;i++)	/* zero out number vector */
		n[i] = 0;
	/* collect the possible pivots of each type */
	for (i = ThisCol;i<=LastRow;i++) {
		j = Modulus(ThisCol, i);
		indexarray[n[j]][j] = rowptr(i);/*put row pointers into array*/
		n[j]++;		/* increment number of pivots of this type */
	}
	k = ThisCol;
	/* Order rows so nonzero pivot constraints 
					 come before nonzero pivot conditions which
					 come before zero pivot rows
					*/
	for (j = 2;j>=0;j--)
		for (i = 0;i<n[j];i++) {
			RowPtr[k] = indexarray[i][j];
			if (j > 0)
				LastNonzero = k;
			k++;
		}
}

ApplyConstraintTransform (column)
int column;
/*This is actually just an elementary Gaussian elimination */
/*on each row that has a leading nonzero element.This*/
/*is the limit of a Givens transformation on that row as */
/*the weight of the pivot row tends to infinity. */
{
	int j, k, colj,rowclm;

	double divisor, s, Old, New, Delta,f1;
	if (notzero(Matrix(column,column)))
		divisor = 1.0 / Matrix(column,column);	/* compute reciprocal of pivot*/
	else
		fatalerror("Apply Constraint Transformation Division by zero--\n"," "); /* error if attempt to divide by zero */
	for (j = column + 1;j<=LastCol;j++)	/* Do for all columns */
	{
		s = divisor * Matrix(column,j);	/* compute normalized element from pivot row */
		if (s != 0.0)	/* only need to do it for nonzero element */
		{
			for (k = column + 1;k<=LastNonzero;k++)/*do for each col */
			{
				Old = Matrix(k,j);/*subtract element from row */
				Delta = s * Matrix(k,column);
				New = Old - Delta;
				Matrix(k,j) = New;
				if (RowType(k) == Condition)
					CondModulus(j) -= 
					    /*CondModulus(j)update 
						condition Modulus -*/ 
					Delta * (Old + New);
				else
					ConstModulus(j) -= 
					    /*ConstModulus(j)update 
						constraint modulus -*/ 
					Delta * (Old + New);
			}
			/* see Lawson and Hanson for this step */
			ConstModulus(j) = RoundToZero(ConstModulus(j)
			    - sqr(Matrix(column,j)),ConstTest(j));

		}
	}
}

ApplyHouseholderTransform (column)
int column;
/*This is straight out of Lawson and Hanson, except that */
/*we check to see if s==0 and don"t transform a column */
/*if that is the case. */
{
	int j, k;
	int columnCP, columnRP, jCP;
	double divisor, s, s2, h, b, Result,f1, f2;
	double *matRow;

	columnCP = ColPtr[column];
	columnRP = RowPtr[column];
	s = 0.0;
	for (j = column;j<=LastNonzero;j++) { /* Form modulus of the pivot column */
		s2 = matrix[RowPtr[j]][columnCP];
		s += s2 * s2;
	}
	s = sqrt(s);		/* square root of the sum of the squares */
	h = matrix[columnRP][columnCP];	/* compute b (reflection coefficient) */
	if (h > 0.0) s = -s;
	h -= s;
	matrix[columnRP][columnCP] = s;	
	b = s * h;
	if (notzero(b)) {		/* ONly do it if b != 0 */
		divisor = 1.0 / b;
		/* Zero out the column and replace it with the 
					orthogonal transformation vector */
		for (j = column + 1;j<=LastCol;j++) {
			jCP = ColPtr[j];
			s = h * matrix[columnRP][jCP];
			for (k = column + 1;k<=LastNonzero;k++) {
				matRow = matrix[RowPtr[k]];
				s += matRow[columnCP] * matRow[jCP];
			}
			s = s * divisor;
			if (s != 0.0) { /*no need to transform the column if s==0*/
				Result = matrix[columnRP][jCP] += s * h;
				f1 = condmodulus[jCP] - sqr(Result);
				f2 = condtest[jCP];
				condmodulus[jCP] = RoundToZero(f1,f2);
				for (k = column + 1;k<=LastNonzero;k++) {
					matRow = matrix[RowPtr[k]];
					matRow[jCP] += s * matRow[columnCP];
				}
			}
		}
	}
}

ApplyTransformation (ThisCol)	/* Apply transformation to each column */
int ThisCol;
{
	SortPivotColumn(ThisCol);/* First sort the column to get the pivot */
	if (RowType(ThisCol) == Constraint)	/* if pivot is constrained */
		ApplyConstraintTransform(ThisCol);/* apply constraint transform */
	else			/* otherwise appy HouseHolder transform */
		ApplyHouseholderTransform(ThisCol);
}

CalculateModuli() /* Modulus of each column */
{
	double ConstSum, CondSum;
	int i, j;

	for (j = CurrentRow;j<LastCol;j++) /* do for each column */
	{
		ConstSum = 0.0; /*separate moduli for conditions and constraints */
		CondSum = 0.0;
		/* sum over all rows starting at current row */
		for (i = CurrentRow;i<=LastRow;i++) {
			if (RowType(i) == Constraint)/* add approriate term to sum */
				ConstSum += sqr(Matrix(i,j));
			else 
				CondSum += sqr(Matrix(i,j));
		}
		ConstModulus(j) = ConstSum;
		ConstTest(j) = ConstSum * Tol;/* used to test if precision has been lost
				consttest filled */
		CondModulus(j) = CondSum;
		CondTest(j) = CondSum * Tol;/* used to test if precision has been lost
				condtest filled*/
	}
	Recalculate = 0;/* Set recalculation flag to zero */
	/*
		Provision is made to recalculate the moduli
		if precision is lost.
	*/
}

CalculateColumnSum() /* Normalized value of each column */
{
	int i, j;
	double colsum;

	for (j = 0;j<LastCol;j++) /* do for each column */
	{
		colvec[j] = 0.0;
		colsum = 0.0;
		/* sum over all rows starting at current row */
		for (i = 0;i<=LastRow;i++)
		{
		  /* add approriate term to sum */
			colsum += sqr(Matrix(i,j));
		}
				/*	fprintf(fp,"\n Colvecsum: \n");
					fprintf(fp,"[%d] =%lf\t",j,colsum);*/
		colvec[j] = sqrt(colsum);
			/*	fprintf(fp,"\t sqrt of colvecsum: ");
				fprintf(fp,"[%d] =%lf\n\n",j,colvec[j]);*/
	}
		/*fprintf(fp," Colvec:  \n");
			for (j = 0;j<LastCol;j++)
			fprintf(fp,"%lf\t",colvec[j]);
			fprintf(fp,"\n  \n");
			fprintf(fp,"Matrix before new values \n");
			for (i = 0;i<=LastCol;i++)
				{
				for (j = 0;j<=LastRow;j++)
					fprintf(fp,"%lf\t",Matrix(i,j));
				fprintf(fp,"\n");
				}*/
	for (j = 0;j<LastCol;j++) /* do for each column */
	{
		sigvec[j] = Matrix(j,j); 
		for (i = 0;i<=LastRow;i++)
		{
		  /* load new values into matrix */
			Matrix(i,j) = Matrix(i,j)/colvec[j];
		}
	}
}
	

CalculateRowSum() /* Normalized value of each row */
{
	int i, j;
	double rowsum,sqrsum;
	double mxrow,mnrow;	

	mxrow = 0.0;
	mnrow = 10000000000000000.0;
	for (i = 0;i<LastRow;i++) /* do for each row */
	{
		rowsum = 0.0;
		/* sum over all rows starting at current row */
		for (j = 0;j<LastCol;j++)
		{
		  /* add approriate term to sum */
			rowsum += sqr(Matrix(i,j));
		}
		sqrsum = sqrt(rowsum);

		mnrow = minval(mnrow,sqrsum);
		mxrow = maxval(mxrow,sqrsum);
	}
	if (mnrow != 0.0)
		fprintf(fp,"Condition = %20.10le\n\n",mxrow/mnrow);
	else 
		fprintf(fp,"mxrow = %lf,  mnrow = %lf\n\n",mxrow, mnrow);

}
	
	
	double TypeModulus (i)
	int i;
	{
		if(ColType(i)==RightHandType)
			return (double)RightHandType;
		else if(ConstModulus(i))
			return (double)Constraint;
		else
			return (double)Condition;
	}
	
	
	double NameModulus(i)
	int i;
	{
		if(ConstModulus(i) || CondModulus(i))
			return (double)ColType(i);
		else
			return 0.0;
	}
	
	double CCModulus(i)
	int i;
	{
		double x;
	
		if(x = ConstModulus(i))
			return x;
		else
			return CondModulus(i);
	}
	
	double CompareCol (i, j)/* Compare two columns to determine */
	int i,j;/* precedence>-for pivoting. */
	{
		double x;

		if(x = TypeModulus(i) - TypeModulus(j))
			return x; /* Constraint >- Condition >- _RHS */
		else if(x = NameModulus(i) - NameModulus(j))
			return x;/* Subscripted >- Globals */
		else
			return CCModulus(i) - CCModulus(j); /* largest >- smallest */
	}

	FindPivotColumn (ThisCol)
		int ThisCol;
	/*Pivoting strategy is:*/
	/*Never pivot on RHS */
	/*Constraints before condition equations*/
	/*Low-numbered subscripts after high-numbered*/
	/*Globals after subscripted variables*/
	/*Large column (measured by Sum-Of-Squares) before small */
	{
		int i, maximum;

		maximum = ThisCol;/* limits of search */
		CurrentRow = ThisCol;
		if (Recalculate)/* recalculate moduli if significance lost */
			CalculateModuli();
		for (i = ThisCol + 1;i<=LastCol;i++) /* get maximum pivot */
			if (CompareCol(maximum,i) < 0.0)
				maximum = i;
		SwitchCol(ThisCol, maximum);/* switch it into first place */
	}

double sign(dvalue)
double dvalue;
{
	if (dvalue < 0.0)
		return (-1.0);
	else
		return (1.0);
}

SolveLinearSystem()
{
	int i, k;
	double s, smallest, largest,whichsign;

	if(LastRow<LastCol-1)	/* can't solve system if fewer rows than cols */
		fatalerror(
		"Linear system has fewer rows than columns - unable to solve system\n"
		,"");
	smallest = 1.0 / Tol;
	largest = 0.0;
	for (i = 0;i<LastCol;i++) {	/* zero out parameter correction */
		Deltap(i) = 0.0; /* deltap filled */
 		if (MARQ) {
 			whichsign = sign(Matrix(i,i));
 			/* adjust matrix diagonal with lambda */
 			Matrix(i,i) = whichsign*(sqrt(sqr(Matrix(i,i)) + sqr(lambda)));
  		}
 	}
 	CalculateColumnSum();
 	CalculateRowSum();
	for (i = LastCol-1;i>=0;i--)	/* Go from last to first column */
	{
		s = Matrix(i,LastCol);	/* get RHS */
		for (k = LastCol-1;k>i;k--) /* Subtract terms already solved for */
			s -= Matrix(i,k) * Deltap(k);
		if (Matrix(i,i) == 0.0) /* Check if matrix is singular */
		{
			fatalerror("Matrix Singular!\n","");
			Deltap(i) = 0.0; /* This line can never be activated. */
		} else {
			/*if nonsingular, compute parameter correction */
			Deltap(i) = s / Matrix(i,i);/* deltap filled */
		}
		/* update smallest and largest matrix element*/
		/*smallest = minval(smallest, fabs(Matrix(i,i)));
		largest = maxval(largest, fabs(Matrix(i,i)));*/
	}
	for (i = LastCol-1;i>=0;i--)	/* Go from last to first column */
 	{
 		if (colvec[i] != 0.0)
 			Deltap(i) = Deltap(i)/colvec[i];
 	}
	/* estimate condition as ratio */
	/*fprintf(fp,"Condition = %7.1le\n\n", largest/maxval(smallest,Tol));*/
}

SumIt() 
{
	double KFAC;
	if(DOF <= 0.0)		/* can't solve if DOF <= 0/0 */
		fatalerror("The observation set is underdetermined!\n","");
	SumRho *= 2.0/DOF;	/* Sums for this iteration */
	SumPsi /= NEQN;
	SumPsiSq /= DOF;
	SumPsiP /= NEQN;
	SumPsiPSq /= NEQN;

	/* compute new sigma and scale factor */

	KFAC = 1.0 + (NEQN-DOF)/NEQN*(SumPsiPSq - sqr(SumPsiP));
	Sigma1Sq = KFAC*SumPsiSq/SumPsiP;
	Sigma1 = sqrt(Sigma1Sq);
	Sigma = Sigma1*(ScaleFac==0.0?1.0:ScaleFac);
	putenvval("sigma",Sigma);
	ScaleFac = (ScaleFac==0.0?1.0:ScaleFac)*sqrt(SumRho);
	putenvval("scale",ScaleFac);

	/* Print results of iteration */

	printiter_results(stdout,KFAC);
	printiter_results(fp,KFAC);
}

printiter_results(fp,KFAC)
FILE *fp;
double KFAC;
{
	fprintf(fp,"scale     =\t");
	printdouble(fp,ScaleFac);
	fprintf(fp,"\n");
	fprintf(fp,"SumRho    =\t");
	printdouble(fp,SumRho);
	fprintf(fp,"\n");
	fprintf(fp,"SumPsi    =\t");
	printdouble(fp,SumPsi);
	fprintf(fp,"\n");
	fprintf(fp,"SumPsiSq  =\t");
	printdouble(fp,SumPsiSq);
	fprintf(fp,"\n");
	fprintf(fp,"SumPsiP   =\t");
	printdouble(fp,SumPsiP);
	fprintf(fp,"\n");
	fprintf(fp,"SumPsiPSq =\t");
	printdouble(fp,SumPsiPSq);
       	fprintf(fp,"\n\n");
       	fprintf(fp,"NEQN = %5d, DOF = %5d\n",(int)NEQN,(int)DOF);
	fprintf(fp,"KFAC =\t ");
	printdouble(fp,KFAC);
	fprintf(fp,"\n");
	fprintf(fp,"Sigma =\t ");
	printdouble(fp,Sigma);
	fprintf(fp,"\n\n");
}

CovarianceMtx()
{
	int i,j,k,width,begin,inc;
	int indsize, mxprnam, colwid;
	double s;

	if(getenvval("minsum"))	{ /* see if covariance matrix can be provided */
			printf("Sorry, can't get covariance matrix when doing minsum\n");
			return 0;		/*Can't get cov mtx when doing minsum */
		}
	if(getenvval("irls")) {
		if ((getenvval("fair")) || (getenvval("trim")) || 
			(getenvval("tukey")) || (getenvval("huber")) ||
			(getenvval("minsum")))
		{
			printf("Sorry, covariance matrix not implemented for robust estimation using IRLS\n");
			return 0;		/* Can't get covariance mtx when doing IRLS */
		}
	}

	printf("Calculating Covariance Matrix\n\n\n");
	for(k=LastCol-1; k>=0; k--)	{ /* Do for each column */
		for (i=0; i<LastCol; i++)	/* zero out the vector */
			vector[i] = 0.0; /*vector filled */
		for (i=k; i>=0;i--)	{ /* compute inverse of upper triang. matrix */
			s = deltafn(i,k)*deltafn(RowType(i),Condition);
			/* Handle covariance of condition eqn */
			/* rhs vector is zero for a constraint,
								 1 on diagonal for a condition */
			for (j=k; j>i; j--)
				s -= Matrix(i,j) * vector[j];
			if (Matrix(i,i) == 0.0) { /* Back substitute the column */
				fprintf(fp,"Matrix Singular!\n");
				vector[i] = 0.0; /* vector filled */
			} else		/* recompute column of inverse matrix */
				vector[i] = s / Matrix(i,i); /* vector filled */
		}

		for(i=0;i<LastCol;i++) { /*put inverse as a row in lower triangle */
			Matrix(k,i) = vector[i];	/* R Transpose to Matrix */
		}
	}

	for(k=LastCol-1; k>=0; k--)	/* multiply matrix by it's transpose */
		for(i=0; i<=k; i++) {
			s = 0.0;
			for(j=k; j<LastCol; j++)
				s += Matrix(j,i) *Matrix(j,k);
			Matrix(i,k) = s*Sigma1Sq; /* times variance of unit weight */
		}

	/* print correlation matrix if cleared */

	indsize = getindexsz();
	mxprnam = getmaxprmname();

	if (indsize > 8) lbl = 1;

	if (indsize == 0) {
			indsize = mxprnam;
			noindex = 1;
		}

	if (lbl) colwid = 8;
	else colwid = indsize;

	if (lbl) width = WIDTH-(mxprnam+8)/colwid+2;	
	else  width = WIDTH-(mxprnam+indsize)/colwid+2;	

	begin = 0;
	inc = width;
	i = 1;
	prSigma(indsize,mxprnam);
	while (inc <LastCol) {
		PrintCorrelationMatrix(begin,inc,indsize,colwid,mxprnam);
		i++;
		begin = inc;
		inc = i* width;
	}
	PrintCorrelationMatrix(begin,LastCol,indsize,colwid,mxprnam);
}	

PrintCorrelationMatrix(start,end,indsize,colwid,mxprnam)
int start, end,indsize,mxprnam, colwid;
{
	int i,j,ndim;
	double	div;

	ndim = getdimnum();
	fprintf(fp,"\n");
	fprintf(fp, "Correlation Matrix * 100\n");
	fprintf(fp,"\n");

	if (noindex)
		fprintf(fp,"%*s  ",mxprnam," ");
	else
		fprintf(fp,"%*s ",mxprnam+indsize," ");

	for(i=start;i<end;i++)	
		fprintf(fp,"%*s  ", colwid,VarName(i));
	fprintf(fp,"\n");

	if ((!noindex)  && (!lbl)){
		fprintf(fp,"%*s ",indsize+mxprnam," ");
		for(i=start;i<end;i++)	{
			if(Number(i,4) != 0) {
				prIndex(fp,NumberX(i),indsize);
				fprintf(fp, "  ");
			} else {
				fprintf(fp,"%*s", indsize," ");
				fprintf(fp, "  ");
			}
		}
		fprintf(fp,"\n");
	}

	if ((!noindex)  && (lbl)) {
		for (j = 0; j<ndim; j++) {
			fprintf(fp,"%*s ",indsize+mxprnam," ");
			for(i=start;i<end;i++)	{
				if(Number(i,4) != 0) {
					prSIndex(fp,NumberX(i),colwid,j);
					fprintf(fp, "  ");
				} 
				else{
					fprintf(fp,"%*s", colwid,"");
					fprintf(fp, "  ");
				}
			}
			fprintf(fp,"\n");
		}
	}
	fprintf(fp,"\n");

	for(i=0;i<LastCol;i++) {
		if ((Number(i,4) == 0) && (noindex==1)) {
			fprintf(fp,"%*s  ", mxprnam,VarName(i));
			}
		else if ((Number(i,4) == 0) && (noindex==0)){
			fprintf(fp,"%*s", mxprnam,VarName(i));
			fprintf(fp,"%*s", indsize," ");
			fprintf(fp," ");
			}
		else {
			fprintf(fp,"%*s",mxprnam,VarName(i));
			prIndex(fp,NumberX(i),-indsize);
			fprintf(fp," ");
		}
		for(j=start;j<end;j++) {
	 		div =  (sqrt(Matrix(i,i)* Matrix(j,j)));
			if (i >j)
				fprintf(fp,"%*s  ", colwid,"");
			else
			{
	 			if (notzero(div))
		 			fprintf(fp,"%*d  ",colwid,(int)(100* (Matrix(i,j)/div)));
				else
					fprintf(fp,"%*s  ",colwid,"0");
			}
		}
		fprintf(fp,"\n");
	}
	fprintf(fp,"\n");
}


prSigma(indsize,mxprnam)
int indsize, mxprnam;
{
    int i,j;
	fprintf(fp,"\n");
	fprintf(fp, "Sigma Values \n");
	fprintf(fp,"\n");
	for(i=0;i<LastCol;i++) {
		if ((Number(i,4) == 0) &&(noindex))
			fprintf(fp,"\tSigma %*s   ", mxprnam,VarName(i));
		else if ((Number(i,4) == 0) &&(!noindex))
			fprintf(fp,"\tSigma %-*s", mxprnam+indsize,VarName(i));
		else {
			fprintf(fp,"\tSigma %*s",mxprnam,VarName(i));
			prIndex(fp,NumberX(i),-indsize);
			printf("   ");
		}
		fprintf(fp,"%20.15le\n",(sqrt(Matrix(i,i)))*(1.0/colvec[i]));
	}
}

prIndex(fd,indx,indsize)
FILE *fd;
int indx[5],indsize;
{
	int i;
	char string[100];
	char tiny[10];

	if (indx[4] < 1 || indx[4] > 4) {
		fprintf(stderr, "\nprintIndex: Number of dimensions = %d\n",indx[4]);
		exit(-1);
	}
	strcpy(string,"[");
	for (i=0; i<indx[4]; ++i) {
		if (i!=0) strcat(string, ",");
		sprintf(tiny, "%d",indx[indx[4]-1-i]);
		strcat(string,tiny);
	}
	strcat(string, "]");
	fprintf(fd,"%*s",indsize,string);
}


prSIndex(fd,indx,indsize,level)
FILE *fd;
int indx[5],indsize,level;
{
	int i;
	char tiny[10];

	if (indx[4] < 1 || indx[4] > 4) {
		fprintf(stderr, "\nprintIndex: Number of dimensions = %d\n",indx[4]);
		exit(-1);
	}
	sprintf(tiny, "[%d]",indx[indx[4]-1-level]);
	fprintf(fd,"%*s",indsize,tiny);
}

printdouble(fp,dvalue)
double dvalue;
FILE *fp;
{
	double floor();
	double truncnum,diffround,tnum;
	int i, j;

	if ((int)dvalue == -32746)
		fprintf(fp,"#N/A\t");
	else
	{
		if ((fabs(dvalue) < 1.0E-10) || (fabs(dvalue) > 1.0E+9))
			fprintf(fp,"% 17.15le\t",dvalue);
		else
		{
			i = 1;
			diffround = 1.0;
			while ((diffround > 1.0E-16) && (i < 16))
			{
				tnum = pow(10.0,(double)i);
				truncnum = (floor(tnum *dvalue))/tnum;
				diffround = dvalue - truncnum;
				i++;
			}
	
			j = 1;
			while (dvalue > pow(10.0,(double)j))
				j++;
	
			j = j + i;
			i--;
		
			fprintf(fp,"% *.*lf\t",j,i,dvalue);
		}
	}
}

getindexsz()
{
	int indxsiz,i,j,k;
	int imax[4];
	static int sumsize = -1;

	if (sumsize == -1)
	{
		sumsize = 0;
		for (i=0;i<4; i++)
			imax[i] = -1;
		for (i=0;i<LastCol;i++)
			for(j=0;j<Number(i,4);j++)
				if (Number(i,j) > imax[j]) imax[j] = Number(i,j);
		/* find size of indices */
		for (i=0; i<4;i++)
		{
			if (imax[i] > -1)
			{
				k = 1;
				while (imax[i] > (pow(10.0, (double)k))) k++;
				sumsize = sumsize + k;
			}
				
		}
		/* add space for commas*/
		for (i=0;i<4;i++)
			if (imax[i] > -1)
				sumsize++;
		if (sumsize > 0) sumsize ++;
	}		
	return (sumsize);
}
 	 
getmaxprmname()
{
	int length,i;
	static int maxp = -1;

	if (maxp == -1)
	{
		maxp = 0;
		for (i = 0; i< LastCol; i++)
			if (maxp < (length = strlen(varname[i])))
				maxp =length;
	}
	return maxp;
}

getdimnum()
{
	int i;
	static int maxd = -1;

	if (maxd == -1)
	{
		maxd = 0;
		for (i = 0; i< LastCol; i++)
			if (maxd < (Number(i,4))) maxd  = Number(i,4);
	}
	return maxd;
}


Solve1()		/* Solve matrix as it exists so far */
{
	int ThisCol;
	int i,j;
	int EndAt;


	if(!Touched)
		return;		/* Matrix has already been transformed */
	FirstPivots = 0;/* we now always start at zero */
	Recalculate = 1;	/* recalculate moduli after solution */
	/*fprintf(fp,"\nTransformation Number ");*/
	EndAt = minval(LastRow,LastCol); /* last row to transform */
	for (ThisCol = StartAt;ThisCol<=EndAt;ThisCol++) /* transform each column */
	{
		/*fprintf(fp,", %3d", ThisCol);*/
		if ((ThisCol % 20) == 0) fprintf(fp,"\n");
		FindPivotColumn(ThisCol); /* Find pivot columns and */
		ApplyTransformation(ThisCol);/* apply appropriate transformation */
	}
	for(i=1;i<=EndAt;i++)		/* zero out the lower triangle */
		for(j=0;j<i;j++)
			Matrix(i,j) = 0.0;
	LastRow = EndAt;		/* reset rows/cols that have been */
	StartAt = LastCol;		/* transformed 			*/
	PrintMatrix();			/* Print matrix if desired*/
	Touched = 0;			/* Matrix now pristine*/
}

SolveMatrix()			/* Use chosen algorithm to solve matrix */
{
	int kode,iter;
	double error;
	int iz[5];


	iz[0] = iz[1] = iz[2] = iz[3] = iz[4] = 0;
	printf("Solving Matrix...\n\n");
	if(getenvval("minsum"))	{	/* Do minsum method? */
		int i,j,k,pivtype;

		/* compute scale factor */
		ScaleFac = 1.48*MAD(resdum,numresiduals)
			*(ScaleFac==0.0?1.0:ScaleFac);
		putenvval("sigma",ScaleFac); /* insert into environment file */
		putenvval("scale",ScaleFac);

		printf("ScaleFac = ");
		printdouble(stdout,ScaleFac);
		printf("Sigma    = ");
		printdouble(stdout,ScaleFac,(int)DOF);
		printf(", DOF = %d\n\n",(int)DOF);

		fprintf(fp,"ScaleFac = ");
		printdouble(fp,ScaleFac);
		fprintf(fp,"Sigma    = ");
		printdouble(fp,ScaleFac,(int)DOF);
		fprintf(fp,", DOF = %d\n\n",(int)DOF);
		/* Set up for minsum algorithm */
		kode = 0;
		iter = 10*LastRow;
		i = getcolumn(RightHandType,"_RHS",iz); /* move RHS to last col*/
		SwitchCol(i,LastCol);

		i = 0;
		j = LastRow;		/* move conditions before constraints */
		pivtype = RowType(j);
		do {
			for(;i<j && RowType(i) <= pivtype;)
				i = i+1;
			for(;j>i && RowType(j) >= pivtype;)
				j = j-1;
			if(i<j) {
				k = rowptr(i);
				RowPtr[i] = rowptr(j);
				RowPtr[j] = k;
			}
		} while (i<j);
		k = rowptr(LastRow);
		RowPtr[LastRow] = rowptr(i);
		RowPtr[i] = k;

		PrintMatrix();
		if ((matsize.nrows < LastRow +3) || /*minsum needs 2 extra rows*/
		(matsize.ncols < LastCol+2))/* minsum needs 1 extra column */
		{
			if (matsize.ncols < LastCol+2) {
				matsize.lastcol = matsize.ncols;
				matsize.ncols = matsize.ncols +1;
				colspace();
			}
			if (matsize.nrows < LastRow +3) {
				matsize.lastrow = matsize.nrows;
				matsize.nrows = matsize.nrows +2;
				rowspace();
			}
			matspace();
		}
		cl1(matsize.nrows,matsize.ncols,LastRow+1-Constraints,Constraints,0,LastCol,
		matrix,deltap,resdum,RowPtr,ColPtr,
		&kode,1.0e-10,&error,&iter);/* call minsum routine */
		/* Print results */
		fprintf(fp,"Error = %lf\n",error);
		fprintf(fp,"Kode= %d\n",kode);
		fprintf(fp,"Iters = %d\n\n",iter);
		fprintf(fp,"\n");
	} else {
		Solve1();		/* Solve matrix */
		SolveLinearSystem();	/* back substitute for parameters */
	}
}

triangularize1()	/* Triangularize matrix */
{
	if(!getenvval("minsum")) { /* Can't triangularize if doing minsum */
		printf("\nTriangularizing Matrix...\n\n");
		Solve1();	/* Call triangularize routine */
	}
}

triangularize()		/* Externally callable triangularize routine */
{
	triangularize1();
	pushtrue();
}

insertresidual(x)
	double x;
{
	if (!getenvval("minsum"))
		return;
	if (numresiduals >= matsize.nrows)
	{
		matsize.lastrow = matsize.nrows;
		matsize.nrows = numresiduals + 10;/*numresiduals*maxval(1.0,0.2*numresiduals);*/
		rowspace();
		matspace(); 
	}
	resdum[numresiduals]= x;
	numresiduals++;
}

dump(s)
	char *s;
{
	int i, j,k,l;
	int collimit, rowlimit;

	if(DontDump)
		return;
	if(CountErrors>100)
		return;
	CountErrors++;
	fprintf(fp,s);
	fprintf(fp,", Entry # %d\n",CountErrors);
	collimit = matsize.ncols;
	rowlimit = minval(LastRow,30);
	fprintf(fp,"Matrix\n");
	for (j = 0; j <collimit; j++)
		fprintf(fp,"%3d ",j);
	fprintf(fp,"\n");
	for (i = 0; i <rowlimit; i++)
	{
		fprintf(fp,"%7d",i);
		for (j = 0; j <collimit; j++)
		{
			fprintf(fp,"%7.3f",Matrix(i,j));
		}
		fprintf (fp,"\n");
	}
	fprintf (fp,"\n");

	fprintf(fp,"RowType\n");
	for (i = 0; i <rowlimit; i++)
	fprintf(fp,"%7d \n",RowType(i));
	fprintf(fp,"indexarray\n");
	for (i = 0; i <rowlimit; i++)
	fprintf(fp,"%7d \n",indexarray[i]);
	fprintf(fp,"RowPtr\n");
	for (i=0;i<rowlimit;i++)
	fprintf(fp,"%7d \n",RowPtr[i]);
	fprintf(fp,"ColPtr\n");
	for (i=0;i<collimit;i++)
	fprintf(fp,"%7d \n",ColPtr[i]);
	fprintf(fp,"ColType\n");
	for (j = 0; j <collimit; j++)
	 fprintf(fp,"%7d \n",ColType(j));
	fprintf(fp,"Number\n");
	for (j = 0; j <collimit; j++)
	 fprintf(fp,"%7d %7d %7d %7d     %7d\n",Number(j,0),Number(j,1),Number(j,2),
		Number(j,3),Number(j,4));
	fprintf(fp,"ConstModulus\n");
	for (j = 0; j <collimit; j++)
	 fprintf(fp,"%7.3f \n",ConstModulus(j));
	fprintf(fp,"CondTest\n");
	for (j = 0; j <collimit; j++)
	 fprintf(fp,"%7.3f \n",CondTest(j));
	fprintf(fp,"CondModulus\n");
	for (j = 0; j <collimit; j++)
	 fprintf(fp,"%7.3f \n",CondModulus(j));
	fprintf(fp,"ConstTest\n");
	for (j = 0; j <collimit; j++)
	 fprintf(fp,"%7.3f \n",ConstTest(j));
	fprintf(fp,"Deltap\n");
	for (j = 0; j <collimit; j++)
	 fprintf(fp,"%7.3f \n",Deltap(j));
	fprintf(fp,"vector\n");
	for (j = 0; j <collimit; j++)
	 fprintf(fp,"%7.3f \n",vector[j]);
	fprintf(fp,"VarName\n");
	for (j = 0; j <collimit; j++)
	 fprintf(fp,"%7s \n",VarName(j));

}
	
	
dump8() {
	int inc;
	inc = 0;
	fprintf(fp,"dump8\n");
	while (inc < LastCol+20) {
		d8(inc,inc+20);
		inc = inc + 20;
	}
	d8(inc,LastCol);
}
	
d8(FCol,LCol)
	int LCol,FCol;
{
	int i, j;
	for (j=0;j<8;j++) {
		fprintf (fp,"%d",j);
		for (i=FCol;i<LCol;i++)
				fprintf(fp,"%4d ",(int)(1000*Matrix(i,j)));
		fprintf(fp,"\n");
	}
	fprintf(fp,"\n");
}



printenvtoRes(fp)
FILE *fp;
{
#ifndef VMS
	FITS *fitpnt;
	fitpnt = getenvtabpntr();
	fprintf(fp,"Contents of the Environment File for this Run: \n\n");
	if (fitpnt)
		fitswrite(fp, fitpnt);
#endif
}

printdate(fp)
FILE *fp;
{
	long ltime;

	time(&ltime);
	fprintf(fp,"\nTime of GaussFit Run:  %s\n",ctime(&ltime));

}

Get_LastCol() { return LastCol; }
 
double Get_Sigma() { return Sigma; }
