/*
	GAUSS - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/*  Interface between interpreter and table I/O routines */

#define  import_spp
#define  import_libc
#define  import_stdio
#include <iraf.h>

#include "defines.h"
#include "sfiles2.h"
/*#include "com.h"*/
#include "datum.h"
#include "strings.h"
#include "machine.h"

#define MAXDATASETS 100         /* maximum number of data files */
#define MAXPARAMFILES 1         /* maximum number of parameter files */
#define MAXFINDEX 100           /* maximum number of fast index directories */
#define MAXPINDEX 100           /* maximum number of fast index directories */
            
extern FILE *fp;
extern SYMBOL *symtable;
extern DECLARATION declarestack[];

static int datatable = 0;       /* pointer to data table (none initially)*/
static int datarow = -1;        /* pointer to data row (none initially)*/
static int datatableid = -1;    /* pointer to data table I.D. (none initially)*/
static int datafnum = 0;        /* placement in data file list */              
static int drows =0;            /* number of rows in data file */              

static int xpartable[10] = {0,0,0,0,0,0,0,0,0,0 };  /* pointer to par table*/
static int xparrow = -1;        /* pointer to data row (none initially)*/
static int pfn = 0;             /* number of paramter files */

static int nfindices = 0;       /* number of fast index tables */
static FINDEX fsdx[100];        /* fastindices to parameters(from all p files */

static int (*Qcompare)();       /* compare routine for quicksort */ 
static int (*Qexchange)();  	/* exchange routine for quicksort */
static int *Frow,*Fxvars,Ffile; /* globals passed to qwiksort */ 
int FCompare(), FExchange();

saveenv()     /* close any open environment table */
{
}

openenvtable(s)   /* open environment table named s */
char *s;
{       
       	parse_dlist("datalist",DATA_TABLE);   
	parse_dlist("paramlist",PARAM_TABLE);                
}

char *getenvstr(s) /* get string corresponding to keyword s from environment*/
char *s;
{                                              
	/*char *stptr;

	stptr = getfitsstr(s);
	printf(" getfittsstr %s = %s\n\n",s, stptr);*/
	return getfitsstr(s);
}

double getenvval(s) /* get float(double) for keyword s from environment*/
char *s;
{
	double val;                    
	char *cstr, *getenvstr();

       	if (strcmp("trim",s) == 0)
	       	return 0.0;
	if ((strcmp("fair",s)== 0) ||   (strcmp("tukey",s) == 0) ||
		 (strcmp("huber",s) == 0) ||  (strcmp("minsum",s) == 0))
	{
		cstr = getenvstr("method"); 
		if (strcmp("minsum",cstr) == 0)
			return 1.0;
		if (strcmp(s,cstr) == 0)
			return (getdblval("are"));
		else
	                return 0.0;  
	}
        val =   getdblval(s);
       /*	printf("getenvval %s = %lf\n\n",s,val);  */
	return val; /* find value in environment */
}

putenvval(name,value) /* put value into keyword name in environment */
char *name;
double value;
{      
	if (strcmp("sigma",name) == 0)
		return;
	putdbleval(name,value);  /* put value into table */
}

getenvint(s)
char *s;
{       
	int intval;
	intval = getintval(s);
       /*	printf("getenvint %s = %d \n\n",s,intval);*/
	return intval;
}                                   

putenvint(name,value) /* put value into keyword name in environment */
char *name;
int value;
{
	putintval(name,value);  /* put value into table */
}

datafileopen()
{                                 
	if(datatable)
	{
		datatable = midasclose(DATA_TABLE);
		if(getenvint("triang"))
			triangularize1();
	}
	datarow = -1;                
	if (datafnum < getdatafilenum())
	{
			datatable = midasopen(DATA_TABLE,
				getdatafilename(datafnum),drows);
			drows = datatable;
		        datafnum++; 
                       	return 1;
	}   
	datafnum = 0; /*done reset datafnum to 0 */
	return 0;
}


getitemnumber()
{       
	return ++datarow < drows;
}

import(code)        
{
	DATUMPTR k;                
	doexport();
	if(!datatable)
		if(!datafileopen())              
		{
	   	  fatalerror("No data file name specifications found.\n"," ");
		  return 0;	/* Couldn't open a datafile at all. */
		}
	while(!getitemnumber())
	{
		if(!datafileopen())
		{
			pushfalse();	/* Last Datafile */
			unsave();
			return 0;
		}
	}
	pushtrue();
	unsave();
	return 1;
}


double getdataval(s)
char *s;
{
	if(datatable)
		return getmidasval(DATA_TABLE,s,datarow+1,0);
	fatalerror("No MIDAS table %s has been opened\n",s);
}
                                                               
putresidual(name,val)
char *name;
double val;
{
	char resname[64];
	
	strcpy(resname,"_");
	strcat(resname,name);
	putdataval(resname,val);
}

double getresidual(name)
char *name;
{
	char resname[64];
	
	strcpy(resname,"_");
	strcat(resname,name);
	return getdataval(resname);
}

double gettheobs(corename)
char *corename;
{
	return getdataval(corename) + getresidual(corename);
}

getdataint(s)
char *s;
{
	return (int)getdataval(s);
}


putdataval(s,val)
char *s;
double val;
{
	if(datatable)
	{
		putmidasval(DATA_TABLE,s,datarow+1,val,0);
		return;
	}
	fatalerror("No MIDAS table %s has been opened\n",s);
}


savexpar()
{
	if(xpartable[0] != 0)
		xpartable[0] = midasclose(PARAM_TABLE);
}






saveparams()
{
	savexpar();
}




xparfileopen()  /* open all  parameter files  */
{
	char *theName;
	char str[10];
	int i,j,k,m;
        int limit;

	k = 0;
	initfindex();  /* initialize fast index col names */
	/* open all parameter files */
	limit = getparamfilenum();
	for (pfn = 0; pfn < limit; pfn++) { /* get next parameter table id */
	       	xparopen(pfn);
	}
	for (i = 0; i < pfn; i++) { 
		k = getnumcols(PARAM_TABLE,i);
		for (j=0;j < k;  j++) {
#ifdef DEBUG4D
fprintf(stderr, "param file column : \"%s\"\n",getcolname(j,PARAM_TABLE,i));
#endif
			m = findsymbol(getcolname(j,PARAM_TABLE,i));
			symtable[m].filenum = i;
			symtable[m].colnum = j;
		}
	}
	if (pfn != 0)
		return 1; /* success */
	else
		fatalerror("No Parameter Files Specified.\nUse keyword * params * or param# * to specify file name in the environment file\n","");
}


xparopen(num)                 
int num;
{
	char *theName;    
	int nrows;

	theName = getparamfilename(num); /* is such a keyword in the env? */
	xpartable[num] =  midasopen(PARAM_TABLE,theName,nrows);
	printf("\nParameter file read:  %s\n\n",theName);
}


int
BCompare(a, indx)		/* compare multi-dimension indices for binsearch */
	int a, indx[5];
{
	int i, ndims, xvar, vala, valb;
	int row, col;

	ndims = Fxvars[4];
	row = Frow[a];
/*
#ifdef DEBUG4D
	fprintf(stderr, "BCompare: [%d,%d,%d,%d](%d) [",
		indx[0],indx[1],indx[2],indx[3],indx[4]);
	for (i=0; i<ndims; ++i) {
		if (i!=0) fprintf(stderr, ",");
		xvar = Fxvars[i];
		col = symtable[xvar].colnum;
		vala = getmidasrcval(PARAM_TABLE,col,row+1,Ffile);  
		fprintf(stderr, "%d", vala);
	}
	fprintf(stderr, "]\n");
#endif
*/
	for (i=0; i<ndims; ++i) {
		xvar = Fxvars[i];
		col = symtable[xvar].colnum;
		vala = getmidasrcval(PARAM_TABLE,col,row+1,Ffile);
		valb = indx[i];
		if (vala < valb) return -1;
		else if (vala > valb) return 1;
	}
	return 0;
}




searchmidas(file,s,thisindx,indx) /*search parameter "s" for indx */ 
int file,thisindx,s;
int indx[5];
{
	/* convert to binary search format for quicksort */
	int rownum,insz,i;

	/* look first in the same file for the indx */
	Fxvars = symtable[s].x.xvars;
	Ffile = file;
	Frow = fsdx[thisindx].row;
	rownum = BinSearch(indx);


	/* NOT ACTIVE   look second in other files for the indx 
	i = 0;
	while ((rownum == -1) && (i < pfn))
	{
		if (i == file) i++;
		if (i < pfn)
		{
			
			Ffile = file;
			rownum = BinSearch(indx);
		}
		i++;
	}*/

	/* if index cannot be found */
	if (rownum == -1) {
		insz = getindexsz();
		fprintf(stderr, "Indexed parameter/datum \"%s",getnam(s));
		prIndex(stderr, indx,insz);
		fprintf(stderr, "\" not found in file.\n");
		if (fp != NULL) {
			fprintf(fp, "Indexed parameter/datum \"%s",getnam(s));
			prIndex(fp, indx,insz);
			fprintf(fp, "\" not found in file.\n");
		}
		fatalerror("","");
	}
/*#ifdef DEBUG4D
	fprintf(stderr, 
		"searchmidas: %d %s indx[%d,%d,%d,%d](%d) found: [",
		s, getnam(s),
		indx[0],indx[1],indx[2],indx[3],indx[4]);
	{ int i;
		for (i=0; i<indx[4]; ++i) {
			if (i!=0) fprintf(stderr, ",");
			fprintf(stderr, "%d",
			(int)xpartable[file]->value[Frow[rownum]][symtable[Fxvars[i]].colnum]);
		}
		fprintf(stderr, "]\n");
	}
#endif */
	return Frow[rownum];
}


BinSearch(indx)
	int indx[5];
{
	int low, high, mid, cmp;
	int here = -1;

	low = 0;
	high = xpartable[Ffile] - 1;

	while (low <= high) {
		mid = (low + high)/2;
		if ((cmp = BCompare(mid, indx)) > 0) 
			high = mid -1;
		else if (cmp < 0)
			low = mid + 1;
		else {
			here = mid;
			break;
		}
	}
	return here;
}




initfindex()
{
	int num,i,j;

	for (i=0;i<MAXFINDEX;i++)
	{		
		fsdx[i].name = -1;
		fsdx[i].row = NULL;
	}
		
}


putxparval(name,indx,val) /* put item into parameter table */
int name;
int indx[5];
double val;
{
	int therow,thefile;
	int thisindx;
	int *xvars;

	if(xpartable[0] == 0)  /* make sure table is open */
		xparfileopen();

	/* get which file has parameter in it */
	thefile = getxparnum(name);

	xvars = symtable[name].x.xvars;
	thisindx = fastindex(thefile,name,xvars);

	/* look for index on column name */
	therow = searchmidas(thefile,name,thisindx,indx);

	/* replace value in this row in column "s" */	
	putmidasval(PARAM_TABLE,getnam(name),therow+1,val,thefile);
#ifdef DEBUG4D
	fprintf(stderr, 
		"putxparval: %d %s  (%d)[%d,%d,%d,%d] (%d)[%d,%d,%d,%d] %f\n",
		name, getnam(name), xvars[4],xvars[0],xvars[1],xvars[2],xvars[3],
		indx[4],indx[0],indx[1],indx[2],indx[3], val);
#endif
	return;
}



double getxparval(name,indx) /* get item from paramter table */
int name; /* pointer to parameter */
int indx[5]; /* index values */
{
	int therow,thefile; 
	int thisindx;
	double dnum;
	int *xvars;

	if (xpartable[0] == 0) /* make sure table is open */
		xparfileopen();
	/* get whixparvalch file has parameter in it */
	thefile = getxparnum(name);

	xvars = symtable[name].x.xvars;
	thisindx = fastindex(thefile,name,xvars);

	/* look for index in column name */
	therow = searchmidas(thefile,name,thisindx,indx);
	/* get value in this row from column "s" */
	dnum = getmidasval(PARAM_TABLE,getnam(name),therow+1,thefile);
	dnum = (dnum == UND) ? 0.0 : dnum;
#ifdef DEBUG4D
	fprintf(stderr, 
		"getxparval: %d %s  (%d)[%d,%d,%d,%d] (%d)[%d,%d,%d,%d] %f\n",
		name, getnam(name), xvars[4],xvars[0],xvars[1],xvars[2],xvars[3],
		indx[4],indx[0],indx[1],indx[2],indx[3], dnum);
#endif
	return dnum;
}

fastindex(thefile,name,xvars)  /* Has fast index been created ? */
int thefile;
int xvars[5];
int name;
{
	int thisindx;
	if ((thisindx = symtable[name].findex) == -1) {
		thisindx = symtable[name].findex = makeindex(thefile,name,xvars);
	}
	return thisindx;
}


makeindex(file, name, xvars) /* fast index column s of xpartable */
int file, name, xvars[5];
{
	int i,j,k, m,n;
	char *MemAlloc();

	/* load column name to be fast-indexed in */

	i = nfindices ++ ;
	
	/* allocate space for index */
	fsdx[i].name = name;
	fsdx[i].row = (int*)MemAlloc("fastsearch",(long)xpartable[file]*sizeof(int));	
	for (n=0; n<xpartable[file];n++)
		fsdx[i].row[n] = n;

	Qcompare = FCompare;
	Qexchange = FExchange;
	Frow = fsdx[i].row;
	Fxvars = symtable[name].x.xvars;
	Ffile = file;

	qwiksort(0, xpartable[file]-1);
/*#ifdef DEBUG4D
	fprintf(stderr, "%s:\n", getnam(name));
	for (j=0; j<xpartable[file]; ++j) {
		int xvar, col, vala, ndims;
		fprintf(stderr, "%d [", j);
		ndims = Fxvars[4];
		for (k=0; k<ndims; ++k) {
			xvar = Fxvars[k];
			col = symtable[xvar].colnum;
			vala = getmidasrcval(PARAM_TABLE,col,Frow[j]+1,Ffile); 
			fprintf(stderr, ",%d",vala);
		}
		fprintf(stderr, "]\n");
	}
#endif */
	return i;
}

int
FExchange(a,b)		/* exchange two elements in findex */
	register int a, b;
{
	register int temp;
	temp = Frow[a];
	Frow[a] = Frow[b];
	Frow[b] = temp;
}

int
FCompare(a, b)		/* compare multi-dimension indices for sort */
	int a, b;
{
	int i, ndims, xvar, vala, valb;
	int col;

	ndims = Fxvars[4];
	for (i=0; i<ndims; ++i) {
		xvar = Fxvars[i];
		col = symtable[xvar].colnum;
		vala = getmidasrcval(PARAM_TABLE,col,Frow[a]+1,Ffile);
		valb = getmidasrcval(PARAM_TABLE,col,Frow[b]+1,Ffile);
		if (vala < valb) return -1;
		else if (vala > valb) return 1;
	}
	return 0;
}

qwiksort(left,right)  /* an adaptation of Hoare's quick sort */
int left, right;
{
	register int i,j,mid;
	i = left; 
	j = right;
	mid = (left + right)/2;

	do {
		while (i < right && (*Qcompare)(i,mid) < 0) i++;
		while (j > left  && (*Qcompare)(j,mid) > 0) j--;
		if (i <= j) {
			if (i == mid) mid = j;
			else if (j == mid) mid = i;
			(*Qexchange)(i++,j--);
		}
	} while (i <= j);

	if (left  < j) qwiksort(left,j);
	if (i < right) qwiksort(i,right);
}
		

double getparamval(name) /* get a value of a parameter (global) */
int   name;

{
	int thefile;
	double dnum;

	if(xpartable[0] == 0)  /* make sure table is open */
		xparfileopen();

	/* get which file has parameter in it */
	thefile = getxparnum(name);

	/* find in column anme, row 0 */
	dnum = getmidasval(PARAM_TABLE,getnam(name),0+1,thefile);
	if (dnum == UND) {	/*	return 0.0;*/
		fatalerror2c("Parameter value %s does not exist in paramfile %s.\n",
			getnam(name),getparamfilename(thefile));
	}
	return dnum;
}
   


putparamval(name,value)/* put a value of a global paramter into table */
int name;
double value;
{
	int thefile;

	if(xpartable[0] == 0) /* make sure table is open */
		xparfileopen();

	/* get which file has parameter in it */
	thefile = getxparnum(name);

	/* put into column name, row 0 */
	putmidasval(PARAM_TABLE,getnam(name),0+1,value,thefile); 
}



getxparnum(name)
int name;
{
	if (symtable[name].filenum == -1)
		fatalerror("Parameter value %s does not exist in any parameter file.\n",
					getnam(name));
	return  symtable[name].filenum;
}
#ifndef VMS  
FITS *getenvtabpntr()
{
	if(envtable)
		return(envtable);
}
#endif
