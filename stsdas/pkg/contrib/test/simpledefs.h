/*

	GaussFit - A System for Least Squares and Robust Estimation
	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/



/* typedef definitions of FITS files for main  */


#define FITSROWS 100  /* max number of rows in FITS table */
#define KEYLEN 9      /* max length of a key word */
#define FSTRLEN 23    /* max length of a string to be rad in */

enum
{
	LOGICAL,      /* types of data */
	REAL,
	INTEGER,
	STRING
};



typedef union object  /* data that the FITS file can have */
{
	int ival;
	double dval;
	char sval[FSTRLEN];
	int lval;
} 
OBJECT;


typedef struct entre  /* Entry in FITS file */
{
	char name[KEYLEN];      /* name of entry */
	OBJECT value;           /* value of entry */
	int type;               /* type of entry */
} 
ENTRE;

typedef struct fits   /* FITS file header */
{
	int nrows;              /* number of rows in table */
	char filename[64];      /* name of file */
	ENTRE *value;           /* pointer to table */
} 
FITS;

/* typedef definitions of MIDAS files for main  */


#define STRLEN 10                     /* length of a string */
#define LSTRLEN 64                    /* length of a string */


typedef struct storage
{
	double **rowarray;
	double  *midasrow;
	char   ***chararray;
	char   **charrow;
	char   *charitem;
}
STORAGE;

typedef struct midas                   /* header for a table */
{
	char fname[64];               /* name of table file */
	int nrows;                    /* number of rows filled */
	int ncols;                    /* number of columns filled */
	int cncols;                   /* number of char columns filled */
	char **colname;               /* address of colname ptr array */
	char **coltype;               /* address of coltype ptr array */
	double **value;               /* address of row ptr array */
	char   ***cvalue;             /*address of char elements of rptr array*/
	STORAGE *mspt;
}
MIDAS;


#ifndef UND
#define UND -32767                   /* define Bill's NaN */ 
#endif

MIDAS *midasclose();
MIDAS *midasopen();
double getmidasval();
char *getmidasstr();
char *getfitsstr();   /* get string pointer from FITS table */
double getfitsval();  /* get float from FITS table */
FITS *fitsopen();     /* open FITS file */
FITS *fitsclose();    /* close FITS file */
char *MemAlloc();
