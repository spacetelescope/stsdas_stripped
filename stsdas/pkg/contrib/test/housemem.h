/*

	GaussFit - A System for Least Squares and Robust Estimation
	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* Header file for Tables */

#define MINCOLS 10                   /* max columns */
#define MINROWS 10                   /* max rows */
#define STRLEN 10                     /* length of a string */
#define LSTRLEN 64                    /* length of a string */
#define MAXLIN  1100                   /* maximum size of column line */
#define UND -32767                   /* define Bill's NaN */


typedef struct memsize
{
	int    nrows;
	int    ncols;
	int    lastrow;
	int    lastcol;
}
MEMSIZE;
