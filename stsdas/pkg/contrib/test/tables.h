/*
	GAUSS - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* Header file for Tables */

#define UND -32767                   /* define Bill's NaN */     
#define PARAM_TABLE  2
#define DATA_TABLE  1
#define GET 1
#define PUT 2

typedef struct sppcol
{
	int sppcolptr[50];
	char sppcolname[50][64];
	int nc; 
	int nr;
} SPPCOL;
