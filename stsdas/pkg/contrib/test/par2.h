 /*
	GAUSS - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/


#define PARAM_TABLE 2
#define DATA_TABLE 1

int sxf = 64;
int ote = 128;
int one = 1;
int zero = 0;


extern int numfil;

extern double clgetd();


typedef struct tbnames
{
	char dptr[15][64];
	char pptr[15][64];
	int dfnum;
	int pfnum;
}TBNAMES;
