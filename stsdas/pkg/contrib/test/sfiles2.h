          /*
	GAUSS - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

#define DATA_TABLE 1
#define PARAM_TABLE 2    
#define UND -32767

static char parfname[20];

char *getparamfilename();
char *getdatafilename();
char *getfitsstr();
char *getcolname();

double getdblval();
double getmidasval();
double getmidascrval();
double getdataval();

typedef struct findex
{
	int name; 	 /* name of param indexed */
	int xvars[5];	/* index variables + ndims*/
	int *row;	/* fastindex array */
}FINDEX;
