          /*
	GAUSS - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/*  header file for routines that access data files */

double getdataval();    /* get data from data file */
double getenvval();     /* get data from environement file */
double getparamval();   /* get parameter from parameter file */
double getresidual();   /* get residual from data file */
double gettheobs();     /* get observation from data file */
double getxparval();    /* get indexed parameer value */

/*
double getitemval();    
double getsetval();
*/

char *getenvstr();      /* get string (pointer) from environment file */
char *getnam();         /* get name from name list */
