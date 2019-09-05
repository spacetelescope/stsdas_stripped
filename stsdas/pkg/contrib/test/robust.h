/*

	GaussFit - A System for Least Squares and Robust Estimation



	Source Code Copyright (C) 1987 by William H. Jefferys,

	Michael J. Fitzpatrick and Barbara E. McArthur

	All Rights Reserved.

*/



/* header file for robust estimation code */



double tuningc();   /* get tuning constant */

double integral();  /* get integral of rho functions */

double Weightfn();  /* get weight */

double rhofn();     /* rho function */

double psifn();     /* derivative of rho */

double psipfn();    /* second derivative of rho */
