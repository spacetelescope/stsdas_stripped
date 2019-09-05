/*

	GaussFit - A System for Least Squares and Robust Estimation
	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/
/* program Householder    */

/* Header file for Householder transformations */


#define deltafn(i,j) ((i==j)?1:0)        /* Kronedcker delta */
#define sqr(x)            ((x)*(x))      /* square of x */
#define trunc(x)          ((int)(x))     /* integer part of x */
#define minval(x,y)       (((x)<=(y))?(x):(y)) /* minimum of 2 arguments */
#define maxval(x,y)       (((x)>=(y))?(x):(y)) /* maximum of 2 arguments */
#define RoundToZero(x,y)  (fabs(x)<y?(0.0):(x))
/* 
	Rounds a number that has lost 
	significance  (i.e., is <y ) to zero
*/

#define  Condition  1                    /* Condition/constraint equations */
#define  Constraint  2
#define  RightHandType  0                /* Type of a matrix column */
#define  DataType 1
#define  ObsType 2
#define  GlobalType  3
#define  IndexedType 4

double getdeltas();                      /* get parameter corrections */
double LSB();                            /* get least significant bit */

typedef struct key
{
	char name[64];              /* name of column indexed  */
	int  col;                   /* col where info resides */
	int  type;                 /*  type of var */
}KEY;
