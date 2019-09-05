/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

/* sorting and related routines */

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_math
#include <iraf.h>

double *linebuf;

static exchange(lp1,lp2) /* exchange two pointers */
double *lp1,*lp2;
{
	register double temp;

	temp = *lp1;
	*lp1 = *lp2;
	*lp2 = temp;
}

static cmp(i,j)  /* compare two values */
double i,j;
{
	register double x;
	x = i-j;
	if (x<0)
		return -1;
	if (x>0)
		return 1;
	return 0;
}

static rquick(lo,hi)  /* quicksort main routine */
int lo,hi;
{
	int i,j;
	double pivline;

	if(lo<hi)
	{
		i = lo;
		j = hi;
		exchange(&linebuf[j],&linebuf[(i+j)/2]); /* first exchange */
		/*
						This trick prevents sort on already sorted
						array from being "worst case"
					*/
		pivline = linebuf[j]; /* pivot line */
		do
		    {
			for(;i<j && cmp(linebuf[i],pivline) <= 0;)/* search for out-ofplace line */
				i = i+1;
			for(;j>i && cmp(linebuf[j],pivline) >= 0;)
				j = j-1;
			if(i<j)
				exchange(&linebuf[i],&linebuf[j]);/* exchange out-of-place line */
		} 
		while (i<j);
		exchange(&linebuf[i],&linebuf[hi]);
		if(i-lo < hi-i)		/* Recursive so do shorter sort first */
		{
			rquick(lo,i-1); /* sort subarrays */
			rquick(i+1,hi);
		}
		else
		{
			rquick(i+1,hi); /* sort subarrays */
			rquick(lo,i-1);
		}
	}
}

printvec(v,n) /* print an n-vector (for debug) */
double *v;
int n;
{
#ifdef DEBUG
	int i;

	printf("n = %d\n",n);
	for(i=0;i<n;i++)
		printf("v[%d] = %lf\n",i,v[i]);
	printf("\n");
#endif
}

quicksort(nlines,buf) /* quicksort - itself */
int nlines;
double *buf;
{
	linebuf = buf;
	rquick(0,nlines-1);
}

double median(v,n) /*find median */
double *v;
int n;
{
	int i;

	if(n==0)
		fatalerror("No elements in median array\n","");
	quicksort(n,v);
	return (v[n/2] + v[(n-1)/2])/2;
}

double MAD(v,n) /* median absolute deviation */
double *v;
int n;
{
	int i;
	double med;

	printvec(v,n);
	med = median(v,n);  /* get median */
	printvec(v,n);
	for(i=0;i<n;i++) /* subtract median and take modulus */
		v[i] = fabs(v[i] - med);
	printvec(v,n);
	med = median(v,n); /* get median */
	printvec(v,n);
	/*	printf("median = %lf\n",med);	*/
	return med; /* that's the result */
}
