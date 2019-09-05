/*
	GaussFit - A System for Least Squares and Robust Estimation

	Source Code Copyright (C) 1987 by William H. Jefferys,
	Michael J. Fitzpatrick and Barbara E. McArthur
	All Rights Reserved.
*/

#define  import_spp
#define  import_libc
#define  import_stdio
#define  import_math
#include <iraf.h>

#include "defines.h"
#include "house.h"
#include "files.h"

#ifndef PI
#define	PI		(3.14159265358979323846)
#endif

#define EPSILON 0.001

extern FILE *fp;
extern double ScaleFac;
static double c = 0.0;
static double in = 1.0;
/* constants for calculation of C(ARE) for Fair */
static double cvals[7]={
	0.073,0.06999,0.06351,0.05000,0.03520,0.1740,0.0};
static double fair_table[4]  = {
	0.1760,0.3333,0.6351,1.3998};
static double tukey_table[4] = {
	3.1369,3.4437,3.8827,4.6851};
static double huber_table[4] = {
	0.5294,0.7317,0.9818,1.3450};

static double AREF = 0.0;
static double AREH = 0.0;
static double AREM = 0.0;
static double ARET = 0.0;
static double TRIM = 0.0;

double rhoprim(u)
double u;
{
	double w, x;
	if (!notzero(c))
		fatalerror("Attempt to divide by zero in rhoprim\n"," ");
	if(AREF != 0.0 && ScaleFac!=0.0) /* compute constants if needed */
	{
		x = u/c;
		w = sqr(c)*(x-log(1.0 + x)); /* rho for "fair" =p(u)/integral */
	}
	else if(AREH != 0.0 && ScaleFac != 0.0)
	{
		x = u/c;
		if(x <= 1.0)
			w = 0.5*sqr(x*c);
		else
			w = sqr(c)*(x - 0.5);
	}
	else if(ARET != 0.0 && ScaleFac != 0.0)
	{
		x = u/c;
		if(x <= 1.0)
		{
			x = 1.0 - x*x;
			x = x*x*x;
			w = sqr(c)/6.0*(1.0 - x);
		}
		else
			w = sqr(c)/6.0;
	}
	else if(TRIM != 0.0 && ScaleFac != 0.0)
	{
		x = u/c;
		if(x <= 1.0)
			w = 0.5*sqr(c*x);
		else
			w = 0.5*sqr(c);
	}
	else
	{
		x = u/c;
		w = 0.5*sqr(x*c); /* rho for least squares, minsum	*/
		/* replaced sqr(u[i]) 				*/
	}
	return w/in;
}

double integrand(x) /* to compute integral (phi*exp(-0.5*x*x)) = <phi> */
double x;
{
	double u,phi;

	u = x/c;  /* normalize x */
	/* phi = sqr(c)*(u - log(1.0+u));*/  /* definition of phi */
	phi = rhoprim(x);
	return phi*exp(-0.5*sqr(x));  /* integrand */
}

double simpson(n)  /* Simpson's rule integration. n nodes */
int n;
{
	double sum,dx,val;
	int i;

	sum = 0.0;
	dx = 5.0/n; /* integrating from 0 to 5 */
	for(i=0;i<=n;i++)  /* sum over all nodes */
	{
		val = integrand(i*dx);
		if(i==0 || i==n) /*endpoint weights = 1 */
			sum += val;
		else if((i/2)*2 == i) /* even points weights = 2 */
			sum += 2.0*val;
		else  /* odd points weights = 4 */
		sum += 4.0*val;
	}
	return dx*sum/3.0;
}

getintegral()   /* weighting factor */
{
	int i;
	double old, new;

	old = 0.0;  /* old and new values */
	new = 1.0;
	for(i=32;fabs(old-new)>=0.0001;i=2*i)
		/* halve interval each time until converged */
	{
		old = new;
		new = simpson(i);  /*compute new approximation */
	}
	in = 2.0*sqrt(2.0/PI)*new;  /* normalizing factor */
	printf("integral  =\t");
	printdouble(stdout,in);
	printf("\n");
	fprintf(fp,"integral =\t");
	printdouble(fp,in);
	fprintf(fp,"\n");
}

double interpolate(x,n,v,h) /* interpolate in a table V[] at point n;
                               spacing h, value x */
double x,v[],h;
int n;
{
	double a, b, c;

	a = v[n+1];   /* constant */
	b = (v[n+2]-v[n])/(2*h);  /* first derivative */
	c = ((v[n+2]+v[n])/2 - a)/(h*h);  /* second derivative */
	a = a + x*(b + x*c);  /* quadratic interpolation */
	return a;
}

double lininterp(x,table)
double x, table[];
{
	int n;
	double w;

	if(x < 0.8 || x > 0.95)
		fatalerror("ARE must be between 0.8 and 0.95\n","");
	if (x == 0.95)
		return table[3];
	n = (int)((x - 0.8)/0.05);
	w = table[n] + (x - (0.8 + 0.05*n))/0.05*(table[n+1] - table[n]);
	/*printf("weight from lininterp = %lf, nvalue = %d\n",w,n);*/
	return w;
}

getfaircon(ARE)   /* compute c(ARE) for "fair" */
double ARE;                 
{
	double u;
	int n;

	if(ARE>=0.950)  /* ARE can't be >= 0.950 */
		fatalerror("Value of keyword FAIR >= 0.95\n","");
	else if (ARE<=0.8) /* and can't be <= 0.8 */
		fatalerror("Value of keyword FAIR <= 0.8\n","");
	u = 1.0 - ARE;  /* compute interpolation variable */
	n = u/0.05; /* compute number of point to be interpolated */
	c = interpolate(u-0.05*(n+1),n,cvals,0.05)/u; /* interpolate on table for c */
	printf("fair ARE  =\t");
	printdouble(stdout,ARE);
	printf("\n");
	fprintf(fp,"fair ARE  =\t");
	printdouble(fp,ARE);
	fprintf(fp,"\n");

	printf("fair c    =\t");
	printdouble(stdout,c);
	printf("\n");
	fprintf(fp,"fair c    =\t");
	printdouble(fp,c);
	fprintf(fp,"\n");
	getintegral(); /* compute integral also */
}

gettrimcon(ARE)
double ARE;
{
	c = ARE; /* Value for Trim sigma */

	printf("trim sigma =\t");
	printdouble(stdout,ARE);
	printf("\n");
	fprintf(fp,"trim sigma =\t");
	printdouble(fp,ARE);
	fprintf(fp,"\n");

	printf("trim c     =\t");
	printdouble(stdout,c);
	printf("\n");
	fprintf(fp,"trim c     =\t");
	printdouble(fp,c);
	fprintf(fp,"\n");

	getintegral(); /* compute integral also */
}

gettukeycon(ARE)
double ARE;
{
	c = lininterp(ARE,tukey_table);

	printf("tukey ARE =\t");
	printdouble(stdout,ARE);
	printf("\n");
	fprintf(fp,"tukey ARE =\t");
	printdouble(fp,ARE);
	fprintf(fp,"\n");

	printf("tukey c   =\t");
	printdouble(stdout,c);
	printf("\n");
	fprintf(fp,"tukey c   =\t");
	printdouble(fp,c);
	fprintf(fp,"\n");

	getintegral(); /* compute integral also */
}

gethubercon(ARE)
double ARE;
{
	c = lininterp(ARE,huber_table);
	
	printf("huber ARE =\t");
	printdouble(stdout,ARE);
	printf("\n");
	fprintf(fp,"huber ARE =\t");
	printdouble(fp,ARE);
	fprintf(fp,"\n");

	printf("huber c   =\t");
	printdouble(stdout,c);
	printf("\n");
	fprintf(fp,"huber c   =\t");
	printdouble(fp,c);
	fprintf(fp,"\n");

	getintegral(); /* compute integral also */
}

getcon()
{
	int flag;

	if(c)  /* return if its already calculated */
		return;
	if ((getenvval("huber")) && (getenvval("orm")== -1))
		fatalerror("You must use orm with Huber.  Set orm to 1.0 in the environment file.\n","");
	flag = 0;
	if(AREM = getenvval("minsum")) flag++;
	if(AREF = getenvval("fair")) flag++;
	if(AREH = getenvval("huber")) flag++;
	if(ARET = getenvval("tukey")) flag++;
	if(TRIM = getenvval("trim")) flag++;
	if(flag > 1)
		fatalerror("You can only use one of: Huber, Tukey, Fair, Trim, Minsum\n","");
	if(AREF != 0.0) /* compute constants if needed */
		getfaircon(AREF);
	else if(AREH != 0.0) /* compute constants if needed */
		gethubercon(AREH);
	else if(ARET != 0.0) /* compute constants if needed */
		gettukeycon(ARET);
	else if(TRIM != 0.0) /* compute constants if needed */
		gettrimcon(TRIM);
	else
		c = 1.0;
}

double ulength(u,i,n)
double u[];
int i,n;
{
int k;
/*		for(k=0;k<n;k++)
			printf("u[%d] = %f  ",k,u[k]);
	printf("\n");*/
	if(getenvval("orm") != -1)
	{
		double sum;
		int j;

		sum = 0.0;
		for(j=0;j<n;j++)
			sum += sqr(u[j]);
	/*		printf("ulength orm  = %lf\n",sqrt(sum));*/
		return sqrt(sum);
	}
	else
			{
			/*printf("ulength   = %lf\n",fabs(u[i]));*/
		return fabs(u[i]);
	}
}

double rhofn(u,i,n)  /* compute rho(u) */
double u[];
int i,n;
{
	getcon();
/*	printf("from rhofn");*/
	return rhoprim(ulength(u,i,n));
}

double Weightfn(u,i,n) /* weight = psi(u)/u */
double u[];
int i,n;
{
	double w, x;


	getcon();
	if (!notzero(c))
		fatalerror("Attempt to divide by zero in Weightfn.\n"," ");
	if(AREF != 0.0 && ScaleFac!=0.0)
	{
		/*printf("from weightfn AREF");*/
		w = 1.0/(1.0 + ulength(u,i,n)/c); /* value for "fair" */
	}
	else if (AREH != 0.0 && ScaleFac != 0.0)
	{
		/*printf("from weightfn AREH");*/
		w = ulength(u,i,n);
		if(w <= c)
			w = 1.0;
		else
			w = c/w;
	}
	else if (ARET != 0.0 && ScaleFac != 0.0)
	{
		/*printf("from weightfn ARET");*/
		x = ulength(u,i,n)/c;
		if(x < 1.0)
		{
			x = 1.0 - x*x;
			w = x*x;
		}
		else
			w = 0.0;
		if(w < EPSILON)
			w = EPSILON;
	}
	else if (TRIM != 0.0 && ScaleFac != 0.0)
	{
		/*printf("from weightfn TRIM");*/
		x = ulength(u,i,n)/c;
		if(x <= 1.0)
			w = 1.0;
		else
			w = EPSILON;
	}
	else
		w = 1.0; /* constant for least squares, minsum */
	return w;
}

double Weightfnp(u,i,n) /* weight = psi(u)/u */
double u[];
int i,n;
{
	double w, x;

	if (!notzero(c))
		fatalerror("Attempt to divide by zero in Weightfnp.\n"," ");
	getcon();
	if(AREF != 0.0 && ScaleFac!=0.0)
	{
	   /*printf("from weightfnp AREF");*/
		w = 1.0/(1.0 + ulength(u,i,n)/c); /* value for "fair" */
		w = -sqr(w)/c;
	}
	else if (AREH != 0.0 && ScaleFac != 0.0)
	{
	   /*printf("from weightfnp AREH");*/
		w = ulength(u,i,n);
		if(w <= c)
			w = 0.0;
		else
			w = -c/sqr(w);
	}
	else if (ARET != 0.0 && ScaleFac != 0.0)
	{
	   /*printf("from weightfnp ARET");*/
		x = ulength(u,i,n)/c;
		if(x < 1.0)
		{
			w = x*x - 1.0;
			w = 4.0/c*x*w;
		}
		else
			w = 0.0;
	}
	else
		w = 0.0; /* zero for least squares, minsum */
	/*printf("w from weightfn\n");*/
	return w;
}

double psifn(u,i,n) /* psi(x) = D(u[i]) rho(u) = weight*u[i] */
double u[];
int i,n;
{
	return u[i]*Weightfn(u,i,n);
		/*double w;
		
		getcon();
		if(AREF != 0.0 && ScaleFac!=0.0)
	   {
			w = u[i]/(1.0 + ulength(u,i,n)/c); 
			}
		else if(AREH != 0.0 && ScaleFac != 0.0)
		{
			w = ulength(u,i,n);
			if(w<=c)
				w = u[i];
			else
				w = c*u[i]/w;
		}
		else
			w = u[i]; 
		return w;
	*/
	}
	
	
	double psipfn(u,i,j,n) /* psi(u) = D(u[i]u[j]) rho(u) */
	double u[];
	int i,j,n;
	{
		double w, wt, wtp, absu;

		if(getenvval("tukey") && !getenvval("irls"))
			fatalerror("Sorry, Tukey's biweight requires IRLS \n","");
		wt = Weightfn(u,i,n);
		wtp = Weightfnp(u,i,n);
		absu = ulength(u,i,n);
		if(absu == 0.0)
			absu = 1.0;
		w = wt*deltafn(i,j) + wtp*u[i]*u[j]/absu;
		/*printf("w1 returned from psipfn = %lf\n",w);*/
		if(getenvval("orm") == -1)
			w = deltafn(i,j)*w;
/*		printf("w2 returned from psipfn = %lf\n",w);*/
		if(getenvval("huber"))
			w = w + EPSILON*deltafn(i,j);
		/*printf("w3 returned from psipfn = %lf\n",w);*/
		return w;
	} 

	double oldpsipfn(u,i,j,n) /* psi(u) = D(u[i]u[j]) rho(u) */
		double u[];
	int i,j,n;
	{
		double w,ww,absx,delta;

		if (!notzero(c))
			fatalerror("Attempt to divide by zero in oldpsipfn.\n"," ");
		getcon();
		if(AREF != 0.0 && ScaleFac!=0.0)
		{
			absx = ulength(u,i,n);
			ww = 1.0 + absx/c;
			w = 1/sqr(ww);
			if (getenvval("orm") != -1)
			{
				if (absx != 0.0)
					delta = u[i]*u[j]/(absx*c);
				else
					delta = 0.0;
				w = w*(ww*deltafn(i,j)-delta);
			}
			else
				w = w*deltafn(i,j);
		}
		else if(AREH != 0.0 && ScaleFac != 0.0)
		{
			absx = ulength(u,i,n);
			if(absx <= c)
				w = deltafn(i,j);
			else
			{
				if(getenvval("orm") != -1)
					w = c*(deltafn(i,j)*(1.0+EPSILON) - u[i]*u[j]/sqr(absx))/absx;
				else
					w = EPSILON*deltafn(i,j);
			}
		}
		else
			w = deltafn(i,j); /* constant for least squares */
		return w;
	}
