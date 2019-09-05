# include <stdlib.h>
# include <stdio.h>
# include <math.h>
# include "pedsub.h"

# define PI	3.14159265358979323846
# define EPS	1.0e-5

/* This structure stores global data used by
** the gaussian fit routines. */
typedef struct {
	float  dvhist;
	float  gam;
	int    Nvhist;
	float  sg0;
	float  *veltemp;
} Gauss;
Gauss data;

static float chi2h_2d (float *);
static float sdgauss (float);
static float h_pol (int, float);
static float he_pol (int, float);
static void getgauher (float, float, float *, int, float, float *, int,
		       float *, int);
static float expp (float);
static float gammaln (float);
static float gammln (float);

/*
** GAUSSFIT:
**
** Calculate the normalization gam, mean vm and dispersion sg of 
** the best-fitting Gaussian to the histogram veltemp with spacing dvhist.
**
** The initial guess is taken to be given by the input values of
** (gam,vm,sg).
**
** The program uses an amoeba scheme which should be very robust.
**
** Based on the routine GAUSSFIT_AMOEBA2D from R. van der Marel's program
** "unpedestal.f".
**
** H. Bushouse	May-1999
**
*/

void gaussfit (float *veltemp, int Nvhist, float dvhist, float gam, float vm,
	       float *sg) {

	/* Local variables */
	int i, j;
	int iter;
	float y[4], help[3];
	float **p;
	float epsmal, pl1, xmn1;

	/* Function declarations */
	void amoeba (float **p, float y[], int ndim, float ftol,
		     float (*funk)(float []), int *nfunk);

	float *vector (int, int);
	void free_vector (float *, int, int);
	float **matrix (int, int, int, int);
	void free_matrix (float **, int, int, int, int);

	/* Allocate memory */
	p = matrix (1, 3, 1, 2);

	/* Store the velocity profile in a common block */
	data.veltemp = vector (-Nvhist,Nvhist);
	for (i=-Nvhist; i<=Nvhist; i++)
	     data.veltemp[i] = veltemp[i];
	data.dvhist = dvhist;
	data.gam    = gam;
	data.Nvhist = Nvhist;
	data.sg0    = (*sg);

	/* Initialize the starting simplex fits */
	epsmal = 0.2;
	pl1    = 1.0 + epsmal;
	xmn1   = 1.0 - epsmal;

	p[1][1] = vm + (epsmal*(*sg));
	p[1][2] = pl1 * (*sg);
	p[2][1] = vm - (epsmal*(*sg));
	p[2][2] = xmn1 * (*sg);
	p[3][1] = vm + (epsmal*(*sg));
	p[3][2] = xmn1 * (*sg);

	/* Initialize */
	for (i=1; i<=3; i++) {
	     for (j=1; j<=2; j++) {
		  help[j] = p[i][j];
	     }
	     y[i] = chi2h_2d (help);
	}

	amoeba (p, y, 2, EPS, chi2h_2d, &iter);

	vm  = p[1][1];
	(*sg)  = fabs(p[1][2]);

	help[1] = vm;
	help[2] = (*sg);

	free_vector (data.veltemp, -Nvhist, Nvhist);
	free_matrix (p, 1, 3, 1, 2);

	return;

}

static void getgauher (float vm, float sg, float *veltemp, int Nvhist,
		       float dvhist, float *hh, int Nhermmax, float *gam,
		       int ingam) {

/*
** Calculate the first Nhermmax Gauss-Hermite moments from the histogram 
** veltemp with spacing dvhist, using the values vm and sg for the weighting 
** function. The results are returned in hh.
**
** If ingam=1, then gam is taken as input. 
** If ingam=0, then gam is returned on output such that h0=1.
*/

	int i, l;
	float vel, w;

	/* Initialize */
	for (l=0; l<= Nhermmax; l++)
	     hh[l] = 0.0;

	/* Loop over the velocities */
	for (i=-Nvhist; i<=Nvhist; i++) {
	
	     vel = dvhist * (float)(i);
	     w   = (vel-vm)/sg;

	     /* Loop over the Gauss-Hermite moments */
	     for (l=0; l<=Nhermmax; l++)
		  hh[l] += veltemp[i]*sdgauss(w)*h_pol(l,w);
	}

	/* Normalize properly. The factor dvhist arises through the stepsize
	** of the Euler integration. The value of gamma is determined by the
	** constraint that h0=1. */
	if (ingam == 0)
	    *gam = hh[0] * 2.0 * sqrt(PI) * dvhist;

	for (l=0; l<=Nhermmax; l++)
	     hh[l] *= 2.0 * sqrt(PI) * dvhist / (*gam);

}

static float chi2h_2d (float *y) {

/*
** Calculates the chih^2 = (h1^2) + (h2^2) for a Gaussian
** with parameters Vgau = y(1), sig = |y(2)|, 
** for the VP in the common block /vpcur/ 
*/

	float vm, sg;
	float harr[11];

	/* Note: it may be necessary to avoid values of gamma
	** and sigma too close to zero.  */
	vm  = y[1];
	sg  = MAX(0.1*data.sg0,fabs(y[2]));

	getgauher (vm, sg, data.veltemp, data.Nvhist, data.dvhist,
		   harr, 2, &(data.gam), 0);

	return (1.0 + sqrt(MAX(0.0,harr[1]*harr[1]+harr[2]*harr[2])) );

}

static float sdgauss (float x) {

/*
** Returns the standard Gaussian as function of x
*/

	return ( (1.0/sqrt(2.0*PI)) * expp(-0.5*x*x) );

}

static float he_pol (int ll, float x) {

/* Returns the value of the Hermite polynomial He_l(x) as defined
** in Appendix A of van der Marel & Franx.
*/

	/* Local variables */
	int j, l;
	float dl, dj, pfacln;
	float hepol;
	static float pfac[21][21];
	static Bool firstc = True;

	if (firstc) {
	    for (l=0; l<=20; l++) {
		 dl = (float)(l);
		 for (j=l; j>=0; j--) {
		      if (2*((j+l)/2) == j+l) {
			  dj = (float)(j);
			  pfacln = (0.5*gammaln(dl+1.0)) - 
				    gammaln(dj+1.0) - gammaln(dl-dj+1.0) +
				    gammaln(0.5*(dl-dj+1.0)) - 
				    gammaln(0.5) + (0.5*(dl-dj)*log(2.0));
			  pfac[l][j] = pow(-1.0,(double)((l-j)/2))*expp(pfacln);
		      } else {
			  pfac[l][j] = 0.0;
		      }
		 }
	    }
	    firstc = False;
	}

	hepol = 0.0;
	for (j=ll; j>=0; j--)
	     hepol = pfac[ll][j] + (x*hepol);

	return (hepol);

}

static float h_pol (int l, float x) {

/* Returns the value of the Hermite polynomial H_l(x) as defined
** in Appendix A of van der Marel & Franx.
*/
	return (he_pol(l,x*sqrt(2.0)) );

}

static float gammaln (float x) {

/* The logarithm of the gamma function */

	float z;
	float gln;

	if (x >= 1.0)
	    gln = gammln(x);
	else if (x >= 0.0) {
	    z = 1.0 - x;
	    gln = log((PI*z)/sin(PI*z)) - gammln(2.0-x);
	} else {
	    gln = 0.0;
	    sprintf (MsgText, "x < 0 in gammaln");
	    n_error (MsgText);
	}

	return (gln);

}

static float gammln (float xx) {

	int j;
	float gln;
	double ser, tmp, x, y;
	static double stp = 2.5066282746310005e0;
	static double cof[6] = { 76.18009172947146e0,
			      -86.50532032941677e0,
			       24.01409824083091e0,
			      -1.231739572450155e0,
			     0.1208650973866179e-2,
			      -0.5395239384953e-5};

	x = xx;
	y = x;
	tmp = x + 5.5e0;
	tmp = (x+0.5e0)*log(tmp)-tmp;
	ser = 1.000000000190015e0;
	for (j=0; j<=5; j++) {
	     y += 1.e0;
	     ser += cof[j]/y;
	}
	gln = tmp + log(stp*ser/x);

	return (gln);

}


static float expp (float x) {

/* Exponential function that avoids underflow */

	if (x >= -60.0)
	    return (exp(x));
	else
	    return (0.0);

}

