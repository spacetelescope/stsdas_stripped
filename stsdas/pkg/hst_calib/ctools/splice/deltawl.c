# include <stdio.h>
# include <stdlib.h>
# include <math.h>

# include "splice.h"

# define NON_ZERO_SLOPE (1.e-4)

typedef struct {	/* for sorting on dwl */
	double wl;
	double dwl;
} Pair;

static int CreateWl (SpecArray *, int, Spectrum *);
static void Getdwl (Spectrum *,
	double *, double *, double *, double *, double *, double *);
static int ComputeFit (double *, double *, int, int, double *);
static void CheckRight (double *, double *, int,
		int, double *, double *, int *);
static double HowWell (double *, double *, int, double *, double *, int *);

/* This routine determines the wavelengths for the output spectrum.

   Phil Hodge, 1998 Oct 27

   Phil Hodge, 1998 Nov 13:
	In oCreateWl, remove the -0.5 from pixel0.  In the array of
	wavelengths, element i already gives the wavelength at the start
	of pixel i, so it isn't appropriate to subtract 0.5 again.
	The effect of the -0.5 was that in some cases a half pixel
	of the output spectrum would be lost.

   Phil Hodge, 1998 Dec 16:
	Replace the algorithm for fitting a line to the (wl,dwl) data.

   Phil Hodge, 1999 Feb 19:
	Add wl_spacing argument, and modify oComputeFit and oCheckRight
	to check this value.

   Phil Hodge, 1999 Apr 20:
	In oCreateWl, if the slope a[1] is zero or negative, set the
	wavelengths to be linear with pixel number.  Add reject to calling
	sequence of oHowWell, to guard against a[0] + a[1] * lambda <= 0.

   Phil Hodge, 1999 Oct 25:
	Move AllocOut to a separate file, and rename it to AllocSpec;
	rename functions.

   Phil Hodge, 2000 Apr 7:
	In CreateWl, the computation of min_wl and max_wl was incorrect:
	if both the min and max were outside the current range, only
	min_wl was being updated.

   Phil Hodge, 2013 May 3:
	Add checks for too few elements (or none at all).

   Phil Hodge, 2014 March 26:
	Change the test for significant_slope in CreateWl.  Find min & max
	dwl, and set `use_this_dwl` for computing max_pixel and the
	wavelengths (if the slope is negligible).
*/

int DeltaWl (SpecArray *spectra, int wl_spacing, Spectrum *outspec) {

	int nelem;		/* number of elements in output spectrum */
	int i;
	int status;
	int AllocSpec (Spectrum *, int);

	if (spectra->nspec < 1) {
	    printf ("no input data\n");
	    return (1);
	}

	if (spectra->nspec == 1) {

	    if (spectra->spec[0]->nelem < 1) {
		printf ("The input spectrum has no valid data\n");
		return (1);
	    }

	    nelem = (spectra->spec[0])->nelem;
	    if ((status = AllocSpec (outspec, nelem)))
		return (status);

	    /* Copy wavelengths from input to output. */
	    for (i = 0;  i <= outspec->nelem;  i++)
		outspec->wl[i] = (spectra->spec[0])->wl[i];

	} else {

	    /* Fit a straight line to d(pixel) / d(wavelength)
		(pixels per Angstrom) as a function of wavelength.
	    */
	    if ((status = CreateWl (spectra, wl_spacing, outspec)))
		return (status);
	}

	return (0);
}

/* This routine determines the wavelength range and spacing per pixel,
   and it assigns wavelengths to the wl array in outspec.

   If dlambda / dpixel = a0 + a1 * lambda, then

	pixel = ln (a0 + a1 * lambda) / a1 + pixel0

	lambda = {exp [a1*(pixel-pixel0)] - a0} / a1

Let wavelength = lambda0 at index = 0 (start of first interval), then

	pixel0 = - ln (a0 + a1 * lambda0)/ a1

*/

static int CreateWl (SpecArray *spectra, int wl_spacing, Spectrum *outspec) {

	/* dwl[n] is one of the (dlambda / dpixel) at one end of a spectrum;
	   wl[n] is the wavelength at that end.  These two arrays are for
	   saving these values.
	*/
	double *dwl;
	double *wl;
	double min_wl, max_wl;	/* min and max wavelengths in all spectra */
	double min_dwl, max_dwl;	/* min and max delta wavelengths */
	double use_this_dwl;	/* depends on wl_spacing */
	double wl_first, wl_last;	/* min & max wl in one spectrum */
	double a[2];		/* coefficients of linear fit */
	double pixel0, max_pixel;
	double xi;
	int nelem;
	int n;
	int i;
	int first_valid_input;	/* flag:  true for first input spectrum */
	int nfit;	/* 2 * number of spectra */
	int significant_slope;	/* true if |a[1]| > NON_ZERO_SLOPE */
	int status;
	int AllocSpec (Spectrum *, int);

	/* Allocate enough for two values for each spectrum, one value at
	   the beginning and one at the end.
	*/
	nfit = 2 * spectra->nspec;
	wl = calloc (nfit, sizeof(double));
	dwl = calloc (nfit, sizeof(double));
	if (wl == NULL || dwl == NULL) {
	    printf ("out of memory\n");
	    return (1);
	}
	/* For each spectrum, get wavelength increment, etc. */
	first_valid_input = 1;	/* current spectrum (in loop) is the first */
	for (n = 0;  n < spectra->nspec;  n++) {
	    if (spectra->spec[n]->nelem < 1)
		continue;
	    Getdwl (spectra->spec[n],
			&wl[2*n], &dwl[2*n], &wl[2*n+1], &dwl[2*n+1],
			&wl_first, &wl_last);
	    if (first_valid_input) {
		first_valid_input = 0;
		min_wl = wl_first;
		max_wl = wl_last;
	    } else {
		if (wl_first < min_wl)
		    min_wl = wl_first;
		if (wl_last > max_wl)
		    max_wl = wl_last;
	    }
	}
	if (first_valid_input) {
	    printf ("No data, at least none with positive wavelengths.\n");
	    return 1;
	}

	/* Fit a line to the (wl,dwl) data. */
	if ((status = ComputeFit (wl, dwl, nfit, wl_spacing, a)))
	    return (status);

	/* Find the min and max values of dwl. */
	min_dwl = fabs(dwl[0]);
	max_dwl = min_dwl;
	for (i = 1;  i < nfit;  i++) {
	    /* Use use_this_dwl for scratch. */
	    use_this_dwl = fabs(dwl[i]);
	    if (use_this_dwl < min_dwl)
		min_dwl = use_this_dwl;
	    if (use_this_dwl > max_dwl)
		max_dwl = use_this_dwl;
	}
	/* Now set use_this_dwl for real. */
	if (wl_spacing == MIN_D_WL) {
	    use_this_dwl = min_dwl;
	} else {
	    use_this_dwl = max_dwl;
	}

	/* The test on a[0] + a[1] * min_wl is included because that
	   expression will be used as the argument to the log function.
	*/
	if (a[1] > 0. && a[0] + a[1] * min_wl > 0. &&
	    (max_dwl - min_dwl) / ((max_dwl + min_dwl) / 2.) > NON_ZERO_SLOPE) {
	    significant_slope = 1;
	} else {
	    significant_slope = 0;
	}

	if (significant_slope) {

	    /* Get the constant of integration of dpixel / dlambda. */
	    pixel0 = -log (a[0] + a[1] * min_wl) / a[1];

	    /* Compute the pixel number of the last pixel. */
	    max_pixel = log (a[0] + a[1] * max_wl) / a[1] + pixel0;
	    if (max_pixel <= 0.) {
		printf ("CreateWl:  Error computing the range of pixels.\n");
		return (1);
	    }

	} else {

	    max_pixel = (max_wl - min_wl) / use_this_dwl;
	}

	/* Round up, and set the number of array elements. */
	nelem = (int)(max_pixel + 1.);

	/* Compute the wavelengths, and assign in the output spectrum. */
	if ((status = AllocSpec (outspec, nelem)))
	    return (status);
	for (i = 0;  i <= nelem;  i++) {
	    xi = (double)i;
	    if (significant_slope)
		outspec->wl[i] = (exp (a[1] * (xi - pixel0)) - a[0]) / a[1];
	    else
		outspec->wl[i] = min_wl + use_this_dwl * xi;
	}

	free (wl);
	free (dwl);

	return (0);
}

/* This routine looks at delta wavelength (per pixel) at each end of the
   current spectrum, and it returns dwl, the smaller of those two values,
   together with the wavelength wl at the center of that pixel.  It also
   returns the wavelengths at both ends (pixel edges).
*/

static void Getdwl (Spectrum *inspec,
		double *wl_1, double *dwl_1,
		double *wl_2, double *dwl_2,
		double *wl_first, double *wl_last) {

/* arguments:
Spectrum *inspec   i: current input spectrum
double *wl         o: wavelength corresponding to dwl
double *dwl        o: smaller of the dlambda / dpixel at the ends
double *wl_first   o: smaller of the wavelengths at the ends
double *wl_last    o: larger of the wavelengths at the ends
*/

	int nelem;

	nelem = inspec->nelem;

	*dwl_1 = fabs (inspec->wl[1] - inspec->wl[0]);
	*wl_1 = (inspec->wl[0] + inspec->wl[1]) / 2.;
	*dwl_2 = fabs (inspec->wl[nelem] - inspec->wl[nelem-1]);
	*wl_2 = (inspec->wl[nelem-1] + inspec->wl[nelem]) / 2.;

	if (inspec->wl[0] <= inspec->wl[nelem]) {
	    *wl_first = inspec->wl[0];
	    *wl_last = inspec->wl[nelem];
	} else {
	    *wl_first = inspec->wl[nelem];
	    *wl_last = inspec->wl[0];
	}
}

/* This routine fits a line to the (wl,dwl) data:  a[0] + a[1] * wl[n]
   All (wl,dwl) points will be on or above this line (or below, depending
   on wl_spacing).

   For STIS data the set of points is really quite flat; the largest dwl
   is much smaller than any wl.

   We have a set of points (wl,dwl) in the plane.  Imagine a convex
   polygon surrounding these points, such that each vertex of the polygon
   is one of these points, and no point falls outside the polygon.
   If wl_spacing is MIN_D_WL, we are looking for the lower portion of
   the polygon; if wl_spacing is MAX_D_WL, we want the upper portion.
   We want to find the side which, when extended to a full line, minimizes
   the maximum vertical distance from the line to each of the points.
   The parameters of this line will be returned.
*/

static int ComputeFit (double wl[], double dwl[], int npts,
		int wl_spacing, double a[]) {

/* arguments:
double wl[]      i: wavelength corresponding to dwl
double dwl[]     i: dlambda / dpixel
int npts         i: number of elements in wl, dwl arrays
int wl_spacing   i: min spacing or max spacing
double a[]       o: a[0] is the intercept, a[1] is the slope
*/

	double pt_best_dwl[2];		/* point with min (or max) dwl */
	double pt_l[2], pt_r[2];	/* left & right endpoints */

	/* These three variables are for the current best line (side of
	   the polygon).  best_max is the current minimum of the maximum
	   deviations, and pl_l_best & pl_r_best are the endpoints of the
	   line segment (they're vertices of the polygon).
	*/
	double pt_l_best[2], pt_r_best[2];
	double best_max;	/* current minimum of maximum deviations */

	double current_max;	/* current value of maximum deviation */

	int n;			/* loop index */
	int done;		/* loop termination flag */

	/* reject will be true if the line through the current pair of
	   points crosses zero for any wavelength in the set.
	*/
	int reject;

	reject = 0;

	/* Find minimum (or maximum) delta wavelength. */
	pt_best_dwl[0] = wl[0];		/* initial values */
	pt_best_dwl[1] = dwl[0];
	for (n = 1;  n < npts;  n++) {
	    if (dwl[n] <= 0.)
		continue;
	    if ((wl_spacing == MIN_D_WL && dwl[n] < pt_best_dwl[1]) ||
		(wl_spacing == MAX_D_WL && dwl[n] > pt_best_dwl[1])) {
		pt_best_dwl[0] = wl[n];
		pt_best_dwl[1] = dwl[n];
	    }
	}

	/* Initialize for a horizontal line through min (or max) dwl. */
	pt_l_best[0] = pt_best_dwl[0];
	pt_l_best[1] = pt_best_dwl[1];
	pt_r_best[0] = pt_best_dwl[0] + 1.;	/* arbitrary positive number */
	pt_r_best[1] = pt_best_dwl[1];
	best_max = HowWell (wl, dwl, npts, pt_l_best, pt_r_best, &reject);

	/* Find minimum wavelength.  This will be our starting point,
	   the leftmost vertex of the polygon.
	*/
	pt_l[0] = wl[0];
	pt_l[1] = dwl[0];
	for (n = 1;  n < npts;  n++) {
	    if (wl[n] < pt_l[0]) {
		pt_l[0] = wl[n];
		pt_l[1] = dwl[n];
	    }
	}

	done = 0;
	while (!done) {
	    /* Find the next vertex to the right of the current one. */
	    CheckRight (wl, dwl, npts, wl_spacing, pt_l, pt_r, &done);
	    if (!done) {
		current_max = HowWell (wl, dwl, npts, pt_l, pt_r, &reject);
		if (current_max < best_max && !reject) {
		    pt_l_best[0] = pt_l[0];
		    pt_l_best[1] = pt_l[1];
		    pt_r_best[0] = pt_r[0];
		    pt_r_best[1] = pt_r[1];
		    best_max = current_max;
		}
		/* Step to the right endpoint to get ready to look for
		   the next vertex.
		*/
		pt_l[0] = pt_r[0];
		pt_l[1] = pt_r[1];
	    }
	}

	/* Assign coefficients of best line. */
	if (reject) {
	    a[1] = 0.;
	    a[0] = pt_best_dwl[1];		/* min or max delta wl */
	} else {
	    a[1] = (pt_r_best[1] - pt_l_best[1]) /
		   (pt_r_best[0] - pt_l_best[0]);
	    a[0] = pt_l_best[1] - a[1] * pt_l_best[0];
	}

	return (0);
}

/* The current point is pt_l, and this is one vertex of the enclosing
   polygon.  This routine finds the point in the (wl,dwl) array which
   is the next vertex to the right and assigns its coordinates to pt_r.
   If there is no vertex to the right (i.e. if pt_l[0] is the largest
   wavelength in the wl array), then done will be set to true and pt_r
   will not be changed.
*/

static void CheckRight (double wl[], double dwl[], int npts,
		int wl_spacing, double pt_l[], double pt_r[], int *done) {

/* arguments:
double wl[]      i: wavelength corresponding to dwl
double dwl[]     i: dlambda / dpixel
int npts         i: number of elements in wl, dwl arrays
int wl_spacing   i: min spacing or max spacing
double pt_l[]    i: current point
double pt_r[]    o: next point to right that is a vertex
int done         o: set to true if there are no more points to the right
*/

	int n;
	int larger_wl;		/* true if there is a point with larger wl */
	double pt_save[2];
	double slope_save;
	double slope;

	/* Run through list of points to see if any has a larger wl. */
	larger_wl = 0;
	for (n = 0;  n < npts;  n++) {
	    if (wl[n] > pt_l[0]) {
		larger_wl = 1;
		break;
	    }
	}
	if (!larger_wl) {
	    *done = 1;
	    return;
	}

	/* Set initial values using the one point we found that has
	   a larger wavelength than the current point.
	*/
	pt_save[0] = wl[n];
	pt_save[1] = dwl[n];
	slope_save = (dwl[n] - pt_l[1]) / (wl[n] - pt_l[0]);

	/* Of all points with wavelength larger than the wavelength at
	   the current point, select the one that has smallest slope
	   (or largest, depending on wl_spacing) from the current point.
	*/
	for (n = 0;  n < npts;  n++) {
	    if (wl[n] > pt_l[0]) {
		slope = (dwl[n] - pt_l[1]) / (wl[n] - pt_l[0]);
		if ((wl_spacing == MIN_D_WL && slope < slope_save) ||
		    (wl_spacing == MAX_D_WL && slope > slope_save)) {
		    pt_save[0] = wl[n];
		    pt_save[1] = dwl[n];
		    slope_save = slope;
		}
	    }
	}

	/* Set the right endpoint to be the one that gave minimum
	   (or maximum) slope.
	*/
	pt_r[0] = pt_save[0];
	pt_r[1] = pt_save[1];
}

/* The current pair of points is pt_l & pt_r.  This function returns
   a measure of how well the line through those two points matches
   all the (wl,dwl) data.  The value returned is the maximum difference
   (in the vertical direction) between the line and each of the (wl,dwl)
   points.

   The reject argument will be set to one if the line between the current
   pair of points touches the dwl = 0 axis for any wavelength in the set.
   This is because we take the log of that value (a[0] + a[1] * wavelength)
   in CreateWl.
*/

static double HowWell (double wl[], double dwl[], int npts,
		double pt_l[], double pt_r[], int *reject) {

	double slope;		/* slope of line through pt_l & pt_r */
	double diff, max_diff;
	double y;		/* line evaluated at one of the wl[n] */
	int n;

	slope = (pt_r[1] - pt_l[1]) / (pt_r[0] - pt_l[0]);

	/* initial value */
	max_diff = fabs (dwl[0] - ((wl[0] - pt_l[0]) * slope + pt_l[1]));

	for (n = 1;  n < npts;  n++) {
	    y = (wl[n] - pt_l[0]) * slope + pt_l[1];
	    if (y <= 0.) {
		*reject = 1;
		return (1.);
	    }
	    diff = fabs (dwl[n] - y);
	    if (diff > max_diff)
		max_diff = diff;
	}

	*reject = 0;
	return (max_diff);
}
