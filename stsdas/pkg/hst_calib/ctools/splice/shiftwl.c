/* This routine shifts the wavelengths (in-place) so that the values in
   the wl array are the wavelengths at the beginning of each interval,
   rather than at the center.

   Phil Hodge, 1999 Oct 25:
	Function extracted from readtable.c;
	change the algorithm for generating shifted wavelengths.
*/

void ShiftWl (double *wl, int nelem) {

/* arguments:
double wl[]    io: array of wavelengths
int nelem      i: the wl array has nelem values on input, nelem+1 on output
*/

	double orig_wl;		/* an element before modifying the values */
	double save_wl;		/* save a wavelength for next loop */
	int i;

	save_wl = wl[0];

	wl[0] = wl[0] - (wl[1] - wl[0]) / 2.;

	wl[nelem] = wl[nelem-1] + (wl[nelem-1] - wl[nelem-2]) / 2.;

	for (i = 1;  i < nelem;  i++) {
	    orig_wl = save_wl;		/* this is the previous wl[i-1] */
	    save_wl = wl[i];
	    wl[i] = (orig_wl + wl[i]) / 2.;
	}
}
