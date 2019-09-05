# include "splice.h"

/* This function checks that all wavelengths in the wl array are positive.
   If there are zero or negative wavelengths at the beginning of the wl
   array, all the Spectrum arrays (wl, flux, error, dq, weight) will be
   shifted until the first wavelength is positive.

   Note that this function should be called before calling ShiftWl,
   because the wl array now contains only nelem values.  After calling
   ShiftWl, the wavelengths will have been shifted by half of a pixel,
   and one more wavelength will have beed appended to the end of the
   wl array, giving nelem + 1 elements.

   A function value of -1 or 0 is OK; -1 is bad.

   Phil Hodge, 2013 Apr 30:
	Function created.
*/

int positiveWl(Spectrum *inspec, int *mod_start, int *mod_end) {

/* arguments:
Spectrum *inspec   io: current input spectrum
int *mod_start      o: 1 if there were negative wavelengths at the beginning
int *mod_end        o: 1 if there were negative wavelengths at the end

mod_start and mod_end are flags (value 0 or 1) to let the calling function
know whether something was changed, e.g. for printing a warning message.

The function value will be 1 (bad) if there are no positive wavelengths
or if there are zero or negative wavelengths between the first and last
positive wavelengths.
The function value will be -1 if there are zero or negative wavelengths
at either the beginning or end of the wavelength array but not at any
intermediate location.  In this case, this function will correct the
problem by simply reducing nelem or by shifting all the arrays (wl, flux,
etc.) so the first element has positive wavelength.
Otherwise, 0 will be returned.
*/

	int kstart, kend;	/* first and last indices of good wl */
	int k;
	int status = 0;

	*mod_start = 0;
	*mod_end = 0;

	kstart = -1;
	kend = -1;
	/* Note:  The wl array currently has nelem elements. */
	for (k = 0;  k < inspec->nelem;  k++) {
	    if (inspec->wl[k] > 0.) {
		kstart = k;
		break;
	    }
	}
	for (k = inspec->nelem - 1;  k >= 0;  k--) {
	    if (inspec->wl[k] > 0.) {
		kend = k;
		break;
	    }
	}

	if (kstart < 0 || kend < 0) {
	    *mod_end = 1;
	    inspec->nelem = 0;
	    return 1;
	}

	/* All wavelengths in this range ought to be good, but check. */
	for (k = kstart;  k <= kend;  k++) {
	    if (inspec->wl[k] <= 0.) {
		inspec->nelem = 0;
		return 1;
	    }
	}

	/* This is the normal situation, no bad wavelengths. */
	if (kstart == 0 && kend == inspec->nelem - 1)
	    return 0;

	if (kend < inspec->nelem - 1) {
	    *mod_end = 1;
	    status = -1;
	    inspec->nelem = kend - 1;
	}

	if (kstart > 0) {
	    int j;
	    *mod_start = 1;
	    *mod_end = 1;
	    status = -1;
	    for (j = 0, k = kstart;  k <= kend;  j++, k++) {
		inspec->wl[j] = inspec->wl[k];
		inspec->flux[j] = inspec->flux[k];
		inspec->error[j] = inspec->error[k];
		inspec->dq[j] = inspec->dq[k];
		inspec->weight[j] = inspec->weight[k];
	    }
	    inspec->nelem = kend - kstart + 1;
	}

	return status;
}
