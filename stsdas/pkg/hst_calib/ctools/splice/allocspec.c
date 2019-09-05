# include <stdio.h>
# include <stdlib.h>
# include "splice.h"

/* This routine allocates memory for a spectrum.

   Phil Hodge, 1999 Oct 25:
	Function extracted from deltawl.c.
*/

int AllocSpec (Spectrum *spec, int nelem) {

	int i;

	spec->nelem = nelem;
	spec->scalar_weight = 1.;

	spec->wl = calloc (nelem+1, sizeof(double));	/* note:  nelem + 1 */
	spec->flux = calloc (nelem, sizeof(double));
	spec->error = calloc (nelem, sizeof(double));
	spec->dq = calloc (nelem, sizeof(short));
	spec->weight = calloc (nelem, sizeof(double));

	if (spec->wl == NULL || spec->flux == NULL ||
	    spec->error == NULL || spec->dq == NULL ||
	    spec->weight == NULL) {
	    printf ("out of memory\n");
	    return (1);
	}

	for (i = 0;  i < nelem;  i++) {
	    spec->wl[i] = 0.;
	    spec->flux[i] = 0.;
	    spec->error[i] = 0.;
	    spec->dq[i] = 0;
	    spec->weight[i] = 1.;
	}
	spec->wl[nelem] = 0.;

	return (0);
}
