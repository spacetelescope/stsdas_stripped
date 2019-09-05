/* This file contains:
	void InitSpec		initialize an array of spectra
	int NewSpec		add a new Spectrum to the array
	void FreeSpec		free memory for array of spectra
	void InitSpectrum	initialize a new Spectrum
	void FreeSpectrum	free memory for one Spectrum
*/
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include "splice.h"

# define INCREMENT_NSPEC  10

void InitSpec (SpecArray *spectra) {

	spectra->nspec = 0;
	spectra->max_spec = 0;
	spectra->spec = NULL;
}

int NewSpec (SpecArray *spectra) {

	int new_max_spec;	/* for allocating array of spectra */
	int n;			/* array index for a spectrum */

	if (spectra->max_spec < spectra->nspec + 1) {

	    /* Allocate or reallocate an array of spectra. */
	    new_max_spec = spectra->nspec + INCREMENT_NSPEC;
	    spectra->spec = realloc (spectra->spec,
			new_max_spec * sizeof(Spectrum *));
	    if (spectra->spec == NULL) {
		printf ("out of memory\n");
		return (1);
	    }
	    spectra->max_spec = new_max_spec;
	}

	(spectra->nspec)++;
	n = spectra->nspec - 1;
	spectra->spec[n] = calloc (1, sizeof(Spectrum));

	return (0);
}

void FreeSpec (SpecArray *spectra) {

	int i;
	void FreeSpectrum (Spectrum *);

	if (spectra->spec != NULL) {

	    for (i = 0;  i < spectra->nspec;  i++) {
		FreeSpectrum (spectra->spec[i]);
		free (spectra->spec[i]);
	    }

	    free (spectra->spec);
	}
}

/* This routine initializes a spectrum. */

void InitSpectrum (Spectrum *spec) {

	spec->nelem = 0;
	spec->wl = NULL;
	spec->flux = NULL;
	spec->error = NULL;
	spec->dq = NULL;
	spec->weight = NULL;
	spec->scalar_weight = 1.;
}

/* This routine frees memory associated with a spectrum. */

void FreeSpectrum (Spectrum *spec) {

	spec->nelem = 0;

	if (spec->wl != NULL)
	    free (spec->wl);

	if (spec->flux != NULL)
	    free (spec->flux);

	if (spec->error != NULL)
	    free (spec->error);

	if (spec->dq != NULL)
	    free (spec->dq);

	if (spec->weight != NULL)
	    free (spec->weight);

	spec->scalar_weight = 1.;
}
