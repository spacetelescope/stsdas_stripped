# include <stdio.h>
# include <stdlib.h>
# include <math.h>
 
# include "splice.h"

# define  MAX(x,y)  ((x) > (y) ? (x) : (y))
# define  MIN(x,y)  ((x) < (y) ? (x) : (y))

/****************************************************************************
 *
 * SumSpectra
 * Routine to rebin input spectral arrays and appropriately weight the
 * data.  This is a prototype only written for use with STIS x1d data. 
 * 
 * Michele D. De La Pena
 * 20 October 1998
 *
 * Phil Hodge, 25 October 1999
 *   Modify for the case that the output spectrum does not cover all of
 *   the input spectra.
 *
 * Phil Hodge, 2014 July 30
 *  Remove DATAREJECT macro; add dq_fill argument.
 *  Add badDQ array; for the case that there is no good input corresponding
 *  to a given output pixel, set DQ in the output based on DQ from the input,
 *  or use dq_fill if there is no corresponding input at all.
 *
 ***************************************************************************/

int
SumSpectra (SpecArray *spectra, short sdqflags, int dq_fill,
            Spectrum *outspec) {

    int i, j, k;		/* Generic indices                     */
    double dlambda;		/* Portion of pixel in bin (angstroms) */
    double *totWFlux   = NULL;	/* Weighted flux accumulator           */
    double *totWError2 = NULL;	/* Weighted error squared accumulator  */
    double *totWeight  = NULL;	/* Weight accumulator                  */
    short  *totDQ      = NULL;	/* DQ accumulator                      */
    short  *badDQ      = NULL;	/* DQ accumulator for rejected input   */
    double flux, error, weight; /* values for a bin in input spectrum  */
    short dq;
    int reject;                 /* true if input is flagged as bad     */
    double err_tmp;             /* error * weight * dlambda            */
    double wl_low, wl_high;     /* overlap between input & output bins */
    int k1, k2;                 /* range of output bins for one input  */

    /* Initialize the output components for the final combined spectrum */
    for (i = 0; i < (outspec->nelem); i++) {
        *(outspec->flux+i)   = 0.0;
        *(outspec->error+i)  = 0.0;
        *(outspec->dq+i)     = 0;
        *(outspec->weight+i) = 0.0;
    }

    /* Allocate and initialize the accumulators */
    totWFlux   = (double *) calloc (outspec->nelem, sizeof (double));
    totWError2 = (double *) calloc (outspec->nelem, sizeof (double));
    totWeight  = (double *) calloc (outspec->nelem, sizeof (double));
    totDQ      = (short *)  calloc (outspec->nelem, sizeof (short));
    badDQ      = (short *)  calloc (outspec->nelem, sizeof (short));
    if (totWFlux == NULL || totWError2 == NULL || totWeight == NULL ||
        totDQ == NULL || badDQ == NULL) {
        printf ("*** ERROR: Not enough memory to allocate accumulators.\n");
        return (1);
    }
    for (i = 0;  i < outspec->nelem;  i++) {
        totWFlux[i] = 0.;
        totWError2[i] = 0.;
        totWeight[i] = 0.;
        totDQ[i] = 0;
        badDQ[i] = 0;
    }

    /* Loop over each input spectrum */
    for (i = 0; i < (spectra->nspec); i++) {

        /* Initialize index for current output bin */
        k = 0;

        /* Combine the input weight and the scaling factor */
        for (j = 0; j < (spectra->spec[i]->nelem); j++) 
            *(spectra->spec[i]->weight+j) *= spectra->spec[i]->scalar_weight;

        /* Loop over each bin in an individual input spectrum */
        reject = 0;                     /* boolean */
        for (j = 0; j < (spectra->spec[i]->nelem); j++) {

            /* Determine if the input bin should be used based upon DQ value */
            if (sdqflags & *(spectra->spec[i]->dq+j))
                reject = 1;             /* true, reject current input pixel */
            else
                reject = 0;

            /* Skip this input bin if it does not overlap any output bin. */
            if (*(spectra->spec[i]->wl+j+1) <= *(outspec->wl) ||
                *(spectra->spec[i]->wl+j) >= *(outspec->wl+outspec->nelem))
                continue;

            /* Find the output bin(s) that overlap the current input bin. */

            /* left endpoint of input bin */
            while (k < outspec->nelem &&
                   *(spectra->spec[i]->wl+j) >= *(outspec->wl+k+1))
                k++;
            k1 = k;

            /* right endpoint of input bin */
            while (k < outspec->nelem &&
                   *(spectra->spec[i]->wl+j+1) >= *(outspec->wl+k+1))
                k++;
            k = MIN (k, outspec->nelem - 1);
            k2 = k;

            flux = *(spectra->spec[i]->flux+j);
            error = *(spectra->spec[i]->error+j);
            weight = *(spectra->spec[i]->weight+j);
            dq = *(spectra->spec[i]->dq+j);

            /* Loop over all output bins that overlap current input bin. */
            for (k = k1;  k <= k2;  k++) {

                wl_low  = MAX (*(spectra->spec[i]->wl+j),   *(outspec->wl+k));
                wl_high = MIN (*(spectra->spec[i]->wl+j+1), *(outspec->wl+k+1));
                dlambda = wl_high - wl_low;
                if (dlambda <= 0.)
                    continue;

                if (reject) {

                    /* This will only be used if there is no good input data
                       corresponding to the current output pixel.
                    */
                    *(badDQ+k) |= dq;

                } else {

                    *(totWFlux+k)   += flux * weight * dlambda; 
                    *(totWeight+k)  += weight * dlambda;
                    err_tmp = error * weight * dlambda;
                    *(totWError2+k) += err_tmp * err_tmp;
                    *(totDQ+k) |= dq;
                }
            }

            /* Get ready for next input pixel. */
            k = k2;

        }
    }

    /* Loop over each bin in the accumulators to compute the output */
    for (i = 0; i < (outspec->nelem); i++) {
        if (*(totWeight+i) == 0.0) { 
            *(outspec->flux+i)   = 0.0;
            *(outspec->error+i)  = 0.0;
            *(outspec->weight+i) = 0.0;
            if (*(badDQ+i) == 0) {
                *(outspec->dq+i) = dq_fill;
            } else {
                *(outspec->dq+i) = *(badDQ+i);
            }
        }
        else {
            *(outspec->flux+i)   = *(totWFlux+i) / *(totWeight+i);
            *(outspec->error+i)  = sqrt(*(totWError2+i)) / *(totWeight+i);
            *(outspec->dq+i)     = *(totDQ+i);
            *(outspec->weight+i) = *(totWeight+i);
        }
    }

    /* Clean up */
    free (totWFlux);
    free (totWError2);
    free (totWeight);
    free (totDQ);
    free (badDQ);
   
    return (0);
}
