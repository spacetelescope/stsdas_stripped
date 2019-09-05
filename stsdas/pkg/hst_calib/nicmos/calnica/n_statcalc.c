# include <stdio.h>
# include <stdlib.h>
# include <math.h>
# include <float.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_STATCALC: Compute statistics for NICMOS science images. The min, max,
** mean and standard deviation of "good" (unflagged) pixels are computed
** for the entire image, as well as each 128x128 quadrant. A tally of the
** number of pixels with a each DQ flag is also computed. All results are
** written to keywords in the science image extension header. This
** routine does NOT modify the input image data in any way.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	09-Mar-1998	Added computation of median to n_stats; added
**				sort routine for use in computing medians
**				(Version 3.2)
** H.Bushouse	17-Apr-1998	Fixed bug in use of sort routine for computing
**				medians - must pass arr as "arr-1" because
**				Num.Rec. routines assume 1-indexed arrays
**				(Version 3.2)
** H.Bushouse	07-Oct-1998	Removed nh argument from free_ivector
**				definition, call, and routine (Version 3.3)
** H.Bushouse	09-Feb-1999	Updated use of putKey routines for HSTIO v2.1
**				(Version 3.2.2)
** H.Bushouse	16-Jun-2000	Updated n_stats routine to newer version that
**				uses low/high and bitmask rejection and moved
**				n_stats to an external module. Removed sort,
**				ivector, and free_ivector routines, as they
**				are now contained in the new n_numrec.c module.
**				Added new insertKeyF routine (Version 4.0)
*/

int n_statcalc (SingleNicmosGroup *input) {

/* Arguments:
**	input	io: input image
*/

	/* Local variables */
	short bitmask;				     /* DQ mask value */
	int nflags[17];				     /* flag counter array */
	float mean, median, mode, stdv, min, max;    /* statistics values */

	/* Function definitions */
	void n_nflags (SingleNicmosGroup *, int *);
	int n_stats (SingleNicmosGroup *, int, int, int, int, float, float,
		     short, float *, float *, float *, float *, float *,
		     float *);
	int insertKeyF (Hdr *, char *, float, char *, char *);

	/* Count the number of pixels with each DQ flag value */
	n_nflags (input, nflags);

	/* Store results in header keywords */
	if (putKeyI (&input->sci.hdr, "NQUAL00", nflags[0], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL01", nflags[1], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL02", nflags[2], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL03", nflags[3], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL04", nflags[4], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL05", nflags[5], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL06", nflags[6], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL07", nflags[7], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL08", nflags[8], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL09", nflags[9], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL10", nflags[10], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL11", nflags[11], ""))
	    return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL12", nflags[12], ""))
	    return (status = 1);
        if (putKeyI (&input->sci.hdr, "NQUAL13", nflags[13], ""))
            return (status = 1);
        if (putKeyI (&input->sci.hdr, "NQUAL14", nflags[14], ""))
            return (status = 1);
        if (putKeyI (&input->sci.hdr, "NQUAL15", nflags[15], ""))
            return (status = 1);
	if (putKeyI (&input->sci.hdr, "NQUAL16", nflags[16], ""))
	    return (status = 1);

	/* Set the bitmask to reject pixels with any flag value */
	bitmask = 32767;

	/* Calculate statistics for the entire SCI image */
	if (n_stats (input, 0, 255, 0, 255, -FLT_MAX, FLT_MAX, bitmask,
		     &mean, &median, &mode, &stdv, &min, &max))
	    return (status = 1);

	/* Store results in header keywords */
	if (putKeyF (&input->sci.hdr, "GOODMEAN", mean, ""))
	    return (status = 1);
	if (insertKeyF (&input->sci.hdr, "GOODMEDN", median,
		       "median of good pixels", "GOODMEAN"))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "GOODSTDV", stdv, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "GOODMIN",  min, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "GOODMAX",  max, ""))
	    return (status = 1);

	/* Calculate statistics for SCI image quadrant A */
	if (n_stats (input, 0, 127, 0, 127, -FLT_MAX, FLT_MAX, bitmask,
		     &mean, &median, &mode, &stdv, &min, &max))
	    return (status = 1);

	/* Store results in header keywords */
	if (putKeyF (&input->sci.hdr, "QAMEAN", mean, ""))
	    return (status = 1);
	if (insertKeyF (&input->sci.hdr, "QAMEDN", median,
		       "median of good pixels in quadrant A", "QAMEAN"))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QASTDV", stdv, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QAMIN",  min,  ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QAMAX",  max,  ""))
	    return (status = 1);

	/* Calculate statistics for SCI image quadrant B */
	if (n_stats (input, 0, 127, 128, 255, -FLT_MAX, FLT_MAX, bitmask,
		     &mean, &median, &mode, &stdv, &min, &max))
	    return (status = 1);

	/* Store results in header keywords */
	if (putKeyF (&input->sci.hdr, "QBMEAN", mean, ""))
	    return (status = 1);
	if (insertKeyF (&input->sci.hdr, "QBMEDN", median,
		       "median of good pixels in quadrant B", "QBMEAN"))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QBSTDV", stdv, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QBMIN",  min,  ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QBMAX",  max,  ""))
	    return (status = 1);

	/* Calculate statistics for SCI image quadrant C */
	if (n_stats (input, 128, 255, 128, 255, -FLT_MAX, FLT_MAX, bitmask,
		     &mean, &median, &mode, &stdv, &min, &max))
	    return (status = 1);

	/* Store results in header keywords */
	if (putKeyF (&input->sci.hdr, "QCMEAN", mean, ""))
	    return (status = 1);
	if (insertKeyF (&input->sci.hdr, "QCMEDN", median,
		       "median of good pixels in quadrant C", "QCMEAN"))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QCSTDV", stdv, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QCMIN",  min,  ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QCMAX",  max,  ""))
	    return (status = 1);

	/* Calculate statistics for SCI image quadrant D */
	if (n_stats (input, 128, 255, 0, 127, -FLT_MAX, FLT_MAX, bitmask,
		     &mean, &median, &mode, &stdv, &min, &max))
	    return (status = 1);

	/* Store results in header keywords */
	if (putKeyF (&input->sci.hdr, "QDMEAN", mean, ""))
	    return (status = 1);
	if (insertKeyF (&input->sci.hdr, "QDMEDN", median,
		       "median of good pixels in quadrant D", "QDMEAN"))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QDSTDV", stdv, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QDMIN",  min,  ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QDMAX",  max,  ""))
	    return (status = 1);

	/* Successful return */
	return (status = 0);
}

/* N_NFLAGS: Count the number of pixels assigned each DQ flag value. */

void n_nflags (SingleNicmosGroup *in, int *nflags) {

/* Arguments:
**	in	i: input image
**	nflags	o: flag counter array
*/

	/* Local variables */
	int i, j, k;		/* loop indexes */
	unsigned short dqVal;		/* DQ value */
	unsigned short fval[16] = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512,
                                   1024, 2048, 4096, 8192, 16384, 32768};
				/* List of flag values to count */

	/* Initialize the flag counter array */
	for (k=0; k<17; k++)
	     nflags[k] = 0;

	/* Loop through the DQ image, counting the pixels with flags */
	for (j=0; j<in->dq.data.ny; j++) {
	     for (i=0; i<in->dq.data.nx; i++) {
		  dqVal = DQPix(in->dq.data,i,j);
		  if (dqVal == 0) {
		      nflags[0]++;
		  } else {
		      for (k=0; k<16; k++) {
			   if (dqVal & fval[k])
			       nflags[k+1]++;
		      }
		  }
	     }
	}

}

int insertKeyF (Hdr *hdr, char *name, float value, char *comment, char *prvnm) {

	FitsKw kw;

	kw = findKw (hdr, name);

	if (kw == NotFound) {
	    kw = findKw (hdr, prvnm);
	    insertFloatKw (kw, name, value, comment);
	} else {
	    if (putKeyF (hdr, name, value, ""))
		return (status = 1);
	}

	return (status = 0);
}

