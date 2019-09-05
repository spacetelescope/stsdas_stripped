# include <math.h>
# include <stdio.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

/* N_STATCALC: Compute statistics for NICMOS science images. The min, max,
** mean and standard deviation of "good" (unflagged) pixels are computed
** for the entire image, as well as each quadrant. A tally of the
** number of pixels with a each DQ flag is also computed. All results are
** written to keywords in the science image extension header. This
** routine does NOT modify the input image data in any way.
**
** Revision history:
** H.Bushouse	Oct. 1996	Build 1
** H.Bushouse	09-Mar-1998	Added computation of median in n_stats; moved
**				sort routine in from n_imstat (Version 2.2)
** H.Bushouse	06-May-1998	Fixed bug in use of sort routine for computing
**				medians - must pass arr as "arr-1" because
**				Num.Rec. routines assume 1-indexed arrays
**				(Version 2.2)
** H.Bushouse	09-Feb-1999	Updated use of putKey routines for HSTIO v2.1
**				(Version 2.2.1)
*/

int n_statcalc (SingleNicmosGroup *input) {

/* Arguments:
**	input	io: input image
*/

	/* Local variables */
	int nflags[13];				/* flag counter array */
	double mean, median, stdv, min, max;	/* statistics values */
	int nx, ny;				/* image x,y dimensions */

	/* Function definitions */
	void n_nflags (SingleNicmosGroup *, int *);
	int n_stats (SingleNicmosGroup *, int, int, int, int, double *, 
		     double *, double *, double *, double *);

	/* What are the image dimensions? */
	nx = input->sci.data.nx;
	ny = input->sci.data.ny;

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

	/* Calculate statistics for the entire SCI image */
	if (n_stats (input, 0, nx-1, 0, ny-1, &mean, &median, &stdv, &min,
	    &max))
	    return (status);

	/* Store results in header keywords */
	if (putKeyF (&input->sci.hdr, "GOODMEAN", (float)mean, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "GOODMEDN", (float)median, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "GOODSTDV", (float)stdv, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "GOODMIN",  (float)min, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "GOODMAX",  (float)max, ""))
	    return (status = 1);

	/* Calculate statistics for SCI image quadrant A */
	if (n_stats (input, 0, nx/2-1, 0, ny/2-1, &mean, &median, &stdv, &min,
		     &max))
	    return (status);

	/* Store results in header keywords */
	if (putKeyF (&input->sci.hdr, "QAMEAN", (float)mean, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QAMEDN", (float)median, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QASTDV", (float)stdv, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QAMIN",  (float)min,  ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QAMAX",  (float)max,  ""))
	    return (status = 1);

	/* Calculate statistics for SCI image quadrant B */
	if (n_stats (input, 0, nx/2-1, ny/2, ny-1, &mean, &median, &stdv, &min,
		     &max))
	    return (status);

	/* Store results in header keywords */
	if (putKeyF (&input->sci.hdr, "QBMEAN", (float)mean, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QBMEDN", (float)median, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QBSTDV", (float)stdv, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QBMIN",  (float)min,  ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QBMAX",  (float)max,  ""))
	    return (status = 1);

	/* Calculate statistics for SCI image quadrant C */
	if (n_stats (input, nx/2, nx-1, ny/2, ny-1, &mean, &median, &stdv, &min,
		     &max))
	    return (status);

	/* Store results in header keywords */
	if (putKeyF (&input->sci.hdr, "QCMEAN", (float)mean, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QCMEDN", (float)median, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QCSTDV", (float)stdv, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QCMIN",  (float)min,  ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QCMAX",  (float)max,  ""))
	    return (status = 1);

	/* Calculate statistics for SCI image quadrant D */
	if (n_stats (input, nx/2, nx-1, 0, ny/2-1, &mean, &median, &stdv, &min,
		     &max))
	    return (status);

	/* Store results in header keywords */
	if (putKeyF (&input->sci.hdr, "QDMEAN", (float)mean, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QDMEDN", (float)median, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QDSTDV", (float)stdv, ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QDMIN",  (float)min,  ""))
	    return (status = 1);
	if (putKeyF (&input->sci.hdr, "QDMAX",  (float)max,  ""))
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
	short dqVal;		/* DQ value */
	short fval[12] = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048};
				/* List of flag values to count */

	/* Initialize the flag counter array */
	for (k=0; k<13; k++)
	     nflags[k] = 0;

	/* Loop through the DQ image, counting the pixels with flags */
	for (j=0; j<in->dq.data.ny; j++) {
	     for (i=0; i<in->dq.data.nx; i++) {
		  dqVal = DQPix(in->dq.data,i,j);
		  if (dqVal == 0) {
		      nflags[0]++;
		  } else {
		      for (k=0; k<12; k++) {
			   if (dqVal & fval[k])
			       nflags[k+1]++;
		      }
		  }
	     }
	}

}

/* N_STATS: Compute mean, median, stdv, min, max of unflagged pixels
** in a NICMOS SCI image. */

int n_stats (SingleNicmosGroup *in, int x1, int x2, int y1, int y2,
	     double *mean, double *median, double *stdv, double *min,
	     double *max) {

/* Arguments:
**	in	i: input image
**	x1, x2	i: column limits for statistics calculation
**	y1, y2	i: row limits for statistics calculation
**	mean	o: mean value of unflagged pixels
**	median	o: median value of unflagged pixels
**	stdv	o: standard deviation of unflagged pixels
**	min	o: minimum unflagged pixel value
**	max	o: maximum unflagged pixel value
*/

	/* Local variables */
	int i, j;		/* loop indexes */
	int ngood;		/* number of unflagged pixels in image */
	float *arr;		/* array of good pixel values */
	double val;		/* SCI pixel value */
	double sumx, sumx2;	/* sum and sum**2 of unflagged pixel values */

	/* Function definitions */
	int sort (unsigned long, float *);

	/* Initialize the counters and results */
	ngood   = 0;
	sumx    = 0;
	sumx2   = 0;
	*mean   = 0;
	*median = 0;
	*stdv   = 0;
	*min    = 0;
	*max    = 0;

	/* Allocate memory for the temporary array */
	arr = (float *) calloc(in->sci.data.nx*in->sci.data.ny, sizeof(float));
	if (arr == NULL) {
	    sprintf (MsgText, "Memory allocation failure in n_stats");
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Loop through the requested image section, computing the
	** sum, sum of the squares, min, max and number of unflagged
	** pixels. */
	for (j=y1; j<=y2; j++) {
	     for (i=x1; i<=x2; i++) {
		  if (DQPix(in->dq.data,i,j) == 0) {
		      val = Pix(in->sci.data,i,j);
		      arr[ngood] = val;
		      sumx  += val;
		      sumx2 += val*val;
		      ngood++;
		      if (ngood == 1) {
			  *min = val;
			  *max = val;
		      } else {
			  if (val < *min) *min = val;
			  if (val > *max) *max = val;
		      }
		  }
	     }
	}

	/* Compute the mean */
	if (ngood > 0) {
	    *mean = sumx / ngood;
	}

	/* Compute the median */
	if (ngood > 0) {

	    /* Sort the good values */
	    if (sort((unsigned long)ngood, arr-1)) {
		free (arr);
		return (status);
	    }

	    /* Find the median value */
	    *median = arr[(int)ngood/2];
	}

	/* Compute the standard deviation */
	if (ngood > 1) {
	    *stdv = ngood/(ngood-1.) * (sumx2/ngood - (*mean)*(*mean));
	    if (*stdv >= 0)
		*stdv = sqrt (*stdv);
	    else
		*stdv = 0.0;
	}

	/* Free local memory */
	free (arr);

	/* Successful return */
	return (status = 0);

}

#define NR_END 1
#define FREE_ARG char*
#define SWAP(a,b) temp=(a);(a)=(b);(b)=temp;
#define M 7
#define NSTACK 34
 
int sort(unsigned long n, float arr[]) {
 
        unsigned long i, ir=n, j, k, l=1;
        int jstack=0, *istack;
        float a, temp;
 
        int *ivector (long nl, long nh);
        void free_ivector (int *v, long nl);
 
        istack = ivector(1,NSTACK);
        if (istack == NULL) return (status=1);
        for (;;) {
                if (ir-l < M) {
                        for (j=l+1; j<=ir; j++) {
                                a = arr[j];
                                for (i=j-1; i>=l; i--) {
                                        if (arr[i] <= a) break;
                                        arr[i+1] = arr[i];
                                }
                                arr[i+1] = a;
                        }
                        if (jstack == 0) break;
                        ir = istack[jstack--];
                        l = istack[jstack--];
                } else {
                        k = (l+ir) >> 1;
                        SWAP(arr[k],arr[l+1])
                        if (arr[l] > arr[ir]) {
                                SWAP(arr[l],arr[ir])
                        }
                        if (arr[l+1] > arr[ir]) {
                                SWAP(arr[l+1],arr[ir])
                        }
                        if (arr[l] > arr[l+1]) {
                                SWAP(arr[l],arr[l+1])
                        }
                        i = l+1;
                        j = ir;
                        a = arr[l+1];
                        for (;;) {
                                do i++; while (arr[i] < a);
                                do j--; while (arr[j] > a);
                                if (j < i) break;
                                SWAP(arr[i],arr[j]);
                        }
                        arr[l+1] = arr[j];
                        arr[j] = a;
                        jstack += 2;
                        if(jstack > NSTACK) {
                           sprintf (MsgText, "NSTACK too small in sort");
                           n_error (MsgText);
                           return (status = 1);
                        }
 
                        if (ir-i+1 >= j-l) {
                                istack[jstack] = ir;
                                istack[jstack-1] = i;
                                ir = j-1;
                        } else {
                                istack[jstack] = j-1;
                                istack[jstack-1] = l;
                                l = i;
                        }
 
                }
        }
        free_ivector(istack,1);
        return (status = 0);
}

#undef M
#undef NSTACK
#undef SWAP
 
/* allocate an int vector with subscript range v[nl..nh] */
 
int *ivector(long nl, long nh) {
        int *v;
 
        v=(int *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(int)));
        if (!v) {
            sprintf (MsgText, "Memory allocation failure in ivector()");
            n_error (MsgText);
            return (NULL);
        }
        return v-nl+NR_END;
}
 
/* free an int vector allocated with ivector() */
 
void free_ivector(int *v, long nl) {
        free((FREE_ARG) (v+nl-NR_END));
}
 
