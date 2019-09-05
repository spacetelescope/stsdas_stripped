# include <stdio.h>
# include <math.h>
# include <float.h>
# include <errno.h>
# include <ximio.h>
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "msstat.h"


/*   E_REPORT -  Computes stats and reports them.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   18 Jun 96  -  Implementation with NO midpt and mode support (IB)
 *   15 Jul 96  -  Midpt and mode computation (IB)
 *   21 Oct 96  -  Revised after code review (IB)
 *   06 Mar 97  -  Modified definition of weighted variance (IB)
 *   03 Jun 10  -  Fixed bug caused by midpt and mode not always being
 *		   initialized (HAB)
 *
 */

void e_report (Control *con, int hdu, int ncol, Bool hist) {

	IRAFPointer pp;
	char        outstr[SZ_OUTLINE];
	char        str[SZ_STR], str1[SZ_FNAME];
	long        npix;
	int         i;
	double	    dmean, dstddev;
	float       mean, stddev, skew, kurt;
	float       wmean, wvar, daux1, daux2;
	float       midpt, mode;
	float       aux;

	void e_strExtract (int, char *, char *);
	int  e_midmod (Control *, int, float *, float *);
	void e_buildNames (Control *, HDUType, char *);

	/* Temporary replacement for c_clppsetf */
	void l_clppsetf(IRAFPointer, char *, float);

	/* If accumulator is empty, return at once. */
	if (con->accum[hdu].npix == 0L)
	    return;
	else
	    npix = con->accum[hdu].npix;

	/* Compute stats. */
	dmean = con->accum[hdu].sum / (double)npix;
	mean  = (float)dmean;
	if (npix > 1L) {
	    dstddev = (con->accum[hdu].sum2 - con->accum[hdu].sum * mean) /
                      (double)(npix - 1L);
	    if (dstddev < 0.0)
	        stddev = E_INDEF;
	    else {
	        errno = 0;
	        dstddev = sqrt (dstddev);
	        if (errno)
	            stddev = E_INDEF;
	        else
	            stddev = (float)dstddev;
	    }
	} else
	    stddev = E_INDEF;
	con->accum[hdu].stddev = stddev;

	daux1 = dmean * dmean;
	if (stddev != E_INDEF)
	    daux2 = dstddev * dstddev;
	if ((con->accum[hdu].sum3 > 0.0) && (stddev != E_INDEF)) {
	    skew = (con->accum[hdu].sum3 - 3.0 * dmean *
	            con->accum[hdu].sum2 + 3.0 * daux1 *
		    con->accum[hdu].sum  - (double)npix * daux1 * dmean)/
		    ((double)npix * daux2 * dstddev);
	} else
	    skew = E_INDEF;
	if ((con->accum[hdu].sum4 > 0.0) && (stddev != E_INDEF)) {
	    kurt = (con->accum[hdu].sum4 - 4.0 * dmean *
	            con->accum[hdu].sum3 + 6.0 * daux1 *
	            con->accum[hdu].sum2 - 4.0 * daux1 * dmean * 
	            con->accum[hdu].sum + (double)npix * daux1 * daux1) / 
                    ((double)npix * daux2 * daux2) - 3.0;
	} else
	    kurt = E_INDEF;
	if (con->accum[hdu].sumww > 0.0) {
	    wmean = (float) (con->accum[hdu].sumxw / con->accum[hdu].sumww);
	    wvar  = (float) (1.0 / (1.0/npix * con->accum[hdu].sumww));
	} else {
	    wmean = E_INDEF;
	    wvar  = E_INDEF;
	}

	/* Histogram-related quantities. This must be called
         * only after the above sttdev computation. 
         */
	midpt = E_INDEF;
	mode  = E_INDEF;
	if (hist) {
	    if (e_midmod (con, hdu, &midpt, &mode)) {
	        midpt = E_INDEF;
	        mode  = E_INDEF;
	    }
	}

	/* Begin by storing file name at output line's start. 
	 * File name is assembled in auxiliary str1 string and
	 * printed into output line with fixed length. This length
	 * is in turn given by a task parameter. 
         */
	e_buildNames (con, con->hdu[hdu], str1);
	sprintf (str, "%%-%d.%ds", ncol, ncol);
	sprintf (outstr, str, str1);

	/* Append stats into output line. */
	for (i = 0; i < con->nstats; i++) {

	    switch (con->stats[i]) {
	        case NPIX:   aux = (float)npix;                break;
	        case MIN:    aux = (float)con->accum[hdu].min; break;
	        case MAX:    aux = (float)con->accum[hdu].max; break;
	        case SUM:    aux = (float)con->accum[hdu].sum; break;
	        case MEAN:   aux = mean;                       break;
	        case STDDEV: aux = stddev;                     break;
	        case SKEW:   aux = skew;                       break;
	        case KURT:   aux = kurt;                       break;
	        case WMEAN:  aux = wmean;                      break;
	        case WVAR:   aux = wvar;                       break;
	        case MIDPT:  aux = midpt;                      break;
	        case MODE:   aux = mode;                       break;
	        default:     aux = E_INDEF;                    break;
	    }

	    /* This does not work properly and causes the task to bomb
             * out depending on the value of E_INDEF.
	    if (aux == E_INDEF)
	        sprintf (str, "  INDEF   ");
	    else
	        sprintf (str, "%11.6g ", aux);
	    strcat (outstr, str);                   */

	    /* Temporary replacement code for the above. This works with
             * E_INDEF = 0 and causes confusion between indefinite and zero 
             * in the output report, but is robust. 
             */
	    sprintf (str, "%11.6g ", aux);
	    strcat (outstr, str);
	}

	/* Print at STDOUT */
	strcat (outstr, "\n");
	e_message (outstr);

# if defined(NATIVE_IRAF)

	/* Output CL parameters. */
	if (con->hdu[hdu] == con->cl_hdu) {
	    pp = c_clopset (OUT_PSET);
	    c_clppseti (pp, "npix",      (int)npix);
	    l_clppsetf (pp, "min",       (float)con->accum[hdu].min);
	    l_clppsetf (pp, "max",       (float)con->accum[hdu].max);
	    l_clppsetf (pp, "sum",       (float)con->accum[hdu].sum);
	    l_clppsetf (pp, "mean",      mean);
	    l_clppsetf (pp, "stddev",    stddev);          
	    l_clppsetf (pp, "histmidpt", midpt);    
	    l_clppsetf (pp, "histpeak",  mode);     
	    l_clppsetf (pp, "skew",      skew);
	    l_clppsetf (pp, "kurt",      kurt);
	    l_clppsetf (pp, "wmean",     wmean);
	    l_clppsetf (pp, "wvar",      wvar);
	    c_clcpset (pp);
	} 
# endif
}
