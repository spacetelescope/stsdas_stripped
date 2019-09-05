# include <stdio.h>
# include <time.h>
# include "nicmos.h"

/*   N_LIB  --  General-purpose functions:
**
**   - assemble time stamp 
**   - messaging at stdout
**
**
**    Revision history:
**    ----------------
**    20 May 98  -  Implementation (IB).
**    27 Sep 2005 - Added cv_errchk routine (H.Bushouse)
**
*/


/*  Assemble a time stamp string. */

void n_timeStamp (char *time_stamp) {

	time_t  now;

	now = time (NULL);
	strftime (time_stamp, SZ_TIMESTAMP-1, "%a %H:%M:%S %Z %d-%b-%Y",
		  localtime(&now));
}


/* 
**   Routines for handling messaging at stdout. n_message does NOT append
**   a newline because it is used for fancier output formatting.
*/

void n_message (char *message) {
        printf ("%s", message);
        fflush(stdout);
}
void n_warn (char *message) {
	printf ("*** WARNING: %s\n", message);
        fflush(stdout);
}
void n_error (char *message) {
        printf ("*** ERROR: %s\n", message);
        fflush(stdout);
}
void n_IRAFerror (void) {
        printf ("*** IRAF ERROR: %s (%d)\n", c_iraferrmsg(), c_iraferr());
        fflush(stdout);
}

void errchk(void) {
	if (hstio_err()) {
	    fprintf (stdout, "*** ERROR in HST I/O functions:%s\n",
		     hstio_errmsg());
	    fflush (stdout);
	}
}

void cv_errchk(void) {
	if (c_iraferr()) {
	    fprintf (stdout, 
        "*** ERROR in CVOS function: IRAF code %d\nIRAF message: %s\n",
	    c_iraferr(), c_iraferrmsg());
	    fflush (stdout);
	}
}
