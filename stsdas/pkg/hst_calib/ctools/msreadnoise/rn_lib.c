# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <time.h>
# include "readnoise.h"


/*   RN_LIB  --  General-purpose functions:
 *
 *   - alloc/free memory
 *   - assemble time stamp 
 *   - messaging at stdout
 *
 *
 *
 *
 *    Revision history:
 *    ----------------
 *    29 Oct 96  -  Implementation (IB).
 *
 */


/*  Alloc memory. */

int rn_allocMemory (Image *img, Algorithm *alg) {

	char *msg = "Cannot allocate memory.";
	int   i;

	/* First, alloc the "buffer" 2-D storage area. */
	alg->buffer = (float **) malloc (img->blkSize * sizeof(float *));
	if (alg->buffer == NULL) {
	    rn_error (msg);
	    return (1);
	}
	for (i = 0; i < img->blkSize; i++) {
	    alg->buffer[i] = (float *) malloc (img->nimages * sizeof(float));
	    if (alg->buffer[i] == NULL) {
	        rn_error (msg);
	        return (1);
	    }
	}

	/* Now the simpler "rnoise". */
	alg->rnoise = (float *) malloc (img->xsize*img->ysize*sizeof(float));
	if (alg->rnoise == NULL) {
	    rn_error (msg);
	    return (1);
	}

	return (0);
}



/*  Free memory. */

void rn_freeMemory (Image *img, Algorithm *alg) {

	int   i;

	for (i = 0; i < img->blkSize; i++) {
	    if (alg->buffer[i] != NULL)
	        free (alg->buffer[i]);
	}
	if (alg->buffer != NULL)
	    free (alg->buffer);
	if (alg->rnoise != NULL)
	    free (alg->rnoise);
}



/*  Assemble a time stamp string. */

void rn_timeStamp (char *time_stamp) {

	time_t  now;

	now = time (NULL);
	strftime (time_stamp, SZ_TIMESTAMP-1, "%a %H:%M:%S %Z %d-%b-%Y",
		  localtime(&now));
}




/* 
 *   Routines for handling messaging at stdout. rn_message does NOT append
 *   a newline because it is used for fancier output formatting.
 */

void rn_message (char *message) {
        printf ("%s", message);
        fflush(stdout);
}
void rn_warn (char *message) {
	printf ("*** WARNING: %s\n", message);
        fflush(stdout);
}
void rn_error (char *message) {
        printf ("*** ERROR: %s\n", message);
        fflush(stdout);
}
void rn_IRAFerror () {
        printf ("*** IRAF ERROR: %s (%d)\n", c_iraferrmsg(), c_iraferr());
        fflush(stdout);
}

