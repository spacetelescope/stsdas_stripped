# include <stdio.h>
# include <time.h>
# include "nbadpix.h"


/*   E_LIB  --  General-purpose functions:
 *
 *   - assemble time stamp 
 *   - messaging at stdout
 *
 *
 *
 *
 *    Revision history:
 *    ----------------
 *    02 Aug 96  -  Implementation (IB).
 *
 */




/*  Assemble a time stamp string. */

void b_timeStamp (char *time_stamp) {

	time_t  now;

	now = time (NULL);
	strftime (time_stamp, SZ_TIMESTAMP-1, "%a %H:%M:%S %Z %d-%b-%Y",
		  localtime(&now));
}




/* 
 *   Routines for handling messaging at stdout. b_message does NOT append
 *   a newline because it is used for fancier output formatting.
 */

void b_message (char *message) {
        printf ("%s", message);
        fflush(stdout);
}
void b_warn (char *message) {
	printf ("*** WARNING: %s\n", message);
        fflush(stdout);
}
void b_error (char *message) {
        printf ("*** ERROR: %s\n", message);
        fflush(stdout);
}
void b_IRAFerror () {
        printf ("*** IRAF ERROR: %s (%d)\n", c_iraferrmsg(), c_iraferr());
        fflush(stdout);
}

