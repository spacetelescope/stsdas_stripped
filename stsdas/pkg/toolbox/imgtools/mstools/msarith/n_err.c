# include <stdio.h>
# include "msarith.h"




/*  N_ERR: Error handling routines.



    Revision history:
    ---------------
    01 Mar 96  -  Implementation (IB)

*/

void syntax_err(char *message) {
        printf ("*** SYNTAX ERROR: %s\n", message);
        fflush(stdout);
}

void n_filerr (char *name) {
	sprintf (ErrText, "Cannot access file %s (HSTIO)", name);
	n_error (ErrText);
}

void n_warn (char *message) {
	printf ("*** WARNING: %s\n", message);
        fflush(stdout);
}

void n_error (char *message) {
        printf ("*** ERROR: %s\n", message);
        fflush(stdout);
}

void n_message (char *message) {
        printf ("%s", message);
        fflush(stdout);
}
