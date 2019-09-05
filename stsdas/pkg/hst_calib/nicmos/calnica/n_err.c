/* N_ERR: Contains various error handling routines.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	23-Oct-97	Modified n_message, n_warn, n_error routines
**				to handle message strings longer than 80 chars
**				(Version 3.0)
** H.Bushouse	05-Oct-1998	Added n_kwwarn routine (Version 3.3)
** H.Bushouse	20-Jun-2000	Added void to functions with no arguments
**				(Version 4.0)
*/

# include <stdio.h>
# include <string.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */

void errchk(void) {
	if (hstio_err()) {
	fprintf (stdout, "*** ERROR in HST I/O functions:%s\n",
			 hstio_errmsg());
	fflush (stdout);
	status = 1;
	}
}

void n_kwerr (char *keyword, char *file) {
	sprintf (MsgText, "Keyword \"%s\" not found in %s", keyword, file);
	n_error (MsgText);
}

void n_kwwarn (char *keyword, char *file) {
	sprintf (MsgText, "Keyword \"%s\" not found in %s", keyword, file);
	n_warn (MsgText);
}

void n_openerr (char *name) {
	sprintf (MsgText, "Can't open file %s", name);
	n_error (MsgText);
}

void n_readerr (char *name) {
	sprintf (MsgText, "Can't read file %s", name);
	n_error (MsgText);
}

void n_filerr (char *name) {
	sprintf (MsgText, "while trying to read file %s", name);
	n_error (MsgText);
}

void n_message (char *message) {
	int n;
	n = strlen(message);
	if (n <= 80)
	    printf ("%s\n", message);
	else {
	    printf ("%80.80s\n", message);
	    printf ("%s\n", message+80);
	}
        fflush(stdout);
}

void n_warn (char *message) {
	int n;
	n = strlen(message);
	if (n <= 67)
	    printf ("*** WARNING: %s\n", message);
	else {
	    printf ("*** WARNING: %67.67s\n", message);
	    printf ("             %s\n", message+67);
	}
        fflush(stdout);
}

void n_error (char *message) {
	int n;
	n = strlen(message);
	if (n <= 69)
            printf ("*** ERROR: %s\n", message);
	else {
	    printf ("*** ERROR: %69.69s\n", message);
	    printf ("           %s\n", message+69);
	}
        fflush(stdout);
}
