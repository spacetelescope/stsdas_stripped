# include <stdio.h>
# include <c_iraf.h>
# include "echscript.h"

/*****************************************************************************
 *
 * ERRORS: Error handling routines.
 *
 * Author:  Ivo Busko (MDD borrowed and abridged from IB code.)
 * Date:    24 September 1997
 * Mods:    04 December 1998 - MDD: e_echscript.h -> echscript.h
 *
******************************************************************************/

void e_warn (char *message) {
	printf ("*** WARNING: %s\n", message);
        fflush (stdout);
}

void e_error (char *message) {
        printf ("*** ERROR: %s\n", message);
        fflush (stdout);
}

void e_message (char *message) {
        printf ("%s", message);
        fflush (stdout);
}
