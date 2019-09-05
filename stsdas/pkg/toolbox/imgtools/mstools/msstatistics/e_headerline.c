# include <stdio.h>
# include "msstat.h"


/*  E_HEADERLINE  --  Prints header line at STDOUT.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   18 Jun 96  -  Implementation (IB)
 *   21 Oct 96  -  Revised after code review (IB)
 *
 */

void e_headerLine (Control *con, int ncol) {

	char          outstr[SZ_OUTLINE];
	char   str[SZ_STR], str1[SZ_STR];
	int                            i;

	void e_strExtract (int, char *, char *);

	sprintf (outstr, "# image ");
	if (ncol > 9) {
	    for (i = 0; i < ncol-9; i++) 
	        strcat (outstr, " ");
	}

	for (i = 0; i < con->nstats; i++) {
	    e_strExtract ((int)con->stats[i]+2, str, STAT_TYPES);
	    sprintf (str1, "%12.12s", str);
	    strcat (outstr, str1);
	}

	strcat (outstr, "\n#\n");
	e_message (outstr);
}
