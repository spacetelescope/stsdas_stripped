# include <stddef.h>
# include <stdlib.h>
# include <string.h>
# include <stdio.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_HISTORY: This file contains routines for saving history info and
** writing it to an image header. These are the functions:
**
** savHist: save a history record in the structure
**
** putHist: write all history records to output image; call clr_hist
**
** clrHist: free memory and reset nhist to zero
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	13-Mar-1998	Fixed bug in savHist to increase length of
**				malloc to "length+1" (Version 3.1.2)
** H.Bushouse	20-Jun-2000	Updated putHist to insert all History records
**				immediately after last current History record
**				in header. Added void to functions with no
**				arguments (Version 4.0)
*/

# define MAX_HISTORY 25		/* Size of array of history records */

int	nhist = 0;		/* current number of history records */
char	*phist[MAX_HISTORY];	/* array of pointers to history records */

/* Save a string so we can write it as history at the appropriate time */

int	savHist (char *history) {

	char *p;		/* temp */
	int length;		/* length of history record */

	if (nhist+1 > MAX_HISTORY) {
	    sprintf (MsgText, "too many history records");
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Note that nhist is the number of records, so it's one indexed */
	nhist++;
	length = strlen (history);
	if ((p = (char *)malloc (length+1)) == NULL) {
	    sprintf (MsgText, "savHist: can't allocate memory");
	    n_error (MsgText);
	    return (status = 1);
	}

	p[0] = '\0';
	strcpy (p, history);
	phist[nhist-1] = p;

	return (status);
}

/* Write all history records to the image header */

int	putHist (Hdr *prihdr) {

	int i;
	int last_pos;
	FitsKw kw;
	void clrHist(void);

	/* Find first existing HISTORY record */
	kw = findKw (prihdr, "HISTORY ");

	/* If there aren't any, add this record to the end of the header */
	if (kw == NotFound) {

	    for (i=0; i < nhist; i++) {
		 addHistoryKw (prihdr, phist[i]);
		 if (hstio_err())
		     return (status = 1);
	    }

	}

	/* else insert it immediately after the last HISTORY record */
	else {

	    /* Find out how many history records already exist */
	    for (i = 0; i < prihdr->nlines; i++) {
		 kw = findnextKw (prihdr, "HISTORY ");
		 if (kw == NotFound) {
		     last_pos = i;
		     break;
		 }
	    }
	    /* Reposition the kw index to the last history record */
	    kw = findKw (prihdr, "HISTORY ");
	    for (i=0; i < last_pos; i++)
		 next (kw);
	    /* Insert the new ones */
	    for (i=0; i < nhist; i++) {
		 insertHistoryKw (kw, phist[i]);
		 if (hstio_err())
		     return (status = 1);
	    }
	}

	/* Clear out the history record buffer */
	clrHist();

	return (status = 0);
}

/* Free memory and reset the number of history records */

void	clrHist (void) {

	int i;

	/* nhist is the number of records, not the index */
	for (i=0; i < nhist; i++)
	     free (phist[i]);

	nhist = 0;

	return;
}
