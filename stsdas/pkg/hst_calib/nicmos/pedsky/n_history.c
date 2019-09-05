# include <stdio.h>
# include <time.h>
# include "pedsky.h"

/*  N_HISTORY: Appends HISTORY record to image header. The output file
**  inherits the header (except FILENAME) from the input file.
**
**
**
**  Revision history:
**  ---------------
**  H. Bushouse		05-May-1999	Implemented
**
*/

int n_history (TaskInfo *info, SingleNicmosGroup *img, char *outfile) {

/* Arguments:
**	info	i: task info structure
**	img    io: image data
**	outfile	i: output file name
*/

	/* Local variables */
	char   HistText[240];  /* HISTORY record  */
	char   timeStamp[61];  /* time stamp      */
	time_t now;

	/* Function declarations */
	int n_addHistory (Hdr *, char *);

	/* File name is output image's name */
	if (putKeyS (img->globalhdr, "FILENAME", outfile, ""))
	    return (1); 

	/* Assemble time stamp string. */
	now = time (NULL);
	strftime (timeStamp, 60, "%a %H:%M:%S %Z %d-%b-%Y", localtime(&now));

	/* Add HISTORY records. */
	sprintf (HistText, "PEDSKY v%s at %s", VERSION, timeStamp);
	if (n_addHistory (&(img->sci.hdr), HistText))
	    return (1);

	return (0);
}



/* Add one HISTORY record. */

int n_addHistory (Hdr *hdr, char* text) {
 
	addHistoryKw (hdr, text);
	if (hstio_err())
	    return (1);
	return (0);
}

