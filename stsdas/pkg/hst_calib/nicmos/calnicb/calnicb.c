# include <stdio.h>
# include <stdlib.h>
# include <string.h>

/* CALNICB: CALNICB driver. Retrieves input table name and calls the
** main CALNICB pipeline routine.
**
** Revision history:
** H.Bushouse	April 1996	Build 1
** H.Bushouse	March 1997	Declare main routine to be type int; include
**				stdlib.h (Version 0.1.5)
** H.Bushouse	01-Dec-1997	Added subbkg, usemeanbkg, crthresh, and xcwin
**				as command-line arguments; increased size of
**				MsgText from 81	to 161 (Version 2.2)
** H.Bushouse	17-Mar-1998	Added readbkg and readoffs as command-line
**				arguments (Version 2.2)
*/

int	status;		/* status flag; value of zero indicates OK */
char	MsgText[161];	/* error message text */

int main(int argc, char **argv) {

	/* Local variables */
	char in_file[81];		/* input file name */
	char subbkg[4];			/* subtract background flag */
	char usemeanbkg[4];		/* mean background flag */
	char readbkg[4];		/* read bkg values flag */
	char readoffs[4];		/* read image offsets flag */
	float crthresh;			/* CR rejection threshold */
	int  xcwin;			/* xcorr window search half width */
	int i;				/* loop index */

	/* Function definitions */
	void c_irafinit (int, char **);
	int n_calnicb (char *, char *, char *, char *, char *, float, int);

	/* Initialize status to OK and MsgText to null */
	status = 0;
	MsgText[0] = '\0';

	/* Initialize IRAF environment */
	c_irafinit(0, 0);

	/* Get command line arguments: 
	**         1. input file name
	**         2. subtract scalar background flag
	**         3. mean background flag
	**         4. read bkg values from assoc table flag
	**         5. read image offsets from assoc table flag
	**         6. CR rejection threshold
	**         7. xcorr window half width
	*/
	in_file[0]    = '\0';
	subbkg[0]     = '\0';
	usemeanbkg[0] = '\0';
	readbkg[0]    = '\0';
	readoffs[0]   = '\0';
	crthresh      = 0;
	xcwin         = 0;

	if (argc > 1) {
	    for (i = 1; i < argc; i++) {
		 if (i == 1)
		     strcpy (in_file, argv[i]);
		 else if (i == 2)
		     strcpy (subbkg, argv[i]);
		 else if (i == 3)
		     strcpy (usemeanbkg, argv[i]);
		 else if (i == 4)
		     strcpy (readbkg, argv[i]);
		 else if (i == 5)
		     strcpy (readoffs, argv[i]);
		 else if (i == 6)
		     crthresh = atof (argv[i]);
		 else if (i == 7)
		     xcwin = atoi (argv[i]);
		 else {
		     printf ("Too many command line arguments\n");
		     exit (99);
		 }
	    }
	} else {
	    printf ("Wrong syntax: calnicb input\n");
	    exit (99);
	}

	/* Call the CALNICB main program */
	if (n_calnicb (in_file, subbkg, usemeanbkg, readbkg, readoffs,
		       crthresh, xcwin)) {

	    /* Error during processing */
	    printf ("CALNICB processing NOT completed for %s\n", 
		     in_file);
	    exit (99);
	}

	/* Successful completion */
	printf ("CALNICB successful completion for %s\n", in_file);

	/* Exit the program */
	exit (0);
}
