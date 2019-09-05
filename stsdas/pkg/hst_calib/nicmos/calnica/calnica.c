# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# define True 1
# define False 0

/* CALNICA: CALNICA driver. Retrieves input/output file names
** and calls the main CALNICA pipeline routine.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Oct.  1996	Upgraded for Build 2
** H.Bushouse	11-Sep-1997	Added cr_thresh and zs_thresh as
**				command-line arguments (Version 3.0)
** H.Bushouse	23-Oct-1997	Increased size of MsgText from 81 to 161
**				(Version 3.0)
** H.Bushouse	01-Oct-1998	Added samprej task parameter (Version 3.3)
** H.Bushouse	15-Oct-1998	Added bar_thresh task parameter (Version 3.3)
** R.Jedrzejw   01-Feb-2002     Added the writedark parameter (Version 4.0)
*/

int	status;		/* status flag; value of zero indicates OK */
char	MsgText[161];	/* error message text */

int main(int argc, char **argv) {

	/* Local variables */
	char in_file[129];		/* input file name */
	char out_file[129];		/* output file name */
	float cr_thresh;		/* CR rejection threshold */
	float zs_thresh;		/* Zero-read signal threshold */
	float bar_thresh;		/* Bar detection threshold */
	int writedark;                  /* Write dynamically-generated dark? */
	int samp_rej;			/* Number of samples to reject */
	int i;				/* loop index */
	int loc_write;

	/* Function definitions */
	int n_calnica (char *, char *, float, float, float, int, int);

	/* Initialize status to OK and MsgText to null */
	status = 0;
	MsgText[0] = '\0';

	/* Get command line arguments: 
	**         1. input file name
	**         2. output file name
	**	   3. CR rejection threshold
	**	   4. Zero-read signal threshold
	**	   5. Bar detection threshold
	**	   6. Number of initial samples to reject
        **         7. Write dynamically generated dark?
	*/
	in_file[0] = '\0';
	out_file[0] = '\0';
	cr_thresh = 0.0;
	zs_thresh = 0.0;
	bar_thresh = 0.0;
	samp_rej = 0;
	writedark = False;
	loc_write = 0;
	if (argc > 1) {
	  /* First we have to strip out "-write" if it's there  */
	  for (i=1; i < argc; i++) {
	    if (!strcmp (argv[i], "-write")) {
	      writedark = True;
	      loc_write = i;
	    }
	  }
	  if (loc_write != 0) {
	    for (i=loc_write; i < argc-1; i++) {
		strcpy(argv[i], argv[i+1]);
	    }
	    argc--;
	  }
	    for (i = 1; i < argc; i++) {
		 if (i == 1)
		     strcpy (in_file, argv[i]);
		 else if (i == 2)
		     strcpy (out_file, argv[i]);
		 else if (i == 3)
		     cr_thresh = atof (argv[i]);
		 else if (i == 4)
		     zs_thresh = atof (argv[i]);
		 else if (i == 5)
		     bar_thresh = atof (argv[i]);
		 else if (i == 6)
		     samp_rej = atoi (argv[i]);
		 else {
		     printf ("Too many command line arguments\n");
		     exit (99);
		 }
	    }
	} else {
	    printf ("Wrong syntax: calnica input [output]\n");
	    exit (99);
	}

	/* Call the CALNICA main program */
	if (n_calnica (in_file, out_file, cr_thresh, zs_thresh, bar_thresh,
		       samp_rej, writedark)) {

	    /* Error during processing */
	    printf ("CALNICA processing NOT completed for %s\n", in_file);
	    exit (99);
	}

	/* Successful completion */
	printf ("CALNICA successful completion for %s\n", in_file);

	/* Exit the program */
	exit (0);
}
