# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <ximio.h>
# include <xxtools.h>

/* This file contains:
	NameInit	open image name templates
	NameGio		get next input & output names
*/

/* This routine opens the input and output filename templates,
   and it checks whether the output is just a directory name.
   The function value will be zero if there was no error.

   Phil Hodge, 1999 July 12:  functions created
*/

int NameInit (char *inlist, char *outlist,
	IRAFPointer *in_t, IRAFPointer *out_t, int *dir_only) {

/* arguments:
char *inlist        i: list of input names
char *outlist       i: list of output names, or a directory name
IRAFPointer *in_t   o: image template pointer for input list
IRAFPointer *out_t  o: image template pointer for output list
int *dir_only       o: true if output is a directory name
*/

	char *outdir;	/* scratch for name of output directory */

	/* this declaration is temporary; it will eventually be in xxtools.h */
	int c_isdirectory (char *, char *, int);

	if ((outdir = malloc (IRAF_SZ_LINE * sizeof (char))) == NULL) {
	    fprintf (stderr, "Out of memory.\n");
	    return (111);
	}

	*dir_only = (c_isdirectory (outlist, outdir, IRAF_SZ_LINE) > 0);

	*in_t = c_imtopen (inlist);
	*out_t = c_imtopen (outlist);

	if (!*dir_only) {

	    if (c_imtlen (*in_t) != c_imtlen (*out_t)) {
	        c_imtclose (*in_t);
	        c_imtclose (*out_t);
	        fprintf (stderr,
		"The numbers of input and output files are not the same.\n");
		fflush (stderr);
		return (1);
	    }
	}

	free (outdir);
	return (0);
}

/* This routine gets the next input and output names in the list.
   If the output specified by the user is just a directory name,
   the input file name will be extracted from input by removing any
   bracketed expression on the end (extension and/or image section)
   and then by offsetting past any directory prefix.

   The function value will be IRAF_EOF if there are no more image
   names in the list (either input or output).  Otherwise, zero is
   the normal return value.
*/

int NameGio (IRAFPointer in_t, IRAFPointer out_t, int dir_only,
		char *input, char *output, int maxch) {

/* arguments:
*/

	char *filename;		/* input name without image section, etc */
	int i;
	int junk;

	/* Get the next input image name. */
	if (c_imtgetim (in_t, input, maxch) == IRAF_EOF)
	    return (IRAF_EOF);

	if (dir_only) {

	    /* First copy the output directory name to output. */
	    c_imtrew (out_t);
	    junk = c_imtgetim (out_t, output, maxch);

	    /* Append / if necessary.
		NOTE that we're assuming iraf filename syntax here.
	    */
	    i = strlen (output) - 1;
	    if (output[i] != '$' && output[i] != '/')
		strcat (output, "/");

	    if ((filename = malloc (IRAF_SZ_LINE * sizeof (char))) == NULL) {
		fprintf (stderr, "Out of memory.\n");
		return (111);
	    }

	    /* We need the name of the input file without any expression
		in brackets appended.  Extract it to filename.
	    */
	    c_imgcluster (input, filename, IRAF_SZ_LINE);

	    /* Check for a directory prefix.  (IRAF filename syntax again) */
	    for (i = strlen (filename) - 1;  i >= 0;  i--) {
		if (filename[i] == '/' || filename[i] == '$') {
		    i++;
		    break;
		}
	    }

	    /* Append the input file name, skipping its directory prefix. */
	    strcat (output, &filename[i]);

	    free (filename);

	} else {

	    /* Get the next output image name. */

	    if (c_imtgetim (out_t, output, maxch) == IRAF_EOF)
		return (IRAF_EOF);
	}

	return (0);
}
