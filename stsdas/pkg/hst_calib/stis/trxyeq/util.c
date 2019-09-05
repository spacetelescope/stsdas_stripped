# include <stdio.h>
# include <stdlib.h>		/* calloc */
# include <string.h>

# include <c_iraf.h>
# include <ximio.h>

# include "trxyeq.h"

/* Get information from the primary header. */

int getPrimaryInfo (char *input, char *idctab,
		char *select, char *ksel_val, char *note) {

/* arguments:
char *input         i: name of input image
char *idctab       io: reference table of distortion coefficients (set to
                           "none" if GEOCORR is equal to "COMPLETE" or
                           if there is no IDCTAB keyword)
char *select        i: keyword and column name for row selection
char *ksel_val      o: value of select keyword
char *note          o: comment if no geometric correction will be applied
*/

	IRAFPointer im;
	char primary[SZ_FNAME+1];	/* input with [0] instead of extn */
	char geocorr[SZ_CBUF+1];	/* calibration switch keyword */
	int i, len;

	note[0] = '\0';			/* initial values */
	ksel_val[0] = '\0';

	strcpy (primary, input);

	len = strlen (input);
	if (input[len-1] == ']') {

	    for (i = len-1;  i >= 0;  i--) {
		if (input[i] == '[') {
		    primary[i] = '\0';
		    break;
		}
	    }
	}
	strcat (primary, "[0]");

	im = c_immap (primary, IRAF_READ_ONLY, 0);
	if (c_iraferr())
	    return (2);

	if (strcmp (idctab, "none") == 0) {

	    strcpy (note, "Note:  no geometric correction");

	} else if (c_imaccf (im, "geocorr") > 0) {

	    c_imgstr (im, "geocorr", geocorr, SZ_CBUF);
	    if (c_iraferr())
		return (2);
	    if (strcmp (geocorr, "COMPLETE") == 0) {
		strcpy (note,
		"Note:  no geometric correction (GEOCORR is COMPLETE)");
		strcpy (idctab, "none");
	    }
	}

	if (strcmp (idctab, "none") != 0) {
	    /* An IDC table was specified, or defaults to IDCTAB keyword. */
	    if (select[0] == '\0') {
		strcpy (ksel_val, "ANY");
	    } else {
		c_imgstr (im, select, ksel_val, SZ_CBUF);
		if (c_iraferr())
		    return (2);
	    }

	    if (idctab[0] == '\0') {
		/* idctab = "" means we should use the keyword in the header. */
		if (c_imaccf (im, "idctab") > 0) {
		    c_imgstr (im, "idctab", idctab, SZ_FNAME);
		    if (c_iraferr())
			return (2);
		} else {
		    /* No geometric correction is to be applied. */
		    strcpy (idctab, "none");
		    strcpy (note,
		"Note:  no geometric correction (IDCTAB keyword not found)");
		}
	    }
	}

	c_imunmap (im);

	return (0);
}

void getLTInfo (IRAFPointer im, double ltm[], double ltv[]) {

/* arguments:
IRAFPointer im      i: for input image
double *ltm[]       o: diagonal elements of LTMi_j matrix:  LTM1_1 and LTM2_2
double *ltv[]       o: elements of LTV vector:  LTV1 and LTV2
*/

	double ltm11, ltm22, ltv1, ltv2;

	ltm11 = c_imgetd (im, "ltm1_1");
	if (c_iraferr()) {
	    clear_cvoserr();
	    ltm11 = 1.;
	}
	ltm22 = c_imgetd (im, "ltm2_2");
	if (c_iraferr()) {
	    clear_cvoserr();
	    ltm22 = 1.;
	}
	ltv1 = c_imgetd (im, "ltv1");
	if (c_iraferr()) {
	    clear_cvoserr();
	    ltv1 = 0.;
	}
	ltv2 = c_imgetd (im, "ltv2");
	if (c_iraferr()) {
	    clear_cvoserr();
	    ltv2 = 0.;
	}

	ltm[0] = ltm11;
	ltm[1] = ltm22;
	ltv[0] = ltv1;
	ltv[1] = ltv2;
}

/* This function trims trailing blanks from 'name', in place. */

void trim_blanks (char *name) {

	int i, len;

	len = strlen (name);
	for (i = len-1;  i >= 0;  i--) {
	    if (name[i] == ' ') {
		name[i] = '\0';
	    }
	}
}

void freeDist (DistInfo *dist) {

	if (dist->allocated) {
	    free (dist->xcoeff);
	    free (dist->ycoeff);
	    dist->allocated = 0;
	}
}

int checkError (void) {

	if (c_iraferr()) {
	    printf ("IRAF error %d:  %s\n", c_iraferr(), c_iraferrmsg());
	    return (2);
	}
	return (0);
}
