# include <stdio.h>
# include <string.h>
# include "biaseq.h"

/*   DOBIASCORR --  Apply the bias correction to one file.
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	25-Mar-1999	Implementation.
**   H. Bushouse	02-May-2000	Prepared for v1.0 public release.
**   H. Bushouse	08-May-2000	Broke n_makebias routine into
**					separate steps.
**
*/

int doBiasCorr (BiasInfo *info, MultiNicmosGroup *input, Bool verbose) {

/* Arguments:
**	info	i: task info structure
**	input	i: input image
**	verbose	i: verbose output switch
*/

	int i, j;			/* loop indexes */
	char biasfile[SZ_NAME];		/* output bias file name */
	char jumpfile[SZ_NAME];		/* output jump file name */
	SingleNicmosGroup sky;		/* sky image */
	SingleNicmosGroup bias;		/* bias image */
	int *reglim;			/* bias region limits */
	int nregs;			/* number of regions */
	FILE *jl;			/* jump log file pointer */

	/* Function definitions */
	int n_copyGroup (SingleNicmosGroup *, SingleNicmosGroup *);
	int n_firstDiffs (MultiNicmosGroup *);
	int makeSky (MultiNicmosGroup *, SingleNicmosGroup *, int, int *, int,
		     int, short);
	int makeBias (SingleNicmosGroup *, SingleNicmosGroup *);
	int findJumps (SingleNicmosGroup *, int, short, int, float, int *,
		       int *);
	int subBias (SingleNicmosGroup *, SingleNicmosGroup *, int, short,
		     int *, int);
	int n_reaccum (MultiNicmosGroup *);
	void printSkyInfo (int *, int);
	int saveSkyImage (char *, SingleNicmosGroup *, Bool);
	void makeBiasName (char *, char *, Bool);
	FILE *openJumpFile (char *, char *, Bool);

	/* Allocate memory for the bias regions */
	reglim = (int *)calloc(NIC_XSIZE, sizeof(int));

	/* Create first differences of IMSETs in file. */
	if (verbose) {
	    sprintf (MsgText, "  Computing first differences of IMSETs ...\n");
	    n_message (MsgText);
	}
	if (n_firstDiffs (input))
	    return (1);

	/* Report info about the sky image, if requested */
	if (verbose)
	    printSkyInfo (info->skysamps, info->nskys);

	/* Make the sky image */
	if (makeSky (input, &sky, info->nskys, info->skysamps, info->nlow,
		     info->nhigh, info->bitmask))
	    return (1);

	/* Save the sky image, if requested */
	if (info->keepSky) {
	    if (saveSkyImage (input->group[0].filename, &sky, verbose))
		return (1);
	}

	/* Construct an output file name for saving the bias image,
	** if requested. */
	if (info->keepBias)
	    makeBiasName (input->group[0].filename, biasfile, verbose);

	/* Open a jump log file, if requested */
	if (info->keepJump)
	    jl = openJumpFile (input->group[0].filename, jumpfile, verbose);

	/* Loop over input imsets (except for last imset) */
	for (i=0; i < input->ngroups-1; i++) {

	     /* Copy the first difference image to the bias image */
	     if (n_copyGroup (&bias, &(input->group[i])))
		 return (1);

	     /* Make the bias image */
	     if (makeBias (&bias, &sky))
		 return (1);

	     /* Save the bias image, if requested */
	     if (info->keepBias) {
		 if (putSingleNicmosGroup (biasfile, i+1, &bias, 0))
		     return (1);
	     }

	     /* Find jumps in bias image, if requested */
	     if (info->fitJumps) {

		 if (findJumps (&bias, info->camera, info->bitmask,
		     info->jmp_filt, info->jmp_thresh, reglim, &nregs))
		     return (1);

		 if (info->keepJump) {
		     for (j=0; j<nregs; j++) {
			  fprintf (jl, "    %2d   %3d %3d\n", i+1,
				   reglim[2*j]+1, reglim[2*j+1]+1);
		     }
		 }
	     }

	     /* Otherwise, just set region to full quadrant limits */
	     else {
		 nregs = 1;
		 reglim[0] = 0;
		 reglim[1] = NIC_QSIZE - 1;
	     }

	     /* Subtract the bias level in each region from the
	     ** first difference image */
	     if (subBias (&bias, &(input->group[i]), info->camera,
			  info->bitmask, reglim, nregs))
		 return (1);

	     freeSingleNicmosGroup (&bias);
	}

	/* For completeness, copy the last group to the bias save file,
	** if requested */
	if (info->keepBias) {
	    if (n_copyGroup (&bias, &(input->group[input->ngroups-1])))
		return (1);
	    if (putSingleNicmosGroup (biasfile, input->ngroups, &bias, 0))
		return (1);
	}

	/* Close the jump log file, if necessary */
	if (info->fitJumps && info->keepJump)
	    fclose (jl);
	
	/* Reaccumulate the corrected first differences */
	if (verbose) {
	    sprintf (MsgText, "  Reaccumulating corrected IMSETs ...\n");
	    n_message (MsgText);
	}
	if (n_reaccum (input))
	    return (1);

	/* Free memory */
	free (reglim);
	freeSingleNicmosGroup (&sky);

	/* Successful return */
	return (0);

}

void printSkyInfo (int *skysamps, int nskys) {

	int i;

	sprintf (MsgText, "  Making sky image from IMSETS");
	n_message (MsgText);
	for (i=0; i < nskys; i++) {
		 sprintf (MsgText, " %d", skysamps[i]);
		 n_message (MsgText);
	}
	sprintf (MsgText, "\n");
	n_message (MsgText);
}

int saveSkyImage (char *inputname, SingleNicmosGroup *sky, Bool verbose) {

	char skyfile[SZ_NAME];
	int n_putCalData (SingleNicmosGroup *, char *);

	strcpy (skyfile, inputname);
	strcpy (strrchr(skyfile,'.'),"_sky.fits");

	if (verbose) {
	    sprintf (MsgText, "  Saving sky image to %s\n", skyfile);
	    n_message (MsgText);
	}

	if (n_putCalData (sky, skyfile))
	    return (1);

	return (0);
}

void makeBiasName (char *inputname, char *biasfile, Bool verbose) {

	strcpy (biasfile, inputname);
	strcpy (strrchr(biasfile, '.'), "_bias.fits");
	if (verbose) {
	    sprintf (MsgText, "  Saving bias image to %s\n", biasfile);
	    n_message (MsgText);
	}
}
	
FILE *openJumpFile (char *inputname, char *jumpfile, Bool verbose) {

	FILE *fp;

	strcpy (jumpfile, inputname);
	strcpy (strrchr(jumpfile,'.'), "_jump.log");

	if (verbose) {
	    sprintf (MsgText, "  Saving jump log to %s\n", jumpfile);
	    n_message (MsgText);
	}

	fp = fopen (jumpfile, "w");
	fprintf (fp, "#imset start end\n");

	return (fp);
}

