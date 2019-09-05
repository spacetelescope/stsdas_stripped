# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <ximio.h>	/* for image template routines */

# include "splice.h"

/* This splices the spectra.

   Phil Hodge, 1998 Oct 27:
	Initial version.

   Phil Hodge, 1999 Feb 19:
	Add wl_spacing and verbose arguments.

   Phil Hodge, 1999 Oct 25:
	Add wavetab to calling sequence of Splice;
	call SetWl if there is a wavetab; rename functions.

   Phil Hodge, 2014 Aug 5:
	Add dq_fill argument.
*/

int SpliceSpec (char *intable, char *outtable, char *wavetab, char *colname[],
		int wl_spacing, short sdqflags, int dq_fill, Bool verbose) {

	IRAFPointer tnt;	/* table name template */
	char *table;		/* an input table */
	SpecArray spectra;	/* array of spectra */
	Spectrum outspec;	/* output spectrum */
	int status;

	int ReadTable (char *, char **, Bool, SpecArray *);
	int SetWl (char *, char *, Spectrum *);
	int DeltaWl (SpecArray *, int, Spectrum *);
	int SumSpectra (SpecArray *, short, int, Spectrum *);
	int WriteTable (char *, IRAFPointer, char **, Spectrum *);
	void InitSpec (SpecArray *);
	void FreeSpec (SpecArray *);
	void InitSpectrum (Spectrum *);
	void FreeSpectrum (Spectrum *);

	InitSpec (&spectra);
	InitSpectrum (&outspec);

	/* Open image template (table name template, actually). */
	tnt = c_imtopen (intable);
	if ((status = c_iraferr()))
	    return (status);

	if ((table = calloc (STIS_LINE+1, sizeof(char))) == NULL) {
	    printf ("out of memory\n");
	    return (1);
	}

	/* Read each table in the input list into memory. */
	while (c_imtgetim (tnt, table, STIS_LINE) > 0) {

	    if ((status = ReadTable (table, colname, verbose, &spectra)))
		return (status);
	}

	/* Determine output wavelengths.
	   Memory for outspec is allocated by these routines, and
	   the number of elements is also assigned.
	*/
	if (wl_spacing == EXPLICIT_WL) {
	    if ((status = SetWl (wavetab, colname[WL_INDEX], &outspec)))
		return (status);
	} else {
	    if ((status = DeltaWl (&spectra, wl_spacing, &outspec)))
		return (status);
	}

	/* Sum (splice) the spectra. */
	if ((status = SumSpectra (&spectra, sdqflags, dq_fill, &outspec)))
	    return (status);

	/* Write the spliced spectrum to the output table, using the
	   first input table as a template.
	*/
	if ((status = WriteTable (outtable, tnt, colname, &outspec)))
	    return (status);

	/* Free memory. */
	c_imtclose (tnt);
	FreeSpec (&spectra);
	FreeSpectrum (&outspec);
	free (table);

	return (0);
}
