# include <stdio.h>
# include <xclio.h>
# include <xtables.h>
# include "../lib/nicmos.h"

# define NCOLS		7	/* number of columns in expanded ASN table */

char MsgText[SZ_OUTLINE];

/* This task adds new columns to an existing NICMOS association table.
** The columns that are added are: BCKIMAGE, MEANBCK, XOFFSET, and YOFFSET.
** These columns are normally added by CALNICB to the output association
** (ASC) tables that it produces. This task offers users a way of adding
** these columns to an ASN table without first running it through CALNICB.
**
** At this time, the task is configured to be built only as an IRAF native
** task.  Appropriate alternate coding for use as a host-level task has not
** been included.
**
**
** Revision history:
**	H.Bushouse	04-Aug-1999	Task created.
**
*/

IRAFTASK (asnexpand) {

	/* Local variables */
	char input[IRAF_SZ_FNAME];	/* input file name */
	char output[IRAF_SZ_FNAME];	/* output file name */
	Bool verbose;			/* print file names? */

	/* Function declarations */
	int ExpandTbl (char *, char *);

	/* Initialize the IRAF CVOS interface */
	c_irafinit (argc, argv);

	/* Get the task parameters */
	c_clgstr ("input", input, IRAF_SZ_LINE);
	c_clgstr ("output", output, IRAF_SZ_LINE);
	verbose = c_clgetb ("verbose");

	/* Print the input and output file names, if desired */
	if (verbose) {
	    sprintf (MsgText, "%s --> %s\n", input, output);
	    n_message (MsgText);
	}

	/* Expand the table */
	if (ExpandTbl (input, output))
	    return;

	return;
}

/* EXPANDTBL: This routine does the actual work of creating the new
** table. It is first created as a copy of the input table, and then
** the new columns are added to it (if they don't already exist).
**
** External dependencies: Uses the n_getPriHdr and n_putPriHdr routines
** from the nicmos package library.
*/

int ExpandTbl (char *input, char *output) {

/* Arguments:
**	input	i: name of input table
**	output	i: name of output table
*/

	/* Local variables */
	Hdr PriHdr;			/* primary file header */
	int row, col;			/* loop indexes */
	int nrows;			/* number of rows in table */
	char mname[IRAF_SZ_LINE+1];	/* table member name */
	char mtype[IRAF_SZ_LINE+1];	/* table member type */
	Bool mstatus;			/* table member status */
	IRAFPointer asn_tp, asc_tp;	/* table pointers */
	IRAFPointer asnptr[NCOLS];	/* input table column pointers */
	IRAFPointer ascptr[NCOLS];	/* output table column pointers */

	/* table column names */
	char colname[NCOLS][SZ_COLNAME+1] = {"MEMNAME", "MEMTYPE", "MEMPRSNT",
					     "BCKIMAGE", "MEANBCK", "XOFFSET",
					     "YOFFSET"};

	/* table column units */
	char colunits[NCOLS][SZ_COLUNITS+1] = { "", "", "", "", "DN/sec",
						"pixels", "pixels" };
 
	/* table column print formats */
	char colfmt[NCOLS][SZ_COLFMT+1] = { "", "", "", "", "%7.2f", "%7.2f",
					    "%7.2f" };
 
	/* table column data types */
	int coltype[NCOLS] = {-14, -14, -1, IRAF_BOOL, IRAF_REAL, IRAF_REAL,
			      IRAF_REAL};
 
	/* Function declarations */
	int n_getPriHdr (char *, Hdr *);
	int n_putPriHdr (char *, Hdr *);

	/* Initialize the table column pointers */
	for (col = 0; col < NCOLS; col++) {
	     asnptr[col] = 0;
	     ascptr[col] = 0;
	}
 
	/* Open the primary header of the ASN file to copy it */
	if (n_getPriHdr (input, &PriHdr))
	    return (1);

	/* Update the FILENAME keyword in the primary header */
	if (putKeyS (&PriHdr, "FILENAME", output, ""))
	    return (1);

	/* Now write the primary header to the output file */
	if (n_putPriHdr (output, &PriHdr)) {
	    freeHdr (&PriHdr);
	    return (1);
	}
	freeHdr (&PriHdr);

	/* Open the ASN table to use as a template */
	asn_tp = c_tbtopn (input, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't open input table %s", input);
	    n_error (MsgText);
	    return (1);
	}

	/* Open the output table as a copy of the ASN table */
	asc_tp = c_tbtopn (output, IRAF_NEW_COPY, asn_tp);
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't open output table %s", output);
	    n_error (MsgText);
	    c_tbtclo (asn_tp);
	    return (1);
	}

	/* Check to see which columns are already in the ASN table;
	** for any that are missing, define them in the output table */
	for (col=0; col < NCOLS; col++) {
	     c_tbcfnd1 (asn_tp, colname[col], &(asnptr[col]));
	     if (asnptr[col] == 0)
		 c_tbcdef1 (asc_tp, &(ascptr[col]), colname[col], colunits[col],
			    colfmt[col], coltype[col], 1);
	}
 
	/* Create the output table */
	c_tbtcre (asc_tp);
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't create output table %s", output);
	    n_error (MsgText);
	    c_tbtclo (asn_tp);
	    return (1);
	}

	/* Copy the ASN header parameters to the output table */
	c_tbhcal (asn_tp, asc_tp);
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't copy table header parameters");
	    n_error (MsgText);
	    c_tbtclo (asn_tp);
	    c_tbtclo (asc_tp);
	    return (1);
	}
 
	/* Find the columns in the output table */
	for (col=0; col < NCOLS; col++) {
	     ascptr[col] = 0;
	     c_tbcfnd1 (asc_tp, colname[col], &(ascptr[col]));
	     if (c_iraferr() || ascptr[col] == 0) {
		 sprintf (MsgText, "Can't find column %s in %s", colname[col],
			  output);
		 n_error (MsgText);
		 c_tbtclo (asn_tp);
		 c_tbtclo (asc_tp);
		 return (1);
	     }
	}

	/* Find out how many rows are in the input table */
	nrows = 0;
	nrows = c_tbpsta (asn_tp, TBL_NROWS);
        if (nrows <= 0) {
            sprintf (MsgText, "Invalid number of rows in %s", input);
            n_error (MsgText);
            c_tbtclo (asn_tp);
            c_tbtclo (asc_tp);
            return (1);
        }

	/* Copy each row of data from the input to the output table */
	for (row = 1; row <= nrows; row++) {
	     c_tbegtt (asn_tp, asnptr[0], row, mname, 14);
	     c_tbeptt (asc_tp, ascptr[0], row, mname);

	     c_tbegtt (asn_tp, asnptr[1], row, mtype, 14);
	     c_tbeptt (asc_tp, ascptr[1], row, mtype);

	     c_tbegtb (asn_tp, asnptr[2], row, &mstatus);
	     c_tbeptb (asc_tp, ascptr[2], row, mstatus);

	     c_tbrudf (asc_tp, &ascptr[3], 1, row);
	     c_tbeptr (asc_tp, ascptr[4], row, 0.0);
	     c_tbeptr (asc_tp, ascptr[5], row, 0.0);
	     c_tbeptr (asc_tp, ascptr[6], row, 0.0);
	}

	/* Close the tables */
	c_tbtclo (asn_tp);
	c_tbtclo (asc_tp);

	return (0);

}

