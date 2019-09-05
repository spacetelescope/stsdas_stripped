# include <ctype.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include <xtables.h>	/* defines TABLE I/O functions */
# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnicb.h"	/* defines CALNICB data structures */

# define NCOLS	3	/* number of columns in ASNTAB */
# define CCOLS	7	/* number of columns in ASCTAB */

/* N_GETASNTABLE: Read the Association (ASN) table and load the
** list of member dataset names. Also check to see if any of the
** members are listed as missing. If so, quit CALNICB.
**
** Revision history:
** [Note that Build 1 and 2 developments were done in parallel, thus dates
**  of modifications are not always chronologically sequential with versions.]
** H.Bushouse	April 1996	Build 1
** H.Bushouse	30-Jan-1997	Modified n_getAsnTable so error message is
**				issued if output product names are not in ASN
**				table; modified n_getAsnTable so that MEM_PRSNT
**				is only checked for input files; modified
**				n_putAscTable to change output column names to
**				BCKIMAGE and MEANBCK (Version 2.0)
** H.Bushouse	25-Feb-1997	Modified n_ckStep to check for null ILLMFILE
**				file name (Version 2.0)
** H.Bushouse	27-Feb-1997	Added asn.docombine to n_initAsnInfo
**				(Version 2.0)
** H.Bushouse	28-Apr-1997	Modified n_putAscTable to update FILENAME and
**				EXTNAME keywords in the ASC file (Version 0.1.6)
** H.Bushouse	29-Apr-1997	Changed MemberInfo.imgtype to mtype
**				(Version 2.0)
** H.Bushouse	06-May-1997	Changed single quotes (') to double (") in all
**				output strings (Version 2.0)
** H.Bushouse	01-Dec-1997	Initialize ref.dummy=False in n_ckStep; added
**				subbkg, usemeanbkg, crthresh, and xcwin to
**				n_initAsnInfo; added routine n_sortProducts to
**				sort the list of output product	file names
**				(Version 2.2)
** H.Bushouse	16-Mar-1998	Added new routines n_getAsnBkgs and
**				n_getAsnOffs to read background and offset
**				values from input ASN table; added calls to
**				these routines in n_getAsnTable; added readbkg
**				and readoffs to n_initAsnInfo; modified
**				n_putAscTable to check for existence of ASC
**				columns in input ASN table (Version 2.2)
** H.Bushouse	09-Feb-1999	Updated use of putKey routines for HSTIO v2.1
**				(Version 2.2.1)
** H.Bushouse	04-Aug-1999	Fixed bug in n_putAscTable involving the
**				assignment of column pointers that was causing
**				BCKIMAGE, MEANBCK, XOFFSET, and YOFFSET data to
**				come out as INDEFs in the asc table when the
**				asn table already had one or more of these
**				columns present (Version 2.3.1).
*/

int n_getAsnTable (AsnInfo *asn, NicInfo *nic) {

/* Arguments:
**	asn		o: Association info structure
**	nic		o: observation info structure
*/

	/* Local variables */
	int i;				/* loop index */
	int nrows;			/* number of rows in ASNTAB */
	int col, row;			/* loop indexes */
	char type[20];			/* member type */
	IRAFPointer tp;			/* ASNTAB table pointer */
	IRAFPointer colptr[NCOLS];	/* ASNTAB column pointers */
					/* ASNTAB column names */
	char colname[NCOLS][SZ_COLNAME+1] = {
		"MEMNAME",
		"MEMTYPE",
		"MEMPRSNT"
	};
    char *word;
    char memsubtype[SZ_COLNAME+1];
    int posid;
    int nmembers,pmembers;

	/* Function definitions */
	void n_sortProducts (AsnInfo *);
	int  n_ckStep (IRAFPointer, AsnInfo *, char *, char *, char *, char *,
		       CalStep *);
	int  n_allocAsnInfo (AsnInfo *, int);
	int  n_getAsnBkgs (IRAFPointer, AsnInfo *);
	int  n_getAsnOffs (IRAFPointer, AsnInfo *);
	void n_freeAsnInfo (AsnInfo *);

	/* Open the ASN table */
	tp = c_tbtopn (asn->asn_table, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    n_openerr (asn->asn_table);
	    return (status = 1);
	}

	/* Get pointers to columns in ASNTAB */
	for (col=0; col < NCOLS; col++) {
	     c_tbcfnd1 (tp, colname[col], &(colptr[col]));
	     if (c_iraferr() || colptr[col] == 0) {
		 sprintf (MsgText, "Can't find column %s in %s", colname[col],
			  asn->asn_table);
		 n_error (MsgText);
		 c_tbtclo (tp);
		 return (status = 1);
	     }
	}

	/* Find out how many rows are in ASNTAB */
	nrows = 0;
	nrows = c_tbpsta (tp, TBL_NROWS);
	if (nrows <= 0) {
	    sprintf (MsgText, "Invalid number of rows in %s", asn->asn_table);
	    n_error (MsgText);
	    c_tbtclo (tp);
	    return (status = 1);
	}

	/* Allocate slots for all members in ASN info structure */
	if (n_allocAsnInfo (asn, nrows)) {
	    c_tbtclo (tp);
	    return (status);
	}

	/* Read each row of ASNTAB */
	for (row = 0; row < asn->tmembers; row++) {

	     /* Get the MEMBER NAME in this row */
	     c_tbegtt (tp, colptr[0], row+1, asn->member[row].name, 14);
	     if (c_iraferr()) {
		 sprintf (MsgText, "Can't read %s in row %d in %s", colname[0],
			  row+1, asn->asn_table);
		 n_error (MsgText);
		 c_tbtclo (tp);
		 n_freeAsnInfo (asn);
		 return (status = 1);
	     }
	     /* Convert to lowercase for use as a file name */
	     for (i = 0; i < strlen(asn->member[row].name); i++)
		  asn->member[row].name[i] = tolower(asn->member[row].name[i]);

	     /* Get the TYPE in this row */
	     c_tbegtt (tp, colptr[1], row+1, asn->member[row].mtype, 14);
	     if (c_iraferr()) {
		 sprintf (MsgText, "Can't read %s in row %d in %s", colname[1],
			  row+1, asn->asn_table);
		 n_error (MsgText);
		 c_tbtclo (tp);
		 n_freeAsnInfo (asn);
		 return (status = 1);
	     }

	     /* Get the STATUS in this row */
	     c_tbegtb (tp, colptr[2], row+1, &(asn->member[row].status));
	     if (c_iraferr()) {
		 sprintf (MsgText, "Can't read %s in row %d in %s", colname[2],
			  row+1, asn->asn_table);
		 n_error (MsgText);
		 c_tbtclo (tp);
		 n_freeAsnInfo (asn);
		 return (status = 1);
	     }
	}

	/* Read through the list to figure out which are input vs. output
	** files, and see if any input files are missing. */
    nmembers = 0;
    pmembers = 0;
	for (row = 0; row < nrows; row++) {

	     /* Is this an output file? */
	     type[0] = '\0';
         memsubtype[0]='\0';
	     strcpy (type, asn->member[row].mtype);
	     for (i = 0; i < strlen(type); i++)
		  type[i] = tolower(type[i]);

          strcpy(memsubtype,type);

		/* Let's start by breaking up the MEMTYPE string */
         word = strtok(type,"-");
		/* The end of this second part of MEMTYPE has the POSID */
         word = strtok(NULL,"\n");
         if (isalnum(word[3])){          
			/* Interpret the POSID from the second part of MEMTYPE */
             posid = (int)strtol(&word[3],(char **)NULL,10);
         } else {
            /* Else, set a default value of 1*/
             posid = 1;
         }

	     if (strncmp (memsubtype, "prod", 4) == 0) {
            /* build up info on product for later use. */
		     pmembers += 1;
             asn->member[row].bkgprod = posid;
         } else if (strncmp(memsubtype, "exp-bck",7) == 0){
             /* Record the position ID for this background exposure
             to make it easier to match to the product later. */
             asn->member[row].bkgid = posid;         
             nmembers += 1;
         } else {
             /* count the number of input exposures (both science and prod) */
             nmembers += 1;
         }

	     /* If the member is missing, shut down */
	     if (!asn->member[row].status) {
		 sprintf (MsgText,
		"Member \"%s\" is not present; can't run CALNICB",
		 asn->member[row].name);
		 n_error (MsgText);
		 c_tbtclo (tp);
		 n_freeAsnInfo (asn);
		 return (status = 1);
	     }
	}

	/* Set the ASN number of members to the number of input members only */
	asn->nmembers = nmembers;
	asn->pmembers = pmembers;

	/* Check for existence of output file names */
	if (asn->tmembers == asn->nmembers) {
	    sprintf (MsgText,
		     "No output product names specified in assoc. table");
	    n_error (MsgText);
	    c_tbtclo (tp);
	    n_freeAsnInfo (asn);
	    return (status = 1);
	}

	/* Sort the output file names into the expected order */
	n_sortProducts (asn);

	/* Get the calibration switch and reference file info */
	if (n_ckStep (tp, asn, "ILLMCORR", "ILLMDONE", "ILLMFILE", "ILLMPDGR",
		      &(nic->BACK))) {
	    c_tbtclo (tp);
	    n_freeAsnInfo (asn);
	    return (status);
	}

	/* Read image background levels from ASN table, if requested */
	if (asn->readbkg) {
	    if (n_getAsnBkgs (tp, asn)) {
		c_tbtclo (tp);
		n_freeAsnInfo (asn);
		return (status);
	    }
	}

	/* Read the image offset values from ASN table, if requested */
	if (asn->readoffs) {
	    if (n_getAsnOffs (tp, asn)) {
		c_tbtclo (tp);
		n_freeAsnInfo (asn);
		return (status);
	    }
	}

	/* Close the ASN table */
	c_tbtclo (tp);

	/* Successful return */
	return (status = 0);
}

/* N_GETASNBKGS: Reads background signal values from the input ASN table.
** This includes reading the values listed in the MEANBCK column, as well
** as the MEAN_BKG header parameter.
*/

int n_getAsnBkgs (IRAFPointer tp, AsnInfo *asn) {

/* Arguments:
**	tp	 i: table pointer
**	asn	io: Association info structure
*/

	/* Local variables */
	int col, row;			/* column and row numbers */
	IRAFPointer colptr[2];		/* ASN table column pointers */
					/* ASN table column names */
	char colname[2][SZ_COLNAME+1] = {
		"BCKIMAGE",
		"MEANBCK"
	};

	/* Get pointers to the background columns in the ASN table */
	for (col = 0; col < 2; col++) {
	     c_tbcfnd1 (tp, colname[col], &(colptr[col]));
	     if (c_iraferr() || colptr[col] == 0) {
		 sprintf (MsgText, "Can't find column %s in %s", colname[col],
			  asn->asn_table);
		 n_error (MsgText);
		 return (status = 1);
	     }
	}

	/* Read the background data for the input members */
	for (row = 0; row < asn->nmembers; row++) {
	     
	    /* Get the BCKIMAGE value in this row */
	    c_tbegtb (tp, colptr[0], row+1, &(asn->member[row].bkgimg));
	    if (c_iraferr()) {
		sprintf (MsgText, "Can't read %s in row %d in %s", colname[0],
			 row+1, asn->asn_table);
		n_error (MsgText);
		return (status = 1);
	    }

	    /* Get the MEANBCK value in this row */
	    c_tbegtr (tp, colptr[1], row+1, &(asn->member[row].bkg));
	    if (c_iraferr()) {
		sprintf (MsgText, "Can't read %s in row %d in %s",
			 colname[1], row+1, asn->asn_table);
		n_error (MsgText);
		return (status = 1);
	    }
	}

	/* Now read the MEAN_BKG header parameter */
	asn->meanbkg = c_tbhgtr (tp, "MEAN_BKG");
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't read MEAN_BKG keyword in %s",
		     asn->asn_table);
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Successful return */
	return (status = 0);
}

/* N_GETASNOFFS: Reads image offset values from the input ASN table. */

int n_getAsnOffs (IRAFPointer tp, AsnInfo *asn) {

/* Arguments:
**	tp	 i: table pointer
**	asn	io: Association info structure
*/

	/* Local variables */
	int col, row;			/* column and row numbers */
	IRAFPointer colptr[2];		/* ASN table column pointers */
					/* ASN table column names */
	char colname[2][SZ_COLNAME+1] = {
		"XOFFSET",
		"YOFFSET"
	};

	/* Get pointers to the pixel offset columns in the ASN table */
	for (col = 0; col < 2; col++) {
	     c_tbcfnd1 (tp, colname[col], &(colptr[col]));
	     if (c_iraferr() || colptr[col] == 0) {
		 sprintf (MsgText, "Can't find column %s in %s", colname[col],
			  asn->asn_table);
		 n_error (MsgText);
		 return (status = 1);
	     }
	}

	/* Read the offset data for the input members */
	for (row = 0; row < asn->nmembers; row++) {
	     
	    /* Get the XOFFSET value in this row */
	    c_tbegtr (tp, colptr[0], row+1, &(asn->member[row].dx));
	    if (c_iraferr()) {
		sprintf (MsgText, "Can't read %s in row %d in %s", colname[0],
			 row+1, asn->asn_table);
		n_error (MsgText);
		return (status = 1);
	    }

	    /* Get the YOFFSET value in this row */
	    c_tbegtr (tp, colptr[1], row+1, &(asn->member[row].dy));
	    if (c_iraferr()) {
		sprintf (MsgText, "Can't read %s in row %d in %s",
			 colname[1], row+1, asn->asn_table);
		n_error (MsgText);
		return (status = 1);
	    }
	}

	/* Successful return */
	return (status = 0);

}

/* N_SORTPRODUCTS: Sorts the list of output product file names in the ASN
** table to make sure they're in the expected order of TARG, BCK1, BCK2, ...,
** BCKn.
*/

void n_sortProducts (AsnInfo *asn) {

/* Arguments:
**	asn	io: association info structure
*/
	int i;			/* loop index */
	int row;		/* table row number */
	char tname[SZ_NAME+1];	/* tmp member name */
	char ttype[20];		/* tmp member type */
	Bool tstatus;		/* tmp member status */

	/* Loop over the ASN rows containing the output products */
	for (row = asn->nmembers; row < asn->tmembers; row++) {

	     /* Build the memtype that we're expecting in this row */
	     if (row == asn->nmembers)
		 sprintf (ttype, "PROD-TARG");
	     else
		 sprintf (ttype, "PROD-BCK%1d", row-(asn->nmembers));

	     /* Compare with the memtype in this row */
	     if (strncmp(ttype, asn->member[row].mtype, 9) != 0) {

		 /* Loop through the remaining rows until finding the
		 ** correct match */
		 for (i = row+1; i < asn->tmembers; i++) {

		      /* Does this row contain the one we want? */
		      if (strncmp(ttype, asn->member[i].mtype, 9) == 0) {

			  /* Yes! Swap it with the row it should be in */
			  strcpy (tname, asn->member[row].name);
			  strcpy (ttype, asn->member[row].mtype);
			  tstatus = asn->member[row].status;
			  strcpy (asn->member[row].name, asn->member[i].name);
			  strcpy (asn->member[row].mtype, asn->member[i].mtype);
			  asn->member[row].status = asn->member[i].status;
			  strcpy (asn->member[i].name, tname);
			  strcpy (asn->member[i].mtype, ttype);
			  asn->member[i].status = tstatus;
			  break;
		      }
		 }
	     }
	}

}

/* N_CKSTEP: Reads switch and reference file name
** for an individual calibration step.
*/
 
int n_ckStep (IRAFPointer tp, AsnInfo *asn, char *calSwitch, char *calIndicator,
	      char *calFile, char *calPedigree, CalStep *step) {
 
/* Arguments:
**	tp		i: association table pointer
**      asn             i: association info structure
**      calSwitch       i: calibration switch keyword name
**      calIndicator    i: calibration indicator keyword name
**      calFile         i: calibration file keyword name
**      calPedigree     i: calibration file pedigree keyword name
**      step            io: calibration step info structure
*/
 
	/* Function definitions */
	int n_getSwitch    (IRAFPointer, AsnInfo *, CalStep *);
	int n_getIndicator (IRAFPointer, AsnInfo *, CalStep *);
 
	/* Initialize the CalStep information */
	step->corr            = -1;
	step->done            = -1;
	step->swname[0]       = '\0';
	step->indname[0]      = '\0';
	step->pdname[0]       = '\0';
	step->ref.name[0]     = '\0';
	step->ref.pedigree[0] = '\0';
	step->ref.descrip[0]  = '\0';
	step->ref.dummy       = False;
 
	/* Copy the keyword names into the CalStep structure */
	strcpy (step->swname,  calSwitch);
	strcpy (step->indname, calIndicator);
	strcpy (step->pdname,  calPedigree);
 
	/* Get the switch value from the table header */
	if (n_getSwitch (tp, asn, step))
	    return (status);
 
	/* Get the indicator value from the table header */
	if (n_getIndicator (tp, asn, step))
	    return (status);
 
	/* Get the reference file name from header, if necessary */
	if (step->corr == PERFORM && calFile[0] != '\0') {
	    c_tbhgtt (tp, calFile, step->ref.name, SZ_NAME);
	    if (c_iraferr()) {
		sprintf (MsgText, "Can't read %s keyword in %s", calFile,
			 asn->asn_table);
		n_error (MsgText);
		return (status = 1);
	    }

	    /* Is the reference file name blank? */
	    if (step->ref.name[0] == '\0') {
		sprintf (MsgText, "%s keyword in %s is null", calFile,
			 asn->asn_table);
		n_error (MsgText);
		return (status = 1);
	    }
	}
 
	/* Issue a warning if this step has already been performed */
	if (step->corr==PERFORM && step->done==PERFORMED) {
	    sprintf (MsgText, "%s has been performed previously on this data",
		     step->swname);
	    n_warn (MsgText);
	    sprintf (MsgText, "%s will be performed again", step->swname);
	    n_warn (MsgText);
	}
 
	/* Successful return */
	return (status = 0);
}
 
/* N_GETSWITCH: Read the value of a calibration switch */
 
int n_getSwitch (IRAFPointer tp, AsnInfo *asn, CalStep *step) {
 
/* Arguments:
**	tp	i: table pointer
**      asn     i: association info structure
**      step    io: calibration step info structure
*/
 
        /* Local variables */
        char kword[9];
 
        /* Get the string value of the keyword */
        kword[0] = '\0';
	c_tbhgtt (tp, step->swname, kword, SZ_STRKWVAL);
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't read %s header param in %s",
		     step->swname, asn->asn_table);
	    n_error (MsgText);
	    return (status = 1);
	}

        /* Check the value of the switch */
        if (strncmp (kword, "PERFORM", 7) == 0)
            step->corr = PERFORM;
        else if (strncmp (kword, "OMIT", 4) == 0)
            step->corr = OMIT;
        else {
            sprintf (MsgText, "Keyword \"%s\" has invalid value \"%s\"",
                     step->swname, kword);
            n_error (MsgText);
            status = 1;
        }
 
        return (status);
}
 
/* N_GETINDICATOR: Read the value of a calibration indicator */
 
int n_getIndicator (IRAFPointer tp, AsnInfo *asn, CalStep *step) {
 
/* Arguments:
**	tp	i: table pointer
**      asn     i: association info structure
**      step    io: calibration step info structure
*/
 
        /* Local variables */
        char kword[10];
 
        /* Get the string value of the keyword */
        kword[0] = '\0';
	c_tbhgtt (tp, step->indname, kword, SZ_STRKWVAL);
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't read %s header param in %s",
		     step->indname, asn->asn_table);
	    n_error (MsgText);
	    return (status = 1);
	}
 
        /* Check the value of the indicator */
        if (strncmp (kword, " ", 1) == 0 || kword[0] == '\0')
            step->done = BLANK;
        else if (strncmp (kword, "OMITTED", 7) == 0)
            step->done = OMITTED;
        else if (strncmp (kword, "PERFORMED", 9) == 0)
            step->done = PERFORMED;
        else if (strncmp (kword, "SKIPPED", 7) == 0)
            step->done = SKIPPED;
        else {
            sprintf (MsgText, "Keyword \"%s\" has invalid value \"%s\"",
                     step->indname, kword);
            n_error (MsgText);
            status = 1;
        }
 
        return (status);
}

/* N_PUTASCTABLE: Write the Association (ASC) table, including new
** information for members derived during CALNICB processing.
** The ASC table is first created as a copy of the input ASN table,
** and then the new columns of information are added.
*/

int n_putAscTable (AsnInfo *asn, CalStep *step) {

/* Arguments:
**	asn		i: association info structure
**	step		i: background step info structure
*/

	/* Local variables */
	Hdr PriHdr;			/* ASN file primary header */
	int row, col;			/* loop indexes */
	IRAFPointer asn_tp, asc_tp;	/* table pointers */
	IRAFPointer colptr[CCOLS];	/* ASCTAB column pointers */

		/* ASCTAB column names */
	char colname[CCOLS][SZ_COLNAME+1] = {
		"MEMNAME",
		"MEMTYPE",
		"MEMPRSNT",
		"BCKIMAGE",
		"MEANBCK",
		"XOFFSET",
		"YOFFSET"
	};
		/* ASCTAB column units */
	char colunits[CCOLS][SZ_COLUNITS+1] = { "", "", "",
					"", "DN/sec", "pixels", "pixels" };

		/* ASCTAB column print formats */
	char colfmt[CCOLS][SZ_COLFMT+1] = { "", "", "",
					    "", "%7.2f", "%7.2f", "%7.2f" };

		/* ASCTAB column data types */
	int coltype[CCOLS] = {-14, -14, -1,
			      IRAF_BOOL, IRAF_REAL, IRAF_REAL, IRAF_REAL};

	/* Function definitions */
	int n_getPriHdr (char *, Hdr *);
	int n_putPriHdr (char *, Hdr *);

	/* Initialize the table column pointers */
	for (col = 0; col < CCOLS; col++)
	     colptr[col] = 0;

	/* Open the primary header of the ASN file to copy it */
	if (n_getPriHdr (asn->asn_table, &PriHdr))
	    return (status);
	
	/* Update the FILENAME header keyword */
	if (putKeyS (&PriHdr, "FILENAME", asn->asc_table, ""))
	    return (status = 1);

	/* Now write the primary header to the ASC file */
	if (n_putPriHdr (asn->asc_table, &PriHdr)) {
	    freeHdr (&PriHdr);
	    return (status);
	}
	freeHdr (&PriHdr);

	/* Open the ASN table to use as a template */
	asn_tp = c_tbtopn (asn->asn_table, IRAF_READ_ONLY, 0);
	if (c_iraferr()) {
	    n_openerr (asn->asn_table);
	    return (status = 1);
	}

	/* Open the ASC table as a copy of the ASN table */
	asc_tp = c_tbtopn (asn->asc_table, IRAF_NEW_COPY, asn_tp);
	if (c_iraferr()) {
	    n_openerr (asn->asc_table);
	    c_tbtclo (asn_tp);
	    return (status = 1);
	}

	/* Check to see if the new ASC columns are already in the ASN table;
	** if not, define the new columns in the ASC table */
	for (col=NCOLS; col < CCOLS; col++) {
	     c_tbcfnd1 (asn_tp, colname[col], &(colptr[col]));
	     if (colptr[col] == 0)
		 c_tbcdef1 (asc_tp, &(colptr[col]), colname[col], colunits[col],
			    colfmt[col], coltype[col], 1);
	}

	/* Create the ASC table */
	c_tbtcre (asc_tp);
	if (c_iraferr()) {
	    n_openerr (asn->asc_table);
	    c_tbtclo (asn_tp);
	    return (status = 1);
	}

	/* Copy the ASN header parameters to the ASC */
	c_tbhcal (asn_tp, asc_tp);
	if (c_iraferr()) {
	    n_filerr (asn->asc_table);
	    c_tbtclo (asn_tp);
	    return (status = 1);
	}

	/* Close the ASN table */
	c_tbtclo (asn_tp);

	/* Find the columns in the ASC table */
	for (col=0; col < CCOLS; col++) {
	     colptr[col] = 0;
	     c_tbcfnd1 (asc_tp, colname[col], &(colptr[col]));
	     if (c_iraferr() || colptr[col] == 0) {
		 sprintf (MsgText, "Can't find column %s in %s",
			  colname[col], asn->asc_table);
		 n_error (MsgText);
		 c_tbtclo (asc_tp);
		 return (status = 1);
	     }
	}

	/* Update the EXTNAME to "ASC" */
	c_tbhptt (asc_tp, "EXTNAME", "ASC");
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't update parameter EXTNAME in %s",
		     asn->asc_table);
	    n_error (MsgText);
	    c_tbtclo (asc_tp);
	    return (status = 1);
	}

	/* Update the cal step indicator header parameter */
	if (step->corr == PERFORM)
	    c_tbhptt (asc_tp, step->indname, "PERFORMED");
	else if (step->corr == SKIP)
	    c_tbhptt (asc_tp, step->indname, "SKIPPED");
	else if (step->corr == OMIT)
	    c_tbhptt (asc_tp, step->indname, "OMITTED");
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't update parameter %s in %s", step->indname,
		     asn->asc_table);
	    n_error (MsgText);
	    c_tbtclo (asc_tp);
	    return (status = 1);
	}

	/* Update the cal file pedigree header parameter */
	if (step->corr == PERFORM || step->corr == SKIP)
	    c_tbhptt (asc_tp, step->pdname, step->ref.pedigree);
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't update parameter %s in %s", step->pdname,
		     asn->asc_table);
	    n_error (MsgText);
	    c_tbtclo (asc_tp);
	    return (status = 1);
	}

	/* Write the mean bkg value to a header parameter */
	c_tbhadr (asc_tp, "MEAN_BKG", asn->meanbkg);
	if (c_iraferr()) {
	    sprintf (MsgText, "Can't add parameter MEAN_BKG to %s",
		     asn->asc_table);
	    n_error (MsgText);
	    c_tbtclo (asc_tp);
	    return (status = 1);
	}

	/* Write the input member info to the ASC table */
	for (row = 0; row < asn->nmembers; row++) {
	     c_tbeptt (asc_tp, colptr[0], row+1, asn->member[row].name);
	     c_tbeptt (asc_tp, colptr[1], row+1, asn->member[row].mtype);
	     c_tbeptb (asc_tp, colptr[2], row+1, asn->member[row].status);
	     c_tbeptb (asc_tp, colptr[3], row+1, asn->member[row].bkgimg);
	     if (asn->member[row].bkgimg == False && asn->member[row].bkg == 0)
		 c_tbrudf (asc_tp, &colptr[4], 1, row+1);
	     else
	         c_tbeptr (asc_tp, colptr[4], row+1, asn->member[row].bkg);
	     c_tbeptr (asc_tp, colptr[5], row+1, asn->member[row].dx);
	     c_tbeptr (asc_tp, colptr[6], row+1, asn->member[row].dy);
	}

	/* Write the output member info to the ASC table */
	for (row = asn->nmembers; row < asn->tmembers; row++) {
	     c_tbeptt (asc_tp, colptr[0], row+1, asn->member[row].name);
	     c_tbeptt (asc_tp, colptr[1], row+1, asn->member[row].mtype);
	     if (!asn->member[row].status) asn->member[row].status = True;
	     c_tbeptb (asc_tp, colptr[2], row+1, asn->member[row].status);
	     c_tbeptb (asc_tp, colptr[3], row+1, asn->member[row].bkgimg);
	     c_tbrudf (asc_tp, &colptr[4], 1, row+1);
	     c_tbrudf (asc_tp, &colptr[5], 1, row+1);
	     c_tbrudf (asc_tp, &colptr[6], 1, row+1);
	}

	/* Close the ASC table */
	c_tbtclo (asc_tp);

	/* Successful return */
	return (status = 0);
}

void n_initAsnInfo (AsnInfo *asn) {

	asn->asn_table[0] = '\0';
	asn->asc_table[0] = '\0';
	asn->id[0] = '\0';
	asn->pattern[0] = '\0';
	asn->numpos = 0;
	asn->nummos = 0;
	asn->patt_type = -1;
	asn->patt_name = -1;
	asn->niter = 0;
	asn->nobj = 0;
	asn->nsky = 0;
	asn->posmems = NULL;
	asn->mosmems = NULL;
	asn->meanbkg = 0;
	asn->nmembers = 0;
	asn->tmembers = 0;
	asn->member = NULL;
	asn->docombine = False;
	asn->subbkg = True;
	asn->usemeanbkg = True;
	asn->readbkg = False;
	asn->readoffs = False;
	asn->crthresh = 0;
	asn->xcwin = 0;

}

int n_allocAsnInfo (AsnInfo *asn, int n) {

	/* Local variables */
	int i;			/* loop index */

	/* Function definitions */
	void n_initAsnMember (MemberInfo *);
	void n_freeAsnInfo (AsnInfo *);

	/* Free the structure if necessary */
	if (asn->member != NULL)
	    n_freeAsnInfo(asn);

	/* Set the number of members in the structure */
	asn->nmembers = n;
	asn->tmembers = n;
    asn->pmembers = 0;

	/* Allocate the member structures */
	asn->member = (MemberInfo *)calloc(n,sizeof(MemberInfo));

	/* Check for error during allocation */
	if (asn->member == NULL) {
	    asn->nmembers = 0;
	    asn->tmembers = 0;
	    sprintf (MsgText, "Insufficient memory to allocate ASN structure");
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Initialize each member structure */
	for (i=0; i < n; i++)
	     n_initAsnMember (&(asn->member[i]));

	/* Succesful return */
	return (status = 0);
}

void n_freeAsnInfo (AsnInfo *asn) {

	/* Local variables */
	int i;			/* loop index */

	/* Function definitions */
	void n_initAsnMember (MemberInfo *);
	void n_initAsnInfo (AsnInfo *);

	for (i=0; i<asn->tmembers; i++)
	     n_initAsnMember (&(asn->member[i]));
	free (asn->member);
	free (asn->posmems);
	free (asn->mosmems);

	n_initAsnInfo (asn);

}

void n_initAsnMember (MemberInfo *member) {

	void n_initWCS (WCS *);

	member->name[0]  = '\0';
	member->mtype[0] = '\0';
	member->status = False;
	n_initWCS (&(member->wcs));
	member->type = -1;
	member->patpos = 0;
	member->mospos = 0;
	member->backest1 = 0;
	member->backest2 = 0;
	member->backest3 = 0;
	member->bkgimg = False;
	member->bkg = 0;
	member->dx = 0;
	member->dy = 0;
	member->xi = 0;
	member->yi = 0;
    member->bkgid = 0;
    member->bkgprod = 0;
    member->expstart = -1; /* set to -1 to signal no valid expstart */
    member->expend = -1; /* set to -1 to signal no valid expend */
    member->exptime = 0.0; /* set to -1 to signal no valid exptime */

}

void n_initWCS (WCS *wcs) {

	wcs->crpix[0] = 0;
	wcs->crpix[1] = 0;
	wcs->crval[0] = 0;
	wcs->crval[1] = 0;
	wcs->cd[0][0]  = 0;
	wcs->cd[0][1]  = 0;
	wcs->cd[1][0]  = 0;
	wcs->cd[1][1]  = 0;
	wcs->ctype[0][0] = '\0';
	wcs->ctype[1][0] = '\0';

}

