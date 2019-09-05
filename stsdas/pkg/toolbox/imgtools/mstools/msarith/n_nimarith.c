# include <stdio.h>
# include <math.h>
# include <string.h>
# include <time.h>
# include <float.h>
# include <c_iraf.h>
# include <xclio.h>
# include <ximio.h>
# if defined (NATIVE_IRAF)
# include <xclio.h>
# endif
# include "msarith.h"


/*  MSARITH driver  -  This is the actual entry module for task nimarith.

    It performs CL parameter input (or command-line processing in former
    versions), syntax checking, expands eventual file name lists and passes 
    the appropriate file names and other parameters to the main processing 
    routine.

    Revision history:
    ----------------
    01 Mar 96  -  Task created  (I. Busko)
    08 Mar 96  -  File name list processing (IB)
    15 Mar 96  -  File name suffix checking (IB)
    18 Apr 96  -  Read parameters from the CL (IB)
    17 May 96  -  Shared input and output data structures (IB)
    11 Nov 96  -  STIS and BUNIT support - version 2.0 (IB)
    06 Feb 97  -  Check/add extension in output file name - v. 2.1 (IB)
    28 Feb 97  -  Group number printout in verbose mode - v. 2.2 (IB)
    21 Apr 97  -  More flexible group input, print id - v. 2.3 (IB)
    06 May 97  -  Allow constant as first operand - v. 2.4 (IB)
    14 May 97  -  Modified logic that tests valid IMSET combinations (IB)
    29 May 97  -  Renamed to "msarith" (IB) 
    03 Jun 97  -  Added new <stdio.h> (IB) 
    24 Sep 98  -  Set number of groups to min in file and list (IB)

*/

int n_nimarith (int argc, char **argv) {

	/* Local variables */
	char         f2nd[SZ_NAME];    /* 2nd operand, when file name/list    */
	char         dirName[SZ_NAME]; /* Output directory                    */
	char         str[SZ_NAME];     /* Work string                         */
	char         cl_argv[6][SZ_NAME];/* Holds CL parameters               */
	IRAFPointer  listi1;           /* Pointer to 1st input file name/list */
	IRAFPointer  listi2;           /* Pointer to 2nd input file name/list */
	IRAFPointer  listo;            /* Pointer to output list/directory    */
	arithControl control;          /* Main control structure              */
	int          rootLength;
	int          ng1, ng2;         /* Number of groups in files           */
	int          nl1, nl2;         /* Number of groups in group lists     */
	Bool         multiFile;        /* More than one file as 2nd operand   */
	Instrument   inst1, inst2;     /* Instrument type (NICMOS, STIS)      */
	char         *msg1= "Incompatible instrument types";
	char         *msg2= "Incompatible number of IMSETs in oper1 and oper2";
	char         timeStamp[61];    /* time stamp                          */
	time_t       now;

	/* Function definitions */
	void  n_getOper (char *, arithControl *);
	int   n_fileInfo (char *, Instrument *, int *, Bool *);
	int   n_doSingleFile (arithControl *, Instrument, int);
	int   n_getGroupList (arithControl *, char *, int);
	int   c_isdirectory(char *, char *, int); /* Temporary C bindings */
	int   c_fnldir(char *, char *, int);
	float l_clgetf (char *);

	/* Read CL parameters. */
	c_clgstr ("operand1", cl_argv[0], BUFSIZ);
	c_clgstr ("op",       cl_argv[1], BUFSIZ);
	c_clgstr ("operand2", cl_argv[2], BUFSIZ);
	c_clgstr ("result",   cl_argv[3], BUFSIZ);
	c_clgstr ("list1",    cl_argv[4], BUFSIZ);
	c_clgstr ("list2",    cl_argv[5], BUFSIZ);
	control.isCRUser = c_clgetb ("crate");
	control.divZero_replace = l_clgetf ("divzero");
	control.verbose = c_clgeti ("verbose");

	/* Print id string. */
	if (control.verbose) {
	    now = time (NULL);
	    strftime (timeStamp, 60, 
            "%a %H:%M:%S %Z %d-%b-%Y", localtime(&now));
	    sprintf (ErrText,"MSARITH v%s at %s\n", MSARITH_VERSION, 
                     timeStamp);
	    n_message (ErrText);
	}

	/* Initialize control structure parameters */
	control.num_operand.error = 0.0;
	control.divZero           = 0;
	control.ngroup1           = 0;
	control.ngroup2           = 0;
	control.list1             = NULL;
	control.list2             = NULL;

	/* Retrieve operator */
	if ((control.oper = strpbrk (cl_argv[1], OPERATORS)) == NULL) {
	    syntax_err("Invalid operator.");
	    return (1);
	}

	/* To allow a constant as first operand (a feature not in the 
           original design...) we cheat by swapping the first and the 
           second operands. Routine n_getOper is used on the first operand 
           just to see if it is a file spec or a constant. The isInv
           flag is set to tell the underlying math routines (n_doArith)
           to perform the appropriate inverse operation.
         */
	control.isInv = False;
	n_getOper (cl_argv[0], &control);
	if (!(control.isFile)) {
	    strcpy (str, cl_argv[0]);
	    strcpy (cl_argv[0], cl_argv[2]);
	    strcpy (cl_argv[2], str);
	    strcpy (str, cl_argv[4]);
	    strcpy (cl_argv[4], cl_argv[5]);
	    strcpy (cl_argv[5], str);
	    control.isInv = True;
	}

	/* From this point on, the first operand is always a file list. */

	/* Now retrieve 2nd operand. */
	n_getOper (cl_argv[2], &control);
	if (control.isFile)
	    strcpy (f2nd, control.infile2);

	/* Get group lists. */
	nl1 = n_getGroupList (&control, cl_argv[4], 1);
	if (control.isFile)
	    nl2 = n_getGroupList (&control, cl_argv[5], 2);

	/* This logic is necessary to enable one single group in operand2
           even when operand1 has more than one group. */
	control.ngroup1 = nl1;
	if (control.isFile) {
	    if (((nl2 == 0) && (nl1 != 0))   ||
	        ((nl2 >  1) && (nl1 != nl2))) {
	        n_error ("IMSET lists have incompatible sizes.");
	        return (1);
	    } else
	        control.ngroup2 = nl2;
	}

	/* Check division by zero; abort if such */
	if ( ((*(control.oper)) == '/') && !(control.isFile) &&
	     (fabs (control.num_operand.value) < DBL_MIN)) {
	    n_error ("Division by a zero constant.");
	    return (1);
	}

	/* First operand is trivial, it's always a file name/list. */
	if ((listi1 = c_imtopen (cl_argv[0])) == (IRAFPointer)NULL) {
	    sprintf (ErrText, "Error in first file name/list.");
	    n_error (ErrText);
	    return (1);
	}

	/* If second operand is a file name, check that input lists
	   have either the same size or 2nd list has only one file. */
	if (control.isFile) {
	    if ((listi2 = c_imtopen (f2nd)) == (IRAFPointer)NULL) {
	        sprintf (ErrText, "Error in second file name/list.");
	        n_error (ErrText);
	        return (1);
	    }
	    if ((c_imtlen (listi1) != c_imtlen (listi2)) &&
	        (c_imtlen (listi2) != 1)) {
	        c_imtclose (listi1);
	        c_imtclose (listi2);
	        sprintf (ErrText, "Discrepancy in number of input files.");
	        n_error (ErrText);
	        return  (1);
	    }
	    /* If 2nd list has only one file, get its name once and for all */
	    if (c_imtlen (listi2) == 1) {
	        c_imtgetim (listi2, control.infile2, SZ_NAME);
	        multiFile = False;
	    } else
	        multiFile = True;
	}

	/* Look for a directory path as output file. I/O processing will
	   be different for a directory as opposed to a plain list. */
	if (c_isdirectory (cl_argv[3], dirName, SZ_NAME) > 0) {

	    /* Expand input list(s) */
	    while (c_imtgetim (listi1, control.infile1, SZ_NAME) != IRAF_EOF) {
	        if (control.isFile && multiFile)
	            c_imtgetim (listi2, control.infile2, SZ_NAME);

	        /* Get instrument type and number of groups. */
	        if (n_fileInfo (control.infile1, &inst1, &ng1, &(control.cr1))) 
	            return (1);
	        if (control.isFile) {
	            if (n_fileInfo (control.infile2, &inst2, &ng2, 
                                    &(control.cr2)))
	                return (1);
	            if (inst1 != inst2) {
	                n_error (msg1);
	                return (1);
	            }
	            if ((ng1 != ng2) && (nl1 != 0 || nl2 != 0)) {
	                n_error (msg2);
	                return (1);
	            }

	            /* If actual number of groups in files is smaller
                       than number of groups in lists, supersede them. */
	            if (ng1 < nl1)
	                control.ngroup1 = ng1;
	           if (ng2 < nl2)
	                control.ngroup2 = ng2;
	        }

	        /* Output file names are copies from first input list. */
		rootLength = c_fnldir (control.infile1, str, SZ_NAME);
		strcpy (control.outfile, dirName);
		strcat (control.outfile, control.infile1+rootLength);

	        /* Control structure is all set; do it ! */	
    	        if (n_doSingleFile (&control, inst1, ng1)) {
	            sprintf (ErrText, "MSARITH: Error.");
	            n_error (ErrText);
	            return (2);
	        }
	    }
	} else {
	    /* Not a directory, so check to see if output
	       list size agrees with input list size. */
	    if ((listo = c_imtopen (cl_argv[3])) == (IRAFPointer)NULL) {
	        sprintf (ErrText, "Error in output file name/list.");
	        n_error (ErrText);
	        return  (1);
	    }
	    if (c_imtlen (listi1) != c_imtlen (listo)) {
	        c_imtclose (listi1);
	        if (control.isFile)
	            c_imtclose (listi2);
	        c_imtclose (listo);
	        sprintf (ErrText, "Discrepancy in number of I and O files.");
	        n_error (ErrText);
	        return  (1);
	    } else {

	        /* Expand input list(s), and output as well. */
	        while (c_imtgetim (listi1, control.infile1, SZ_NAME) != 
	               IRAF_EOF) {
	            if (control.isFile && multiFile)
	                c_imtgetim (listi2, control.infile2, SZ_NAME);
	            c_imtgetim (listo, control.outfile, SZ_NAME);

	            /* Get instrument type. */
	            if (n_fileInfo (control.infile1, &inst1, &ng1, 
                                    &(control.cr1)))
	                return (1);

	            if (control.isFile) {
	                if (n_fileInfo (control.infile2, &inst2, &ng2, 
                                        &(control.cr2)))
	                    return (1);
	                if (inst1 != inst2) {
	                    n_error (msg1);
	                    return (1);
	                }
	                if ((ng1 != ng2) && (nl1 == 0 || nl2 == 0)) {
	                    n_error (msg2);
	                    return (1);
	                }

	                /* If actual number of groups in files is smaller
                           than number of groups in lists, supersede them. */
	                if (ng1 < nl1)
	                    control.ngroup1 = ng1;
	                if (ng2 < nl2)
	                    control.ngroup2 = ng2;
	            }

	            /* Control structure is all set; do it ! */	
    	            if (n_doSingleFile (&control, inst1, ng1)) {
	                sprintf (ErrText, "MSARITH: Error.");
	                n_error (ErrText);
	                return  (2);
	            }
	        }
	    }
	}

	/* Now close the I/O lists. */
	c_imtclose (listi1);
	if (control.isFile)
	    c_imtclose (listi2);
	if (strlen (dirName) == 0);
	    c_imtclose (listo);

	/* Free memory. Shouldn't this be done at each error return as well ?*/
	if (control.list1 != NULL)
	    free (control.list1);
	if (control.list2 != NULL)
	    free (control.list2);

	return (0);
}
