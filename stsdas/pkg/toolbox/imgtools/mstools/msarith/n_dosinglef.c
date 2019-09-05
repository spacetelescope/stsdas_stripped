# include <stdlib.h>	/* malloc */
# include <c_iraf.h>
# include <hstio.h>
# include <stdio.h>
# include <math.h>
# include "msarith.h"


/*  This routine processes one single input/output image file, 
    expanding an eventual multi-group structure inside the input 
    file(s), and for each group: 
      - read data in appropriate structure(s);
      - perform the arithmetic;
      - update the headers;
      - write result.

    NICMOS file pixel types are checked for compatibility and count rate 
    mode is set accordingly.


    Revision history:
    ---------------
    01 Mar 96  -  Implementation (IB)
    12 Nov 96  -  STIS support (IB)
    13 Nov 96  -  Multi-group files (IB)
    06 Feb 97  -  Check extension in output file name (IB)
    28 Feb 97  -  Group number printout in verbose mode (IB)
    21 Apr 97  -  More flexible group input (IB)
    06 May 97  -  Print only if operand2 is a file (IB)
    14 May 97  -  Update NEXTEND keyword in output PHU (IB)

*/

int n_doSingleFile (arithControl *ac, Instrument instrument, int ngroups) {

	/* Local variables */
	GenericGroup input1;   /* Input image 1 and output image */
	GenericGroup input2;   /* Input image 2                  */
	int *list1 = NULL;     /* lists of groups                */
	int *list2 = NULL;
	int angroup;           /* actual number of groups        */
	int onextend;          /* number of extensions in output */
	int i;

	/* Function definitions */
	void n_checkPtype (arithControl *);
	void n_freeGenericGroup (GenericGroup *);
	int  n_checkSize (GenericGroup *, GenericGroup *);
	int  n_getData (arithControl *,int fno, GenericGroup *);
	int  n_putData (arithControl *,GenericGroup *);
	int  n_doArith (arithControl *,GenericGroup *,GenericGroup *);
	int  n_updateKws (GenericGroup *);
	int  n_history (arithControl *,GenericGroup *, GenericGroup *, int);
	int  n_checkOutName (arithControl *);
	int  n_intUpdateHeader (char *, char *, int, char *);

	/* Initialize GenericGroup structures. */
	input2.sng        = &(input2.dsng);
	input2.sg         = &(input2.dsg);
	input2.instrument = instrument;
	input1.sng        = &(input1.dsng);
	input1.sg         = &(input1.dsg);
	input1.instrument = instrument;

	/* Check compatibility between input image(s) pixel type
	   and count rate input flag. Set flag if appropriate. 
        */
	ac->isCountRate = ac->isCRUser;
	if (instrument == NICMOS)
            n_checkPtype (ac);

	/* Check that output file name carry proper extension. */
	if (n_checkOutName (ac))
            return (1);

	/* Make sure that it will NOT be appended to any existing file. */
	if (c_ximaccess (ac->outfile, IRAF_READ_WRITE))
	    c_imdelete (ac->outfile);

	/* Build local lists of groups. These may have repetitive entries. */
	angroup = (ac->ngroup1 > 0) ? ac->ngroup1 : ngroups;
	list1 = (int *) malloc (angroup * sizeof (int));
	if (ac->isFile)
	    list2 = (int *) malloc (angroup * sizeof (int));
	for (i = 0; i < angroup; i++) {
	    list1[i] = (ac->ngroup1 > 0) ? ac->list1[i] : i+1;
	    if (ac->isFile) {
	        if (ac->ngroup2 > 1)
	            list2[i] = ac->list2[i];
	        else if (ac->ngroup2 == 1)
	            list2[i] = ac->list2[0];
	        else
	            list2[i] = i+1;
	    }
	}

	/* Do for each group in file. The group counter is the output group
           number, since on output groups must be numbered sequentially
           starting from 1 
        */
	onextend = 0;
	for (ac->ogroup = 1; ac->ogroup <= angroup; ac->ogroup++) {

	    /* Get actual group ids for the two operands. */
	    ac->group1 = list1[(ac->ogroup)-1];
	    if (ac->isFile)
	        ac->group2 = list2[(ac->ogroup)-1];

	    if (ac->verbose > 0) {
	        if (ac->ngroup1 != 0 && ac->ngroup2 != 0)
	            sprintf (ErrText,
                    "Working on output IMSET %d (from inputs %d and %d) ...\n", 
                    ac->ogroup, ac->group1, ac->group2);
	        else 
	            sprintf (ErrText, "Working on IMSET %d ...\n", ac->ogroup);
	        n_message (ErrText);
	    }

	    ac->divZero = 0;  /* Reset div0 counter */ 

	    /* Load the input data */
	    if (n_getData (ac, 1, &input1))
		return (1);
	    if (ac->isFile) {
	        if (n_getData (ac, 2, &input2))
		    return (1);
	        if (n_checkSize (&input1, &input2)) {
	            n_error ("Incompatile axis sizes.");
		    return (1);
	        }
	    }

	    /* Do the arithmetic */
	    if (n_doArith (ac, &input1, &input2)) {
	        n_freeGenericGroup (&input1);
	        if (ac->isFile) 
	            n_freeGenericGroup (&input2);
	        return (1);
	    }

	    /* Append HISTORY records. */
	    if (n_history (ac, &input1, &input2, ngroups)) {
	        n_freeGenericGroup (&input1);
	        if (ac->isFile) 
	            n_freeGenericGroup (&input2);
	        return (1);
	    }

	    /* Update output file DATAMAX and DATAMIN */
	    if (n_updateKws (&input1))
	        return (1);

	    /* Write result into output file */
	    if (n_putData (ac, &input1)) {
	        n_freeGenericGroup (&input1);
	        if (ac->isFile) 
	            n_freeGenericGroup (&input2);
	        return (1);
	    }

	    /* Update number of extensions in output file. */
	    onextend += (instrument == NICMOS || instrument == WFC3IR) ? 5 : 3;

	    /* Warning messages for this group */
	    if (ac->verbose > 0) {
	        if (ac->divZero > 0) {
	            sprintf (ErrText, "Division by zero in %d pixels.", 
	                    ac->divZero);
	            n_warn (ErrText);
	        }
	    }

	    /* Free memory for this group */
	    n_freeGenericGroup (&input1);
	    if (ac->isFile) 
	        n_freeGenericGroup (&input2);
	}

	/* Update NEXTEND keyword in output primary header. */
	if (n_intUpdateHeader (ac->outfile, "NEXTEND", onextend, 
                               "number of extensions"))
	    return (1);

	/* Free list memory. */
	free (list1);
	if (ac->isFile)
	    free (list2);

	/* Successful return */
	return (0);
}

