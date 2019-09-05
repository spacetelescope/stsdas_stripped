# include <string.h>
# include "msarith.h"

/*  Tests incompatibility between NICMOS input image pixel type and count 
    rate task parameter. 

    Pixel type is derived from BUNIT keyword. Routine also checks 
    compatibility with file name suffixes.

    Only names terminated with the standard NICMOS suffixes _raw, _cal, 
    _bkg, _cmp and _mos are tested, other names are ignored. 

    Sets the count rate flag to appropriate mode based on file pixel type, 
    so file pixel type takes precedence over crate flag whenever possible.


      Revision history:
      ---------------
      05 Mar 96  -  Implementation (IB)
      13 Nov 96  -  BUNIT keyword support (IB)
      06 May 97  -  Support for inverse division and subtraction (IB)
      18 Aug 97  -  Replaced strchr by strrchr (IB)

*/

void n_checkPtype (arithControl *ac) {

	char	*underline;		/* pointer to "_" character */
	char	suf1[4];		/* holds suffixes */
	char	suf2[4];
	int	i;
	enum	{NotDef, 		/* no valid suffix, type not defined*/
	         Counts, 		/* suffix implies counts */
	         CountRate} 		/* suffix implies count rate */
	         type1, type2,		/* pixel type implied by suffix */
	         btype1, btype2;	/* pixel type implied by BUNIT  */

	char	warn1[] = "Incompatible value in `crate' parameter.";
	char	warn2[] = "Using image-defined count rate mode.";

	/* Extract file name suffixes. If no suffixes, ignore 
	   and return at once. */
	if ((underline = strrchr (ac->infile1, '_')) == NULL)
	    return;
	for (i=0; i < 3; i++)
	    suf1[i] = *(underline + 1 + i);
	suf1[3] = '\0';
	if (ac->isFile) {
	    if ((underline = strrchr (ac->infile2, '_')) == NULL)
	        return;
	    for (i=0; i < 3; i++)
	        suf2[i] = *(underline + 1 + i);
	    suf2[3] = '\0';
	}

	/* Find pixel type in each file, both the one implied by 
           BUNIT keyword as well as the one implied by suffix. */
	type1  = NotDef;
	btype1 = NotDef;
	if (strcmp (suf1, "raw") == 0)
	    type1 = Counts;
	else if ((strcmp (suf1, "cal") == 0) ||
	         (strcmp (suf1, "bkg") == 0) ||
	         (strcmp (suf1, "mos") == 0) ||
	         (strcmp (suf1, "cmp") == 0))
	    type1 = CountRate;
	if (ac->cr1)
	    btype1 = CountRate;
	else
	    btype1 = Counts;
	type2  = NotDef;
	btype2 = NotDef;
	if (ac->isFile) {
	    if (strcmp (suf2, "raw") == 0)
	        type2 = Counts;
	    else if ((strcmp (suf2, "cal") == 0) ||
	             (strcmp (suf2, "bkg") == 0) ||
	             (strcmp (suf1, "mos") == 0) ||
	             (strcmp (suf2, "cmp") == 0))
	        type2 = CountRate;
	    if (ac->cr2)
	        btype2 = CountRate;
	    else
	        btype2 = Counts;
	}

	/* Check compatibility between keyword and suffix. */
	if ((type1 != btype1) ||
            (type2 != btype2))
	    n_warn ("Incompatibility between BUNIT and suffix.");

	/* Handles the case where both operands are files. */
	if (ac->isFile) {

	    /* First test incompatibility between pixel types. Return
               at once if detected, do not touch count rate flag so
            flag set by user will take precedence.  */
	    if (((btype1 == CountRate) && (btype2 == Counts)) ||
	        ((btype1 == Counts)    && (btype2 == CountRate))) {
	        if (ac->verbose > 0)
	            n_warn ("Files have incompatible pixel units.");

	        /* This warning is issued only if the particular math
                   operation is affected by count rate mode. */
	        if ((*(ac->oper) == '+') && (ac->verbose > 0))
	            n_warn ("Using `crate' task parameter to set mode.");
	        return;
	    }

	    /* Pixel types agree, so test count rate flag and, if it does
               not agree with files, set it accordingly and issue warning.
               These actions take place only if the particular math
               operation is affected by count rate mode. */
	    else if ((btype1 == Counts) && (btype2 == Counts)) {
	        if ((ac->isCountRate) && (*(ac->oper) == '+')) {
	            ac->isCountRate = False;
	            if (ac->verbose > 0) {
	                n_warn (warn1);
	                n_warn (warn2);
	            }
	            return;
	        } 
	    }
	    else if ((btype1 == CountRate) && (btype2 == CountRate)) {
	        if (!(ac->isCountRate) && (*(ac->oper) == '+')) {
	            ac->isCountRate = True;
	            if (ac->verbose > 0) {
	                n_warn (warn1);
	                n_warn (warn2);
	            }
	            return;
	        }
	    }

	/* Handles the case (file) op (constant). Ignore the case of
           inverse division since this operation is defined to be 
           insensitive to the count rate flag. 
        */
	} else {
	    if ((btype1 == Counts) && 
                (ac->isCountRate)  &&
	        ((*(ac->oper) == '*') || (*(ac->oper) == '/'))) {
	        ac->isCountRate = False;
	        if (ac->verbose > 0 && !(ac->isInv)) {
	            n_warn (warn1);
	            n_warn (warn2);
	        }
	        return;
	    }
	    if ((btype1 == CountRate) && 
                (!(ac->isCountRate))  &&
	        ((*(ac->oper) == '*') || (*(ac->oper) == '/'))) {
	        ac->isCountRate = True;
	        if (ac->verbose > 0 && !(ac->isInv)) {
	            n_warn (warn1);
	            n_warn (warn2);
	        }
	        return;
	    }
	}

	return;
}
