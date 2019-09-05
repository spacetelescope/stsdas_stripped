# include <hstio.h>
# include <errno.h>
# include <stdio.h>
# include <math.h>
# include "msarith.h"


/*  Error routines specific for all underlying math libraries */

void n_sqrterr (float *errpix, int verb) {
	if (verb > 1)
	    n_warn ("Error in sqrt: pixel error zeroed.");
	*errpix = 0.0F;
	errno = 0;
}



/*  N_MATH:  Library with general utilities for performing arithmetic
    on generic data groups.
  
    There are three basic types of operation: image X image, 
    image X numeric constant and numeric constant X image. Separate 
    routines are provided for each case. Routines in this library only 
    take care of calling the appropriate instrument-dependent routine in 
    either n_mathnic or n_mathstis.



    Revision history:
    ---------------
    11 Nov 96  -  Implementation to support STIS (I. Busko)
    06 May 97  -  Support for inverse division and subtraction (IB)

*/




int n_imageAdd (GenericGroup *a, GenericGroup *b,
	        Bool crate, int verb, int *divZero, float rep) {

	if (a->instrument == NICMOS)
	    return (nn_imageAdd (a->sng, b->sng, crate, verb, divZero, rep));
	else 
	    return (ns_imageAdd (a->sg, b->sg, verb, divZero, rep));
}




int n_constAdd (GenericGroup *a, mathConst *constant) {

	if (a->instrument == NICMOS)
	    return (nn_constAdd (a->sng, constant));
	else 
	    return (ns_constAdd (a->sg, constant));
}




int n_imageSub (GenericGroup *a, GenericGroup *b) {

	if (a->instrument == NICMOS)
	    return (nn_imageSub (a->sng, b->sng));
	else 
	    return (ns_imageSub (a->sg, b->sg));
}




int n_constSub (GenericGroup *a, mathConst *constant) {

	if (a->instrument == NICMOS)
	    return (nn_constSub (a->sng, constant));
	else 
	    return (ns_constSub (a->sg, constant));
}




int n_constSubInv (GenericGroup *a, mathConst *constant) {

	if (a->instrument == NICMOS)
	    return (nn_constSubInv (a->sng, constant));
	else 
	    return (ns_constSubInv (a->sg, constant));
}




int n_imageMult (GenericGroup *a, GenericGroup *b) {

	if (a->instrument == NICMOS)
	    return (nn_imageMult (a->sng, b->sng));
	else 
	    return (ns_imageMult (a->sg, b->sg));
}




int n_constMult (GenericGroup *a, mathConst *constant, 
	         Bool count_rate) {

	if (a->instrument == NICMOS)
	    return (nn_constMult (a->sng, constant));
	else 
	    return (ns_constMult (a->sg, constant));
}




int n_imageDiv (GenericGroup *a, GenericGroup *b,
	        int verb, int *divZero, float rep) {

	if (a->instrument == NICMOS)
	    return (nn_imageDiv (a->sng, b->sng, verb, divZero, rep));
	else 
	    return (ns_imageDiv (a->sg, b->sg, verb, divZero, rep));

}




int n_constDiv (GenericGroup *a, mathConst *constant, Bool crate) {

	if (a->instrument == NICMOS)
	    return (nn_constDiv (a->sng, constant, crate));
	else 
	    return (ns_constDiv (a->sg, constant));
}




int n_constDivInv (GenericGroup *a, mathConst *constant, int verb, 
                   int *divZero, float rep) {

	if (a->instrument == NICMOS)
	    return (nn_constDivInv (a->sng, constant, verb, divZero, rep));
	else 
	    return (ns_constDivInv (a->sg, constant, verb, divZero, rep));
}
