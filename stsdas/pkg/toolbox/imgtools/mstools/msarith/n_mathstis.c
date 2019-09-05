# include <hstio.h>
# include <stdio.h>
# include <math.h>
# include <float.h>
# include <errno.h>
# include "msarith.h"

/*  Library with general utilities for performing arithmetic on 
    SingleGroup data groups (look for the NICMOS-specific twin 
    library n_mathnic.c)
  
    There are two basic types of operation: image X image and
    image X numeric constant. Separate routines are provided for
    each case. 



    Revision history:
    ---------------
    11 Nov 96  -  Implementation to support STIS (I. Busko)
    06 May 97  -  Support for inverse division and subtraction (IB)
    21 Aug 97  -  Propagate divisor's DQ when division-by-zero (IB)

*/




/*  --------------------------------------------------------------------

    Error routines specific for this math library.

    There are two routines, for handling the cases were one or both
    operands are files. The output DQ is either propagated from the
    divisor (if the divisor's DQ is non-zero) or set to BADPIX (if the
    divisor's DQ is zero).

   ----------------------------------------------------------------------
*/


void ns_div0err (SingleGroup *a, int i, int j, float replace, int verb) {
	if (verb > 1) {
	    sprintf (ErrText,"Division by zero: pixel %d %d flagged.",i+1,j+1);
	    n_warn (ErrText);
	}
	Pix(a->sci.data,i,j) = replace;
	Pix(a->err.data,i,j) = 0.0F;
	if (DQPix(a->dq.data,i,j)  == 0)
	    DQSetPix(a->dq.data,i,j, DATALOST);
}

void ns_div0err2 (SingleGroup *a, SingleGroup *b, int i, int j, 
        float replace, int verb) {
	if (verb > 1) {
	    sprintf (ErrText,"Division by zero: pixel %d %d flagged.",i+1,j+1);
	    n_warn (ErrText);
	}
	Pix(a->sci.data,i,j) = replace;
	Pix(a->err.data,i,j) = 0.0F;
	if (DQPix(b->dq.data,i,j)  == 0)
	    DQSetPix(a->dq.data,i,j, DATALOST);
	else
	    DQSetPix(a->dq.data,i,j, DQPix(b->dq.data,i,j));
}








/*  --------------------------------------------------------------------

                           MAIN ROUTINES                    

   ----------------------------------------------------------------------
*/



/* Add two SingleGroup triplets:

   (*a) = (*a) + (*b)

   The science arrays are added together, the error arrays are combined,
   the data quality arrays are "or'ed". 
*/
int ns_imageAdd (SingleGroup *a, SingleGroup *b,
	        int verbose, int *divZero, float replace) {

	int i, j;	/* array indexes */
	double aerr;	/* a error */
	double berr;	/* b error */

	errno = 0;
	for (j=0; j < a->sci.data.ny; j++) {
	    for (i=0; i < a->sci.data.nx; i++) {

	        /* data quality are or'ed */
	        DQSetPix(a->dq.data,i,j, DQPix(a->dq.data,i,j) | 
	                                 DQPix(b->dq.data,i,j) );

	        /* science and error data */
	        Pix(a->sci.data,i,j) = Pix(a->sci.data,i,j) + 
                                       Pix(b->sci.data,i,j);
	        aerr = Pix(a->err.data,i,j);
	        berr = Pix(b->err.data,i,j);
	        Pix(a->err.data,i,j) = sqrt(aerr*aerr + berr*berr);
	        if (errno)
	            n_sqrterr (&Pix(a->err.data,i,j), verbose);
	    }
	}
	return (0);
}




/* Add numeric constant to SingleGroup triplet:

   (*a) = (*a) + constant

   The constant is added to the science array. The constant error is
   combined with each pixel of the error array. The data quality array
   is left unchanged.

*/
int ns_constAdd (SingleGroup *a, mathConst *constant) {

	int i, j;	/* array indexes */
	double aerr;	/* a error */
	double berr2;	/* constant error squared */

	errno = 0;
	berr2 = constant->error * constant->error;

	for (j=0; j < a->sci.data.ny; j++) {
	    for (i=0; i < a->sci.data.nx; i++) {
	        Pix(a->sci.data,i,j)  = Pix(a->sci.data,i,j) + 
	                                (float)constant->value;
	        aerr = Pix(a->err.data,i,j);
	        Pix(a->err.data,i,j) = sqrt(aerr*aerr + berr2);
	        if (errno)
	            n_sqrterr (&Pix(a->err.data,i,j), 0);
	    }
	}
	return (0);
}




/* Subtract two SingleGroup triplets:

   (*a) = (*a) - (*b)

   The science data arrays are subtracted; the error arrays are combined; 
   the data quality arrays are "or'ed".

*/
int ns_imageSub (SingleGroup *a, SingleGroup *b) {

	int i, j;	/* array indexes */
	double aerr;	/* a error */
	double berr;	/* b error */

	errno = 0;
	for (j=0; j < a->sci.data.ny; j++) {
	    for (i=0; i < a->sci.data.nx; i++) {

	        /* science data */
	        Pix(a->sci.data,i,j)  = Pix(a->sci.data,i,j) - 
                                        Pix(b->sci.data,i,j);
	        /* error data */
	        aerr = Pix(a->err.data,i,j);
	        berr = Pix(b->err.data,i,j);
	        Pix(a->err.data,i,j) = sqrt(aerr*aerr + berr*berr);
	        if (errno)
	            n_sqrterr (&Pix(a->err.data,i,j), 0);

	        /* data quality */
	        DQSetPix(a->dq.data,i,j, DQPix(a->dq.data,i,j) | 
	                                 DQPix(b->dq.data,i,j) );
	    }
	}
	return (0);
}




/* Subtract numeric constant from SingleGroup triplet:

   (*a) = (*a) - constant

   The constant is subtracted from the science array. The constant error
   is combined with each pixel of the error array.The data quality array
   is left unchanged.

*/
int ns_constSub (SingleGroup *a, mathConst *constant) {

	int i, j;	/* array indexes */
	double aerr;	/* a error */
	double berr2;	/* constant error squared */

	errno = 0;
	berr2 = (float)(constant->error * constant->error);

	for (j=0; j < a->sci.data.ny; j++) {
	    for (i=0; i < a->sci.data.nx; i++) {
	        Pix(a->sci.data,i,j)  = Pix(a->sci.data,i,j) - 
                                        (float)constant->value;
	        aerr = Pix(a->err.data,i,j);
	        Pix(a->err.data,i,j) = sqrt(aerr*aerr + berr2);
	        if (errno)
	            n_sqrterr (&Pix(a->err.data,i,j), 0);
	    }
	}
	return (0);
}




/* Subtract SingleGroup triplet from numeric constant:

   (*a) = constant - (*a)

   Each pixel from the science array is subtracted from the constant.
   The constant error is combined with each pixel of the error array. 
   The DQ array is left unchanged.

*/
int ns_constSubInv (SingleGroup *a, mathConst *constant) {

	int i, j;	/* array indexes */
	double aerr;	/* a error */
	double berr2;	/* constant error squared */

	errno = 0;
	berr2 = (float)(constant->error * constant->error);

	for (j=0; j < a->sci.data.ny; j++) {
	    for (i=0; i < a->sci.data.nx; i++) {
	        Pix(a->sci.data,i,j)  = (float)constant->value -
                                        Pix(a->sci.data,i,j);
	        aerr = Pix(a->err.data,i,j);
	        Pix(a->err.data,i,j) = sqrt(aerr*aerr + berr2);
	        if (errno)
	            n_sqrterr (&Pix(a->err.data,i,j), 0);
	    }
	}
	return (0);
}




/* Multiply two SingleGroup triplets:

   (*a) = (*a) * (*b)

   The science data arrays are multiplied together; the error arrays are
   combined; the data quality arrays are "or'ed". 

*/
int ns_imageMult (SingleGroup *a, SingleGroup *b) {

	int i, j;	/* array indexes */
	double a_db;	/* a value * b error */
	double b_da;	/* b value * a error */

	errno = 0;
	for (j=0; j < a->sci.data.ny; j++) {
	    for (i=0; i < a->sci.data.nx; i++) {

	        /* error data */
	        a_db = Pix(a->sci.data,i,j) * Pix(b->err.data,i,j);
	        b_da = Pix(b->sci.data,i,j) * Pix(a->err.data,i,j);
	        Pix(a->err.data,i,j) = sqrt (a_db*a_db + b_da*b_da);
	        if (errno)
	            n_sqrterr (&Pix(a->err.data,i,j), 0);

	        /* science data */
	        Pix(a->sci.data,i,j)  = Pix(a->sci.data,i,j) *
                                        Pix(b->sci.data,i,j);

	        /* data quality */
	        DQSetPix(a->dq.data,i,j, DQPix(a->dq.data,i,j) | 
	                                 DQPix(b->dq.data,i,j) );
	    }
	}
	return (0);
}




/* Multiply numeric constant into SingleGroup triplet:

   (*a) = (*a) * constant

   The science array is multiplied by the constant; the error array is
   combined pixel-by-pixel with the constant error. The data quality
   array is not modified. 

*/
int ns_constMult (SingleGroup *a, mathConst *constant) {

	int i, j;	/* array indexes */
	double a_dct;	/* a value * constant error */
	double ct_da;	/* constant value * a error */

	errno = 0;
	for (j=0; j < a->sci.data.ny; j++) {
	    for (i=0; i < a->sci.data.nx; i++) {

	        /* error */
	        a_dct = Pix(a->sci.data,i,j) * constant->error;
	        ct_da = constant->value      * Pix(a->err.data,i,j);
	        Pix(a->err.data,i,j) = sqrt (a_dct*a_dct + ct_da*ct_da);
	        if (errno)
	            n_sqrterr (&Pix(a->err.data,i,j), 0);

	        /* science */
	        Pix(a->sci.data,i,j) = Pix(a->sci.data,i,j) * 
	                               (float)constant->value;
	    }
	}
	return (0);
}




/* Divide two SingleGroup triplets:

   (*a) = (*a) / (*b)

   The science data arrays are divided; the error arrays are combined; 
   the data quality arrays are "or'ed". Division by zero is processed by
   routine div0err2(), and the divZero counter tells the number of pixels
   where the exception happened. It has to be zeroed by the caller.

*/
int ns_imageDiv (SingleGroup *a, SingleGroup *b,
	        int verbose, int *divZero, float replace) {

	int i, j;	/* array indexes */
	double asci;	/* a science */
	double bsci;	/* b science */
	double bsci2;	/* b science squared */
	double aerr;	/* a error */
	double berr;	/* b error */

	void ns_div0err2 (SingleGroup *, SingleGroup *, int, int, float, int);

	errno = 0;
	for (j=0; j < a->sci.data.ny; j++) {
	    for (i=0; i < a->sci.data.nx; i++) {

	        asci = Pix(a->sci.data,i,j);
	        bsci = Pix(b->sci.data,i,j);
	        bsci2 = bsci*bsci;
	        if (fabs (bsci2) < DBL_MIN) { /* Division by zero */
	            ns_div0err2 (a, b, i, j, replace, verbose);
	            (*divZero)++;
	        } else {

	            /* error data */
	            aerr = Pix(a->err.data,i,j);
	            berr = Pix(b->err.data,i,j);
	            Pix(a->err.data,i,j) = sqrt(aerr*aerr/bsci2 + 
	                                   asci*asci*berr*berr/(bsci2*bsci2));
	            if (errno)
	                n_sqrterr (&Pix(a->err.data,i,j), verbose);

	            /* science data */
	            Pix(a->sci.data,i,j) = asci / bsci;

	            /* data quality */
	            DQSetPix(a->dq.data,i,j, DQPix(a->dq.data,i,j) | 
	                                     DQPix(b->dq.data,i,j) );
	        }
	    }
	}
	return (0);
}




/* Divide SingleGroup triplet by numeric constant:

   (*a) = (*a) / constant

   The science array is divided by the constant; the error array is
   combined pixel-by-pixel with the constant error. The data quality
   array is not modified. Division by zero has to be checked by the caller.

*/
int ns_constDiv (SingleGroup *a, mathConst *constant) {

	int i, j;	/* array indexes */
	double asci;	/* a science */
	double aerr;	/* a error */
	double cval;	/* constant value */
	double cval2;	/* constant value squared */
	double cerr;	/* constant error */
	double aux;

	errno = 0;
	cval  = constant->value;
	cval2 = cval*cval;
	cerr  = constant->error;
	aux   = cerr*cerr / (cval2*cval2);

	for (j=0; j < a->sci.data.ny; j++) {
	    for (i=0; i < a->sci.data.nx; i++) {

	        /* error */
	        asci = Pix(a->sci.data,i,j);
	        aerr = Pix(a->err.data,i,j);
	        Pix(a->err.data,i,j) = sqrt(aerr*aerr/cval2 + asci*asci*aux);
	        if (errno)
	            n_sqrterr (&Pix(a->err.data,i,j), 0);

	        /* science */
	        Pix(a->sci.data,i,j) = asci / cval;
	    }
	}
	return (0);
}




/* Divide numeric constant by SingleGroup triplet by:

   (*a) = constant / (*a)

   The constant is divided by each pixel in the science array. The 
   error array is combined pixel-by-pixel with the constant error. 
   The DQ array is left unchanged. Division by zero is processed by
   routine div0err(), and the divZero counter tells the number of pixels
   where the exception happened. It has to be zeroed by the caller.

*/
int ns_constDivInv (SingleGroup *a, mathConst *constant, 
                   int verbose, int *divZero, float replace) {

	int i, j;	/* array indexes */
	double cval;	/* constant value */
	double asci;	/* a science */
	double asci2;	/* a science squared */
	double cerr;	/* constant error */
	double aerr;	/* a error */

	void ns_div0err (SingleGroup *, int, int, float, int);

	cval = constant->value;
	cerr = constant->error;
	errno = 0;

	for (j=0; j < a->sci.data.ny; j++) {
	    for (i=0; i < a->sci.data.nx; i++) {

	        asci = Pix(a->sci.data,i,j);
	        asci2 = asci*asci;
	        if (fabs (asci2) < DBL_MIN) { /* Division by zero */
	            ns_div0err (a, i, j, replace, verbose);
	            (*divZero)++;
	        } else {

	            /* error data */
	            aerr = Pix(a->err.data,i,j);
	            Pix(a->err.data,i,j) = sqrt(cerr*cerr/asci2 + 
	                                   cval*cval*aerr*aerr/(asci2*asci2));
	            if (errno)
	                n_sqrterr (&Pix(a->err.data,i,j), verbose);

	            /* science data */
	            Pix(a->sci.data,i,j) = cval / asci;
	        }
	    }
	}
	return (0);
}
