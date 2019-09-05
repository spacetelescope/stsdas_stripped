# include <stdio.h>
# include <hstio.h>
# include "msarith.h"
# include "n_math.h"

/*  This module calls the appropriate math library routine that
    performs the requested operation upon the apropriate arrays.


    Revision history:
    ---------------
    01 Mar 96  -  Implementation (IB)
    06 May 97  -  Support for inverse division and subtraction (IB)
    03 Jul 97  -  Fixed access of non-initialized memory (ac->num_operand) (IB)

*/

int n_doArith (arithControl *ac, GenericGroup *input1,
                                 GenericGroup *input2) {

	/* Local variables */
	mathConst constant;

	if (!ac->isFile) {
	    constant.value = ac->num_operand.value;
	    constant.error = ac->num_operand.error;
        }

	/* Select operation. */
	switch (*(ac->oper)) {
	    case '+':
	        if (ac->isFile) {
	            if (n_imageAdd (input1, input2, ac->isCountRate,
	                            ac->verbose, &(ac->divZero), 
                                    ac->divZero_replace))
	                return (2);
	        } else {
	            if (n_constAdd (input1, &constant))
	                return (2);
	        }
	        break;

	    case '-':
	        if (ac->isFile) {
	            if (n_imageSub (input1, input2))
	                return (2);
	        } else if (ac->isInv) {
	            if (n_constSubInv (input1, &constant))
	                return (2);
	        } else  {
	            if (n_constSub (input1, &constant))
	                return (2);
	        }
	        break;

	    case '*':
	        if (ac->isFile) {
	            if (n_imageMult (input1, input2))
	                return (2);
	        } else {
	            if (n_constMult (input1, &constant, ac->isCountRate))
	                return (2);
	        }
	        break;

	    case '/':
	        if (ac->isFile) {
	            if (n_imageDiv (input1, input2, ac->verbose,
	                            &(ac->divZero), ac->divZero_replace))
	                return (2);
	        } else if (ac->isInv) {
	            if (n_constDivInv (input1, &constant, ac->verbose, 
                                       &(ac->divZero), ac->divZero_replace))
	                return (2);
	        } else {
	            if (n_constDiv (input1, &constant, ac->isCountRate))
	                return (2);
	        }
	        break;

	    default:
	        n_error ("Invalid arithmetic operator.");
	        return (2);
	        break;
	}

	/* Successful return */
	return (0);
}


