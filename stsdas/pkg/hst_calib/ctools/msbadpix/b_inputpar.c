# include <stdio.h>
# include <c_iraf_priv.h>
# include <c_iraf.h>
# include <stdlib.h>
# include <numeric.h>
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "nbadpix.h"

# define	OPERAND_LONG	1	/* must conform to the definitions */
# define	OPERAND_DOUBLE	2	/* used in get_numeric routine     */

/*   B_INPUTPAR  -  Read input parameters from CL or command line.
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   02 Aug 96  -  Implementation (IB).
 *   21 Oct 96  -  Revised after code review (IB).
 *   07 Feb 97  -  New get_numeric (IB).
 *   30 May 97  -  Task renamed "msbadpix" (IB)
 *
 */


int b_inputParameters (int argc, char **argv, char *listName,
                       char *maskName, Counter *cou, Bool *verbose) {

# if defined(NATIVE_IRAF)

	float l_clgetf(char *);

	/* This section reads parameters from the CL. The float
         * parameter input only works if the C binding is
         * locally compiled.
         */
	c_clgstr ("input", listName,  SZ_NAME);
	c_clgstr ("output", maskName,  SZ_NAME);
	cou->window    = c_clgeti ("window");
	cou->threshold = l_clgetf ("threshold");
	cou->badfrac   = l_clgetf ("badfrac");
	cou->cleanfrac = l_clgetf ("cleanfrac");
	cou->kclip     = l_clgetf ("kclip  ");
	cou->nclip     = c_clgeti ("nclip");
	*verbose       = c_clgetb ("verbose");


# else

	/* This section reads parameters from the command line. 
         * Syntax is:
         *
         * msbadpix  inlist outmask window thresh badfrac cleanf kclip nclip
         *
         * and missing parameters are set to default values.
         */

	double  work;

	if (argc < 3) return (1);
	strcpy (listName, argv[1]);
	strcpy (maskName, argv[2]);
	cou->window    = 7;
	cou->threshold = 3.0F;
	cou->badfrac   = 0.8F;
	cou->cleanfrac = 0.0F;
	cou->kclip     = 4.0F;
	cou->nclip     = 0;
	*verbose       = True;
	if (argc > 3) {
	    if (n_getDouble (argv[3], &work)) 
	        return (1);
	    cou->window = (int)work;
	}	
	if (argc > 4) {
	    if (n_getDouble (argv[4], &work)) 
	        return (1);
	    cou->threshold = (float)work;
	}
	if (argc > 5) {
	    if (n_getDouble (argv[5], &work)) 
	        return (1);
	    cou->badfrac = (float)work;
	}
	if (argc > 6) {
	    if (n_getDouble (argv[6], &work)) 
	        return (1);
	    cou->cleanfrac = (float)work;
	}
	if (argc > 7) {
	    if (n_getDouble (argv[7], &work)) 
	        return (1);
	    cou->kclip = (float)work;
	}
	if (argc > 8) {
	    if (n_getDouble (argv[8], &work)) 
	        return (1);
	    cou->nclip = (int)work;
	}	

# endif

	return (0);
}

# if defined(NATIVE_IRAF)
# else

/*  Translates a character string to double. Returns 1 if error.
 *
 *  Uses the string-to-numeric conversion facility from hstio.
 *
 *  Notice that, because the way get_numeric works, if the string
 *  begins with a valid substring followed by garbage up to the NULL 
 *  terminator, the valid characters will be correctly translated and 
 *  the garbage ignored.
 *
 */

int n_getDouble (char *parameter, double *result) {

	NumericResult	constant;

	get_numeric(parameter, strlen(parameter), &constant);

	switch (constant.type) {
	    case OPERAND_LONG:   
	        *result = (double)constant.data.l; 
	        break;
	    case OPERAND_DOUBLE: 
	        *result = constant.data.d;         
	        break;
	    default:
	        return (1);
	}
	return (0);
}
# endif



/*  The float and double clget and clput routines in cvos$xclio.c do not
 *  work properly. They return/put garbage. But if we rename and compile 
 *  them locally they work.
 *
 *                                                         IB, 21/10/96
 */
# if defined(NATIVE_IRAF)
# if defined(NO_UNDERSCORE)
# define clgetr_ clgetr
# endif

          extern float clgetr_(short *);
float l_clgetf(char *param) {
        float rtn;
        clear_cvoserr();
        xerpsh_();
        rtn = clgetr_(char2iraf(param,1));
        if (xerpoi_())
            set_cvoserr();
        return rtn;
}
# endif

