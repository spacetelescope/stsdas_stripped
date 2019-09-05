# include <stdio.h>
# include <c_iraf_priv.h>
# include <c_iraf.h>
# include <stdlib.h>
# include <numeric.h>
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "readnoise.h"

# define	OPERAND_LONG	1	/* must conform to the definitions */
# define	OPERAND_DOUBLE	2	/* used in get_numeric routine     */


/*   RN_INPUTPARAMETERS  -  Read input parameters from CL or command line.
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   29 Oct 96  -  Implementation (IB).
 *   07 Feb 97  -  New get_numeric (IB).
 *
 */


int rn_inputParameters (int argc, char **argv, char *listName1, 
                       char *listName2, char *output, Image *img, 
                       Algorithm *alg, int *memory, Bool *process, 
                       Bool *verbose) {


# if defined(NATIVE_IRAF)

	float l_clgetf(char *);

	/* This section reads parameters from the CL. The float
         * parameter input only works if the C binding is
         * locally compiled.
         */
	c_clgstr ("input1", listName1,  SZ_NAME);
	c_clgstr ("input2", listName2,  SZ_NAME);
	c_clgstr ("output", output,     SZ_NAME);
	alg->kclip     = l_clgetf ("kclip  ");
	alg->nclip     = c_clgeti ("nclip");
	alg->cleanfrac = l_clgetf ("cleanfrac");
	*memory        = c_clgeti ("memory");
	img->extver    = c_clgeti ("extver");
	*process       = c_clgetb ("process");
	*verbose       = c_clgetb ("verbose");


# else

	/* This section reads parameters from the command line. 
         * Syntax is:
         *
         * readnoise @lst1 @lst2 out kclip nclip cleanfrac memory extver
         *
         * and missing parameters are set to default values.
         */

	double  work;
	int rn_getDouble (char *, double *);

	if (argc < 4) return (1);
	strcpy (listName1, argv[1]);
	strcpy (listName2, argv[2]);
	strcpy (output,    argv[3]);
	alg->kclip     = 4.0F;
	alg->nclip     = 0;
	alg->cleanfrac = 0.0F;
	*memory        = 20;
        img->extver    = 1;
	*verbose       = True;
	*process       = True;
	if (argc > 4) {
	    if (rn_getDouble (argv[4], &work)) 
	        return (1);
	    alg->kclip = (float)work;
	}	
	if (argc > 5) {
	    if (rn_getDouble (argv[5], &work)) 
	        return (1);
	    alg->nclip = (int)work;
	}
	if (argc > 6) {
	    if (rn_getDouble (argv[6], &work)) 
	        return (1);
	    alg->cleanfrac = (float)work;
	}
	if (argc > 7) {
	    if (rn_getDouble (argv[7], &work)) 
	        return (1);
	    *memory = (int)work;
	}
	if (argc > 8) {
	    if (rn_getDouble (argv[8], &work)) 
	        return (1);
	    img->extver = (int)work;
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

int rn_getDouble (char *parameter, double *result) {

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

