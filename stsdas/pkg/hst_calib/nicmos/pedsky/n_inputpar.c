# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <numeric.h>
# include <c_iraf_priv.h>
# include <xtables.h>		/* For the IRAF INDEF macros */
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "pedsky.h"

# define	OPERAND_LONG	1	/* must conform to the definitions */
# define	OPERAND_DOUBLE	2	/* used in get_numeric routine     */

# define	INDEF	(float)IRAF_INDEFR

/*   N_INPUTPAR  -  Read input parameters from CL or command line.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	04-May-1999	Implementation.
**
*/

int n_inputParameters (int argc, char **argv, char *listName,
                       char *outName, TaskInfo *info, Bool *verbose) {

# if defined(NATIVE_IRAF)

	/* Function declarations */
	float l_clgetf(char *);
	short n_getPixelMask(void);
	int   n_getStatRegions (TaskInfo *);

	/* This section reads parameters from the CL. The float
        ** parameter input only works if the C binding is
        ** locally compiled. */

	c_clgstr ("input", listName,  SZ_NAME);
	c_clgstr ("output", outName,  SZ_NAME);
	c_clgstr ("salgorithm", info->SkyModeName, SZ_NAME);
	info->SkyValue = l_clgetf ("skyvalue");
	info->SkySmin  = l_clgetf ("smin");
	info->SkySmax  = l_clgetf ("smax");
	info->keepFlags = c_clgetb ("keepflags");
	info->FlatName[0] = '\0';
	c_clgstr ("flatfield", info->FlatName, SZ_NAME);
	info->doRingMedian = c_clgetb ("rmedian");
	info->RingInner = l_clgetf ("rin");
	info->RingOuter = l_clgetf ("rout");
	info->MaxIter   = c_clgeti ("maxiter");
	info->Tolerance = l_clgetf ("tol");
	*verbose        = c_clgetb ("verbose");

	if (n_getStatRegions(info))
	    return (1);

	if (c_clgetb ("dqon"))
	    info->BitMask = n_getPixelMask();
	else
	    info->BitMask = 0;

# else

	/* This section reads parameters from the command line. 
         * Syntax is:
         *
         * pedsky @inlist @outlist [salgorithm] [skyvalue] [smin] [smax]
	 *	[flatfield]
         *
         * and missing parameters are set to default values.
         */

	double  work;
	int n_getDouble (char *parameter, double *result);

	if (argc < 3) return (1);
	strcpy (listName, argv[1]);
	strcpy (outName, argv[2]);

	strcpy (info->SkyModeName, "quick");
	info->SkyValue     = 0.0;
	info->SkySmin      = INDEF;
	info->SkySmax      = INDEF;
	info->MaxIter      = 12;
	info->Tolerance    = 0.002;
	info->keepFlags    = True;
	info->FlatName[0]  = '\0';
	info->BitMask      = 4095;
	info->doRingMedian = False;
	info->RingInner    = 6.0;
	info->RingOuter    = 12.0;
	*verbose           = True;

	if (argc > 3) {
	    strcpy (info->SkyModeName, argv[3]);
	}
	if (argc > 4) {
	    if (n_getDouble (argv[4], &work))
		return (1);
	    info->SkyValue = (float)work;
	}
	if (argc > 5) {
	    if (n_getDouble (argv[5], &work))
		return (1);
	    info->SkySmin = (float)work;
	}
	if (argc > 6) {
	    if (n_getDouble (argv[6], &work)) 
	        return (1);
	    info->SkySmax = (int)work;
	}
	if (argc > 7) {
	    strcpy (info->FlatName, argv[7]);
	}

# endif

	if (strncmp (info->SkyModeName, "none", 4) == 0)
	    info->SkyMode = NONE;
	else if (strncmp (info->SkyModeName, "constant", 8) == 0)
	    info->SkyMode = CONSTANT;
	else if (strncmp (info->SkyModeName, "iter", 4) == 0)
	    info->SkyMode = ITER;
	else
	    info->SkyMode = QUICK;

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
        return (float)rtn;
}
# endif

