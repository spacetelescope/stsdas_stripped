# include <stdio.h>
# include <string.h>
# include <stdlib.h>
# include <xtables.h>	/* For the IRAF INDEF macros */
# include <numeric.h>
# include <c_iraf_priv.h>
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "biaseq.h"

# define	OPERAND_LONG	1	/* must conform to the definitions */
# define	OPERAND_DOUBLE	2	/* used in get_numeric routine     */

/*   N_INPUTPAR  -  Read input parameters from CL or command line.
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	25-Mar-1999	Implementation.
**
*/

int n_inputParameters (int argc, char **argv, char *listName,
                       char *outName, BiasInfo *info, Bool *verbose) {

/* Arguments:
**	argc, argv	i: C main command line arguments
**	listName	o: input file list string
**	outName		o: output file list string
**	info	       io: task info structure
**	verbose		o: verbose output switch
*/

# if defined(NATIVE_IRAF)

	/* Function definitions */
	float l_clgetf(char *);
	short n_getPixelMask();

	/* This section reads parameters from the CL. The float
        ** parameter input only works if the C binding is
        ** locally compiled. */

	c_clgstr ("input", listName,  SZ_NAME);
	c_clgstr ("output", outName,  SZ_NAME);
	c_clgstr ("skysamps", info->skylist, SZ_NAME);
	info->nlow     = c_clgeti ("nlow");
	info->nhigh    = c_clgeti ("nhigh");
	info->fitJumps = c_clgetb ("fitjumps");
	if (info->fitJumps) {
	    info->jmp_filt  = c_clgeti ("filtsiz");
	    info->jmp_thresh= l_clgetf ("thresh");
	    info->keepJump  = c_clgetb ("keepjump");
	} else {
	    info->keepJump = False;
	}
	info->keepSky  = c_clgetb ("keepsky");
	info->keepBias = c_clgetb ("keepbias");
	*verbose       = c_clgetb ("verbose");

	info->bitmask = n_getPixelMask();
	if (!c_clgetb ("dqon"))
	    info->bitmask = 0;

# else

	/* This section reads parameters from the command line. 
         * Syntax is:
         *
         * biaseq @inlist @outlist skysamps [nlow] [nhigh]
         *
         * and missing parameters are set to default values.
         */

	double  work;

	int n_getDouble (char *, double *);

	info->skylist[0] = '\0';
	info->nlow       = (int)IRAF_INDEFI;
	info->nhigh      = (int)IRAF_INDEFI;
	info->fitJumps   = True;
	info->jmp_filt   = 15;
	info->jmp_thresh = 5.0;
	info->bitmask    = 4095;
	/*info->dqon     = False;*/
	info->keepSky    = False;
	info->keepBias	 = False;
	info->keepJump	 = False;
	*verbose         = True;

	if (argc < 4) return (1);
	strcpy (listName, argv[1]);
	strcpy (outName, argv[2]);
	strcpy (info->skylist, argv[3]);
	if (argc > 4) {
	    if (n_getDouble (argv[4], &work))
		return (1);
	    info->nlow = (int)work;
	}
	if (argc > 5) {
	    if (n_getDouble (argv[5], &work))
		return (1);
	    info->nhigh = (int)work;
	}
	if (argc > 6) {
	    if (n_getDouble (argv[6], &work)) 
	        return (1);
	    info->jmp_filt = (int)work;
	}	
	if (argc > 7) {
	    if (n_getDouble (argv[7], &work)) 
	        return (1);
	    info->jmp_thresh = (float)work;
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

