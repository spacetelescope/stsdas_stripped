# include <stdio.h>
# include <stdlib.h>
# include <string.h>
# include <numeric.h>
# include <c_iraf_priv.h>
# include <xtables.h>		/* For the IRAF INDEF macros */
# if defined(NATIVE_IRAF)
# include <xclio.h>
# endif
# include "pedsub.h"

# define	OPERAND_LONG	1	/* must conform to the definitions */
# define	OPERAND_DOUBLE	2	/* used in get_numeric routine     */

/*   N_INPUTPAR  -  Read input parameters from CL or command line.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	20-May-1999	Implementation.
**
*/

int n_inputParameters (int argc, char **argv, char *listName,
                       char *outName, PedInfo *info, Bool *verbose) {

/* Arguments:
**	argc     i: number of command line arguments
**	argv     i: command line arguments
**	listName o: list of input file names
**	outName  o: list of output file names
**	info	 o: task info structure
**	verbose  o: verbose output switch
*/

# if defined(NATIVE_IRAF)

	float l_clgetf(char *);
	short n_getPixelMask(void);
	int n_getStatRegions (PedInfo *);

	/* This section reads parameters from the CL. The float
         * parameter input only works if the C binding is
         * locally compiled.
         */
	c_clgstr ("input", listName,  SZ_NAME);
	c_clgstr ("output", outName,  SZ_NAME);
	info->FilterName[0] = '\0';
	c_clgstr ("filter", info->FilterName, SZ_NAME);
	if (strncmp (info->FilterName, "none", 4) == 0)
	    info->Filter = NONE;
	else if (strncmp (info->FilterName, "median", 6) == 0)
	    info->Filter = MEDIAN;
	else
	    info->Filter = MASK;

	if (info->Filter != NONE) {
	    info->MMin     = c_clgeti ("minbox");
	    info->MMax     = c_clgeti ("maxbox");
	}
	info->DoRefine = c_clgetb ("dorefine");
	if (info->DoRefine) {
	    info->Nrefine  = c_clgeti ("nrefine");
	    info->PedStep  = l_clgetf ("refstep");
	    info->Morder   = c_clgeti ("reforder");
	}
	info->EqQuads = c_clgetb ("doquadeq");
	if (info->EqQuads) {
	    info->EqOrder = c_clgeti ("eqorder");
	    info->EqPix1  = c_clgeti ("eqpix1");
	    info->EqPix2  = c_clgeti ("eqpix2");
	    info->EqFlat  = c_clgetb ("eqflat");
	}
	info->FlatName[0] = '\0';
	c_clgstr ("flatfield", info->FlatName, SZ_NAME);
	info->LogName[0] = '\0';
	c_clgstr ("logfile", info->LogName, SZ_NAME);
	*verbose      = c_clgetb ("verbose");

	if (n_getStatRegions(info))
	    return (1);

	if (c_clgetb ("dqon"))
	    info->BitMask = n_getPixelMask();
	else
	    info->BitMask = 0;

# else

	int n_getDouble (char *parameter, double *result);

	/* This section reads parameters from the command line. 
         * Syntax is:
         *
         * pedsub @inlist @outlist [flatname] [filter] [minbox] [maxbox]
         *
         * and missing parameters are set to default values.
         */

	double  work;

	if (argc < 3) return (1);
	strcpy (listName, argv[1]);
	strcpy (outName, argv[2]);

	/* Set default values for params */
	info->BitMask     = 4095;
	strcpy (info->FilterName, "mask");
	info->MMin        = (int)IRAF_INDEFI;
	info->MMax        = (int)IRAF_INDEFI;
	info->DoRefine    = True;
	info->Nrefine     = 41;
	info->PedStep     = (float)IRAF_INDEFR;
	info->Morder      = 5;
	info->EqQuads     = True;
	info->EqOrder     = 1;
	info->EqPix1      = 2;
	info->EqPix2      = 10;
	info->EqFlat      = True;
	info->FlatName[0] = '\0';
	info->LogName[0]  = '\0';
	*verbose         = True;

	if (argc > 3) {
	    strcpy (info->FlatName, argv[3]);
	}
	if (argc > 4) {
	    strcpy (info->FilterName, argv[4]);
	}
	if (argc > 5) {
	    if (n_getDouble (argv[5], &work))
		return (1);
	    info->MMin = (int)work;
	}
	if (argc > 6) {
	    if (n_getDouble (argv[6], &work))
		return (1);
	    info->MMax = (int)work;
	}
	if (argc > 7) {
	    if (n_getDouble (argv[7], &work))
		return (1);
	    info->Nrefine = (int)work;
	}
	if (argc > 8) {
	    if (n_getDouble (argv[8], &work))
		return (1);
	    info->PedStep = (float)work;
	}
	if (argc > 9) {
	    if (n_getDouble (argv[9], &work))
		return (1);
	    info->Morder = (int)work;
	}
	if (argc > 10) {
	    strcpy (info->LogName, argv[10]);
	}

	if (strncmp (info->FilterName, "none", 4) == 0)
	    info->Filter = NONE;
	else if (strncmp (info->FilterName, "median", 6) == 0)
	    info->Filter = MEDIAN;
	else
	    info->Filter = MASK;

# endif

	/* Set the log file switch */
	if (strlen(info->LogName) == 0)
	    info->KeepLog = False;
	else
	    info->KeepLog = True;

	/* Set the median box limits to default values, if necessary */
	if (info->Filter != NONE) {
	    if (info->MMin == (int)IRAF_INDEFI) {
		if (info->Filter == MEDIAN) info->MMin = MMin_1_Default;
		if (info->Filter == MASK)   info->MMin = MMin_2_Default;
	    }
	    if (info->MMax == (int)IRAF_INDEFI) {
		if (info->Filter == MEDIAN) info->MMax = MMax_1_Default;
		if (info->Filter == MASK)   info->MMax = MMax_2_Default;
	    }
	}

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

