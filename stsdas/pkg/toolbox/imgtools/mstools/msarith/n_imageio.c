# include <stdio.h>
# include <hstio.h>
# include "msarith.h"

/*   N_IMAGEIO: Contains routines for reading and writing image
     data to be operated upon. 



     Revision history:
     ---------------
     01 Feb 96  -  Borrowed from CALNICA (IB)
     01 Mar 96  -  Modified to couple into NIMARITH data structure (IB)
     11 Nov 96  -  STIS support (IB)
     21 Apr 97  -  More flexible group input (IB)
     25 Mar 04  -  Add ACS support (as STIS; IB)

*/


/*  N_GETDATA: Read data from input file. One group is read. */

int n_getData (arithControl *ac, int fno, GenericGroup *in) {

/* Arguments:
**	ac	i: control structure
**	fno	i: file number (1st or 2nd operand)
**	in	o: input image data
*/

	switch (in->instrument) {
	case WFC3IR:
	case NICMOS:
	    initSingleNicmosGroup (in->sng);
	    fno-1 ? getSingleNicmosGroup (ac->infile2, ac->group2, in->sng)
	          : getSingleNicmosGroup (ac->infile1, ac->group1, in->sng);
	    if (hstio_err()) {
	        fno-1 ? n_filerr (ac->infile2)
	              : n_filerr (ac->infile1);
	        freeSingleNicmosGroup (in->sng);
	        return (1);
	    }
	    break;
	case STIS:
    case ACS:
    case COS:
	case WFC3UV:
	    initSingleGroup (in->sg);
	    fno-1 ? getSingleGroup (ac->infile2, ac->group2, in->sg)
	          : getSingleGroup (ac->infile1, ac->group1, in->sg);
	    if (hstio_err()) {
	        fno-1 ? n_filerr (ac->infile2)
	              : n_filerr (ac->infile1);
	        freeSingleGroup (in->sg);
	        return (1);
	    }
	    break;
	}

	return (0);
}

/*  N_PUTDATA: Write result data to ouput file. The file is created and
**  the primary header is written when writing the first data group. 
*/

int n_putData (arithControl *ac, GenericGroup *out) {

/* Arguments:
**	ac	i: control structure
**	in	i: input image data
*/

	switch (out->instrument) {
	case WFC3IR:
	case NICMOS:
	    putSingleNicmosGroup (ac->outfile, ac->ogroup, out->sng, 0); 
            break;
	case STIS:
    case ACS:
    case COS:
	case WFC3UV:
	    putSingleGroup (ac->outfile, ac->ogroup, out->sg, 0);
	    break;
	}

	if (hstio_err()) {
	    sprintf (ErrText, "Can't write to output image %s", ac->outfile);
	    n_error (ErrText);
	    return (1);
	}

	return (0);
}




void n_freeGenericGroup (GenericGroup *g) {

	switch (g->instrument) {
    	case WFC3IR:
	    case NICMOS: freeSingleNicmosGroup (g->sng); break;
    	case WFC3UV:
        case COS:
        case ACS:
	    case STIS:   freeSingleGroup (g->sg); break;
	}
}
