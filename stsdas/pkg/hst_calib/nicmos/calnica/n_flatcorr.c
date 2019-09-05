# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_DOFLAT: Call FLATCORR for each readout of a MultiAccum.
**
** Revision history:
** H.Bushouse	12-Jan-2000	Created for Version 4.0
*/

int n_doFlat (NicInfo *nic, MultiNicmosGroup *input, SingleNicmosGroup *flat) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: image to be flat fielded
**	flat	 i: flat field image
*/

	/* Local variables */

	/* Function definitions */
	int n_flatcorr (NicInfo *, SingleNicmosGroup *, SingleNicmosGroup *);
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	/* Do the flatfield correction for each group */
	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

	     if (nic->FLAT.corr == PERFORM) {
		 if (n_flatcorr (nic, &(input->group[nic->group-1]), flat))
		     return (status);
	     }

  		/*update the science header with the extension used*/
    	if(!strcmp(nic->flatmeth,"TEMPERATURE-DEPENDENT")){
	    	if (putKeyI (&input->group[nic->group-1].sci.hdr, "TDFGROUP",nic->flatext, ""))
            	return (status = 1);            
	    	if (putKeyI (input->group[0].globalhdr, "TDFGROUP",nic->flatext, ""))
            	return (status = 1);            

       	}
        
         n_calReport (&nic->FLAT, nic->group,
			  &input->group[nic->group-1].sci.hdr,
			  input->group[nic->group-1].globalhdr);
       
        
	}
    
	/* Successful return */
	return (status = 0);
}

/* N_FLATCORR: Flat field a NICMOS image. The science image
** is multiplied in-place by the (inverse) flat field image.
** Errors and DQ flags from the flat field are combined with
** the science data errors and flags. The input SAMP and TIME
** arrays are unchanged.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	20-Aug-1999	Changed n_math fn's from type "int" to "void"
**				(Version 3.3)
*/

int n_flatcorr (NicInfo *nic, SingleNicmosGroup *input,
		SingleNicmosGroup *flat) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: image to be flat fielded
**	flat	 i: flat field image
*/

	/* Function definitions */
	void n_amul (SingleNicmosGroup *, SingleNicmosGroup *);

	/* Do the flat fielding in-place in input image data */
	n_amul (input, flat);

	/* Successful return */
	return (status = 0);
}

