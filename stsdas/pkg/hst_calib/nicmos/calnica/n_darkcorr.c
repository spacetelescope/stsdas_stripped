# include <stdio.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_DODARK: Call DARKCORR routine for each readout of a MultiAccum.
**
** Revision history:
** H.Bushouse	12-Jan-2000	Created for Version 4.0
**
**
*/

int n_doDark (NicInfo *nic, MultiNicmosGroup *input, SingleNicmosGroup *dark) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: image to be dark subtracted
**	dark	 i: dark current image
*/

	/* Local variables */

        int darkgroup;

	/* Function definitions */
	int n_getDarkImage (NicInfo *, SingleNicmosGroup *, int);
	int n_darkcorr (NicInfo *, SingleNicmosGroup *, SingleNicmosGroup *);
	int n_calReport (CalStep *, int, Hdr *, Hdr *);

	/* Do the dark current subtraction for each group */
	for (nic->group=nic->ngroups; nic->group >= 1; nic->group--) {

		if (nic->DARK.corr == PERFORM) {
			if (n_getDarkImage (nic, dark, nic->group))
		     return (status);
			if (nic->writedark) {
				if (!strcmp (nic->darkmeth, "TEMPERATURE-DEPENDENT")) {
					darkgroup = 26 - (nic->ngroups - nic->group);
					if (!putSingleNicmosGroup (nic->dyndarkfile, darkgroup, dark, 0)) {
					}
				}
			}
			if (n_darkcorr (nic, &(input->group[nic->group-1]), dark))
				return (status);
		}

		n_calReport (&nic->DARK, nic->group,
								 &input->group[nic->group-1].sci.hdr,
								 input->group[nic->group-1].globalhdr);
        if (putKeyF (input->group[0].globalhdr, "AMPSCALE",nic->ampScale, "")){
            return(status=1);
        }
       if (putKeyF (input->group[0].globalhdr, "LINSCALE",nic->linScale, "")){
            return(status=1);
        }


             
	}
	if (nic->writedark && (nic->DARK.corr == PERFORM)) {
	  if (!strcmp (nic->darkmeth, "TEMPERATURE-DEPENDENT")) {
	    sprintf (MsgText,
							 "Dynamically-generated dark reference file written to %s\n",
							 nic->dyndarkfile);
	    n_message (MsgText);
	  } else {
	    sprintf (MsgText, "nic->darkmeth = %s\n", nic->darkmeth);
	    n_message (MsgText);
	  }
	}
	/* Successful return */
	return (status = 0);
}

/* N_DARKCORR: Subtract dark current from NICMOS image. The dark
** current image is selected from the DARKFILE reference file
** and subtracted in-place from the science image. The dark
** image errors and DQ flags are combined with the science
** data errors and DQ flags. The input SAMP and TIME arrays
** are unchanged.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Nov.  1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	12-Sep-1997	Changed "frame" to "imset" in messages
**				(Version 3.0)
** H.Bushouse	20-Aug-1999	Changed n_math fn's from type "int" to "void"
**				(Version 3.3)
*/

int n_darkcorr (NicInfo *nic, SingleNicmosGroup *input,
		SingleNicmosGroup *dark) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	dark	 i: dark current image
**	input	io: image to be dark subtracted
*/

	/* Function definitions */
	void n_asub (SingleNicmosGroup *, SingleNicmosGroup *);

	/* If the exposure time is zero, don't bother with subtraction */
	if (nic->exptime[nic->group-1] == 0)
	    return (status = 0);

	/* Report which ref file frames are being used */
	if (nic->DarkType == MATCH) {
	    sprintf (MsgText,
	   "DARKCORR using dark imset %2d for imset %2d with exptime=%8.6g",
		     nic->darkframe1, nic->group, nic->exptime[nic->group-1]);
	    n_message (MsgText);

	} else if (nic->DarkType == INTERP) {
	    sprintf (MsgText,
		     "DARKCORR using dark imsets %d and %d for imset %d",
		     nic->darkframe1, nic->darkframe2, nic->group);
	    n_warn (MsgText);
	    sprintf (MsgText,
		     "         interpolated to exptime=%g",
		     nic->exptime[nic->group-1]);
	    n_warn (MsgText);

	} else if (nic->DarkType == EXTRAP && nic->darkframe1 != 0) {
	    sprintf (MsgText, "DARKCORR using dark imset %d for imset %d",
		     nic->darkframe1, nic->group);
	    n_warn (MsgText);
	    sprintf (MsgText, "         extrapolated to exptime=%g",
		     nic->exptime[nic->group-1]);
	    n_warn (MsgText);

	} else if (nic->DarkType == EXTRAP && nic->darkframe2 != 0) {
	    sprintf (MsgText, "DARKCORR using dark imset %d for imset %d",
		     nic->darkframe2, nic->group);
	    n_warn (MsgText);
	    sprintf (MsgText, "         extrapolated to exptime=%g",
		     nic->exptime[nic->group-1]);
	    n_warn (MsgText);
	}

	/* Do the dark subtraction in-place in input */
	n_asub (input, dark);

	/* Successful return */
	return (status = 0);
}

