# include <stdio.h>
# include <string.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_DOCALIB: Applies CALNICA calibration steps to an input science data file.
** All calibration steps modify the input image data in-place.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	27-Jan-1997	Modified updateTrailer and updateSciHdr in
**				n_calReport to use new ref.dummy variable.
**				(Version 2.1)
** H.Bushouse	28-Jul-1997	Added new n_zsigcalc routine; added use of zsig
**				image in n_nlincorr; added n_satcheck routine
**				(Version 3.0)
** H.Bushouse	12-Sep-1997	Changed "group" to "imset" in messages (Vsn 3.0)
** H.Bushouse	28-Oct-1997	Added checks for dummy dark data when calling
**				zsigcalc routine (Version 3.0)
** H.Bushouse	13-Feb-1998	Added call to n_calReport for ZSIG step; removed
**				zsig from argument list of this routine - zsig
**				is now local to this routine; changed name of
**				n_zsigcalc to n_zsigcorr (Version 3.2)
** H.Bushouse	06-Oct-1998	Removed checks for dummy ref data when calling
**				n_zsigcorr since it's now handled in
**				n_getRefData; added dark ref image as argument
**				to n_noiscalc (Version 3.3)
** H.Bushouse	09-Oct-1998	Added call to new n_barscorr routine (Vsn 3.3)
** H.Bushouse	09-Feb-1999	Updated use of putKey routines for HSTIO v2.1
**				(Version 3.2.2)
** H.Bushouse	12-Jan-2000	Restructured for V4.0. Removed calReport
**				routines to separate souce file n_calreport.c.
**				Added n_doBseq routine. (Version 4.0)
** H.Bushouse	20-Jun-2000	Renamed from n_InstCal to n_doCalib and combined
**				with the steps previously done separately in
**				n_docalib2 routine (Version 4.0)
** R.Jedrzeje   31-Jan-2002     Removed n_doBseq and n_doPed steps
*/

int n_doCalib (NicInfo *nic, MultiNicmosGroup *input,
	       SingleNicmosGroup *zoff, SingleNicmosGroup *mask,
	       SingleNicmosGroup *nois, SingleNicmosGroup *dark, NlinData *nlin,
	       SingleNicmosGroup *flat, PhotData *phot,
	       SingleNicmosGroup *crimage) {

/* Arguments:
**	nic	 i: NICMOS info structure
**	input	io: input image data
**	zoff	 i: MULTIACCUM zero-read image
**	mask	 i: bad pixel mask
**	nois	 i: read-noise image
**	dark	 i: dark current image
**	nlin	 i: non-linearity coefficients
**	flat	 i: flat field image
**	phot	 i: photometry parameters
**	crimage	 o: CR rejected image
*/

	/* Local variables */
	int i;				/* loop index */
	static SingleNicmosGroup zsig;	/* zero-read signal image */

	/* Function definitions */
	int n_doZsig (NicInfo *, MultiNicmosGroup *, SingleNicmosGroup *,
		      SingleNicmosGroup *, SingleNicmosGroup *, NlinData *,
		      SingleNicmosGroup *);
	int n_doZoff (NicInfo *, MultiNicmosGroup *, SingleNicmosGroup *);
	int n_doMask (NicInfo *, MultiNicmosGroup *, SingleNicmosGroup *);
	int n_doBias (NicInfo *, MultiNicmosGroup *);
	int n_doNois (NicInfo *, MultiNicmosGroup *, SingleNicmosGroup *,
		      SingleNicmosGroup *);
	int n_doDark (NicInfo *, MultiNicmosGroup *, SingleNicmosGroup *);
	int n_doNlin (NicInfo *, MultiNicmosGroup *, NlinData *         ,
		      SingleNicmosGroup *);
	int n_doBars (NicInfo *, MultiNicmosGroup *);
	int n_doFlat (NicInfo *, MultiNicmosGroup *, SingleNicmosGroup *);
	int n_doUnit (NicInfo *, MultiNicmosGroup *);
        int n_photcalc (NicInfo *, PhotData *, MultiNicmosGroup *);
	int n_cridcalc (NicInfo *, MultiNicmosGroup *, SingleNicmosGroup *, SingleNicmosGroup *);
	int n_backcalc (NicInfo *, MultiNicmosGroup *);
	int n_userwarn (NicInfo *, MultiNicmosGroup *);
	int n_statcalc (SingleNicmosGroup *);
	int n_copyGroup (SingleNicmosGroup *, SingleNicmosGroup *);
	void n_macheck (NicInfo *, MultiNicmosGroup *);
	void freeNlinData (NlinData *);

	/* Do MultiAccum zero-read signal correction */
	if (n_doZsig (nic, input, mask, nois, dark, nlin, &zsig))
	    return (status);

	/* Do MultiAccum zero-read subtraction */
	if (n_doZoff (nic, input, zoff))
	    return (status);
	freeSingleNicmosGroup (zoff);

	/* Do bad pixel masking */
	if (n_doMask (nic, input, mask))
	    return (status);
	freeSingleNicmosGroup (mask);

	/* Do BIAS subtraction */
	if (n_doBias (nic, input))
	    return (status);

	/* Do noise (error) calculation */
	if (n_doNois (nic, input, nois, dark))
	    return (status);
	freeSingleNicmosGroup (nois);
    
	/* Do dark subtraction */
	if (n_doDark (nic, input, dark))
	    return (status);
	freeSingleNicmosGroup (dark);

    /*also add the scaling factors to the global header for reference*/
    if(nic->ampScale != 1){
        sprintf(MsgText,"Applied scaling factor=%g to AMPGLOW image",nic->ampScale);
        n_message(MsgText);
    }
    if(nic->linScale != 1){
        sprintf(MsgText,"Applied scaling factor=%g to LINEAR dark image",nic->linScale);
        n_message(MsgText);
   }
    
      
	/* Do linearity correction */
	if (n_doNlin (nic, input, nlin, &zsig))
	    return (status);
	freeNlinData (nlin);

	/* Free the zsig image data */
	if (nic->ZSIG.corr == PERFORM)
	    freeSingleNicmosGroup (&zsig);

	/* Do BARS flagging */
	if (n_doBars (nic, input))
	    return (status);

	/* Do flat fielding */

	if (n_doFlat (nic, input, flat))
	    return (status);

        /* Do photometric correction */
        if (n_photcalc (nic, phot, input))
            return (status);

	/* Do units conversion */

	if (n_doUnit (nic, input))
	    return (status);


	/* Do cosmic-ray identification and rejection */
	if (n_cridcalc (nic, input, crimage, flat))
	    return (status);

        /* Do predicted background calculation */
        if (n_backcalc (nic, input))
            return (status);

        /* Generate User Warnings */
	if (n_userwarn (nic, input))
	    return (status);

	/* Check MultiACCUM groups for decreasing counts */
	if (nic->obsmode == MULTIACCUM) n_macheck (nic, input);

	/* Compute statistics for calibrated data */
	for (i=0; i < nic->ngroups; i++) {
	     if (n_statcalc (&(input->group[i])))
		 return (status);
	}
	if (nic->CRID.corr != OMIT) {
	  if (n_statcalc (crimage))
	      return (status);
	}

	/* Successful return */
	return (status = 0);
}

