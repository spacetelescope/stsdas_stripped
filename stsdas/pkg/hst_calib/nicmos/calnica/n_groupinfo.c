# include <stdio.h>
# include <string.h>

# include <hstio.h>	/* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_GETGROUPINFO: Get group-specific information, such as exposure time,
** data units, and TDF status. Also check input group for null data.
**
** Revision history:
** H.Bushouse	Aug. 1996	Written for Build 2 (Version 2.0)
** H.Bushouse	28-Jul-1997	Changed NicInfo.bunit and tdftrans from scalar
**				to vector (Version 3.0)
** H.Bushouse	09-Feb-1999	Updated use of getKey routines for HSTIO v2.1
**				(Version 3.2.2)
*/

int n_getGroupInfo (NicInfo *nic, SingleNicmosGroup *in) {

/* Arguments:
**	nic	io: NICMOS info structure
**	in	 i: input image data
*/

	/* Local variables */
	float datamin, datamax;		/* science image min and max */

	/* Function definitions */
	void compMinMaxf   (FloatTwoDArray *, float *, float *);
	int n_getExpTime   (NicInfo *, Hdr *);
	int n_getDelTime   (NicInfo *, Hdr *);
	int n_getDataUnits (NicInfo *, Hdr *);
	int n_getTDFTrans  (NicInfo *, Hdr *);

	/* Compute min and max of science image to check for null data */
	compMinMaxf (&in->sci.data, &datamin, &datamax);

	/* If both DATAMIN and DATAMAX = 0, then it's a null image */
	if (datamin == 0.0 && datamax == 0.0)
	    nic->NullData[nic->group-1] = True;
	else
	    nic->NullData[nic->group-1] = False;

	/* Get the exposure time */
	if (n_getExpTime (nic, &(in->sci.hdr)))
	    return (status);

	/* Get the delta times   */
	if (n_getDelTime (nic, &(in->sci.hdr)))
	    return (status);

	/* Get the data units */
	if (n_getDataUnits (nic, &(in->sci.hdr)))
	    return (status);

	/* Get the TDF status */
	if (n_getTDFTrans (nic, &(in->sci.hdr)))
	    return (status);

	/* Successful return */
	return (status = 0);
}

/* N_GETEXPTIME: Read the exposure time keyword from group header */

int n_getExpTime (NicInfo *nic, Hdr *header) {

/* Arguments:
**	nic	io: NICMOS info structure
**	header	 i: image header
*/

	/* Initialize the exposure time */
	nic->exptime[nic->group-1] = 0;

	/* Read the exposure time keyword from the header */
	if (getKeyD (header, "SAMPTIME", &(nic->exptime[nic->group-1]))) {
	    n_kwerr ("SAMPTIME", nic->filename);
	    return (status = 1);
	}

        /* Check the exposure time value for validity */
	if (nic->exptime[nic->group-1] < 0) {
	    sprintf (MsgText, "SAMPTIME keyword value \"%g\" not valid in %s",
			      nic->exptime[nic->group-1], nic->filename);
	    n_error (MsgText);
	    return (status = 1);
	} else if (nic->exptime[nic->group-1] == 0) {
	    if (!(nic->obsmode == MULTIACCUM && nic->group == nic->ngroups)) {
		sprintf (MsgText, "SAMPTIME equal to zero in %s",
			 nic->filename);
		n_warn (MsgText);
	    }
	}

	/* Successful return */
	return (status = 0);
}


/* N_GETDELTIME: Read the exposure time keyword from group header */

int n_getDelTime (NicInfo *nic, Hdr *header) {

/* Arguments:
**	nic	io: NICMOS info structure
**	header	 i: image header
*/

	/* Initialize the delta time */
	nic->delttime[nic->group-1] = 0;

	/* Read the delta time keyword from the header */
	if (getKeyD (header, "DELTATIM", &(nic->delttime[nic->group-1]))) {
	    n_kwerr ("DELTATIM", nic->filename);
	    return (status = 1);
	}

        /* Check the delta time value for validity */
	if (nic->delttime[nic->group-1] < 0) {
	    sprintf (MsgText, "DELTATIM keyword value \"%g\" not valid in %s",
			      nic->delttime[nic->group-1], nic->filename);
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Successful return */
	return (status = 0);
}


/* N_GETDATUNITS: Read the BUNIT keyword from group header */

int n_getDataUnits (NicInfo *nic, Hdr *header) {

/* Arguments:
**	nic	io: NICMOS info structure
**	header	 i: image header
*/

	/* Local variables */
	char units[9];				/* BUNIT keyword value */

	/* Read the BUNIT keyword */
	units[0] = '\0';
	if (getKeyS (header, "BUNIT", units)) {
	    n_kwerr ("BUNIT", nic->filename);
	    return (status = 1);
	}

	/* Check the BUNIT keyword value for validity */
	if (strncmp (units, "COUNTS/S", 8) == 0)
	    nic->bunit[nic->group-1] = COUNTRATE;
	else if (strncmp (units, "COUNTS", 6) == 0)
	    nic->bunit[nic->group-1] = COUNTS;
	else {
	    sprintf (MsgText, "Unrecognized BUNIT value \"%s\" in %s\n", units,
		     nic->filename);
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Successful return */
	return (status = 0);
}

/* N_GETTDFTRANS: Read the TDFTRANS keyword from group header */

int n_getTDFTrans (NicInfo *nic, Hdr *header) {

/* Arguments:
**	nic	io: NICMOS info structure
**	header	 i: image header
*/

	/* Read the TDFTRANS keyword */
	nic->tdftrans[nic->group-1] = -1;
	if (getKeyI (header, "TDFTRANS", &(nic->tdftrans[nic->group-1]))) {
	    n_kwerr ("TDFTRANS", nic->filename);
	    return (status = 1);
	}

	/* Check the TDFTRANS keyword value for validity */
	if (nic->tdftrans[nic->group-1] < 0) {
	    sprintf (MsgText, "TDFTRANS value \"%d\" in %s is illegal\n",
		     nic->tdftrans[nic->group-1], nic->filename);
	    n_error (MsgText);
	    return (status = 1);
	}

	/* Successful return */
	return (status = 0);
}

