# include <stdio.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

/* N_UPDATEHDR: Update various keywords in output image header at the
** end of calibration processing. The keywords that get updated are:
**
** NEXTEND: set to 5*nic.group
** CALSTAGE: set to "CALNICA"
** DATAMIN, DATAMAX: updated in the header of all extensions
** FILENAME: set to name of file being written
** CAL_VER: set to CALNICA_VERSION
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	Aug.  1996	Upgraded for Build 2 (Version 2.0)
** H.Bushouse	25-Apr-1997	Update FILENAME keyword (Version 2.2)
** H.Bushouse	05-Jun-1997	Populate new CAL_VER keyword (Version 2.3)
** H.Bushouse	09-Feb-1999	Updated use of putKey routines for HSTIO v2.1
**				(Version 3.2.2)
** H.Bushouse	20-Jun-2000	Removed nic from argument list (Version 4.0)
*/

int n_updateHdr (SingleNicmosGroup *input, char *fname) {

/* Arguments:
**	input	io: input image
**	fname	 i: output file name
*/

	/* Local variables */
	char version[SZ_STRKWVAL+1];

	/* Function definitions */
	int updMinMaxf (FloatHdrData *);
	int updMinMaxs (ShortHdrData *);

	/* Update the "NEXTEND" keyword */
	if (putKeyI (input->globalhdr, "NEXTEND", 5*input->group_num, ""))
	    return (status = 1);

	/* Update the "FILENAME" keyword */
	if (putKeyS (input->globalhdr, "FILENAME", fname, ""))
	    return (status = 1);

	/* Update the "CALSTAGE" keyword */
	if (putKeyS (input->globalhdr, "CALSTAGE", "CALNICA", ""))
	    return (status = 1);

	/* Update the "CAL_VER" keyword */
	sprintf (version, "Version %s", CALNICA_VERSION);
	if (putKeyS (input->globalhdr, "CAL_VER", version, ""))
	    return (status = 1);

	/* Update the datamin/datamax keywords of the SCI image */
	if (updMinMaxf (&input->sci))
	    return (status);

	/* Update the datamin/datamax keywords of the ERR image */
	if (updMinMaxf (&input->err))
	    return (status);

	/* Update the datamin/datamax keywords of the DQ  image */
	if (updMinMaxs (&input->dq))
	    return (status);

	/* Update the datamin/datamax keywords of the SAMP image */
	if (updMinMaxs (&input->smpl))
	    return (status);

	/* Update the datamin/datamax keywords of the TIME image */
	if (updMinMaxf (&input->intg))
	    return (status);

	/* Successful return */
	return (status = 0);
}

int updMinMaxf (FloatHdrData *image) {

	/* Local variables */
	float min, max;		/* data min and max */

	/* Function definitions */
	void compMinMaxf (FloatTwoDArray *, float *, float *);

	/* Compute the min and max of the data array */
	compMinMaxf (&image->data, &min, &max);

	/* Update the DATAMIN keyword */
	if (putKeyF (&image->hdr, "DATAMIN", min, ""))
	    return (status = 1);

	/* Update the DATAMAX keyword */
	if (putKeyF (&image->hdr, "DATAMAX", max, ""))
	    return (status = 1);

	/* Successful return */
	return (status = 0);
}

int updMinMaxs (ShortHdrData *image) {

	/* Local variables */
	float min, max;		/* data min and max */

	/* Function definitions */
	void compMinMaxs (ShortTwoDArray *, float *, float *);

	/* Compute the min and max of the data array */
	compMinMaxs (&image->data, &min, &max);

	/* Update the DATAMIN keyword */
	if (putKeyF (&image->hdr, "DATAMIN", min, ""))
	    return (status = 1);

	/* Update the DATAMAX keyword */
	if (putKeyF (&image->hdr, "DATAMAX", max, ""))
	    return (status = 1);

	/* Successful return */
	return (status = 0);
}

void compMinMaxf (FloatTwoDArray *data, float *min, float *max) {

	/* Local variables */
	int i, j;		/* pixel indexes */
	float val;		/* pixel value */

	/* Compute the min and max of the data array */
	val = PPix(data,0,0);
	*min = val; *max = val;
	for (j=0; j<data->ny; j++) {
	     for (i=0; i<data->nx; i++) {
		  val = PPix(data,i,j);
		  if (val < *min) *min = val;
		  if (val > *max) *max = val;
	     }
	}
}

void compMinMaxs (ShortTwoDArray *data, float *min, float *max) {

	/* Local variables */
	int i, j;		/* pixel indexes */
	float val;		/* pixel value */

	/* Compute the min and max of the data array */
	val = PPix(data,0,0);
	*min = val; *max = val;
	for (j=0; j<data->ny; j++) {
	     for (i=0; i<data->nx; i++) {
		  val = PPix(data,i,j);
		  if (val < *min) *min = val;
		  if (val > *max) *max = val;
	     }
	}
}
