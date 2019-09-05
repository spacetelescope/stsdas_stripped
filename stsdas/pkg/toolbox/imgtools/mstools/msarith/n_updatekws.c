# include <hstio.h>
# include "msarith.h"

/*   Routines to update header keywords.



    Revision history:
    ---------------
    01 Feb 96  -  Borrowed from CALNICA (IB)
    12 Nov 96  -  STIS support (IB)
    14 May 97  -  Added n_intUpdateHeader and n_PutKeyI routines (IB)
    25 Mar 04  -  Add ACS support (as STIS; IB)

*/

int n_updateKws (GenericGroup *g) {

	/* Function definitions */
	int updMinMaxf (FloatHdrData *);
	int updMinMaxs (ShortHdrData *);

	switch (g->instrument) {
	case WFC3IR:
	case NICMOS:
	    if (updMinMaxf (&(g->sng->sci)))  return (1);
	    if (updMinMaxf (&(g->sng->err)))  return (1);
	    if (updMinMaxs (&(g->sng->dq)))   return (1);
	    if (updMinMaxs (&(g->sng->smpl))) return (1);
	    if (updMinMaxf (&(g->sng->intg))) return (1);
	    break;
	case STIS:
    case ACS:
    case COS:
	case WFC3UV:
	    if (updMinMaxf (&(g->sg->sci))) return (1);
	    if (updMinMaxf (&(g->sg->err))) return (1);
	    if (updMinMaxs (&(g->sg->dq)))  return (1);
	    break;
	}

	return (0);
}


/*  Update (or write) int keyword in primary header. */

int n_intUpdateHeader (char *name, char *keywname, int keyval, char *comment) {

	Hdr hdr;		/* header structure */
	IODescPtr im;		/* image descriptor */

	int n_PutKeyI (Hdr *, char *, int, char *);

	initHdr (&hdr);
	im = openUpdateImage (name, "", 0, &hdr);
	if (hstio_err())
	    return (1);
	if (n_PutKeyI (&hdr, keywname, keyval, comment))
	    return (1);
	putHeader (im);
	if (hstio_err())
	    return (1);
	closeImage (im);
	freeHdr (&hdr);
	return (0);
}


int updMinMaxf (FloatHdrData *image) {

	/* Local variables */
	float min, max;		/* data min and max */

	/* Function definitions */
	void compMinMaxf (FloatTwoDArray *, float *, float *);
	int  putKeyF (Hdr *, char *, float, char *);

	/* Compute the min and max of the data array */
	compMinMaxf (&image->data, &min, &max);

	/* Update the DATAMIN keyword */
	if (putKeyF (&image->hdr, "DATAMIN", min, ""))
	    return (1);

	/* Update the DATAMAX keyword */
	if (putKeyF (&image->hdr, "DATAMAX", max, ""))
	    return (1);

	/* Successful return */
	return (0);
}

int updMinMaxs (ShortHdrData *image) {

	/* Local variables */
	float min, max;		/* data min and max */

	/* Function definitions */
	void compMinMaxs (ShortTwoDArray *, float *, float *);
	int  putKeyF (Hdr *, char *, float, char *);

	/* Compute the min and max of the data array */
	compMinMaxs (&image->data, &min, &max);

	/* Update the DATAMIN keyword */
	if (putKeyF (&image->hdr, "DATAMIN", min, ""))
	    return (1);

	/* Update the DATAMAX keyword */
	if (putKeyF (&image->hdr, "DATAMAX", max, ""))
	    return (1);

	/* Successful return */
	return (0);
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


int n_PutKeyI (Hdr *hd, char *keyword, int value, char *comment) {

/* arguments:
Hdr *hd           i: pointer to header to be updated
char *keyword     i: name of keyword
int value         i: value to be updated or added
char *comment     i: comment to add, if keyword doesn't exist
*/

	FitsKw key;		/* location of keyword in header */

	key = findKw (hd, keyword);
	if (key == NotFound)
	    addIntKw (hd, keyword, value, comment);
	else
	    putIntKw (key, value);

	if (hstio_err())
	    return (1);

	return (0);
}


