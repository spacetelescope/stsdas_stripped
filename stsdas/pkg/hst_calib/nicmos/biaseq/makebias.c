# include "biaseq.h"

/*   MAKEBIAS  --  Make bias image.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	14-Apr-1999	Implementation.
**   H. Bushouse	08-May-200	Complete rewrite, splitting fitjumps
**					and subbias off to separate routines.
**
*/

int makeBias (SingleNicmosGroup *bias, SingleNicmosGroup *sky) {

/* Arguments:
**	bias	       io: bias image
**	sky		i: sky image
*/

	/* Local variables */
	int i, j;			/* loop indexes */
	float skytime;			/* sky image exposure time */
	float deltatime;		/* first difference image exp time */
	float ratio;			/* exposure time ratio */

	/* Get the sky image exposure time */
	if (getKeyF (sky->globalhdr, "EXPTIME", &skytime))
	    return (1);

	/* Get the first difference image exposure time */
	if (getKeyF (&(bias->sci.hdr), "DELTATIM", &deltatime)) {
	    n_error ("Error reading DELTATIM keyword.");
	    return (1);
	}

	ratio = deltatime / skytime;

	/* Subtract the scaled sky from the bias image */
	for (j=0; j < sky->sci.data.ny; j++) {
	     for (i=0; i < sky->sci.data.nx; i++) {

		  Pix(bias->sci.data,i,j) -= Pix(sky->sci.data,i,j)*ratio;

		  DQSetPix(bias->dq.data,i,j,
			DQPix(bias->dq.data,i,j) | DQPix(sky->dq.data,i,j) );
	     }
	}

	return (0);
}

