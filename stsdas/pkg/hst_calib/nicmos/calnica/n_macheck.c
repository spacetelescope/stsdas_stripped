# include <stdio.h>

# include <hstio.h>     /* defines HST I/O functions */
# include "calnic.h"	/* defines NICMOS data structures */
# include "calnica.h"	/* defines CALNICA data structures */

# define  MACHECK_THRESH  3.0

/* N_MACHECK: Check MultiACCUM groups for decreasing counts from one
** readout to the next.
**
** Revision history:
** H.Bushouse	Feb. 1996	Written for Build 2 (Version 2.0)
** H.Bushouse	12-Sep-1997	Changed "group" to "imset" in messages (Vsn 3.0)
** H.Bushouse	08-Apr-1998	Added use of new n_pixOK function (Version 3.2)
*/

void n_macheck (NicInfo *nic, MultiNicmosGroup *input) {

/* Arguments:
**	nic	i: NICMOS info structure
**	input	i: input images
*/

	/* Local variables */
	int i, j, k;		/* loop indexes */
	int nlow;		/* pixel counter */

	/* Function definitions */
	Bool n_pixOK (SingleNicmosGroup *, int, int);

	/* Loop over groups */
	for (k=0; k<nic->ngroups-1; k++) {

	     /* Initialize counter for this group */
	     nlow = 0;

	     /* Loop through science image */
	     for (j=0; j<input->group[k].sci.data.ny; j++) {
		  for (i=0; i<input->group[k].sci.data.nx; i++) {

		       /* Count the number of good (nonflagged) pixels that
		       ** have fewer counts than in the preceding readout.
		       ** Only count those that are lower by more than the
		       ** expected noise level. */
		       if (n_pixOK(&(input->group[k]),  i,j)  &&
			   n_pixOK(&(input->group[k+1]),i,j) ) {

			   if (Pix(input->group[k].sci.data,i,j) <
			       Pix(input->group[k+1].sci.data,i,j) -
			       Pix(input->group[k].err.data,i,j)*MACHECK_THRESH)
			       nlow += 1;
		       }
		  }
	     }

	     /* Report the number of pixels with decreasing countrate */
	     if (nlow > 0) {
		 sprintf (MsgText,
		 "Imset %d has %d pixels with lower countrate than imset %d",
		 k+1, nlow, k+2);
		 n_message (MsgText);
	     }
	}

}
