# include <math.h>

# include <hstio.h>	/* Defines HST I/O functions */
# include "calnic.h"	/* Defines NICMOS data structures */

/* N_MATH: Contains general utilities for performing arithmetic
** on NICMOS two-dimensional data groups. Errors and DQ are updated.
** SAMP and TIME arrays are unchanged.
**
** Revision history:
** H.Bushouse	Sept. 1995	Build 1
** H.Bushouse	20-Aug-1999	Changed declarations of all routines from
**				"int" to "void". Inserted divide-by-zero
**				check in n_adiv (Version 3.3)
*/

void n_aadd (SingleNicmosGroup *a, SingleNicmosGroup *b) {

/* Add two SingleNicmosGroups, leaving the result in the first.

   (*a) += (*b)

   The science data arrays are added together; the error arrays are
   combined; the data quality arrays are "or'ed".
*/

	int i, j;	/* array indexes */
	float aerr;	/* a error */
	float berr;	/* b error */

	for (j=0; j < a->sci.data.ny; j++) {
	     for (i=0; i < a->sci.data.nx; i++) {


		  /* error data */
		  aerr = Pix(a->err.data,i,j);
		  berr = Pix(b->err.data,i,j);
		  Pix(a->err.data,i,j) = sqrt(aerr*aerr + berr*berr);

		  /* science data */
		  Pix(a->sci.data,i,j) = 
			Pix(a->sci.data,i,j) + Pix(b->sci.data,i,j);

		  /* data quality */
		  DQSetPix(a->dq.data,i,j,
		  DQPix(a->dq.data,i,j) | DQPix(b->dq.data,i,j) );

	     }
	}
}

void n_asub (SingleNicmosGroup *a, SingleNicmosGroup *b) {

/* Subtract two SingleNicmosGroups, leaving the result in the first.

   (*a) -= (*b)

   The science data arrays are subtracted; the error arrays are
   combined; the data quality arrays are "or'ed".
*/

	int i, j;	/* array indexes */
	float aerr;	/* a error */
	float berr;	/* b error */

	for (j=0; j < a->sci.data.ny; j++) {
	     for (i=0; i < a->sci.data.nx; i++) {

		  /* error data */
		  aerr = Pix(a->err.data,i,j);
		  berr = Pix(b->err.data,i,j);
		  Pix(a->err.data,i,j) = sqrt(aerr*aerr + berr*berr);

		  /* science data */
		  Pix(a->sci.data,i,j) = 
			Pix(a->sci.data,i,j) - Pix(b->sci.data,i,j);

		  /* data quality */
		  DQSetPix(a->dq.data,i,j,
		  DQPix(a->dq.data,i,j) | DQPix(b->dq.data,i,j) );

	     }
	}
}

void n_amul (SingleNicmosGroup *a, SingleNicmosGroup *b) {

/* Multiply two SingleNicmosGroups, leaving the result in the first.

   (*a) *= (*b)

   The science data arrays are multiplied together; the error arrays are
   combined; the data quality arrays are "or'ed".
*/

	int i, j;	/* array indexes */
	float a_db;	/* a value * b error */
	float b_da;	/* b value * a error */
        float err1, err2, error;

	for (j=0; j < a->sci.data.ny; j++) {
	     for (i=0; i < a->sci.data.nx; i++) {

		  /* error data */
		  a_db = Pix(a->sci.data,i,j) * Pix(b->err.data,i,j);
		  b_da = Pix(b->sci.data,i,j) * Pix(a->err.data,i,j);
		  error = sqrt (a_db*a_db + b_da*b_da);
		  if (error == 0.0) {
		    err1 = Pix(a->err.data,i,j);
		    err2 = Pix(b->err.data,i,j);
		    error = sqrt(err1*err1 + err2*err2);
                  }
		  Pix(a->err.data,i,j) = error;
		  /* science data */
		  Pix(a->sci.data,i,j) = 
			Pix(a->sci.data,i,j) * Pix(b->sci.data,i,j);

		  /* data quality */
		  DQSetPix(a->dq.data,i,j,
		  DQPix(a->dq.data,i,j) | DQPix(b->dq.data,i,j) );

	     }
	}
}

void n_adiv (SingleNicmosGroup *a, SingleNicmosGroup *b) {

/* Divide two SingleNicmosGroups, leaving the result in the first.

   (*a) /= (*b)

   The science data arrays are divided; the error arrays are
   combined; the data quality arrays are "or'ed".
*/

	int i, j;	/* array indexes */
	float asci;	/* a science */
	float bsci;	/* b science */
	float bsci2;	/* b science squared */
	float aerr;	/* a error */
	float berr;	/* b error */

	for (j=0; j < a->sci.data.ny; j++) {
	     for (i=0; i < a->sci.data.nx; i++) {

		  /* error and science data */
		  asci = Pix(a->sci.data,i,j);
		  bsci = Pix(b->sci.data,i,j);
		  bsci2 = bsci*bsci;
		  aerr = Pix(a->err.data,i,j);
		  berr = Pix(b->err.data,i,j);

		  Pix(a->err.data,i,j) =
		      sqrt(aerr*aerr/bsci2 + asci*asci*berr*berr/(bsci2*bsci2));

		  if (bsci != 0)
		      Pix(a->sci.data,i,j) = asci / bsci;
		  else
		      Pix(a->sci.data,i,j) = 0.0;

		  /* data quality */
		  DQSetPix(a->dq.data,i,j,
			DQPix(a->dq.data,i,j) | DQPix(b->dq.data,i,j) );

	     }
	}
}

void n_asubk (SingleNicmosGroup *a, float b) {

/* Subtract in-place a constant from a SingleNicmosGroup.

   (*a) -= b

   The constant is subtracted from the science data array only;
   the error and data quality arrays are unchanged.
*/

	int i, j;	/* array indexes */

	for (j=0; j < a->sci.data.ny; j++) {
	     for (i=0; i < a->sci.data.nx; i++) {

		  /* science data */
		  Pix(a->sci.data,i,j) = Pix(a->sci.data,i,j) - b;

	     }
	}
}

void n_amulk (SingleNicmosGroup *a, float b) {

/* Multiply in-place a SingleNicmosGroup by a constant.

   (*a) *= b

   The science and error data arrays are multiplied by the constant;
   the data quality array is unchanged.
*/

	int i, j;	/* array indexes */

	for (j=0; j < a->sci.data.ny; j++) {
	     for (i=0; i < a->sci.data.nx; i++) {

		  /* science data */
		  Pix(a->sci.data,i,j) = Pix(a->sci.data,i,j) * b;

		  /* error data */
		  Pix(a->err.data,i,j) = Pix(a->err.data,i,j) * b;

	     }
	}
}

void n_aor (SingleNicmosGroup *a, SingleNicmosGroup *b) {

/* Take the logical OR of two SingleNicmosGroup DQ arrays, leaving the result 
** in the first.

   (*a.dq) = (*a.dq) | (*b.dq)

   The science and error data arrays are unchanged.
*/

	int i, j;	/* array indexes */

	/* data quality */
	for (j=0; j < a->dq.data.ny; j++) {
	     for (i=0; i < a->dq.data.nx; i++) {

		  DQSetPix (a->dq.data,i,j,
			DQPix (a->dq.data,i,j) | DQPix (b->dq.data,i,j) );
	     }
	}
}

void n_check (SingleNicmosGroup *a) {

  /* Check the error array for zero or negative values */

  int i, j, nzeroerr;
  nzeroerr=0;
  for (j=0; j < a->sci.data.ny; j++) {
    for (i=0; i < a->sci.data.nx; i++) {

      if (Pix(a->err.data,i,j) <= 0.0) {
/*  printf ("Error <=0 for i=%d, j=%d, group=%d\n", i, j, a->group_num); */
	  nzeroerr++;
      }
    }
  }
  printf ("%d pixels have error <= 0\n", nzeroerr);
}
