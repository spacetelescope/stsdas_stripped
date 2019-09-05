# include "pedsub.h"

extern float A[11];
extern int MA;

/*   SPOLY  --  Evaluate polynomial fit at given pedestal value.
**
**	Based on SPREADPOLY routine from Roeland van der Marel's
**	"unpedestal" program.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	05-July-1999	Implementation
**
*/

float spoly (float x) {

	/* Local variables */
	int j;
	float poly;

	poly = A[MA];
	for (j = MA-1; j >= 1; j--)
	     poly = x*poly + A[j];

	/* Return polynomial value */
	return (poly);

}

