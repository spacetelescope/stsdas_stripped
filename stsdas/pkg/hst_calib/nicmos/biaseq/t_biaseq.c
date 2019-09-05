# include <stdio.h>
# include "../lib/nicmos.h"

/*   T_BIASEQ  --  Main entry/exit point for the BIASEQ task.
**
**   This module exists just to avoid an ungraceful exit() 
**   that might bomb IRAF out.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	25-Mar-1999	Initial Implementation.
**
*/

/* Global string for message output. */
char  MsgText[SZ_OUTLINE];

# if defined(NATIVE_IRAF)
# include <xclio.h>
IRAFTASK(biaseq) {
# else
int main(int argc, char **argv) {
# endif

	int i;
	int n_biaseq (int, char **);

	MsgText[0] = '\0';

	c_irafinit (argc, argv);
# if defined(NATIVE_IRAF)
	i = n_biaseq (argc, argv);
# else
	return (n_biaseq (argc, argv));
# endif

}

