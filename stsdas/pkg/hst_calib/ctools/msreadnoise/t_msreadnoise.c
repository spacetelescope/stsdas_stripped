# include <stdio.h>
# include <stdlib.h>
# include <c_iraf.h>
# include "readnoise.h"


/*   T_READNOISE  --  Main entry/exit point.
 *
 *   This module exists just to avoid an ungraceful exit() 
 *   that might bomb IRAF out.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   29 Oct 96  -  Implementation.
 *   07 Feb 97  -  If not supplied, append extension to output name (IB)
 *   07 Feb 97  -  Reject samples with less than 3 dark pairs (IB)
 *   04 Mar 97  -  Fixed blkSize computation - v 1.2 (IB)
 *   30 May 97  -  Task renamed "msreadnoise" (IB)
 *   03 Jun 97  -  Added new <stdio.h> (IB)
 *
 */

/* Global string for message output. */
char  MsgText[SZ_OUTLINE];

# if defined(NATIVE_IRAF)
IRAFTASK(msreadnoise) {
# else
int main(int argc, char **argv) {
# endif

	int i;
	int rn_readnoise (int, char **);

	MsgText[0] = '\0';

	c_irafinit (argc, argv);
# if defined(NATIVE_IRAF)
	i = rn_readnoise (argc, argv);
# else
	return (rn_readnoise (argc, argv));
# endif
}
