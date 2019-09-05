# include <stdio.h>
# include <c_iraf.h>
# include "msstat.h"

/*   T_MSSTATATISTICS  --  Main entry/exit point.
 *
 *   This module exists just to avoid an ungraceful exit() 
 *   that might bomb IRAF out.
 *
 *
 *
 *
 *   Revision history:
 *   ----------------
 *   06 Jun 96  -  Implementation as IRAF native task.
 *   01 Jul 96  -  DQF as input template/list (IB).
 *   18 Oct 96  -  Revised after code review - version 1.1 (IB).
 *   12 Nov 96  -  Support for compressed HDUs - version 1.2 (IB)
 *   06 Mar 97  -  Modified definition of weighted variance (IB)
 *   06 Mar 97  -  Warns when invalid tokens are found in control strings (IB)
 *   06 Mar 97  -  Support for sections with compressed HDUs - v 1.3 (IB)
 *   16 Apr 97  -  Fixed getLine call in e_hist (IB)
 *   30 May 97  -  Task renamed "msstatistics" (IB)
 *   03 Jun 97  -  Added new <stdio.h> (IB)
 *
 */

char  MsgText[SZ_OUTLINE];  /* Global string for message output. */

# if defined(NATIVE_IRAF)
IRAFTASK(msstatistics) {
# else
int main(int argc, char **argv) {
# endif
	int i;
	int e_gstat (int, char **);

	MsgText[0] = '\0';

	c_irafinit (argc, argv);

# if defined(NATIVE_IRAF)
	i = e_gstat (argc, argv);
# else
	return (e_gstat (argc, argv));
# endif
}
