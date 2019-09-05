# include <stdio.h>
# include <c_iraf.h>
# include "estreak.h"

/*   T_ESTREAKFLAT  --  Main entry/exit point.
 *
 *   This module exists just to avoid an ungraceful exit() that 
 *   might bomb IRAF out.
 *
 *
 *
 *  Revision history:
 *  ----------------
 *  03 May 96  -  Version 0 (support NICMOS only) is mostly a C translation
 *                of original SPP WFPC-only task written by J.C.Hsu, however
 *                with hooks for other instruments.
 *  08 May 96  -  IRAF native task - released (IB).
 *  23 May 96  -  WFPC support - version 1.0 (IB).
 *  31 May 96  -  Virtual files - version 1.1 (IB).
 *  07 Oct 96  -  Revised after code review - version 1.2 (IB).
 *  16 Oct 96  -  Added protection against immap failure (IB)
 *  28 Oct 96  -  Fixed g_select call (IB)
 *  29 Oct 96  -  Window size matches SPP version (IB)
 *  30 May 97  -  Task renamed "msstreakflat" (IB)
 *  03 Jun 97  -  Added new <stdio.h> (IB)
 *
 */

char	ErrText[SZ_OUTLINE];

# if defined(NATIVE_IRAF)
IRAFTASK(msstreakflat) {
# else
int main(int argc, char **argv) {
# endif
	int i;
	int g_gstreak();

	ErrText[0] = '\0';

	c_irafinit (argc, argv);

# if defined(NATIVE_IRAF)
	i = g_gstreak ();
# else
	return (g_gstreak ());
# endif
}
