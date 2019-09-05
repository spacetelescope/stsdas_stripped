# include <stdio.h>
# include <c_iraf.h>
# include "nbadpix.h"

/*   T_NBADPIX  --  Main entry/exit point.
 *
 *   This module exists just to avoid an ungraceful exit() 
 *   that might bomb IRAF out.
 *
 *
 *
  *   Revision history:
 *   ----------------
 *   02 Aug 96  -  Implementation.
 *   21 Oct 96  -  Revised after code review - ver. 1.1 (IB)
 *   11 Nov 96  -  Add STIS support - ver. 1.2 (IB)
 *   07 Feb 97  -  New get_numeric (IB).
 *   07 Feb 97  -  Check/add extension to output name - ver. 1.3 (IB).
 *   30 May 97  -  Task renamed "msbadpix" (IB)
 *   03 Jun 97  -  Added new <stdio.h> (IB)
 *
 */

/* Global string for message output. */
char  MsgText[SZ_OUTLINE];

# if defined(NATIVE_IRAF)
# include <xclio.h>
IRAFTASK(msbadpix) {
# else
int main(int argc, char **argv) {
# endif

	int i;
	int b_nbadpix (int, char **);

	MsgText[0] = '\0';

	c_irafinit (argc, argv);
# if defined(NATIVE_IRAF)
	i = b_nbadpix (argc, argv);
# else
	return (b_nbadpix (argc, argv));
# endif
}
