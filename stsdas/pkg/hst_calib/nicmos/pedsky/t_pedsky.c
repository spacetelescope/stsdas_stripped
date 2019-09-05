# include <stdio.h>
# include "../lib/nicmos.h"

/*   T_PEDSKY  --  Main entry/exit point.
**
**   This module exists just to avoid an ungraceful exit() 
**   that might bomb IRAF out.
**
**
**
**   Revision history:
**   ----------------
**   H. Bushouse	03-May-1999	Initial Implementation.
**
*/

/* Global string for message output. */
char  MsgText[SZ_OUTLINE];

# if defined(NATIVE_IRAF)
# include <xclio.h>
IRAFTASK(pedsky) {
# else
int main(int argc, char **argv) {
# endif

	int i;
	int n_pedsky (int, char **);

	MsgText[0] = '\0';

	c_irafinit (argc, argv);
# if defined(NATIVE_IRAF)
	i = n_pedsky (argc, argv);
# else
	return (n_pedsky (argc, argv));
# endif
}
