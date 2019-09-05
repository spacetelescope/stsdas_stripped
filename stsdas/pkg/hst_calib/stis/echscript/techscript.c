# include <stdio.h>
# include <c_iraf.h>

/*****************************************************************************
 *
 * TECHSCRIPT -- Main entry/exit routine.
 *
 * This module is necessary to avoid an ungraceful exit() that might
 * cause IRAF to abort.
 *
 * Author:  Michele D. De La Pena
 * Date:    23 September 1997
 * Mods:    04 December 1998 - MDD: File name mod (t_echscript -> techscript)
 *
 *
******************************************************************************/

/* Error message text */
char  ErrMsg[81]; 

/* Set up for native or foreign task */
# if defined (NATIVE_IRAF)
IRAFTASK (echscript) {
# else
int main (int argc, char **argv) {
# endif

         /* Declare local variables */
	 int  i;
         
         /* Define function prototypes */
	 int  e_echscript (int, char **);
	 void c_irafinit  (int, char **);

	 ErrMsg[0] = '\0';

         /* Initialize the CVOS libraries */
	 c_irafinit (argc, argv);

# if defined (NATIVE_IRAF)
	i = e_echscript (argc, argv);
# else
	return (e_echscript (argc, argv));
# endif
}
