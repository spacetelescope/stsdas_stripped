# include <stdio.h>
# include <c_iraf.h>

/*  T_MSARITH  --  Main entry/exit point.

    This module exists just to avoid an ungraceful exit() that might
    bomb IRAF out.



    ----------------
    01 Mar 96  -  Task created  (I. Busko)
    08 Mar 96  -  File name list processing (IB)
    15 Mar 96  -  File name suffix checking (IB)
    18 Apr 96  -  Read parameters from the CL (IB)
    17 May 96  -  Shared input and output data structures (IB)
    11 Nov 96  -  STIS and BUNIT support - version 2.0 (IB)
    06 Feb 97  -  Check/add extension in output file name - v. 2.1 (IB)
    28 Feb 97  -  Group number printout in verbose mode - v. 2.2 (IB)
    21 Apr 97  -  More flexible group input, print id - v. 2.3 (IB)
    06 May 97  -  Allow constant as first operand - v. 2.4 (IB)
    14 May 97  -  Modified logic that tests valid IMSET combinations (IB)
    29 May 97  -  Renamed to "msarith" (IB) 
    03 Jun 97  -  Added new <stdio.h> (IB) 
    25 Mar 04  -  Add ACS support (handled exactly as STIS; IB)

*/

char  ErrText[81];           /* error message text */

# if defined (NATIVE_IRAF)
IRAFTASK(msarith) {
# else
int main(int argc, char **argv) {
# endif

	int i;
	int n_nimarith (int, char **);
	void c_irafinit(int, char **);

	ErrText[0] = '\0';

	c_irafinit (argc, argv);

# if defined (NATIVE_IRAF)
	i = n_nimarith (argc, argv);
# else
	return (n_nimarith (argc, argv));
# endif
}
