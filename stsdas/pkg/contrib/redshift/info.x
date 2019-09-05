###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	procedure info(iter, ifact, a, chi, np)
#		int	iter, ifact, np
#		real	a[3], chi

#  Description:	INFO is called by the Fortran routine CHIPER in FQUOT
#		to output intermediate results in the Chi-square fitting.

#  Arguments:	int	iter		The number of the current iteration
#		int	ifact		The current Marquadt factor
#		int	np		Current number of points in the fit
#		real	a[3]		Array of fitted parameters
#		real	chi		Current value of chi-square

#  Returns:	None

#  Notes:	Additional information passed through "fquot.com"

#  History:	June	1987	Gerard Kriss

###########################################################################

include	"fquot.h"

procedure info(iter, ifact, a, chi, np)

int	iter, ifact, np
real	a[3], chi

int	i

include	"fquot.com"

begin

	if ( !(debug) )		# Print intermediate results only in debug mode
		return

	if ( iter == 0 ) {
	    for ( i = 1; i <= nlogfd; i = i + 1) {
		call fprintf(logfd[i], "Iter\tIfact\t  dbin\tln(sig)\t  gamm\t  Chi2 \t\t Npts\n\n")
	    }
	}
	for ( i = 1; i <= nlogfd; i = i + 1) {
		call fprintf(logfd[i], "%d\t%d\t%7.1f\t%7.2f\t%7.3f\t%8.2f\t%d\n")
			call pargi(iter)
			call pargi(ifact)
			call pargr(a[1])
			call pargr(a[2])
			call pargr(a[3])
			call pargr(chi)
			call pargi(np)
	}
end
