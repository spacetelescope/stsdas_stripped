###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	procedure gettemps(templist, tempspec, tempim)
#		int	templist
#		pointer	tempspec[ARB], tempim[ARB]

#  Description:	GETTEMPS opens the list of template spectra, extracts
#		wavelength and velocity information from the headers,
#		and returns the template spectra.

#  Arguments:	int	templist	CL list of template image files

#  Returns:	pointer	tempspec[ARB]	Pointers to the template spectra
#		pointer	tempim[ARB]	Pointers to the template image
#					descriptors

#  Notes:	shares data in common in "fquot.com"

#  History:	June	1987	Gerard Kriss
#
#		June 2, 1989	Lucy Willard
#			Comment out alog10 declaration

###########################################################################

include	"fquot.h"

procedure gettemps(templist, tempspec, tempim)

int	templist
pointer	tempspec[ARB], tempim[ARB]

int	i, npix, j
real	w0, wpc, vel
pointer	tempfile

int	clgfil()
#real	alog10()

include	"fquot.com"

begin

	ntemp = 0
	call malloc(tempfile, SZ_FNAME, TY_CHAR)
	while ( clgfil(templist, Memc[tempfile], SZ_FNAME) != EOF) {
		ntemp = ntemp + 1
		call getimage( Memc[tempfile], tempspec[ntemp], tempim[ntemp],
			npix, tempname[1,ntemp], w0, wpc, vel)
		tempvel[ntemp] = vel
		if ( ntemp == 1) {
			npts = npix
			wave0 = 10**(w0)
			logw0 = w0
			dlogw = wpc
			deltav = dlogw * CLN10
			if ( dlogw > 0.1 )	#Not a log-wavelength scale
			{
				call eprintf("Template spectrum (%s) not on a log-wavelength scale.  dlogw = %e\n")
					call pargstr(Memc[tempfile])
					call pargr(dlogw)
				return
			}
		}
		else {
			if ( npix != npts ) {
				call eprintf("Non-matching number of points in template: %s\n")
				call pargstr(Memc[tempfile])
				return
			}
			if ( w0 != logw0 ) {
				call eprintf("Non-matching wave0 in template: %s\n")
				call pargstr(Memc[tempfile])
				return
			}
			if ( wpc != dlogw ) {
				call eprintf("Non-matching dispersion in template: %s\n")
				call pargstr(Memc[tempfile])
				return
			}
		}
	}

#Record info in the log files.
	for ( i = 1; i <= nlogfd; i = i + 1)
	{
		call fprintf(logfd[i],
		"%4d points.  Wave0 = %7.2f, dlogw = %e, Deltav = %6.2f (km/sec).\n\n")
			call pargi(npts)
			call pargr(wave0)
			call pargr(dlogw)
			call pargr(deltav)
		call fprintf(logfd[i], "Template Star\tVelocity (km/sec)\n\n")
		for ( j = 1; j <= ntemp; j = j + 1)
		{
		    call fprintf(logfd[i], "%2d %s \t%7.1f\n")
			call pargi(j)
			call pargstr(tempname[1,j])
			call pargr(tempvel[j])
		}
	}
	call mfree(tempfile, TY_CHAR)
	call clpcls(templist)

end
