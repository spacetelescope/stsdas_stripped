###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	procedure getspec(specfile, spectrum, specim)
#		char	specfile[ARB]
#		pointer	spectrum, specim

#  Description:	GETSPEC opens the image specified by specfile and
#		returns pointers to the data and the image descriptor.
#		

#  Arguments:	char	specfile[ARB]

#  Returns:	pointer	spectrum	The data
#		pointer specim		Image file descriptor

#  Notes:	Shares data in "fquot.com"

#  History:	June	1987	Gerard Kriss
#		June 2, 1989	Lucy Willard
#			Comment out alog10 declaration

###########################################################################

include	"fquot.h"

procedure getspec(specfile, spectrum, specim)

char	specfile[ARB]
pointer	spectrum, specim

int	npix
real	w0, wpc, vel

#real	alog10()

include	"fquot.com"

begin
	call getimage( specfile, spectrum, specim, npix, specname, w0, wpc, vel)

#Check that it matches template format

	if ( npix != npts ) {
		call eprintf("Non-matching number of points in spectrum: %s\n")
			call pargstr(specfile)
		return
	}

	if ( w0 != logw0 ) {
		call eprintf("Non-matching initial wavelength %e in spectrum: %s\n")
			call pargr(w0)
			call pargstr(specfile)
		return
	}

	if ( wpc != dlogw ) {
		call eprintf("Non-matching dispersion %e in spectrum: %s\n")
			call pargr(wpc)
			call pargstr(specfile)
		return
	}
end
