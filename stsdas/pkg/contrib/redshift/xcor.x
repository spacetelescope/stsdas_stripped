###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	xcor @templates @objects

#  Description:	XCOR is an IRAF task for obtaining redshifts and velocity
#		dispersions using cross correlation mehtods.

#  Arguments:	See parameter file xcor.par

#  Returns:	Output is to user specified log files

#  Notes:	Information shared in common blocks defined in "fquot.com".
#		Most of the routines are written in SPP, but the windowing
#		and FFT routines are Fortran IV called from SPP.

#  History:	Mid 1970's	Paul Schechter
#				Data windowing routines.  Some functions
#				from the IBM SSP library.
#		June	1987	Gerard Kriss
#				Wrote major portions in SPP.

###########################################################################

include	<imhdr.h>
include	<fset.h>
include "fquot.h"

procedure t_xcor ()

int	i, j, fd
int	speclist		# List of input spectra
int	templist		# List of template spectra
int	logfiles		# list of log files

real	x1, x2

pointer	logfile, str, specfile 
pointer	spectrum, specim, tempspec[MAXTEMPS], tempim[MAXTEMPS]

bool	clgetb()
int	clpopnu(), clgeti(), open(), clgfil()
real	clgetr()

include	"fquot.com"

begin

	# Get task parameters.

	templist = clpopnu("templates")
	speclist = clpopnu("spectra")

	pltspec = clgetb("obj_plot")
	pltwin = clgetb("window_plot")
	pltfft = clgetb("fft_plot")
	pltfit = clgetb("fit_plot")
	debug = clgetb("intermediate")

	sig0 = clgetr("sig0")
	sig1 = clgetr("sig1")
	sig2 = clgetr("sig2")
	sig3 = clgetr("sig3")

	order = clgeti("order")
	han = clgetr("bell_window")
	lo = clgeti("low_bin")
	nrun = clgeti("nrun")

	logfiles = clpopnu("logfiles")

#Open log files and write a header.
	call fseti(STDOUT, F_FLUSHNL, YES)

	call malloc(logfile, SZ_FNAME, TY_CHAR)
	call malloc(str, SZ_LINE, TY_CHAR)

	i = 0
	while ( clgfil(logfiles, Memc[logfile], SZ_FNAME) != EOF ) {
		fd = open(Memc[logfile], APPEND, TEXT_FILE)
		call sysid(Memc[str], SZ_LINE)
		call fprintf(fd, "%s\n")
			call pargstr(Memc[str])
		call fprintf(fd, "  Task XCOR\n\n")

		i = i + 1
		logfd[i] = fd
	}
	nlogfd = i
	call mfree(logfile, TY_CHAR)
	call mfree(str, TY_CHAR)
	call clpcls(logfiles)
	
	# Get template spectra and set up scales
	call gettemps(templist, tempspec, tempim)

	# Loop through the list of spectra and get the answers
	call malloc(specfile, SZ_FNAME, TY_CHAR)
	while ( clgfil(speclist, Memc[specfile], SZ_FNAME) != EOF )
	{
		call getspec(Memc[specfile], spectrum, specim)
		for ( j = 1; j <= nlogfd; j = j + 1)
		{
			call fprintf(logfd[j],
"\n\n\n______________________________________________________________________\n")
			call fprintf(logfd[j],
"**********************************************************************\n")
			call fprintf(logfd[j],
"************************* OBJECT: %s *************************\n\n")
				call pargstr(specname)
		}

# If plot enabled, show the object spectrum.
		if ( pltspec ) {
			x1 = 1.
			x2 = npts
			call plotspec(npts, Memr[spectrum], specname, x1, x2)
		}

# Loop through templates doing fits to each one.

		if ( debug )
		{
		    for ( j = 1; j <= nlogfd; j = j + 1)
		    {
			call fprintf(logfd[j]," Object: %s\n")
				call pargstr(specname)
		    }
		}
		for ( i = 1; i <= ntemp; i = i + 1)
		{
			if ( debug )
			{
			    for ( j = 1; j <= nlogfd; j = j + 1)
			    {
				call fprintf(logfd[j],"\n Template: %s\n")
					call pargstr(tempname[1,i])
			    }
			}
			call xcorfit(Memr[spectrum], Memr[tempspec[i]], perdeg[i])

			# Fix problems with zero valued results

			for ( j = 1; j <= 3; j = j + 1)
			{
				if ( z[j] == 0. )
					z[j] = 1.e-15
				if ( ze[j] == 0. )
					ze[j] = 1.e-15
			}

			cz[i] = C * (10. ** (z[1] * dlogw) - 1.) + tempvel[i]
			czerr[i] = ze[1] * deltav
			sig[i] = z[2] * deltav
			sigerr[i] = ze[2] * deltav
		}
		call xcor_sum()
		call imunmap (specim)
	}

# Close the log files
	for ( i = 1; i <= nlogfd; i = i + 1)
	{
		call close (logfd[i])
	}

# Unmap the template images
	for ( i = 1; i <= ntemp; i = i + 1)
	{
		call imunmap (tempim[i])
	}

	call mfree(specfile, TY_CHAR)
	call clpcls(speclist)

end
