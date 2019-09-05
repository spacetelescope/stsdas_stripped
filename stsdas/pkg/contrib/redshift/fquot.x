###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	fquot @templates @galaxies @zguess

#  Description:	An IRAF task for obtaining redshifts and velocity dispersions
#	   using the Fourier Quotient method.  Adapted from (and uses parts of)
#	   a program originally written by Paul Schechter in FORTRAN.

#  Arguments:	See parameter file fquot.par

#  Returns:	Output to log files specified by the user

#  Notes:	Much information shared in common in "fquot.com"
#		Although most of the front end and I/O of this program
#		is written in SPP, the guts are still Fortran IV routines
#		called by SPP routines.

#  History:	Mid 1970's	Paul Schechter
#		June	1987	Gerard Kriss
#				Changed front-end routines for IRAF use

###########################################################################

include	<imhdr.h>
include	<fset.h>
include "fquot.h"

procedure t_fquot ()

int	i, j, fd
int	stat, col
int	speclist		# List of input spectra
int	templist		# List of template spectra
int	zlist			# List of redshift guesses
int	logfiles		# list of log files

double	dval

pointer	logfile, str, specfile, zchar
pointer	spectrum, specim, tempspec[MAXTEMPS], tempim[MAXTEMPS]

bool	clgetb()
int	clpopnu(), clgeti(), open(), clgfil(), ctod()
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

	zlist = clpopnu("zlist")
	sig0 = clgetr("sigma")
	gam0 = clgetr("gamma")

	order = clgeti("order")
	han = clgetr("bell_window")
	cpf = clgetr("counts_per_photon")
	lo = clgeti("low_bin")
	nrun = clgeti("nrun")
	chi0 = clgetr("initial_chi")
	unc = clgetr("converge")
	niter = clgeti("max_iterations")

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
		call fprintf(fd, "  Task FQUOT\n\n")

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

	# Get inital redshift estimate

		call malloc(zchar, SZ_FNAME, TY_CHAR)
		stat =  clgfil(zlist, Memc[zchar], SZ_FNAME)
		col = 1
		stat = ctod(Memc[zchar], col, dval)
		z0 = dval
		call mfree(zchar, TY_CHAR)

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
			call fqfit(Memr[spectrum], Memr[tempspec[i]], perdeg[i])

			# Fix problems with zero valued results

			for ( j = 1; j <= 3; j = j + 1)
			{
				if ( z[j] == 0. )
					z[j] = 1.e-15
				if ( ze[j] == 0. )
					ze[j] = 1.e-15
			}

			cz[i] = C * (10. ** (-z[1] * dlogw) - 1.) + tempvel[i]
			czerr[i] = ze[1] * deltav
			sig[i] = exp( z[2] ) * deltav
			sigerr[i] = ze[2] * sig[i]
			if ( sig[i] == 0. )
			{
				sig[i] = 1.e-10
				sigerr[i] = 1.e-10
			}
			gam[i] = z[3]
			gamerr[i] = ze[3]
		}
		call summary()
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
	call clpcls(zlist)

#	call sfree  (sp)
#	call gclose (gfd)
#	call gt_free (gt)
#	call close  (afd)

end
