###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	procedure summary()

#		procedure mean(npts, points, errs, qual, uniform, avg, disp)
#		int	npts			Number of points
#		real	points[ARB], errs[ARB]	Array of data and errors
#		bool	qual[ARB]		Quality flags for points
#		bool	uniform			Flag for weighting scheme
#		real	avg, disp		Returned mean and dispersion

#  Description:	SUMMARY produces a statistical summary of all the Fourier
#		quotient fits to the object spectrum.
#		
#		MEAN calculates means and dispersions for data arrays

#  Arguments:	See above

#  Returns:	See above

#  Notes:	All input to summary in common in "fquot.com"

#  History:	June	1987	Gerard Kriss

###########################################################################

include	"fquot.h"

procedure summary()

bool	uniform
bool	good[MAXTEMPS]
char	stat
int	i, j
real	czavg, czdisp, czwavg, czwdisp, cavgerr, cmaxerr
real	sigavg, sigdisp, sigwavg, sigwdisp, savgerr, smaxerr
real	gamavg, gamdisp, gamwavg, gamwdisp, gavgerr, gmaxerr
real	disp

include	"fquot.com"

begin

# Qualify results as acceptable first.

	for ( i = 1; i <= ntemp; i = i + 1)
	{
		if (	( cz[i] > CZMIN  &&  cz[i] < CZMAX ) &&
			( sig[i] > SIGMIN && sig[i] < SIGMAX ) &&
			( gam[i] > GAMMIN && gam[i] < GAMMAX ) &&
			( perdeg[i] > CHIMIN && perdeg[i] < CHIMAX ) &&
			( abs ( czerr[i] / cz[i] ) > ERRMIN ) &&
			( abs ( sigerr[i] / sig[i] ) > ERRMIN ) &&
			( abs ( gamerr[i] / gam[i] ) > ERRMIN ) &&
			( abs ( czerr[i] / cz[i] ) < ERRMAX ) &&
			( abs ( sigerr[i] / sig[i] ) < ERRMAX ) &&
			( abs ( gamerr[i] / gam[i] ) < ERRMAX ) )
		    good[i] = true
		else
		    good[i] = false
	}

# Compute statistical information on the results.
	uniform = true
	call mean(ntemp, cz, czerr, good, uniform, czavg, czdisp)
	call mean(ntemp, sig, sigerr, good, uniform, sigavg, sigdisp)
	call mean(ntemp, gam, gamerr, good, uniform, gamavg, gamdisp)

	uniform = false
	call mean(ntemp, cz, czerr, good, uniform, czwavg, czwdisp)
	call mean(ntemp, sig, sigerr, good, uniform, sigwavg, sigwdisp)
	call mean(ntemp, gam, gamerr, good, uniform, gamwavg, gamwdisp)

	uniform = true
	call mean(ntemp, czerr, czerr, good, uniform, cavgerr, disp)
	call mean(ntemp, sigerr, sigerr, good, uniform, savgerr, disp)
	call mean(ntemp, gamerr, gamerr, good, uniform, gavgerr, disp)

	cmaxerr = 0.
	smaxerr = 0.
	gmaxerr = 0.
	for ( i = 1; i <= ntemp; i = i + 1)	# Find largest error on each
	{
	    if ( good[i] ) {
		if ( czerr[i] > cmaxerr )
			cmaxerr = czerr[i]
		if ( sigerr[i] > smaxerr )
			smaxerr = sigerr[i]
		if ( gamerr[i] > gmaxerr )
			gmaxerr = gamerr[i]
	    }
	}

# Set up heading in log files
	for ( i = 1; i <= nlogfd; i = i + 1)
	{
	  call fprintf(logfd[i], "\nResults for object:\t%s\n\n")
		call pargstr(specname)
	  call fprintf(logfd[i],
	  "Template\t     CZ      \t Dispersion \t Line Strength \t Chi-Square\n")
	}

# Record results in log files

	for ( i = 1; i <= ntemp; i = i + 1)
	{
		stat = '-'
		if ( good[i] )
			stat = '+'
		for ( j = 1; j <= nlogfd; j = j + 1)
		{
			call fprintf(logfd[j],
			"%c%1d  %s\t%5.0f +- %4.0f\t%4.0f +- %4.0f\t%5.2f +- %5.2f\t%8.2f\n")
				call pargc(stat)
				call pargi(i)
				call pargstr(tempname[1,i])
				call pargr(cz[i])
				call pargr(czerr[i])
				call pargr(sig[i])
				call pargr(sigerr[i])
				call pargr(gam[i])
				call pargr(gamerr[i])
				call pargr(perdeg[i])
		}
	}

# Record statistical summary in the log files

	for ( i = 1; i <= nlogfd; i = i + 1)
	{
		call fprintf(logfd[i], "\nStatistical summary for object: %s\n\n")
			call pargstr(specname)
		call fprintf(logfd[i], "Parameter\tMean \t\tWeighted Mean \t\tAvg Error \tLargest Error\n\n")
		call fprintf(logfd[i], "CZ       \t%6.0f +- %4.0f\t%6.0f +- %4.0f\t\t%4.0f  \t\t%4.0f\n")
			call pargr(czavg)
			call pargr(czdisp)
			call pargr(czwavg)
			call pargr(czwdisp)
			call pargr(cavgerr)
			call pargr(cmaxerr)

		call fprintf(logfd[i], "Dispersion\t%6.0f +- %4.0f\t%6.0f +- %4.0f\t\t%4.0f  \t\t%4.0f\n")
			call pargr(sigavg)
			call pargr(sigdisp)
			call pargr(sigwavg)
			call pargr(sigwdisp)
			call pargr(savgerr)
			call pargr(smaxerr)

		call fprintf(logfd[i], "Line Strength\t%6.3f +- %5.3f\t%6.3f +- %5.3f\t\t%5.3f  \t\t%5.3f\n")
			call pargr(gamavg)
			call pargr(gamdisp)
			call pargr(gamwavg)
			call pargr(gamwdisp)
			call pargr(gavgerr)
			call pargr(gmaxerr)
	}
end

