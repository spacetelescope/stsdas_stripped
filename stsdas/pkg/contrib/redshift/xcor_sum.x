###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	procedure xcor_sum()

#		procedure mean(npts, points, errs, qual, uniform, avg, disp)
#		int	npts			Number of points
#		real	points[ARB], errs[ARB]	Array of data and errors
#		bool	qual[ARB]		Quality flags for points
#		bool	uniform			Flag for weighting scheme
#		real	avg, disp		Returned mean and dispersion

#  Description:	XCOR_SUM produces a statistical summary of all the cross
#		correlations of templates and the object spectrum.
#		
#		MEAN calculates means and dispersions for data arrays

#  Arguments:	See above

#  Returns:	See above

#  Notes:	All input to summary in common in "fquot.com"

#  History:	June	1987	Gerard Kriss

###########################################################################
include	"fquot.h"

procedure xcor_sum()

bool	uniform
bool	good[MAXTEMPS]
char	stat
int	i, j
real	mid
real	czavg, czdisp, czwavg, czwdisp, cavgerr, cmaxerr
real	sigavg, sigdisp, sigwavg, sigwdisp, savgerr, smaxerr
real	disp

real	median()

include	"fquot.com"

begin

# Qualify results as acceptable first.
	mid = median(ntemp,cz[i])

	for ( i = 1; i <= ntemp; i = i + 1)
	{
		if (	( cz[i] > CZMIN  &&  cz[i] < CZMAX ) &&
			( abs(cz[i] - mid) / czerr[i] < 4. ) &&
			( abs ( czerr[i] / cz[i] ) > ERRMIN ) &&
			( abs ( czerr[i] / cz[i] ) < ERRMAX ) )
		    good[i] = true
		else
		    good[i] = false
	}

# Compute statistical information on the results.
	uniform = true
	call mean(ntemp, cz, czerr, good, uniform, czavg, czdisp)
	call mean(ntemp, sig, sigerr, good, uniform, sigavg, sigdisp)

	uniform = false
	call mean(ntemp, cz, czerr, good, uniform, czwavg, czwdisp)
	call mean(ntemp, sig, sigerr, good, uniform, sigwavg, sigwdisp)

	uniform = true
	call mean(ntemp, czerr, czerr, good, uniform, cavgerr, disp)
	call mean(ntemp, sigerr, sigerr, good, uniform, savgerr, disp)

	cmaxerr = 0.
	smaxerr = 0.
	for ( i = 1; i <= ntemp; i = i + 1)	# Find largest error on each
	{
	    if ( good[i] ) {
		if ( czerr[i] > cmaxerr )
			cmaxerr = czerr[i]
		if ( sigerr[i] > smaxerr )
			smaxerr = sigerr[i]
	    }
	}

# Set up heading in log files
	for ( i = 1; i <= nlogfd; i = i + 1)
	{
	  call fprintf(logfd[i], "\nResults for object:\t%s\n\n")
		call pargstr(specname)
	  call fprintf(logfd[i],
	  "Template\t     CZ      \t Dispersion \tCorrelation Peak\n")
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
			"%c%1d  %s\t%5.0f +- %4.0f\t%4.0f +- %4.0f\t%6.3f\n")
				call pargc(stat)
				call pargi(i)
				call pargstr(tempname[1,i])
				call pargr(cz[i])
				call pargr(czerr[i])
				call pargr(sig[i])
				call pargr(sigerr[i])
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

	}
end

procedure mean(npts, points, errs, qual, uniform, avg, disp)

int	npts
real	points[ARB], errs[ARB]
bool	qual[ARB]
bool	uniform
real	avg, disp

int	i
real	weight, var, norm

begin

	var = 0.
	avg = 0.
	norm = 0.
	for ( i = 1; i <= npts; i = i + 1)
	{
	    if ( qual[i] )
	    {
		if ( uniform )
			weight = 1.
		else if ( errs[i] != 0. )
			weight = 1. / errs[i]
		else
			weight = 1.e10
		norm = norm + weight
		avg = avg + weight * points[i]
		var = var + weight * points[i]**2
	    }
	}
	if ( norm != 0. ) {
		avg = avg / norm
		var = var / norm - avg**2
	}
	else {
		avg = avg / 1.e-10
		var = var / 1.e-10 - avg**2
	}
	disp = sqrt ( var )

end

real procedure median(np, data)

int	np
real	data[ARB]
real	sorted[MAXTEMPS]

begin

# Sort the data and return the middle value
	call asrtr(data,sorted,np)
	return ( sorted[np/2+1] )
end
