include	"mkdark.h"

#  mkdark_in -- Read CL parameters for the task mkdark.
#
#  Description:
#  ------------
#  Read CL parameters and do necessary checking and conversions.
#  
#  Input CL parameters:
#  -----------------
#
#  "infile"		Input science data file template name
#  "outfile"		Output science data file name
#  "outfile2"		Output good point file name
#  "sigmas"		Rejection thresholds
#  "radius"		Radius (in pixels) to propagate the cosmic ray
#  "pfactor"		Propagation factor
#  "hotthresh"		hot pixel threshold in DN
#  "minval"		minimum allowed DN value
#  "readnoise"		Readout noise in DN
#  "gain"		Gain factor (=electrons/DN)
#  "scalenoise"		multiplicative noise in percents
#  "fillval"		Fill value for pixels having cosmic ray in all images
#  "verbose"		print out verbose messages?
#
#  Date		Author			Description
#  ----		------			-----------
#  26-Aug-1993  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure mkdark_in (fin, fout, fout2, niter, sigma, sigmas, psigma, rej, 
			initial, readout, gain, scale, hotthresh, minval, 
			fillval, verbose)

pointer	fin 			# output: file template pointer
char	fout[SZ_FNAME]		# output: output file name
char	fout2[SZ_FNAME]		# output: output good point file name
int	niter			# output: number of iterations
real	sigma[*]
char	sigmas[SZ_LINE]
real	psigma
real	rej		# output: the radius to which pixels adjacent to a
			#   (directly) rejected pixel should also be discarded
real	readout, gain, scale	# output: noise parameters
real	hotthresh
char	initial[SZ_LINE]	# output: scheme of initial estimate
real	minval
real	fillval
bool	verbose

real	clgetr()
int	strtor()
bool	clgetb()
pointer	imtopenp()
#==============================================================================
begin

	# open input file template and find out how many files are in there 
	fin = imtopenp ("infile")
	call clgstr ("outfile", fout, SZ_FNAME)
	call clgstr ("outfile2", fout2, SZ_FNAME)

	# check that the output file can not be empty
	if (fout[1] == EOS)
	    call error (1, "blank output file name")

	call clgstr ("sigmas", sigmas, SZ_LINE)
	
	# parse the sigmas string into numbers
	niter = strtor (sigmas, sigma)
	if (niter > MAX_ITER)
	    call error (1, "max number of iterations is exceeded")
	if (niter <= 0)
	    call error (1, "number of iterations is zero")

	rej = clgetr ("radius")
	psigma = clgetr ("pfactor")
	hotthresh = clgetr ("hotthresh")
	minval = clgetr ("minval")

	# figure out how to do initial value
	call clgstr ("initial", initial, SZ_LINE)

	readout = clgetr ("readnoise")
	gain = clgetr ("gain")
	scale = clgetr ("scalenoise") / 100.

	fillval = clgetr ("fillval")
	verbose = clgetb ("verbose")
end
