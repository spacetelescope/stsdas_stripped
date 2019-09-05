include	"crrej.h"

#  crrej_in -- Read CL parameters for the task crrej.
#
#  Description:
#  ------------
#  Reads CL parameters and does necessary checkings
#  
#  Input CL parameters:
#  -----------------
#
#  "input"		Input images
#  "output"		Output combined image name
#  "masks"		data quality files
#  "sky"		how to calculate the sky levels
#  "expname"		Keyword name for the exposure time
#  "sigmas"             Rejection thresholds
#  "lower"		lower limit of allowed pixel values
#  "upper"		upper limit of allowed pixel values
#  "radius"             Radius (in pixels) to propagate the cosmic ray
#  "pfactor"            Propagation factor
#  "initial"		how to compute the initial comparison image
#  "readnoise"		Readout noise in DN
#  "atodgain"		Gain factor (=electrons/DN)
#  "scalenoise"		multiplicative noise in percents
#  "dq"			Data quality pset
#  "skyname"		name of the keyword to be updated with the sky value
#  "crdqval"		Data quality value for pixels flagged as cosmic ray
#  "fillval"		fill value for pixels with all stack members rejected
#  "verbose"		print out verbose messages?
#
#  Date		Author			Description
#  ----		------			-----------
#  21-Feb-1995  J.-C. Hsu		convert original IDL code by Rick White
#  08-Dec-1995  J.-C. Hsu		expand to include input/output mask, 
#					sky, and expname
#  28-Feb-1996  J.-C. Hsu		add fillval, fix outside range pixel 
#					propagation bug
#------------------------------------------------------------------------------
procedure crrej_in (tpin, fout, tpmask, par, niter, sigma)

# outputs:
pointer	tpin 			# input image template pointer
char	fout[ARB]		# output file name
pointer	tpmask 			# mask template pointer
pointer	par			# par structure pointer
int     niter                   # number of rejection iterations
real    sigma[ARB]		# statistical thrsholds used in CR rejection

# local:
pointer	list
int	nchar
int	i
char	str[SZ_FNAME, MAX_NEXP]

real	clgetr()
bool	clgetb()
int	clgeti()
int	imtlen()
pointer	imtopenp()
int	imtrgetim()
int	strtor()
#==============================================================================
begin

	# open input image and mask templates and find out how 
	# many files are in there 
	tpin = imtopenp ("input")
	call clgstr ("output", fout, SZ_FNAME)
	tpmask = imtopenp ("masks")

        if (imtlen(tpin) > MAX_FILES) 
	    call error (1, "Too many input images")

	# check that (1) must have more than one input images, (2) mask
	# template must be null or match the input image template, 
	# (3) output image can not be empty.
        if (imtlen(tpin) < 2)
            call error (1, "This task needs more than one input images")
	if (imtlen(tpmask) > 0) {
            if (imtlen(tpmask) < imtlen(tpin))
                call error (1, "Not enough files in 'masks'")
	} 
	if (fout[1] == EOS)
	    call error (1, "blank output file name")

	# read the sigmas parameter and parse the sigmas string into numbers
        call clgstr ("sigmas", SIGMAS(par), SZ_LINE)
        niter = strtor (SIGMAS(par), sigma)
        if (niter > MAX_ITER)
            call error (1, "max number of iterations is exceeded")
        if (niter <= 0)
            call error (1, "number of iterations is zero")

	# read other parameters
	call clgstr ("sky", SKY(par), SZ_FNAME)
        LOWER(par) = clgetr ("lower")
        UPPER(par) = clgetr ("upper")
        REJ(par) = clgetr ("radius")
        PSIGMA(par) = clgetr ("pfactor")

	# read the exposure name(s)
	list = imtopenp ("expname")
	NEXPNAMES(par) = imtlen(list)
	if (imtlen(list) > MAX_NEXP) 
	    call error (1, "Too many entries in EXPNAME")
	do i = 1, imtlen(list) {
	    nchar = imtrgetim (list, i, str[1, i], SZ_FNAME)
	    call strupr (str[1, i])
	}
	call strcpy (str[1,1], EXPNAME(par), SZ_FNAME)
	call strcpy (str[1,2], EXPNAME2(par), SZ_FNAME)
	call strcpy (str[1,3], EXPNAME3(par), SZ_FNAME)
	call imtclose (list)

        # figure out how to do initial comparison image
        call clgstr ("initial", INITIAL(par), SZ_LINE)

	# read the noise model
	call clgstr ("readnoise", READNOISE(par), SZ_LINE)
	call clgstr ("atodgain", ATODGAIN(par), SZ_LINE) 
	call clgstr ("scalenoise", SCALENOISE(par), SZ_LINE)

	# read data quality flags
	call dqval (BADBITS(par))

	call clgstr ("skyname", SKYNAME(par), SZ_FNAME)
        CRVAL(par) = clgeti ("crdqval")
        FILLVAL(par) = clgetr ("fillval")
	VERBOSE(par) = clgetb ("verbose")
end
