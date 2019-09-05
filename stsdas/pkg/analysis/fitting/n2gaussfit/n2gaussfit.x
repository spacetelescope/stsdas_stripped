include	<imhdr.h>
include <imset.h>
include	<error.h>
include	<mach.h>
include	<ctype.h>
include <fset.h>
include	<tbset.h>

include "../nlfit/nlfit.h"
include	"../lib/colnames.h"

define	RE_TRY		-99		# error flag value
define 	MAXGAUSS	1		# maximum number of gaussians in psets
define	MAXCOEF		MAXGAUSS * 6 + 1
define	COEF		Memr[coeff+$1]
define	CFLAG		Memb[coefflags+$1]

# N2GAUSSFIT -- Gaussian fit to 2-dim data. Input is one or more image
# sections. Task uses NLFIT routines for the actual fitting process. 

procedure t_n2gaussfit ()

char	input[SZ_LINE]			# Input data names
char	output[SZ_FNAME]		# Output fit file
int	fitfunc
char	errtyp[SZ_LINE]			# Type of error calculation
int	etype
char	method[SZ_LINE]			# Minimization method
int	algor
int	npar				# Number of coefficients in fit func.
int	maxit				# maximum number of amoeba iterations
bool	rt				# Read starting data from table ?
char	tablein[SZ_FNAME]		# Table from which to read start data
int	row				# Table row were to read start data
bool	errors				# Compute parameter errors ?
real	sigma				# Pixel error (if not ccd).
real	epadu, rnoise			# ccd error model.
int	replic				# # of bootstrap samples
int	restart				# # of amoeba restarts
real	alpha, beta, gamma		# simplex control quantities
bool	averbose			# Print amoeba info ?
bool	verbose				# Print results ?

pointer	xpt				# Pointer to x vector
pointer	ypt				# Pointer to y vector
pointer	zpt				# Pointer to z vector
pointer	coeff
pointer	err
pointer	coefflags
pointer	pp				# Pointer to psets
pointer	dt				# table pointer
pointer	nl				# NLFIT pointer
pointer	ft, colptr
char	str[SZ_LINE]
int	npix				# Number of values per vector
int	istat				# errors getting data
int	i
int	px,py				# center of square region to fit
int	boxsize				# size of square region to fit
bool	null
long	cpu, clock			# time variables
real	chisq, rms				# Chisq and rms

int	clgeti(), strdic(), tbpsta()
int	rc2_getim(), strlen()
long	cputime(), clktime()
real	clgetr(), clgpsetr()
int	clgpseti()
bool	clgetb(), clgpsetb()
pointer	clopset(), opendb()

begin
	# Get parameters.
	errors   = clgetb ("resample")
	sigma    = clgetr ("sigma")
	epadu    = clgetr ("epadu")
	rnoise   = clgetr ("readnoise")
	maxit    = clgeti ("maxit")
	verbose	 = clgetb ("verbose")
	averbose = clgetb ("averbose")
	replic   = clgeti ("replicas")
	restart  = clgeti ("restart")
	alpha    = clgetr ("alpha")
	beta     = clgetr ("beta")
	gamma    = clgetr ("gamma")

	call clgstr ("errtyp", errtyp, SZ_LINE)
	etype = strdic (errtyp, errtyp, SZ_LINE, NLERRORS)
	if (etype == 0)
	    call error (0, "Incorrect errtype specification.")

	call clgstr ("method", method, SZ_LINE)
	algor = strdic (method, method, SZ_LINE, NLMETHODS)
	if (algor == 0)
	    call error (0, "Incorrect method specification.")

	# Read initial guesses from table, if this is the case.
	rt = clgetb ("rt")
	if (rt) {

	    call clgstr( "tablein", tablein, SZ_FNAME )
	    row = clgeti ("row")
	    call malloc ( coeff, MAXCOEF, TY_REAL )
	    call malloc ( err, MAXCOEF, TY_REAL)
	    call malloc ( coefflags, MAXCOEF, TY_BOOL)

	    iferr {
	        dt = opendb (tablein)
	        if (row > tbpsta (dt, TBL_NROWS))
	            call error (0, "Error in row specification.")

	        # get function type 
	        call tbcfnd (dt, DB_CFUNC, colptr, 1)
	        call tbrgtt (dt, colptr, str, null, SZ_LINE, 1, row)
	        fitfunc = strdic (str, str, SZ_LINE, NLFUNCTIONS)
	        if (fitfunc != TWODGAUSS)
	            call error (0, "Invalid function in row.")

	        # read chisq
	        call tbcfnd (dt, DB_CCHI, colptr, 1)
	        call tbrgtr (dt, colptr, chisq, null, 1, row)

	        # read rms
	        call tbcfnd (dt, DB_CRMS, colptr, 1)
	        call tbrgtr (dt, colptr, rms, null, 1, row)

	        # realloc space for coefficients
	        call tbcfnd (dt, DB_CDEGR, colptr, 1)
	        call tbrgti (dt, colptr, npar, null, 1, row)
	        call realloc ( coeff, npar, TY_REAL )
	        call realloc ( err, npar, TY_REAL)
	        call realloc ( coefflags, npar, TY_BOOL)

	        # read coefficients
	        do i = 1, npar {
	            call sprintf( str, SZ_FNAME, DB_CCOEF)
		        call pargi( i)
	            call tbcfnd (dt, str, colptr, 1)
	            call tbrgtr (dt, colptr, COEF(i-1), null, 1, row)
	        }

	        # read errors
	        do i = 1, npar {
	            call sprintf( str, SZ_FNAME, DB_CERR)
		        call pargi( i)
	            call tbcfnd (dt, str, colptr, 1)
	            call tbrgtr (dt, colptr, Memr[err+i-1], null, 1, row)
	        }

	        # read flag array and boxsize from pset.
	        pp = clopset ("tgausspars")
	        boxsize  = clgpseti (pp, "boxsize")
	        CFLAG(NL_G2A)    = clgpsetb (pp, "va")
	        CFLAG(NL_G2AMPL) = clgpsetb (pp, "vampl")
	        CFLAG(NL_G2XC)   = clgpsetb (pp, "vxcent")
	        CFLAG(NL_G2YC)   = clgpsetb (pp, "vycent")
	        CFLAG(NL_G2FWHM) = clgpsetb (pp, "vfwhm")
	        CFLAG(NL_G2ELL)  = clgpsetb (pp, "vellip")
	        CFLAG(NL_G2TETA) = clgpsetb (pp, "vtheta")
	        call clcpset (pp)

	        call tbtclo (dt)

	    } then {
	        call erract (EA_WARN)
	        npar = 0			# error in table input
	    }

	} else {

	    # Read parameters from pset instead.
	    fitfunc = TWODGAUSS
	    pp = clopset ("tgausspars")

	    # read up to MAXGAUSS sets of coefficients, until stopped by: 
	    # INDEF value, zero amplitude or zero/negative fwhm.

	    call malloc (coeff, MAXCOEF, TY_REAL)
	    call malloc (err, MAXCOEF, TY_REAL)
	    call malloc (coefflags, MAXCOEF, TY_BOOL)
	    call amovkr (0., Memr[err], MAXCOEF)
	    rms   = 0.
	    chisq = 0.

	    npar = 1
	    boxsize = clgpseti (pp, "boxsize")
	    COEF(NL_G2A) = clgpsetr (pp, "a")
	    CFLAG(NL_G2A) = clgpsetb (pp, "va")
	    do i = 1, MAXGAUSS {
#	        call sprintf( str, SZ_LINE, "ampl%d")
#		    call pargi( i)
	        call sprintf( str, SZ_LINE, "ampl")
	        COEF(NL_G2AMPL) = clgpsetr (pp, str)
#	        call sprintf( str, SZ_LINE, "xcent%d")
#		    call pargi( i)
	        call sprintf( str, SZ_LINE, "xcent")
	        COEF(NL_G2XC) = clgpsetr (pp, str)
	        call sprintf( str, SZ_LINE, "ycent")
	        COEF(NL_G2YC) = clgpsetr (pp, str)
#	        call sprintf( str, SZ_LINE, "fwhm%d")
#		    call pargi( i)
	        call sprintf( str, SZ_LINE, "fwhm")
	        COEF(NL_G2FWHM) = clgpsetr (pp, str)
	        call sprintf( str, SZ_LINE, "ellip")
	        COEF(NL_G2ELL) = clgpsetr (pp, str)
	        call sprintf( str, SZ_LINE, "theta")
	        COEF(NL_G2TETA) = clgpsetr (pp, str) / 180. * 3.1415926

	        if (IS_INDEFR (COEF(NL_G2AMPL)) || COEF(NL_G2AMPL) == 0. || 
		    IS_INDEFR (COEF(NL_G2XC)) || IS_INDEFR (COEF(NL_G2YC)) || 
		    IS_INDEFR (COEF(NL_G2FWHM)) || COEF(NL_G2FWHM) <= 0. ) {

	            break

	        } else {

	            npar = npar + 6
#	            call sprintf( str, SZ_LINE, "vampl%d")
#		        call pargi( i)
	            call sprintf( str, SZ_LINE, "vampl")
	            CFLAG(NL_G2AMPL) = clgpsetb (pp, str)
#	            call sprintf( str, SZ_LINE, "vcent%d")
#		        call pargi( i)
	            call sprintf( str, SZ_LINE, "vxcent")
	            CFLAG(NL_G2XC) = clgpsetb (pp, str)
	            call sprintf( str, SZ_LINE, "vycent")
	            CFLAG(NL_G2YC) = clgpsetb (pp, str)
#	            call sprintf( str, SZ_LINE, "vfwhm%d")
#		        call pargi( i)
	            call sprintf( str, SZ_LINE, "vfwhm")
	            CFLAG(NL_G2FWHM) = clgpsetb (pp, str)
	            call sprintf( str, SZ_LINE, "vellip")
	            CFLAG(NL_G2ELL) = clgpsetb (pp, str)
	            call sprintf( str, SZ_LINE, "vtheta")
	            CFLAG(NL_G2TETA) = clgpsetb (pp, str)
	        }
	    }
	    call clcpset (pp)
	}


	# Everything failed, take default starting conditions (IB 9/15/97)
	if (npar < 7) {
	    npar = 7
	    call realloc ( coeff, npar, TY_REAL )
	    call realloc ( err, npar, TY_REAL)
	    call realloc ( coefflags, npar, TY_BOOL)
	    COEF(NL_G2A)     = INDEFR
	    COEF(NL_G2AMPL)  = INDEFR
	    COEF(NL_G2XC)    = INDEFR
	    COEF(NL_G2YC)    = INDEFR
	    COEF(NL_G2FWHM)  = INDEFR
	    COEF(NL_G2ELL)   = INDEFR
	    COEF(NL_G2TETA)  = INDEFR
	    CFLAG(NL_G2A)    = false
	    CFLAG(NL_G2AMPL) = true
	    CFLAG(NL_G2XC)   = true
	    CFLAG(NL_G2YC)   = true
	    CFLAG(NL_G2FWHM) = false
	    CFLAG(NL_G2ELL)  = false
	    CFLAG(NL_G2TETA) = false
	}

	# set box position
	if (IS_INDEFR(COEF(NL_G2XC)))
	    px = 0                    # zero signals to underlying routine
	else                          # that it should set it to image center
	    px = int (COEF(NL_G2XC))
	if (IS_INDEFR(COEF(NL_G2YC)))
	    py = 0
	else
	    py = int (COEF(NL_G2YC)) 

	# open input list and output table

	call clgstr ("input", input, SZ_LINE)
	call clgstr ("output", output, SZ_FNAME)
	if (strlen(output) > 0)
	    dt = opendb (output)

	# read the data one at a time

	cpu   = cputime (0)
	clock = clktime (0)
	ft    = NULL

	while (rc2_getim (input, str, ft, px, py, boxsize, xpt, ypt, 
                          zpt, npix, istat, boxsize/2) != EOF ) {
	    if ( istat == RE_TRY)
	  	next

	    # Initialize nlfit package.

	    nl = NULL
            call nl_init (nl, fitfunc, Memr[coeff], Memb[coefflags], 
			  npar, npix)
	    call nl_serrors (nl, Memr[err], npar)
	    if (IS_INDEF(sigma))
	        call nl_putr (nl, "sigma", 1.01)
	    else
	        call nl_putr (nl, "sigma", sigma)
	    call nl_putr (nl, "epadu", epadu)
	    call nl_putr (nl, "readnoise", rnoise)
	    call nl_putr (nl, "chisq", chisq)
	    call nl_putr (nl, "rms", rms)
	    call nl_putb (nl, "errors", errors)
	    call nl_puti (nl, "errtyp", etype)
	    call nl_puti (nl, "algorithm", algor)
	    call nl_puti (nl, "maxit", maxit)
	    call nl_putb (nl, "verbose", averbose)
	    call nl_puti (nl, "replic", replic)
	    call nl_puti (nl, "restart", restart)
	    call nl_putr (nl, "alpha", alpha)
	    call nl_putr (nl, "beta",  beta)
	    call nl_putr (nl, "gamma", gamma)

	    # Make sure INDEF-valued coefficients are replaced
	    # by something sensible.

	    call nl_startv (nl, Memr[xpt], Memr[ypt], Memr[zpt])

	    # Do it

	    iferr( call nl_fit2 ( Memr[xpt], Memr[ypt], Memr[zpt], 
				  str, npix, nl, dt, verbose) ) {
		call nl_free (nl)
		call erract( EA_WARN)
		next
	    }

	    call nl_free (nl)
	    call mfree (xpt, TY_REAL)
	    call mfree (zpt, TY_REAL)
	    call mfree (zpt, TY_REAL)
	}

	if (verbose) {
	    call printf ("%7.2f  CPU seconds,  %7.2f  elapsed minutes.\n")
	        call pargr (real (cputime (cpu)) / 1000.)
	        call pargr (real (clktime (clock)) / 60.)
	}

	if (dt != NULL)
	    call tbtclo (dt)
	call mfree (coeff, TY_REAL)
	call mfree (err, TY_REAL)
	call mfree (coefflags, TY_BOOL)
end


# NL_FIT2 -- Given the x, y and z data, fit the 2-d gaussian and write
# results to table.

procedure nl_fit2 (x, y, z, file, npix, nl, out, verbose)

real	x[ARB]				# input x data
real	y[ARB]				# input y data
real	z[ARB]				# input z data
char	file[ARB]			# file name
int	npix				# number of data points
pointer	nl				# NLFIT pointer
pointer	out				# Table pointer
bool	verbose				# Print results ?

int	i, nl_stati()
real	xsafe, ysafe
real	epadu, rnoise, rn2, sigma, nl_statr()
pointer	sp, wts

errchk	nl_fit, nl_wtdb, salloc, smark

begin
	# Allocate memory for weights.
	call smark (sp)
	call salloc (wts, npix, TY_REAL)

	# Read noise-related quantities.
	sigma  = nl_statr (nl, "sigma")
	epadu  = nl_statr (nl, "epadu")
	rnoise = nl_statr (nl, "readnoise")
	rn2 = rnoise * rnoise

	# Set weights.
	do i = 1, npix {
	    Memr[wts+i-1] = 1.0

	    # If sigma parameter was provided by user, use it.
	    if (!IS_INDEF(sigma))
	        Memr[wts+i-1] = 1.0 / sigma

	    # If ccd type was set, this takes preference.
	    if (nl_stati(nl, "errtyp") == CCD) {
	        if (z[i] <= 0.) {
	            if (rnoise > 0)
	                Memr[wts+i-1] = 1.0 / rnoise
	            else
	                Memr[wts+i-1] = 0.0
	        } else
	            Memr[wts+i-1] = 1.0 / sqrt (z[i]/epadu + rn2)
	    }
	}

	# Set all the zero weighted values to something safe.
	do i = 1, npix {
	    if ( Memr[wts+i-1] != 0.0 ) {
		xsafe = x[i]
		ysafe = y[i]
	    }
	}
	do i = 1, npix {
	    if ( Memr[wts+i-1] == 0.0) {
		x[i] = xsafe
		y[i] = ysafe
	    }
	}

	# Do it
        call nl_fit (nl, x, y, z, Memr[wts])
	call nl_wtdb( file, nl, out, verbose )

	call sfree (sp)
end
                                                       

# RC2_GETIM -- Open each image in turn, and generate x, y and z 
# vectors.

int procedure rc2_getim ( input, image, ft, px, py, boxsize, 
                          xpt, ypt, zpt, npix, istat, boundary)

char	input[ARB]		# Operand list
char	image[SZ_LINE]		# File name
pointer	ft			# file pointer
int	px, py			# position of subraster box
int	boxsize			# Box size to fit
pointer	xpt			# Pointer to x vector
pointer	ypt			# Pointer to y vector
pointer	zpt			# Pointer to z vector
int	npix			# Number of values per vector
int	istat			# OK?
int	boundary		# size of boundary extension in images

int	rc2_rdimage()
int	imtopen(), imtgetim()

begin
	istat = NULL
	if ( ft == NULL )
	    ft = imtopen (input)
	if (imtgetim (ft, image, SZ_LINE) == EOF) {
	    call imtclose (ft)
	    return (EOF)
	}

	iferr {
	    npix = rc2_rdimage (image, boundary, xpt, ypt, zpt, 
                                px, py, boxsize)
	} then {
	    call erract (EA_WARN)
	    istat = RE_TRY
	}
	return (OK)
end


# RC2_RDIMAGE  --  Read pixels and generate x, y vectors in pixel units.

int procedure rc2_rdimage (image, boundary, x, y, z, px, py, boxsize)

char	image[ARB]
pointer	x, y, z
int	px, py, boundary, boxsize

#--
int	subr[4], i, j, k, npix
pointer	im, z1, immap(), imgs2r()

errchk	imgs2r, immap, imunmap

begin
	# Open image with boundary extension.
	im = immap (image, READ_ONLY, 0)
	if (IM_NDIM(im) != 2)
	    call error (0, "Wrong image dimension.")
	call imseti (im, IM_TYBNDRY, BT_NEAREST)
	call imseti (im, IM_NBNDRYPIX, boundary)

	# Compute corners of subraster to fit. If no center
	# was supplied, center box in the image array.
	if (px == 0 || py == 0) {
	    px = IM_LEN(im,1) / 2
	    py = IM_LEN(im,2) / 2
	}
	subr[1] = px - boxsize / 2
	subr[2] = px + boxsize / 2
	subr[3] = py - boxsize / 2
	subr[4] = py + boxsize / 2

	# Read subraster.
	npix = (subr[2] - subr[1] + 1) * (subr[4] - subr[3] + 1)
	z1 = imgs2r (im, subr[1], subr[2], subr[3], subr[4])

	if (npix > 5000) {
	    call eprintf ("Warning: boxsize too large, task will slow down.\n")
	    call flush (STDERR)
	}

	# Generate vectors.
	call malloc (x, npix, TY_REAL)
	call malloc (y, npix, TY_REAL)
	call malloc (z, npix, TY_REAL)
	call amovr (Memr[z1], Memr[z], npix)
	k = 0
	do j = subr[3], subr[4] {
	    do i = subr[1], subr[2] {	    
	        Memr[x+k] = real (i)
	        Memr[y+k] = real (j)
	        k = k + 1
	    }
	}

	# Done.	
	call imunmap (im)
	return (npix)	
end

