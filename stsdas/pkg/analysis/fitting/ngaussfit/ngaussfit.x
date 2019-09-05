include <pkg/gtools.h>
include	<error.h>
include	<mach.h>
include	<ctype.h>
include <fset.h>
include	<gset.h>
include	<tbset.h>

include "../nlfit/nlfit.h"
include	"../lib/colnames.h"
include "../ncfit/names.h"		# Ensures use of correct icfit 

define	RE_TRY		-99		# error flag value
define 	MAXGAUSS	3		# maximum number of gaussians in psets
define	MAXCOEF		MAXGAUSS * 3 + 2
define	COEF		Memr[coeff+$1]
define	CFLAG		Memb[coefflags+$1]

# NGAUSSFIT -- Multiple Gaussian fit to 1-dim data. Input may be one or more 
# lists (y or x,y), image sections, or table columns. Multidimensional image 
# sections are reduced to a vector by computing the projection about the 
# indicated axis. Task uses NLFIT routines for the actual fitting process. 
# The fitting parameters may be set interactively using NCFIT (modified 
# ICFIT) package. Maximum initial number of gaussians is 3, this can be
# modified thru interactive cursor options or when reading first guesses
# from a table instead of from psets.

procedure t_ngaussfit ()

char	input[SZ_LINE]			# Input data names
char	output[SZ_FNAME]		# Output fit file
bool	interactive			# Interactive?
char	graphics[SZ_FNAME]		# Graphics device
char	sample[SZ_LINE]			# Sample ranges
int	naverage			# Sample averaging size
char	function[SZ_LINE]		# Gaussian option
int	fitfunc
char	errtyp[SZ_LINE]			# Type of error calculation
char	lintyp[SZ_LINE]			# Plot type
int	etype
char	method[SZ_LINE]			# Minimization method
int	algo
int	npar				# Number of coefficients in fit func.
int	maxit				# maximum number of amoeba iterations
int	replic				# # of bootstrap samples
int	restart				# # of amoeba restarts
real	alpha, beta, gamma		# simplex control quantities
int	pcomp				# plot individual components ?
bool	rt				# Read starting data from table ?
char	tablein[SZ_FNAME]		# Table from which to read start data
int	row				# Table row were to read start data
bool	errors				# Compute parameter errors ?
real	sigma				# Const. error for each data point.
real	epadu, rnoise			# ccd noise parameters
bool	verbose				# Print amoeba info ?
real	low_reject, high_reject		# Rejection thresholds
int	niterate			# Number of rejection iterations
real	grow				# Rejection growing radius
int	axis				# Axis for projection
real	rms, chisq			# Fit rms and chisq

pointer	xpt				# Pointer to x vector
pointer	ypt				# Pointer to y vector
pointer	size				# Pointer to error vector
pointer	coeff
pointer	err
pointer	coefflags
pointer	pp				# Pointer to psets
pointer	dt				# table pointer
pointer	ic				# ICFIT (modified) pointer
pointer	gt				# GTOOLS pointer
pointer	nl				# NLFIT pointer
pointer	ft, fty, colptr
char	word[SZ_FNAME,4]		# Input line words
char	str[SZ_LINE]
char	text[SZ_LINE]
int	npix				# Number of values per vector
int	istat				# errors getting data
int	ltype
int	i
bool	ibool, null
long	cpu, clock			# time variables

int	gt_init()
int	nnames, rc_rdcurves(), fstati()
int	clgeti(), strdic(), tbpsta()
long	cputime(), clktime()
real	clgetr(), clgpsetr()
bool	clgetb(), clgpsetb(), streq()
pointer	clopset(), opendb()

begin
	# get parameters from main parameter file
	errors   	= clgetb ("resample")
	sigma 		= clgetr ("sigma")
	epadu 		= clgetr ("epadu")
	rnoise 		= clgetr ("readnoise")
	maxit		= clgeti ("maxit")
	replic          = clgeti ("replicas")
	restart         = clgeti ("restart")
	alpha           = clgetr ("alpha")
	beta            = clgetr ("beta")
	gamma           = clgetr ("gamma")
	verbose		= clgetb ("verbose")
	naverage 	= clgeti ("naverage")
	low_reject 	= clgetr ("low_reject")
	high_reject 	= clgetr ("high_reject")
	niterate 	= clgeti ("niterate")
	grow 		= clgetr ("grow")
	interactive 	= clgetb ("interactive")
	axis 		= clgeti ("axis")

	call clgstr ("sample", sample, SZ_LINE)
	ibool = clgetb ("pcomp")
	if (ibool)
	    pcomp = YES
	else
	    pcomp = NO

	call clgstr ("errtyp", errtyp, SZ_LINE)
	etype = strdic (errtyp, errtyp, SZ_LINE, NLERRORS)
	if (etype == 0)
	    call error (0, "Incorrect errtype specification.")
	call clgstr ("ltype", lintyp, SZ_LINE)
	ltype = strdic (lintyp, lintyp, SZ_LINE, "|continuous|boxes|") - 1

	call clgstr ("method", method, SZ_LINE)
	algo = strdic (method, method, SZ_LINE, NLMETHODS)
	if (algo == 0)
	    call error (0, "Incorrect method specification.")

	if ( interactive )
	    call clgstr ("device", graphics, SZ_FNAME)

	# read initial guesses from table, if this is the case
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
	            call error (1, "Initial guess table does not exist or error in row specification.")

		# Add colptr checking after each tbcfnd call, JC Hsu 5/25/94
	        # get function type 
	        call tbcfnd (dt, DB_CFUNC, colptr, 1)
		if (colptr == NULL) {
		    call sprintf (text, SZ_LINE, 
		     "Column \"%s\" does not exist in the initial guess table")
			call pargstr (DB_CFUNC)
		    call error (1, text)
		}
	        call tbrgtt (dt, colptr, function, null, SZ_LINE, 1, row)
	        if ( streq( function, "Gaussians" ))
	            fitfunc = GAUSSIANS
	        else if ( streq ( function, "cgauss" )) 
	            fitfunc = CGAUSS
	        else
	            call error (0, "Non-Gaussian function in row.")

	        # read chisq
	        call tbcfnd (dt, DB_CCHI, colptr, 1)
		if (colptr == NULL) {
		    call sprintf (text, SZ_LINE, 
		     "Column \"%s\" does not exist in the initial guess table")
			call pargstr (DB_CCHI)
		    call error (1, text)
		}
	        call tbrgtr (dt, colptr, chisq, null, 1, row)

	        # read rms
	        call tbcfnd (dt, DB_CRMS, colptr, 1)
		if (colptr == NULL) {
		    call sprintf (text, SZ_LINE, 
		     "Column \"%s\" does not exist in the initial guess table")
			call pargstr (DB_CRMS)
		    call error (1, text)
		}
	        call tbrgtr (dt, colptr, rms, null, 1, row)

	        # realloc space for coefficients
	        call tbcfnd (dt, DB_CDEGR, colptr, 1)
		if (colptr == NULL) {
		    call sprintf (text, SZ_LINE, 
		     "Column \"%s\" does not exist in the initial guess table")
			call pargstr (DB_CDEGR)
		    call error (1, text)
		}
	        call tbrgti (dt, colptr, npar, null, 1, row)
	        call realloc ( coeff, npar, TY_REAL )
	        call realloc ( err, npar, TY_REAL)
	        call realloc ( coefflags, npar, TY_BOOL)

	        # read coefficients
	        do i = 1, npar {
	            call sprintf( str, SZ_FNAME, DB_CCOEF)
		        call pargi( i)
	            call tbcfnd (dt, str, colptr, 1)
		    if (colptr == NULL) {
		        call sprintf (text, SZ_LINE, "Column \"%s\" does not exist in the initial guess table")
			    call pargstr (str)
		        call error (1, text)
		    }
	            call tbrgtr (dt, colptr, COEF(i-1), null, 1, row)
	        }

	        # read errors
	        do i = 1, npar {
	            call sprintf( str, SZ_FNAME, DB_CERR)
		        call pargi( i)
	            call tbcfnd (dt, str, colptr, 1)
		    if (colptr == NULL) {
		    	call sprintf (str, SZ_LINE, "Column \"%s\" does not exist in the initial guess table")
			    call pargstr (str)
		    	call error (1, text)
		    }
	            call tbrgtr (dt, colptr, Memr[err+i-1], null, 1, row)
	        }

	        # set flag array
		# set the flags according to the error values, if less than 0,
		# set to false, i.e. does not vary.  JC Hsu 5/26/94
	        do i = 1, npar {
		    Memb[coefflags+i-1] = (Memr[err+i-1] >= 0.)
	        }

	        call tbtclo (dt)

	    } then {

	        call erract (EA_WARN)
	        npar = 0			# error in table input
	    }

	} else {

	    # get type of function and open the appropriate pset

	    call clgstr ("function", function, SZ_LINE)
	    if (streq (function, "Gaussians")) {
	        fitfunc = GAUSSIANS
	        pp = clopset ("gausspars")
	    } else if (streq (function, "cgauss")) {
	        fitfunc = CGAUSS
	        pp = clopset ("cgausspars")
	    } else
	        call error (0, "Unknown function")

	    # read up to MAXGAUSS sets of coefficients, until stopped by: 
	    # INDEF value, zero amplitude or zero/negative fwhm.

	    call malloc (coeff, MAXCOEF, TY_REAL)
	    call malloc (err, MAXCOEF, TY_REAL)
	    call malloc (coefflags, MAXCOEF, TY_BOOL)
	    call amovkr (0., Memr[err], MAXCOEF)
	    rms   = 0.
	    chisq = 0.

	    npar = 2
	    COEF(NL_GA) = clgpsetr (pp, "a")
	    COEF(NL_GB) = clgpsetr (pp, "b")
	    CFLAG(NL_GA) = clgpsetb (pp, "va")
	    CFLAG(NL_GB) = clgpsetb (pp, "vb")
	    do i = 1, MAXGAUSS {
	        call sprintf( str, SZ_LINE, "ampl%d")
		    call pargi( i)
	        COEF(NL_GAMPL(i)) = clgpsetr (pp, str)
	        call sprintf( str, SZ_LINE, "cent%d")
		    call pargi( i)
	        COEF(NL_GCENT(i)) = clgpsetr (pp, str)
	        call sprintf( str, SZ_LINE, "fwhm%d")
		    call pargi( i)
	        COEF(NL_GFWHM(i)) = clgpsetr (pp, str)

	        if (IS_INDEFR (COEF(NL_GAMPL(i))) || COEF(NL_GAMPL(i)) == 0. || 
		    IS_INDEFR (COEF(NL_GCENT(i))) || 
		    IS_INDEFR (COEF(NL_GFWHM(i))) || COEF(NL_GFWHM(i)) <= 0. ) {

	            break

	        } else {

	            npar = npar + 3
	            call sprintf( str, SZ_LINE, "vampl%d")
		        call pargi( i)
	            CFLAG(NL_GAMPL(i)) = clgpsetb (pp, str)
	            call sprintf( str, SZ_LINE, "vcent%d")
		        call pargi( i)
	            CFLAG(NL_GCENT(i)) = clgpsetb (pp, str)
	            call sprintf( str, SZ_LINE, "vfwhm%d")
		        call pargi( i)
	            CFLAG(NL_GFWHM(i)) = clgpsetb (pp, str)
	        }
	    }
	    call clcpset (pp)
	}

	# Everything failed, take default starting conditions
	if (npar < 5) {
	    npar = 5
	    # This was forcing the function to Gaussian type even
            # when the user was setting it to cgauss (IB, Mar 25, 1996).
	    # fitfunc = GAUSSIANS
	    call realloc ( coeff, npar, TY_REAL )
	    call realloc ( err, npar, TY_REAL)
	    call realloc ( coefflags, npar, TY_BOOL)
	    COEF(NL_GA)        = INDEFR   # Changed to INDEF so routine
	    COEF(NL_GB)        = INDEFR   # nl_startv can set proper
	    COEF(NL_GAMPL(1))  = INDEFR   # starting values for these.
	    COEF(NL_GCENT(1))  = INDEFR   # (IB Sep 15, 1997)
	    COEF(NL_GFWHM(1))  = INDEFR
	    CFLAG(NL_GA)       = false
	    CFLAG(NL_GB)       = false
	    CFLAG(NL_GAMPL(1)) = true
	    CFLAG(NL_GCENT(1)) = true
	    CFLAG(NL_GFWHM(1)) = false
	    rms   = 0.
	    chisq = 0.
	}

	# Set the ICFIT structure.

	call ic_open (ic)
	call ic_pstr (ic, "sample", sample)
	call ic_pstr (ic, "algorithm", method)
	call ic_puti (ic, "naverage", naverage)
	call ic_pstr (ic, "function", function)
	call ic_puti (ic, "npar", npar)
	call ic_puti (ic, "pcomp", pcomp)
	call ic_putr (ic, "low", low_reject)
	call ic_putr (ic, "high", high_reject)
	call ic_puti (ic, "niterate", niterate)
	call ic_putr (ic, "grow", grow)
	call ic_puti (ic, "ltype", ltype)

	gt = gt_init()
	call gt_sets (gt, GTTYPE, "line")

	# open main input and output

	if (fstati (STDIN, F_REDIR) == YES)
	    call strcpy ("STDIN", input, SZ_FNAME)
	else
	    call clgstr ("input", input, SZ_LINE)

	call clgstr ("output", output, SZ_FNAME)

	dt = opendb (output)

	# read the data one at a time

	ft   = NULL
	fty  = NULL
	xpt  = NULL
	ypt  = NULL
	size = NULL
	while ( rc_rdcurves( input, str, ft, fty, nnames, word, xpt, ypt, size, 
		    npix, axis, true, istat) != EOF ) {
	    if ( istat == RE_TRY)
	  	next

	    call ic_pstr (ic, "ylabel", str)

	    # Initialize nlfit package.

	    nl = NULL
            call nl_init (nl, fitfunc, Memr[coeff], Memb[coefflags], 
			  npar, npix)
	    call nl_serrors (nl, Memr[err], npar)
	    call nl_putr (nl, "sigma", sigma)
	    call nl_putr (nl, "epadu", epadu)
	    call nl_putr (nl, "readnoise", rnoise)
	    call nl_putr (nl, "chisq", chisq)
	    call nl_putr (nl, "rms", rms)
	    call nl_putb (nl, "errors", errors)
	    call nl_puti (nl, "errtyp", etype)
	    call nl_puti (nl, "algorithm", algo)
	    call nl_puti (nl, "maxit", maxit)
	    call nl_putb (nl, "verbose", verbose)
	    call nl_puti (nl, "replic", replic)
	    call nl_puti (nl, "restart", restart)
	    call nl_putr (nl, "alpha", alpha)
	    call nl_putr (nl, "beta",  beta)
	    call nl_putr (nl, "gamma", gamma)

	    # Do it

	    cpu   = cputime (0)
	    clock = clktime (0)

	    iferr( call nl_fit1 ( Memr[xpt], Memr[ypt], Memr[size], npix, 
				nl, ic, gt, str, interactive, graphics, 
				dt) ) {
		call nl_free (nl)
		call erract( EA_WARN)
		next
	    }

	    call mfree (xpt, TY_REAL)
	    call mfree (ypt, TY_REAL)
	    call mfree (size, TY_REAL)

	    call printf ("%7.2f  CPU seconds,  %7.2f  elapsed minutes.\n")
	        call pargr (real (cputime (cpu)) / 1000.)
	        call pargr (real (clktime (clock)) / 60.)

	    call nl_free (nl)
	}

	call ic_close (ic)
	call gt_free (gt)
	call tbtclo (dt)
	call mfree (coeff, TY_REAL)
	call mfree (err, TY_REAL)
	call mfree (coefflags, TY_BOOL)
end

