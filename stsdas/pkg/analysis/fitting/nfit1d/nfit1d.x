include <pkg/gtools.h>
include	<error.h>
include	<mach.h>
include	<ctype.h>
include <fset.h>
include	<gset.h>
include	<tbset.h>

include "../nlfit/nlfit.h"
include "../ncfit/names.h"		# Ensures use of correct icfit 

define	RE_TRY		-99		# error flag value

# NFIT1D -- Fits 1-dim data with non-linear functions. 
# Supported functions are Planck, power-law, composite (Planck + power-law), 
# two Planck functions, galaxy brigthness profile, and an user-specified 
# function. 
# Input may be one or more lists (y or x,y), image sections, or table
# columns.  Multidimensional image sections are reduced to a vector by
# computing the projection about the indicated axis.  In case of Planck or
# composite functions, specified axis must have wavelength, frequency or
# energy units associated with it thru header keywords.  Task uses NLFIT
# routines for the actual fitting process.  The fitting parameters may be
# set interactively using the NCFIT (modified icfit) package. 
# Initial guesses for function coefficients can be input thru psets,
# interactively, or from a previously created table.
# The maximum allowed number of user function coefficients is defined by
# the constant NUPAR. Its value must be consistent whith the total number 
# of coefficients in parameter file USERPARS.PAR.

procedure t_nfit1d ()

char	input[SZ_LINE]			# Input data names
char	output[SZ_FNAME]		# Output fit table
bool	interactive			# Interactive?
char	graphics[SZ_FNAME]		# Graphics device
char	sample[SZ_LINE]			# Sample ranges
int	naverage			# Sample averaging size
char	function[SZ_LINE]		# Curve fitting function
int	fitfunc
char	errtyp[SZ_LINE]			# Error calculation type
int	etype
char	method[SZ_LINE]			# Minimization method
int	algo
int	npar				# Number of coefficients in fit func.
int	maxit				# Maximum number of amoeba iterations
int	replic				# # of bootstrap samples
int	restart				# # of amoeba restarts
int	pcomp				# Plot composite components ?
bool	rt				# Read starting data from table ?
char	tablein[SZ_FNAME]		# Table from which to read start data
int	row				# Table row were to read start data
char	xunits[SZ_LINE]			# X axis units
int	units
bool	errors				# Compute parameter errors ?
real	sigma				# Const. error bar for each data point.
real	epadu, rnoise			# ccd noise parameters
bool	verbose				# print convergency info ?
real	low_reject, high_reject		# Rejection thresholds
int	niterate			# Number of rejection iterations
real	grow				# Rejection growing radius
int	axis				# Axis for projection
char	lintyp[SZ_LINE]			# Plot type

pointer	xpt				# Pointer to x vector
pointer	ypt				# Pointer to y vector
pointer	size				# Pointer to error vector
char	word[SZ_FNAME,4]		# Input line words
char	funcstr[SZ_LINE]		# user function string
char	image[SZ_FNAME]			# image name
int	npix				# Number of values per vector
int	istat				# errors getting data
int	ltype
real	alpha, beta, gamma		# simplex control quantities
pointer	out				# fit table pointer
pointer	ic				# ICFIT (modified) pointer
pointer	gt				# GTOOLS pointer
pointer	nl				# NLFIT pointer
pointer sp, coeff, coefflags, err
pointer	ft, fty
bool	ibool
long	cpu, clock			# time variables
real	aux

int	strlen(), gt_init()
int	nnames, rc_rdcurves(), fstati()
int	clgeti(), strdic()
long	cputime(), clktime()
real	clgetr()
bool	clgetb()
pointer	opendb()

begin
	call smark (sp)

	# Get task parameters. Read initial guesses from 
	# table, if this is the case. If not, read from pset.

	rt = clgetb ("rt")
	if (rt) {

	    call clgstr( "tablein", tablein, SZ_FNAME )
	    row = clgeti ("row")

	    iferr {
	        call getfit (tablein, row, function, function, function, 
	                     xunits, SZ_LINE, npar, npix, aux, aux, aux, 
	                     aux, coeff, err, coefflags)
	        fitfunc = strdic (function, function, SZ_LINE, NLFUNCTIONS)
	        if ((fitfunc == 0) || (fitfunc == GAUSSIANS) ||
		    (fitfunc == CGAUSS))
	            call error (0, "Invalid function in row.")

	        units = strdic (xunits, xunits, SZ_LINE, NLUNITS)
	        if (units == 0)
	            call error (0, "Invalid units in table.")

	    } then {

	        # Error in table input; read from pset instead.
	        call erract (EA_WARN)
	        call clgstr ("xunits", xunits, SZ_LINE)
	        units = strdic (xunits, xunits, SZ_LINE, NLUNITS)
	        if (units == 0)
	            call error (0, "Incorrect xunits in pset.")
	        call clgstr ("function", function, SZ_LINE)
	        call nf_getpset (function, fitfunc, funcstr, coeff, coefflags, 
			         err, npar)
	    }

	} else {

	    call clgstr ("xunits", xunits, SZ_LINE)
	    units = strdic (xunits, xunits, SZ_LINE, NLUNITS)
	    if (units == 0)
	        call error (0, "Incorrect xunits specification.")

	    call clgstr ("function", function, SZ_LINE)

	    # Read function-specific parameters from appropriate pset.
	    call nf_getpset (function, fitfunc, funcstr, coeff, coefflags, 
			     err, npar)
	}



	# Check for INDEF function coefficients.
#	do i = 1, npar
#	    if (Memr[coeff+i-1] == INDEF) 
#		call error (0,"There are undefined coefficients.")

	naverage 	= clgeti ("naverage")
	low_reject 	= clgetr ("low_reject")
	high_reject 	= clgetr ("high_reject")
	niterate 	= clgeti ("niterate")
	grow 		= clgetr ("grow")
	errors		= clgetb ("resample")
	sigma 		= clgetr ("sigma")
	epadu 		= clgetr ("epadu")
	rnoise 		= clgetr ("readnoise")
	replic          = clgeti ("replicas")
	restart         = clgeti ("restart")
	alpha           = clgetr ("alpha")
	beta            = clgetr ("beta")
	gamma           = clgetr ("gamma")

	if (fstati (STDIN, F_REDIR) == YES)
	    call strcpy ("STDIN", input, SZ_FNAME)
	else
	    call clgstr ("input", input, SZ_LINE)

	call clgstr ("output", output, SZ_FNAME)

	axis 		= clgeti ("axis")
	call clgstr ("sample", sample, SZ_LINE)

	call clgstr ("errtyp", errtyp, SZ_LINE)
	etype = strdic (errtyp, errtyp, SZ_LINE, NLERRORS)
	if (etype == 0)
	    call error (0, "Incorrect errtype specification.")

	call clgstr ("method", method, SZ_LINE)
	algo = strdic (method, method, SZ_LINE, NLMETHODS)
	if (algo == 0)
	    call error (0, "Incorrect method specification.")

	maxit		= clgeti ("maxit")
	interactive 	= clgetb ("interactive")
	verbose		= clgetb ("verbose")
	ibool		= clgetb ("pcomp")
	call clgstr ("ltype", lintyp, SZ_LINE)
	ltype = strdic (lintyp, lintyp, SZ_LINE, "|continuous|boxes|") - 1
	
	if (ibool)
	    pcomp = YES
	else
	    pcomp = NO

	if ( interactive )
	    call clgstr ("device", graphics, SZ_FNAME)

	# Open output table.

	out = opendb (output)

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

	# change the default from "line" to "mark". JC Hsu 6/16/94
	call gt_sets (gt, GTTYPE, "mark")

	# read the data one at a time

	ft   = NULL
	fty  = NULL
	xpt  = NULL
	ypt  = NULL
	size = NULL
	while ( rc_rdcurves( input, image, ft, fty, nnames, word, xpt, ypt, 
			     size, npix, axis, true, istat) != EOF ) {
	    if ( istat == RE_TRY)
	  	next

	    call ic_pstr (ic, "ylabel", image)

	    # Initialize nlfit package.
	    nl = NULL

            call nl_init (nl, fitfunc, Memr[coeff], Memb[coefflags], 
			  npar, npix)
	    call nl_serrors (nl, Memr[err], npar)
	    call nl_puti (nl, "algorithm", algo)
	    call nl_puti (nl, "maxit", maxit)
	    call nl_puti (nl, "units", units)
	    call nl_putb (nl, "errors", errors)
	    call nl_putr (nl, "sigma", sigma)
	    call nl_putr (nl, "epadu", epadu)
	    call nl_putr (nl, "readnoise", rnoise)
	    call nl_puti (nl, "errtyp", etype)
	    call nl_putb (nl, "verbose", verbose)
	    call nl_puti (nl, "replic", replic)
	    call nl_puti (nl, "restart", restart)
	    call nl_putr (nl, "alpha", alpha)
	    call nl_putr (nl, "beta",  beta)
	    call nl_putr (nl, "gamma", gamma)

	    if (fitfunc == USER)
	        call nl_iuser (nl, funcstr, strlen(funcstr)+1)

	    # Do it.

	    cpu   = cputime (0)
	    clock = clktime (0)

	    iferr( call nl_fit1 ( Memr[xpt], Memr[ypt], Memr[size], npix, 
				nl, ic, gt, image, interactive, graphics, 
				out) ) {
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
	call tbtclo (out)
	call sfree (sp)
end


# NF_GETPSET -- Read function-specific parameters from appropriate pset.
# Caller must set stack marker.

# Phil Hodge, 21-May-1992  Change "function[]" to "function[ARB]";
#		in call to strdic, replace strlen(function) by SZ_LINE.

procedure nf_getpset (function, fitfunc, funcstr, coeff, coefflags, err, 
		      npar)

char	function[ARB]			# i: function name
int	fitfunc				# o: function type
char	funcstr[SZ_LINE]		# o: function string (user only)
pointer	coeff				# o: coefficient array
pointer	coefflags			# o: flags array
pointer	err				# o: errors array
int	npar				# o: number of coefficients

pointer	pp				# pset pointer
char	str[SZ_LINE]			# work string
int	i

int	strlen(), strdic()
real	clgpsetr()
bool	clgpsetb()
pointer	clopset()

begin
	fitfunc = strdic (function, function, SZ_LINE, NLFUNCTIONS)
	switch (fitfunc) {

	case POWERLAW:
	    npar = 3
	    call salloc (coeff, npar, TY_REAL)
	    call salloc (coefflags, npar, TY_BOOL)
	    pp = clopset ("powerpars")
	    Memr[coeff]   = clgpsetr (pp, "index")
	    Memr[coeff+1] = clgpsetr (pp, "ampl")
	    Memr[coeff+2] = clgpsetr (pp, "ref")
	    Memb[coefflags]   = clgpsetb (pp, "vindex")
	    Memb[coefflags+1] = clgpsetb (pp, "vampl")
	    Memb[coefflags+2] = clgpsetb (pp, "vref")
	    call clcpset (pp)

	case BBODY:
	    npar = 3
	    call salloc (coeff, npar, TY_REAL)
	    call salloc (coefflags, npar, TY_BOOL)
	    pp = clopset ("bbodypars")
	    Memr[coeff]   = clgpsetr (pp, "temp")
	    Memr[coeff+1] = clgpsetr (pp, "ampl")
	    Memr[coeff+2] = clgpsetr (pp, "ref")
	    Memb[coefflags]   = clgpsetb (pp, "vtemp")
	    Memb[coefflags+1] = clgpsetb (pp, "vampl")
	    Memb[coefflags+2] = clgpsetb (pp, "vref")
	    call clcpset (pp)

	case COMPOSITE:
	    npar = 6
	    call salloc (coeff, npar, TY_REAL)
	    call salloc (coefflags, npar, TY_BOOL)
	    pp = clopset ("comppars")
	    Memr[coeff]   = clgpsetr (pp, "index")
	    Memr[coeff+1] = clgpsetr (pp, "pampl")
	    Memr[coeff+2] = clgpsetr (pp, "pref")
	    Memr[coeff+3] = clgpsetr (pp, "temp")
	    Memr[coeff+4] = clgpsetr (pp, "bampl")
	    Memr[coeff+5] = clgpsetr (pp, "bref")
	    Memb[coefflags]   = clgpsetb (pp, "vindex")
	    Memb[coefflags+1] = clgpsetb (pp, "vpampl")
	    Memb[coefflags+2] = clgpsetb (pp, "vpref")
	    Memb[coefflags+3] = clgpsetb (pp, "vtemp")
	    Memb[coefflags+4] = clgpsetb (pp, "vbampl")
	    Memb[coefflags+5] = clgpsetb (pp, "vbref")
	    call clcpset (pp)

	case TWOBBODY:
	    npar = 6
	    call salloc (coeff, npar, TY_REAL)
	    call salloc (coefflags, npar, TY_BOOL)
	    pp = clopset ("twobbpars")
	    Memr[coeff]   = clgpsetr (pp, "temp1")
	    Memr[coeff+1] = clgpsetr (pp, "ampl1")
	    Memr[coeff+2] = clgpsetr (pp, "ref1")
	    Memr[coeff+3] = clgpsetr (pp, "temp2")
	    Memr[coeff+4] = clgpsetr (pp, "ampl2")
	    Memr[coeff+5] = clgpsetr (pp, "ref2")
	    Memb[coefflags]   = clgpsetb (pp, "vtemp1")
	    Memb[coefflags+1] = clgpsetb (pp, "vampl1")
	    Memb[coefflags+2] = clgpsetb (pp, "vref1")
	    Memb[coefflags+3] = clgpsetb (pp, "vtemp2")
	    Memb[coefflags+4] = clgpsetb (pp, "vampl2")
	    Memb[coefflags+5] = clgpsetb (pp, "vref2")
	    call clcpset (pp)

	case GALPROF:
	    npar = 6
	    call salloc (coeff, npar, TY_REAL)
	    call salloc (coefflags, npar, TY_BOOL)
	    pp = clopset ("galprofpars")
	    Memr[coeff]   = clgpsetr (pp, "se")
	    Memr[coeff+1] = clgpsetr (pp, "re")
	    Memr[coeff+2] = clgpsetr (pp, "s0")
	    Memr[coeff+3] = clgpsetr (pp, "r0")
	    Memr[coeff+4] = clgpsetr (pp, "r1")
	    Memr[coeff+5] = clgpsetr (pp, "backgr")
	    Memb[coefflags]   = clgpsetb (pp, "vse")
	    Memb[coefflags+1] = clgpsetb (pp, "vre")
	    Memb[coefflags+2] = clgpsetb (pp, "vs0")
	    Memb[coefflags+3] = clgpsetb (pp, "vr0")
	    Memb[coefflags+4] = clgpsetb (pp, "vr1")
	    Memb[coefflags+5] = clgpsetb (pp, "vbackgr")
	    call clcpset (pp)

	case USER:
	    npar = 0
	    call salloc (coeff,     NUPAR, TY_REAL)
	    call salloc (coefflags, NUPAR, TY_BOOL)
	    call amovkr (INDEFR, Memr[coeff],     NUPAR)
	    pp = clopset ("userpars")
	    call clgpset (pp, "function", funcstr, SZ_LINE) 
	    if (strlen(funcstr) == 0)
	        call error (0, "Undefined function")
	    do i = 1, NUPAR {
	        call sprintf (str, SZ_LINE, "c%-2d")
	            call pargi (i)
	        Memr[coeff+i-1] = clgpsetr (pp, str)
	        if (!IS_INDEF(Memr[coeff+i-1])) 
	            npar = npar + 1
	        else
	            break
	        call sprintf (str, SZ_LINE, "v%-2d")
	            call pargi (i)
	        Memb[coefflags+i-1] = clgpsetb (pp, str)
	    }
	    call clcpset (pp)
	    if ( npar == 0 )
	        call error (0, "There are undefined coefficients.")

	default:
	    call error (0, "Non supported function.")
	}

	# errors array is empty
	call salloc (err, npar, TY_REAL)
	do i = 0, npar-1
	    Memr[err+i] = 0.0
end

