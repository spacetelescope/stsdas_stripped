# I2GAUSSFIT  -- Iterative 2-d Gaussian fit to images.
#
# Task n2gaussfit is called in a loop, fitting at each time one of
# each Gaussian parameter: (i) amplitude and background, (ii) center,
# and (iii) fwhm. The loop can be repeated 'niter' times. The last
# step is a simultaneous fit of all parameters. Error estimation is
# turned on only at this last step.
#
#						Ivo Busko  7/90

procedure i2gaussfit (input, output, niter, errors, verbose)

char	input, output
int	niter
bool	errors

begin
	file	in, out, temp, tabinsave
	int	nit, prow, rowsave
	char	method
	bool	rt, rtsave, errsave, flag, errfl
	bool	versave, aversave, verb
	bool	vellsave, vtetasave

	# Save called tasks parameters
	rtsave    = n2gaussfit.rt
	tabinsave = n2gaussfit.tablein
	rowsave   = n2gaussfit.row
	errsave   = errorpars.resample
	versave   = n2gaussfit.verbose
	aversave  = n2gaussfit.averbose
	vellsave  = tgausspars.vellip
	vtetasave = tgausspars.vtheta
	method    = controlpars.method

	# Read task parameters.
	in    = input
	out   = output
	nit   = niter
	errfl = errors
	verb  = verbose

	temp = mktemp ("i2g")
	prow = 0
	flag = yes
	errorpars.resample  = no
	n2gaussfit.verbose  = verb
	n2gaussfit.averbose = no
	tgausspars.vellip   = no
	tgausspars.vtheta    = no
	controlpars.method  = "amoeba"

	# Do it

	for (i = 1 ; i <= nit ; i = i + 1) {
	    tgausspars.va     = yes
	    tgausspars.vampl  = yes
	    tgausspars.vxcent = no
	    tgausspars.vycent = no
	    tgausspars.vfwhm  = no
	    if (!flag)
	        n2gaussfit.row = prow
	    n2gaussfit (in, temp)
	    prow = prow + 1
	    if (flag) {
	        n2gaussfit.rt = yes
	        n2gaussfit.tablein = temp
	        flag = no
	    }
	    tgausspars.va     = no
	    tgausspars.vampl  = no
	    tgausspars.vxcent = yes
	    tgausspars.vycent = yes
	    tgausspars.vfwhm  = no
	    n2gaussfit.row    = prow
	    n2gaussfit (in, temp)
	    prow = prow + 1

	    tgausspars.va     = no
	    tgausspars.vampl  = no
	    tgausspars.vxcent = no
	    tgausspars.vycent = no
	    tgausspars.vfwhm  = yes
	    n2gaussfit.row = prow
	    n2gaussfit (in, temp)
	    prow = prow + 1
	}
	tgausspars.va      = yes
	tgausspars.vampl   = yes
	tgausspars.vxcent  = yes
	tgausspars.vycent  = yes
	tgausspars.vfwhm   = yes
	errorpars.resample = errfl
	n2gaussfit.row = prow
	n2gaussfit (in, out)

	# Restore called tasks parameters
	n2gaussfit.rt       = rtsave
	n2gaussfit.tablein  = tabinsave
	n2gaussfit.row      = rowsave
	n2gaussfit.verbose  = versave
	n2gaussfit.averbose = aversave
	tgausspars.vellip   = vellsave
	tgausspars.vtheta    = vtetasave
	errorpars.resample  = errsave
	controlpars.method  = method

	delete (temp // ".tab", verify = no)
end
