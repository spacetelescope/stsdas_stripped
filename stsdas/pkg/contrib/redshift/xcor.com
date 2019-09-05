# Common task parameters for FQUOT

# Parameters that usually stay fixed

int	npts		# Number of points in the spectrum
int	order		# order of the polynomial divided out of the spectrum
real	han		# Fraction of spectrum windowed by a cosine bell
real	cpf		# Detector units per detected photon
int	lo		# Lowest frequency included in the fit
int	nrun		# Initial number of bins included
real	chi0		# Initialization of chi-square
real	unc		# Convergence criterion for chi-square minimization
int	niter		# Max iterations for chi-square minimization

common /fixp/ npts, order, han, cpf, lo, nrun, chi0, unc, niter

# Parameters that vary with each fit

char	specname[SZ_FNAME] # Name of current spectrum
real	z0		# Initial guess at the redshift
real	sig0,sig1,sig2,sig3	# Velocity dispersion conversion coefficients
real	gam0		# Initial guess at the line strength parameter
real	z[NPAR]		# Fitted values of the above
real	ze[NPAR]	# Fitted errors

common /fitp/ specname, z0, sig0, sig1, sig2, sig3, gam0, z, ze

# Parameters that describe the template stars

real	tempvel[MAXTEMPS]	# Observed velocities of each template
char	tempname[SZ_FNAME,MAXTEMPS]	# Names of the template stars
int	ntemp				# Total number of templates

common /ptemp/ ntemp, tempvel, tempname

#Parameters that describe the wavelength scale

real	wave0		# Wavelength of first point
real	logw0		# Its base 10 log
real	dlogw		# Spacing of each pixel in logarithmic space
real	deltav		# Corresponding velocity width

common /wvscl/ wave0, logw0, dlogw, deltav

# The answers

real	cz[MAXTEMPS], czerr[MAXTEMPS]
real	sig[MAXTEMPS], sigerr[MAXTEMPS]
real	gam[MAXTEMPS], gamerr[MAXTEMPS]
real	perdeg[MAXTEMPS]

common /answr/ cz, czerr, sig, sigerr, gam, gamerr, perdeg

# Parameters that govern user interaction

bool	pltspec
bool	pltwin
bool	pltfft
bool	pltfit
bool	debug
int	nlogfd
int	logfd[MAXLOG]

common /users/ pltspec, pltwin, pltfft, pltfit, debug, nlogfd, logfd
