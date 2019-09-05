# Number of target acquisition diodes.
define	N_DIODES	8

#---------------------------------------------------------------------------
.help t_tacount Jul93 source
.ih
NAME
t_tacount -- Calculate observed count rate given predicted count rate.
.ih
DESCRIPTION
The Goddard High Resolution Spectrograph (GHRS) uses an array of
Reticon Diode detectors to count photons of light entering one of the
apertures.  One a photon strikes a diode, it is counted in the
electronics.  This takes a short time.  Within this interval, the
diode+electronics are unable to count photons.  For low count rates,
i.e. less than 2000 counts/diode/second, the error rate is less than
1%.  The error is less than 10% for rates up to about 20,000 and then
increases exponentially.  The detectors saturate at a count rate of
about 100,000.

The equation relating true count rate to observed count rate is as
follows:

.nf
	y = (1.0 - e**(-tx)) / t

where
	x = True count rate
	t = Dead time constant ~ 10.2x10e-6 seconds.
	y = Observed count rate
.fi

This task is meant to take, as input, a predicted (true) count rate,
correct for the PSF and the paired-pulse, and output what would be the
observed count rate.
.ih
REFERENCES
Ball Aerospace, "GHRS Science Verification Program for the Hubble Space
Telescope", Dannis Ebbets, ed., 24February1992, under NASA contracts
NAS5-26000 and NAS5-30433.
.endhelp
#---------------------------------------------------------------------------
procedure t_tacount

bool	clgetb()		# Get boolean parameter.
double	clgetd()		# Get double parameter.
int	i			# Generic.
double	obs_rate		# Observed count rate.
double	pr			# Predicted count rate.
pointer	rate			# Observed count rate per diode.
pointer	sp			# STack pointer.
pointer	sx			# Generic string.
double	t			# Time constant.
bool	verbose			# TRUE to write to standard output.

begin
	call smark(sp)
	call salloc (rate, N_DIODES, TY_DOUBLE)
	call salloc (sx, SZ_LINE, TY_CHAR)

	# Get the predicted count rate.
	pr = clgetd ("input")

	verbose = clgetb ("verbose")
	
	# Get time constant in microseconds and convert to seconds.
	t = 1.0d-6 * clgetd ("dead_time")
	
	# Read in the psf and calculate the individual rates.
	obs_rate = 0.d0
	do i = 1, N_DIODES {
	    call sprintf (Memc[sx], SZ_LINE, "psf%d")
	    call pargi (i)

	    Memd[rate+i-1] = (1.d0 - exp (-t * clgetd (Memc[sx]) * pr)) / t
	    obs_rate = obs_rate + Memd[rate+i-1]
	}

	# Fill output parameters and optionally write values to standard out.
	call clputd ("obs_rate", obs_rate)
	if (verbose) {
	    call printf ("# For predicted count rate %.4g, observed count rate is %.4g\n")
	    call pargd (pr)
	    call pargd (obs_rate)
	    call printf ("# Count rate for each diode is:\n")
	    call printf ("# Diode   Rate\n")
	}

 	do i = 1, N_DIODES {
	    call sprintf (Memc[sx], SZ_LINE, "diode%d")
	    call pargi (i)

	    call clputd (Memc[sx], Memd[rate+i-1])

	    if (verbose) {
		call printf ("%4.4d    %g\n")
		call pargi (i)
		call pargd (Memd[rate+i-1])
	    }
	}

	# That's all folks.
	call sfree(sp)
end
#---------------------------------------------------------------------------
# End of t_tacount
#---------------------------------------------------------------------------
