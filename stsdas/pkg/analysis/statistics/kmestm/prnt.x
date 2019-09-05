# PRNT -- Subroutine prnt adapted from the ASURV routine of the same name
# This routine prints the pl estimators and their errors, and
# the 75, 50, and 25 percentiles

procedure prnt (zu, zc, ntot, iu, ic, s, v, isign)

# inputs --
double	zu[ntot]	# uncensored data points
double	zc[ntot]	# censored data points
int	ntot		# total number of points, = iu + ic
int	iu		# number of uncensored data points
int	ic		# number of censored data points
double	s[ntot]		# pl estimator
double	v[ntot]		# error of pl estimator
int	isign		# indicator of upper/lower limit,
			#  if 1, lower limit
			#    -1, upper limit
# local --
double fint[3]		# 75, 50, and 25 percentiles
double	zzu		# uncensored value ready for printing
double	zzc		# censored value ready for printing
int	kt		# counter of total number
int	ku		# counter of uncensored data
int	kc		# counter of censored data
int	j		# loop index
int	icase		# controls printing of uncensored or censored data

begin

	call printf ("        Variable      PL Estimator       Error\n")

	# Loop over all data points, printing in ascending order
	# interspersing censored and uncensored data accordingly
	ku = 1
	kc = 1
	for (kt=1; kt <= ntot; kt = kt + 1) {
		icase = 1
		if (kc > ic)
			icase = 2
		else if (zu[ku] < zc[kc])
			icase = 2

		switch (icase) {

		case 1:
			zzc = isign*zc[kc]
			call printf ("%15.3f C \n")
			call pargd (zzc)
			kc = kc + 1
			next

		case 2:
			zzu = isign*zu[ku]
			call printf ("%15.3f%15.3f%15.3f \n")
			call pargd (zzu)
			call pargd (s[ku])
			call pargd (v[ku])
			ku = ku + 1
			next
		}
	}

	# compute the percentiles
	call sumry (zu, iu, s, ntot, fint)
	do j = 1,3 
		fint[j] = isign*fint[j]
	call printf("\n")
	call printf("    Percentiles    \n")
	call printf("    75 TH     50 TH     25 TH\n")
	call printf("%10.3f%10.3f%10.3f \n")
	do j=1,3
		call pargd(fint[j])
	call printf ("\n")

end
