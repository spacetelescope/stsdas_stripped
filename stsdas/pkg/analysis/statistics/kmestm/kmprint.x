define		DECPLACE		3

#* HISTORY *
#* D.Ball	18-Apr-88	from the ASURV routine of the same name
#* B.Simon	14-Aug-92	revised to use asurv 1.2
#* H.Bushouse	14-Oct-99	fixed reentrant printf error for warn1, warn2

# KMPRINT -- Print tke KM estimator, percentiles, mean and error

procedure kmprint (verbose, diff, isign, ichange, smean, error, 
		   zu, zc, ntot, ntemp, iu, ic, sx, vx, nbin, bs, bl, dx)

bool	verbose		# i: print out long form of output?
bool	diff		# i: print differential km estimator?
int	isign		# i: indicator of upper/lower limit
int	ichange		# i: indicates whether last point is censored
double	smean		# i: estimated mean
double	error		# i: error in estimated mean
double	zu[ntot]	# i: uncensored data points
double	zc[ntot]	# i: censored data points
int	ntot		# i: total number of points, = iu + ic
int	ntemp		# i: number of points after removing ties
int	iu		# i: number of uncensored data points
int	ic		# i: number of censored data points
double	sx[ntot]	# i: pl estimator
double	vx[ntot]	# i: error of pl estimator
int	nbin		# i: number of bins
double	bs[nbin]	# i: bin starting values
double	bl[nbin]	# i: bin ending values
double	dx[nbin]	# i: differential km estimator
#--
double 	zlim, sum, fint[3]
int	kt, ku, kc, i, icase

string	title1 "%4w # of Data Points :%6d # of %s Limits :%6d \n"
string	title2 "%9w Variable Range      KM Estimator   Error \n"
string  warn1  "%5w Warning: The %s point was changed to a detection \n"
string	warn2  "%5w      for the K-M computation.\n"
string	nopc1  "%5w Since there are less than 4 uncensored points \n"
string	nopc2  "%5w no percentiles were computed.\n"
string	bias1  "%5w Since a censored point was changed to a detection \n"
string	bias2  "%5w the mean estimate is biased.\n"

double	round()

begin
	# Print number of censored points

	call printf (title1)
	call pargi (ntot)
	if (isign == 1) {
	    call pargstr ("Lower")
	} else {
	    call pargstr ("Upper")
	}
	call pargi (ic)

	if (diff) {
	    call printf ("\n")
	    call printf ("%7w Differential KM Estimator \n")
	    call printf ("%7w Bin Center          D \n\n")

	    sum = 0.0
	    do i = 1, nbin {
		sum = sum + ntot * dx[i]

		call printf ("  %13.3f  %13.3f \n")
		call pargd (round (0.5 * (bs[i] + bl[i]), DECPLACE))
		call pargd (round (ntot * dx[i], DECPLACE))
	    }

	    call printf ("%23w-------\n%9w Sum =  %13.3f \n")
	    call pargd (round (sum, DECPLACE))

	    call printf ("\n")
	    call printf ("(D gives the estimated data points in each bin) \n")
	}

	call printf ("\n")

	if (verbose) {
	    # Print the estimator and its error

	    call printf (title2)

	    call printf ("From%9.3f   to%9.3f%12.3f \n")
	    call pargr (0.0)
	    call pargd (round (zu[1], DECPLACE))
	    call pargr (1.0)
	    
	    ku = 1
	    kc = 1
	    for (kt=1; kt <= ntemp; kt = kt + 1) {
		icase = 1
		if (kc > ic)
		    icase = 2
		else if (zu[ku] < zc[kc])
		    icase = 2
		
		switch (icase) {
		case 1:
		    call printf ("%13.3f C \n")
		    call pargd (round (zc[kc], DECPLACE))
		    kc = kc + 1
		    
		case 2:
		    if (ku < iu) {
			call printf ("From%9.3f   to%9.3f%12.3f%12.3f \n")
			call pargd (round (zu[ku], DECPLACE))
			call pargd (round (zu[ku+1], DECPLACE))
			call pargd (round (sx[ku], DECPLACE))
			call pargd (round (vx[ku], DECPLACE))
		    } else {
			call printf ("From%9.3f   onwards    %12.3f%12.3f \n")
			call pargd (round (zu[ku], DECPLACE))
			call pargd (round (sx[ku], DECPLACE))
			call pargd (round (vx[ku], DECPLACE))
		    }
		    ku = ku + 1
		}
	    }
	    
	    # Print a warning message if the last point was changed to
	    # an uncensored point for the K-M calculation

	    if (ichange == -1) {
		call printf (warn1)
		if (isign > 0) {
		    call pargstr ("last")
		} else {
		    call pargstr ("first")
		}
		call printf (warn2)
	    }

	    # Compute and print the percentiles

	    call printf ("\n")
	    if (iu <= 3) {
		call printf (nopc1)
		call printf (nopc2)

	    } else {
		call sumry (zu, iu, sx, ntot, fint)

		call printf ("        Percentiles \n")
		call printf ("         75 TH     50 TH     25 TH \n")
		call printf ("    %10.3f%10.3f%10.3f \n")
		do i= 1, 3
		    call pargd (round (fint[i], DECPLACE))
	    }
	    call printf ("\n")
	}

 	# Print the mean and its error

	if (ichange == -1) {
	    if (isign == 1) {
		zlim = zu[iu]
	    } else {
		zlim = zu[1]
	    }

	    call printf ("Mean=%10.3f +/- %6.3f  limited to %10.3f \n")
	    call pargd (round (smean, DECPLACE))
	    call pargd (round (error, DECPLACE))
	    call pargd (round (zlim, DECPLACE))

	    call printf (bias1)
	    call printf (bias2)

	} else {
	    call printf ("%7w Mean=%10.3f +/- %6.3f \n")
	    call pargd (round (smean, DECPLACE))
	    call pargd (round (error, DECPLACE))
	}

end
