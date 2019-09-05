#* HISTORY *
#* D.Ball	18-Apr-88	original
#* B.Simon	17-Sep-92	extractacted from twokm()

# TKMPRINT -- Print results of Schmitt's regression

procedure tkmprint (alpha, beta, asigma, bsigma, nxbin, nybin, xsize, ysize, 
		    xorg, yorg, nm, ntot, f, nboot, verbose)

double	alpha		# i: intercept coefficient
double	beta		# i: slope coefficient
double	asigma		# i: standard deviation of intercept 
double	bsigma		# i: standard deviation of slope
int	nxbin		# i: number of bins in independent variable
int	nybin		# i: number of bins in dependent variable
double	xsize		# i: size of bins in independent variable
double	ysize		# i: size of bins in dependent variable
double	xorg		# i: origin of bins in independent variable
double	yorg		# i: origin of bins in dependent variable
int	nm[8]		# i: number of limits changed to detections
int	ntot		# i: number of data values
double 	f[nxbin,nybin]	# i: estimated number of data points in bin (i,j)
int	nboot		# i: number of bootstrap runs
bool	verbose		# i: print full output?
#--
double	sum, xb, yb
int	i, j, mm
real	cfrac

string	changed  "%9w Number of censored points changed to detections \n"
string	strlower "%14w (from lower limits) \n"
string	strupper "%14w (from upper limits) \n"
string	cendetlow "%9w  Y only  X only    Both X lower/Y upper \n"
string	cendetupp "%9w  Y only  X only    Both X upper/Y lower \n"
string	warner1  "%9w WARNING: The censoring status of %4.1f%% of the points\n"
string	warner2  "%9w          have been changed \n\n"

double	round()

begin
	# Print messages about censored points changed to detections

	mm = 0
	do i = 1, 4
	    mm = mm + nm[i]

	if (mm != 0) {
	    call printf(changed)
	    call printf(strlower)
	    call printf(cendetlow)
	    call printf("%9w %7d %7d %7d %8d \n")
	    call pargi(nm[1])
	    call pargi(nm[2])
	    call pargi(nm[3])
	    call pargi(nm[4])
	}

	mm = 0
	do i = 5, 8
	    mm = mm + nm[i]

	if (mm != 0) {
	    call printf(changed)
	    call printf(strupper)
	    call printf(cendetupp)
	    call printf("%9w %7d %7d %7d %8d \n")
	    call pargi(nm[5])
	    call pargi(nm[6])
	    call pargi(nm[7])
	    call pargi(nm[8])
	}
	call printf("\n")

	mm = 0
	do i = 1, 8
	    mm = mm + nm[i]

	cfrac = 100.0 * real (mm) / real (ntot)
	if (cfrac >= 10.0) {
	    call printf (warner1)
	    call pargr (cfrac)

	    call printf (warner2)
	}

	# Print estimated number of points in each bin

	sum = 0.0
	if (verbose) {
	    call printf("%9w Final values of 2-D Kaplan-Meier Estimators \n")
	    call printf("%13w X AXIS %7w Y AXIS %8w Number of Points \n")
	}

	do j = 1, nybin {
	    do i = 1, nxbin {
		if (f[i,j] == 0.0)
		    next

		f[i,j] = f[i,j] * ntot
		sum = sum + f[i,j]

		if (verbose) {
		    xb = xorg + xsize / 2.0 + xsize * (i - 1)
		    yb = yorg + ysize / 2.0 + ysize * (j - 1)

		    call printf("%5w %14.3f %14.3f %14.3f \n")
		    call pargd(round (xb, 3))
		    call pargd(round (yb, 3))
		    call pargd(round (f[i,j], 3))
		}
	    }
	}

	if (verbose)
	    call printf("\n")

	# Print general information about parameters

	call printf("%9w Number of data points: %5d; sum of F: %12.3f\n")
	call pargi(ntot)
	call pargd(round (sum, 3))
	call printf("%9w (these should be nearly equal)\n")
	call printf("\n")


	Call printf("%9w Number of X bins:  %6d, number of Y bins:  %6d\n")
	call pargi(nxbin)
	call pargi(nybin)

	call printf("%9w X binsize     :  %8.3f, Y binsize     :  %8.3f\n")
	call pargd(xsize)
	call pargd(ysize)

	call printf("%9w X origin is at:  %8.3f, Y origin is at:  %8.3f\n")
	call pargd(xorg)
	call pargd(yorg)
	call printf("\n")

	# Print regression coefficents and their errors

	if (asigma < 0.0 || bsigma < 0.0) {
	    call printf("%9w Intercept coefficient:  %10.4f\n")
	    call pargd(round (alpha, 4))

	    call printf("%9w Slope coefficient:      %10.4f\n")
	    call pargd(round (beta, 4))

	} else {
	    call printf("%9w Intercept coefficient:  %10.4f +/- %8.4f \n")
	    call pargd(round (alpha, 4))
	    call pargd(round (asigma, 4))

	    call printf("%9w Slope coefficient:      %10.4f +/- %8.4f \n")
	    call pargd(round (beta, 4))
	    call pargd(round (bsigma, 4))

	    call printf ("%9w Bootstrap iterations: %4d")
	    call pargi (nboot)
	}

	call printf("\n")
end
