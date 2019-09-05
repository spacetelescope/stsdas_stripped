#* HISTORY *
#* B.Simon	15-Sep-92	extracted from asurv 1.2 bin() subroutine

# MAKEBIN -- Calculate bin dimensions from data for Schmitt regression

procedure makebin (x, y, ntot, nxbin, nybin, xsize, ysize, xorg, yorg)

double	x[ARB]		# i: independent variable array
double	y[ARB]		# i: dependent variable array
int	ntot		# i: number of data points
int	nxbin		# i: number of x bins
int	nybin		# i: number of y bins
double	xsize		# o: width of x bins
double	ysize		# o: width of y bins
double	xorg		# o: origin of x bins
double	yorg		# o: origin of y bins
#--
double	xhi, xlo, yhi, ylo

begin
	# Get the range of the data

	call alimd (x, ntot, xlo, xhi)
	call alimd (y, ntot, ylo, yhi)

	# Calculate size of each bin from data range and number of bins

	xsize = (xhi - xlo) / double (nxbin - 2)
	ysize = (yhi - ylo) / double (nybin - 2)

	# Calculate origin of bins

	xorg = xlo - 1.5 * xsize
	yorg = ylo - 1.5 * ysize
end
