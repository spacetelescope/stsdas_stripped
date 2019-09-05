include <imhdr.h>
include <gset.h>
include <error.h>		# for EA_ERROR
include <time.h>		# for SZ_TIME
include <tbset.h>

define	BW_TYPE_IMAGE	1	# type of input or output
define	BW_TYPE_TABLE	2

define	BW_NPAR		4	# number of parameters to fit to power spectrum
define	SIGNAL_A	$1[1]	# a + b*x + c*x**2
define	SIGNAL_B	$1[2]	# actually, this is zero
define	SIGNAL_C	$1[3]
define	NOISE		$1[4]

# t_bwfilter -- Fourier filter of a 1-D array
# See Brault & White, 1971, Astron & Astrophys, vol 13, p 169.
# This task filters a 1-D array in the following way.  After subtracting
# a background from the input, the forward Fourier transform is taken.
# The data in the transform domain are multiplied by a filter of the form:
# signal / (signal + noise).  The inverse Fourier transform is then taken,
# the background is added back in, and the result is written to the output.
#
# The input and output may be either an image or a table column, or one of
# each.  For either the 'input' or 'output' parameter, if the value is a
# single word it is assumed to be an image name, but if it is two words
# separated by one or more spaces it is assumed to be a table name and a
# column name.  If the output table already exists it will be written to
# in-place; if the column exists it will be overwritten.
#
# The background that is subtracted before taking the Fourier transform
# is obtained by fitting a straight line.  The linear fit is done to the
# log of the data if all values are positive; otherwise, the fit is done to
# the data values directly.
#
# The signal and noise are estimated as follows.  The task fits a constant
# (the noise level) to the higher frequencies, and it fits a quadratic
# (the signal) to the log power spectrum at the lower frequencies.  Then
# the filter in the Fourier domain is signal / (signal + noise).
#
# Phil Hodge, 19-Aug-1994  Task created.
# Phil Hodge, 12-Jan-1995  If SIGNAL_C(p) >= 0, change sign and continue
#			rather than stopping with an error.

procedure t_bwfilter()

pointer input, output	# scratch for names of input & output images
real	freq		# frequency where signal is comparable to noise
bool	interactive	# true if interactive mode
pointer device		# graphics device (e.g. "stdgraph")
#--
pointer sp
pointer history		# for history record for output image
pointer colname		# column name for history record
pointer oim		# pointer to struct for output image or table
pointer ocp		# pointer to column descriptor if output is table
pointer ix, ox		# pointers to data for input, output
int	npix		# size of array
int	outtype		# type of output (image or table)
real	clgetr()
bool	clgetb()

pointer datetime	# for getting date & time
long	old_time, now
long	clktime()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (history, SZ_LINE, TY_CHAR)
	call salloc (datetime, SZ_TIME, TY_CHAR)
	call salloc (colname, SZ_COLNAME, TY_CHAR)

	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	interactive = clgetb ("interactive")
	if (interactive) {
	    freq = 0.25			# default
	} else {
	    freq = clgetr ("freq")
	    if (freq <= 0.)
		call error (1, "frequency must be positive")
	}
	call clgstr ("device", Memc[device], SZ_FNAME)

	# Open input and output, and get data.
	call bw_get_io (Memc[input], Memc[output], oim, ocp, ix, ox,
		npix, outtype)

	# Apply the filter
	call bwfilter (Memr[ix], Memr[ox], npix,
			interactive, "cursor", Memc[device], freq)

	# If the output is a table, we must copy the data to it.
	if (outtype == BW_TYPE_TABLE)
	    call tbcptr (oim, ocp, Memr[ox], 1, npix)

	# Add two history records to output.
	now = clktime (old_time)
	call cnvtime (now, Memc[datetime], SZ_TIME)
	call sprintf (Memc[history], SZ_LINE, "Filtered by bwfilter on %s;")
	    call pargstr (Memc[datetime])
	if (outtype == BW_TYPE_IMAGE)
	    call imputh (oim, "history", Memc[history])
	else
	    call tbhadt (oim, "history", Memc[history])

	call sprintf (Memc[history], SZ_LINE,
		"  cutoff frequency = %.5g cycles per pixel")
	    call pargr (freq)
	if (outtype == BW_TYPE_IMAGE) {
	    call strcat (".", Memc[history], SZ_LINE)
	    call imputh (oim, "history", Memc[history])
	} else {
	    call strcat (";", Memc[history], SZ_LINE)
	    call tbhadt (oim, "history", Memc[history])
	}

	if (outtype == BW_TYPE_TABLE) {
	    call tbcigt (ocp, TBL_COL_NAME, Memc[colname], SZ_COLNAME)
	    call sprintf (Memc[history], SZ_LINE,
		"  column name for filtered data is %s.")
		call pargstr (Memc[colname])
	    call tbhadt (oim, "history", Memc[history])
	}

	# Close output, free memory.
	if (outtype == BW_TYPE_IMAGE) {
	    call imunmap (oim)
	} else {
	    call tbtclo (oim)
	    call mfree (ox, TY_REAL)
	}
	call mfree (ix, TY_REAL)

	call sfree (sp)
end

# bw_get_io -- get input data and space for output
# The input and output may be either an image or a table, or one of each.
# For either the input or output, if the value is a single word it is
# assumed to be an image name, but if it is two words it is assumed to be
# a table name and a column name.  If the output table already exists it
# will be written to in-place; if the column exists it will be overwritten.
#
# If the output is an image:
#    write the data to Memr[ox], and call imunmap (oim).
#
# If the output is a table:
#    write the data to Memr[ox], call tbcptr to write it to the table,
#    call tbtclo (oim), and call mfree (ox, TY_REAL).
#
# In either case call mfree (ix, TY_REAL).

procedure bw_get_io (input, output, oim, ocp, ix, ox, npix, outtype)

char	input[ARB]	# i: name of input
char	output[ARB]	# i: name of output
pointer oim		# o: pointer to structure describing output
pointer ocp		# o: pointer to output column descriptor
pointer ix		# o: pointer to input data
pointer ox		# o: pointer to space for output data
int	npix		# o: size of space pointed to by ix and ox
int	outtype		# o: type of output (image or table)
#--
pointer sp
pointer fname		# scratch for name of image or table
pointer iim		# pointer to input image or table structure
pointer iscr		# for temporary storage of input image data
pointer icp		# pointer to input column descriptor
pointer colname		# scratch for table column name
pointer nullflag	# array of null flags from table column
int	intype		# type of input (image or table)
int	ip, ctowrd()
pointer immap(), imgl1r(), impl1r()
pointer tbtopn()
int	tbpsta(), tbtacc()
errchk	immap, tbtopn, tbtcre

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (colname, SZ_FNAME, TY_CHAR)

	# Open the input image or table.
	ip = 1
	if (ctowrd (input, ip, Memc[fname], SZ_FNAME) < 1)
	    call error (1, "no input specified")

	if (ctowrd (input, ip, Memc[colname], SZ_FNAME) < 1) {

	    # Only one "word"; this must be an image name.
	    intype = BW_TYPE_IMAGE

	    iim = immap (Memc[fname], READ_ONLY, NULL)
	    npix = IM_LEN(iim,1)
	    if (IM_NDIM(iim) != 1) {
		call imunmap (iim)
		call error (1, "bwfilter:  image must be 1-D")
	    }
	    iscr = imgl1r (iim)

	    # Allocate memory for input data, and copy image data to it.
	    call malloc (ix, npix, TY_REAL)
	    call amovr (Memr[iscr], Memr[ix], npix)

	} else {

	    # Two "words"; these must be table and column names.
	    intype = BW_TYPE_TABLE

	    iim = tbtopn (Memc[fname], READ_ONLY, NULL)
	    npix = tbpsta (iim, TBL_NROWS)
	    call tbcfnd (iim, Memc[colname], icp, 1)	# find column name

	    # Allocate memory for input data, and read it from the table.
	    call malloc (ix, npix, TY_REAL)
	    call salloc (nullflag, npix, TY_BOOL)
	    call tbcgtr (iim, icp, Memr[ix], Memb[nullflag], 1, npix)
	}

	# Open the output image or table.
	ip = 1
	if (ctowrd (output, ip, Memc[fname], SZ_FNAME) < 1)
	    call error (1, "no output specified")

	if (ctowrd (output, ip, Memc[colname], SZ_FNAME) < 1) {

	    # Only one "word"; this must be an image name.
	    outtype = BW_TYPE_IMAGE

	    if (intype == BW_TYPE_IMAGE) {

		oim = immap (Memc[fname], NEW_COPY, iim)
		IM_PIXTYPE(oim) = TY_REAL
		ox = impl1r (oim)

		call imunmap (iim)		# we're done with input image

	    } else {

		call tbtclo (iim)		# we're done with input table

		# We can't make a NEW_COPY image from a table.
		oim = immap (Memc[fname], NEW_FILE, NULL)
		IM_NDIM(oim) = 1
		IM_LEN(oim,1) = npix
		IM_PIXTYPE(oim) = TY_REAL
		ox = impl1r (oim)
	    }

	} else {

	    # Two "words"; these must be table and column names.
	    outtype = BW_TYPE_TABLE

	    # We're done with the input, so close it.
	    if (intype == BW_TYPE_TABLE)
		call tbtclo (iim)
	    else
		call imunmap (iim)

	    if (tbtacc (Memc[fname]) == YES) {

		# Open an existing table.  If the column already exists,
		# overwrite it; otherwise, create it.
		oim = tbtopn (Memc[fname], READ_WRITE, NULL)
		call tbcfnd (oim, Memc[colname], ocp, 1)
		if (ocp == NULL)			# not found; create it
		    call tbcdef (oim, ocp, Memc[colname], "", "", TY_REAL, 1, 1)

	    } else {

		# Create a new table.
		oim = tbtopn (Memc[fname], NEW_FILE, NULL)
		call tbcdef (oim, ocp, Memc[colname], "", "", TY_REAL, 1, 1)
		call tbtcre (oim)
	    }

	    # Allocate memory for output data.
	    call malloc (ox, npix, TY_REAL)
	}

	call sfree (sp)
end

# bwfilter -- Brault & White optimal filter

procedure bwfilter (inr, outr, npix, interactive, cursor, device, freq)

real	inr[npix]	# i: input data
real	outr[npix]	# o: output data
int	npix		# i: size of arrays
bool	interactive	# i: true if interactive mode
char	cursor[ARB]	# i: graphics cursor parameter
char	device[ARB]	# i: graphics device
real	freq		# io: frequency where signal is comparable to noise
#--
pointer sp
pointer xdata		# complex array containing data or Fourier transform
pointer bkg		# complex array containing background data
pointer pspec		# real array containing log power spectrum
real	p[BW_NPAR]	# parameters describing fit to power spectrum
real	cd1_1		# frequency interval per pixel
real	ampl		# Y value of cursor position (ignored)
int	half		# size of power spectrum
bool	append		# append to current plot?
errchk	bw_forward, bw_inverse, bw_plot, bw_clgcur,
	bw_get_filt, bw_apply, bw_pspec

begin
	call smark (sp)

	if (freq <= 0. && !interactive)
	    call error (1, "frequency must be positive")

	# Subtract one because we ignore zero frequency for the power spectrum.
	half = (npix + 1) / 2 - 1

	# Pixel spacing in the Fourier domain.  Just pixel coordinates.
	cd1_1 = 1. / real(npix)

	# Allocate space for the power spectrum.
	call salloc (pspec, half, TY_REAL)

	# Read the input data, and do the forward transform, putting
	# the result in Memx[xdata].
	# Also subtract a background, which is saved in Memr[bkg].
	call bw_forward (inr, xdata, bkg, npix)

	# Compute the log power spectrum.
	call bw_pspec (Memx[xdata], Memr[pspec], half)

	if (interactive) {
	    # Get frequency interactively.
	    append = false
	    call bw_plot (Memr[pspec], half, device, append, cd1_1)
	    call bw_clgcur (cursor, freq, ampl)
	}

	# Find the filter by fitting to the power spectrum.
	call bw_get_filt (Memr[pspec], half, freq, p, cd1_1)

	# Apply the filter to xdata.
	call bw_apply (Memx[xdata], npix, p)

	if (interactive) {
	    # Compute and plot the power spectrum of the filtered xdata.
	    call bw_pspec (Memx[xdata], Memr[pspec], half)
	    append = true
	    call bw_plot (Memr[pspec], half, device, append, cd1_1)
	}

	# Take the inverse transform, add the background back in,
	# and write the result to the output image.
	call bw_inverse (Memx[xdata], Memr[bkg], outr, npix)

	# Deallocate memory allocated by bw_forward.
	call mfree (bkg, TY_REAL)
	call mfree (xdata, TY_COMPLEX)

	call sfree (sp)
end

# bw_forward -- take forward Fourier transform
# Note that this routine also gets the "ftpairs" cl parameter and
# reads that file.

procedure bw_forward (inr, xdata, bkg, npix)

real	inr[npix]	# i: input array
pointer xdata		# o: pointer to input data (minus background)
pointer bkg		# o: pointer to background subtracted from input
int	npix		# i: size of arrays
#--
complex z		# for normalizing the forward transform
bool	fwd		# forward transform?  (true)
errchk	bw_sub_bkg

begin
	fwd = true

	call malloc (xdata, npix, TY_COMPLEX)
	call malloc (bkg, npix, TY_REAL)

	# Fit and subtract a background.
	call bw_sub_bkg (inr, Memx[xdata], Memr[bkg], npix)

	# Do the forward Fourier transform in-place in the complex array.
	call bw_cmplx (Memx[xdata], npix, fwd)
	z = cmplx (npix, 0)
	call adivkx (Memx[xdata], z, Memx[xdata], npix)	# normalize
end

# bw_inverse -- take inverse Fourier transform

procedure bw_inverse (xdata, bkg, outr, npix)

complex xdata[npix]	# i: input data (minus background)
real	bkg[npix]	# i: background to be added to output
real	outr[npix]	# o: output array
int	npix		# i: size of arrays
#--
int	i
bool	fwd		# forward transform?  (false)

begin
	fwd = false

	# Do the inverse Fourier transform in-place in the complex array.
	call bw_cmplx (xdata, npix, fwd)

	# Add the background back in, converting to real.
	do i = 1, npix
	    outr[i] = real (xdata[i]) + bkg[i]
end

# bw_sub_bkg -- subtract background

# Subtract a straight line.  If all the data are positive, we'll fit
# to the log of the data; otherwise, we'll fit to the data directly.

procedure bw_sub_bkg (inr, xdata, bkg, npix)

real	inr[npix]	# i: input data
complex xdata[npix]	# o: data with background subtracted
real	bkg[npix]	# o: background that was subtracted
int	npix		# i: size of arrays
#--
pointer sp
pointer scr		# either a copy of inr or log(inr)
real	minval, maxval	# min and max of data values
real	x, y		# i, inr[i]
real	sumx, sumxx, sumy, sumxy	# for fitting
real	d		# for fitting
real	a, b		# coefficients of fit:  a + b*i
int	i		# loop index
bool	loglinear	# true if we fit to the log of the data

begin
	call smark (sp)
	call salloc (scr, npix, TY_REAL)

	call alimr (inr, npix, minval, maxval)
	# If all the values are greater than zero, take the log before
	# fitting a line; otherwise, just copy.
	loglinear = (minval > 0.)
	if (loglinear) {
	    do i = 1, npix
		Memr[scr+i-1] = log10 (inr[i])
	} else {
	    do i = 1, npix
		Memr[scr+i-1] = inr[i]
	}

	# Fit a straight line.
	sumx = 0.
	sumxx = 0.
	sumy = 0.
	sumxy = 0.
	do i = 1, npix {
	    x = i
	    y = Memr[scr+i-1]
	    sumx  = sumx  + x
	    sumxx = sumxx + x * x
	    sumy  = sumy  + y
	    sumxy = sumxy + x * y
	}

	x = sumx / npix
	y = sumy / npix
	d = sumxx - sumx**2 / npix
	if (d == 0.) {
	    call eprintf (
		"warning:  can only fit constant background\n")
	    b = 0.
	    a = y
	} else {
	    b = (sumxy - x * y * npix) / d
	    a = y - x * b
	}

	# Subtract the background, and convert to complex.
	do i = 1, npix {
	    bkg[i] = a + b * i
	    if (loglinear)
		bkg[i] = 10. ** bkg[i]
	    xdata[i] = cmplx (inr[i] - bkg[i], 0.)
	}

	call sfree (sp)
end

# bw_plot -- plot data

procedure bw_plot (pspec, half, device, append, cd1_1)

real	pspec[half]	# i: array to plot
int	half		# i: size of array
char	device[ARB]	# i: graphics device
bool	append		# i: true if we should append to current plot
real	cd1_1		# i: frequency interval per pixel
#--
pointer sp
pointer title		# title for plot
pointer gp
pointer x		# scratch
real	xmin, xmax, ymin, ymax
int	i
pointer gopen()
errchk	gopen, gswind, glabax, gpline, gclose

begin
	call smark (sp)
	call salloc (x, half, TY_REAL)
	call salloc (title, SZ_LINE, TY_CHAR)

	# Assign the X values.  (Assumes zero frequency is NOT included.)
	do i = 1, half
	    Memr[x+i-1] = real(i) * cd1_1

	if (append) {

	    gp = gopen (device, APPEND, STDGRAPH)
	    call gseti (gp, G_PLTYPE, GL_DASHED)
	    call gpline (gp, Memr[x], pspec, half)
	    call gclose (gp)

	} else {

	    call sprintf (Memc[title], SZ_LINE, "Power Spectrum")

	    # Set the range of X values for the plot.
	    xmin = -Memr[x+half-1] * 0.05
	    xmax = Memr[x+half-1] * 1.05

	    # Get the range of Y values for the plot.
	    call alimr (pspec, half, ymin, ymax)
	    ymin = ymin - (ymax - ymin) / 20.	# extend the range
	    ymax = ymax + (ymax - ymin) / 20.

	    gp = gopen (device, NEW_FILE, STDGRAPH)
	    call gswind (gp, xmin, xmax, ymin, ymax)
	    call gseti (gp, G_XNMINOR, 5)
	    call glabax (gp, Memc[title], "Frequency", "Log Power")
	    call gpline (gp, Memr[x], pspec, half)
	    call gclose (gp)
	}
	call sfree (sp)
end

# bw_clgcur -- read cursor

procedure bw_clgcur (param, freq, ampl)

char	param[ARB]	# i: name of cursor parameter to get
real	freq		# o: frequency where signal is comparable to noise
real	ampl		# o: amplitude where signal is comparable to noise
#--
pointer sp
pointer strval		# string value (if any) returned by clgcur
real	xc, yc		# cursor position
int	wcs, key	# wcs for coords; keystroke key
int	clgcur()

begin
	call smark (sp)
	call salloc (strval, SZ_FNAME, TY_CHAR)

	# Read cursor position.
	if (clgcur (param, xc, yc, wcs, key, Memc[strval],
		SZ_FNAME) == EOF) {
	    call eprintf ("warning:  no cursor position read\n")
	} else {
	    freq = xc
	    ampl = yc
	    if (freq <= 0.)
		call error (1, "frequency must be positive")
	}

	call sfree (sp)
end

# bw_get_filt -- fit to the power spectrum

procedure bw_get_filt (pspec, half, freq, p, cd1_1)

real	pspec[half]	# i: log power spectrum
int	half		# i: size of array
real	freq		# i: frequency where signal is comparable to noise
real	p[ARB]		# o: coefficients of fit
real	cd1_1		# i: frequency interval per pixel
#--
real	x, y, w		# a point and weight
real	sumw, sumx2, sumx4, sumy, sumx2y
int	n
int	i
int	fpix		# pixel corresponding to freq
int	first		# beginning of loop for noise

begin
	fpix = nint (freq / cd1_1) + 1

	if (fpix < 1 || fpix > half)
	    call error (1, "frequency is out of range")

	# Average the noise.
	n = 0
	sumw = 0.
	sumy = 0.
	first = fpix + (half - fpix) / 5
	if (half - first < max (10, half/10))
	    first = fpix + 1
	do i = fpix+1, half {
	    w = 1.
#***	    w = real (i - fpix) / real (half - fpix)
	    y = pspec[i]
	    sumw  = sumw  + w
	    sumy  = sumy  + w * y
	    n = n + 1
	}
	if (n < 1 || sumw == 0.)
	    call error (1, "not enough data at higher frequencies")

	y = sumy / sumw
	NOISE(p) = y

	# Average the signal.
	n = 0
	sumw = 0.
	sumy = 0.
	sumx2 = 0.
	sumx4 = 0.
	sumx2y = 0.
	do i = 1, fpix-1 {
	    w = real (fpix - i) / real (fpix - 1)
	    # Add 1 to offset for zero freq, since pspec doesn't include it.
	    x = i + 1
	    y = pspec[i]
	    sumw   = sumw   + w
	    sumx2  = sumx2  + w * x * x
	    sumx4  = sumx4  + w * x ** 4
	    sumy   = sumy   + w * y
	    sumx2y = sumx2y + w * x * x * y
	    n = n + 1
	}
	if (n < 1 || sumw == 0.)
	    call error (1, "not enough data near zero frequency")

	SIGNAL_A(p) = (sumy * sumx4 - sumx2y * sumx2) /
		      (sumw * sumx4 - sumx2 ** 2)

	SIGNAL_B(p) = 0.

	SIGNAL_C(p) = (sumy * sumx2 - sumx2y * sumw) /
		      (sumx2 ** 2 - sumx4 * sumw)

	# If the signal is concave upward, simply change the sign.
	if (SIGNAL_C(p) >= 0.) {
	    call eprintf (
	"warning:  quadratic fit to low frequencies is concave upward\n")
	    SIGNAL_C(p) = -SIGNAL_C(p)
	}
end

# bw_apply -- apply the filter

procedure bw_apply (xdata, npix, p)

complex xdata[npix]	# io: Fourier transform of image
int	npix		# i: size of array
real	p[ARB]		# i: coefficients of fit
#--
real	filt		# filter value at a pixel
real	signal, noise	# for making filter
int	i

begin
	noise = 10. ** NOISE(p)

	do i = 1, npix/2 {
	    signal = SIGNAL_A(p) + SIGNAL_C(p) * i*i
#***	    signal = SIGNAL_A(p) + SIGNAL_B(p) * i + SIGNAL_C(p) * i*i
	    signal = 10. ** signal
	    filt = signal / (signal + noise)
	    xdata[i] = xdata[i] * filt
	    xdata[npix-i+1] = xdata[npix-i+1] * filt
	}

	i = (npix + 1) / 2
	if (i > npix/2) {				# odd number of pixels
	    signal = SIGNAL_A(p) + SIGNAL_C(p) * i*i
	    signal = 10. ** signal
	    filt = signal / (signal + noise)
	    xdata[i] = xdata[i] * filt
	}
end

# bw_pspec -- compute power spectrum

procedure bw_pspec (xdata, pspec, half)

complex xdata[ARB]	# i: input data
real	pspec[half]	# o: log power spectrum of input
int	half		# i: size of pspec array
#--
real	x, y		# real and imaginary parts
real	minps, maxps	# min & max of power spectrum
int	i

begin
	# Take the power spectrum of the first half.  Ignore the first
	# element; since we have subtracted a background, the Fourier
	# transform at zero frequency should be nearly zero.
	do i = 1, half {
	    x = real (xdata[i+1])		# skip first element
	    y = aimag (xdata[i+1])
	    pspec[i] = x**2 + y**2
	}

	# Get the minimum non-zero value of the power spectrum.
	call alimr (pspec, half, minps, maxps)
	if (maxps <= 0.)
	    call error (1, "power spectrum is entirely zero")
	minps = maxps				# initialize with maximum
	do i = 1, half {
	    if (pspec[i] > 0.)
		minps = min (minps, pspec[i])
	}

	# Take log (base 10) power spectrum.
	do i = 1, half {
	    if (pspec[i] > 0.)
		pspec[i] = log10 (pspec[i])
	    else
		pspec[i] = log10 (minps)
	}
end

procedure bw_cmplx (xdata, npix, fwd)

complex xdata[npix]	# io: array to be transformed
int	npix		# i: length of first axis
bool	fwd		# i: forward transform?
#--
pointer trig		# scratch for array of cosines & sines

begin
	# Allocate scratch space, and initialize the Fourier transform.
	call calloc (trig, 4*npix + 15, TY_REAL)
	call cffti (npix, Memr[trig])

	# Do complex transform in-place.
	if (fwd)				# forward transform
	    call cfftf (npix, xdata, Memr[trig])
	else					# backward transform
	    call cfftb (npix, xdata, Memr[trig])

	call mfree (trig, TY_REAL)
end
