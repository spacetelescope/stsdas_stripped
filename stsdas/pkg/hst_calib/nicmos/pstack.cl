# PSTACK: Plot a stack of pixel values from a NICMOS or WFC3/IR MultiAccum image
#
# This script will read the values from all the readouts for a single
# pixel in an IR MultiAccum file and plot the values as a function
# of exposure time. The pixel values are read from the image and stored
# in a table, and then plotted. The output table can be saved.
#
# Version 1.0: H. Bushouse - March 1997: Initial implementation
# Version 2.0: H. Bushouse - April 1999: Added choice of plot units and
#					 different extension data, and
#					 option to save the output table
# Version 3.0: H. Bushouse - Aug 2010: Added bunit checks for data in DN
#				       or electrons (for WFC3 support).
#

procedure pstack (input, col, row)

file	input	{prompt="Input file name"}
int	col	{prompt="Column number of pixels to plot"}
int	row	{prompt="Row number of pixels to plot"}

string	extname	{"sci",prompt="EXTNAME for data to plot"}
string	units	{"counts",prompt="Plot units (counts or rate)"}
file	logfile	{"",prompt="Log file for storing results"}
char	title	{"",prompt="Plot title"}
char	xlabel	{"",prompt="X-axis plot label"}
char	ylabel	{"",prompt="Y-axis plot label"}
char	device	{"stdgraph",prompt="Graphics device"}

begin

	# Declarations
	file	image1, ofile, tmpfile
	int	x, y
	int	nsamp
	string	bunit, ext
	string	image2, inp
	string	tlab, xlab, ylab
	bool	plrate
	real	exptime, pix, yval

	# Make sure the necessary packages are loaded
	if (!defpac("images")) images
	if (!defpac("imutil")) imutil
	if (!defpac("tables")) tables
	if (!defpac("stsdas")) stsdas motd-
	if (!defpac("graphics")) graphics
	if (!defpac("stplot")) stplot

	# Get the query parameters
	image1 = input
	x = col
	y = row
	ext = extname
	tlab = title
	xlab = xlabel
	ylab = ylabel

	# Check for valid EXTNAME input
	if (ext == "SCI" || ext == "sci")
	    ext = "sci"
	else if (ext == "ERR" || ext == "err")
	    ext = "err"
	else if (ext == "DQ" || ext == "dq")
	    ext = "dq"
	else if (ext == "SAMP" || ext == "samp")
	    ext = "samp"
	else if (ext == "TIME" || ext == "time")
	    ext = "time"
	else
	    error (0, "Invalid extname parameter specified")

	# Determine whether the user wants the plot in units of counts
	plrate = no
	if (ext == "sci" || ext == "err") {
	    if (units == "rate" || units == "RATE")
		plrate = yes
	}

	# Build a plot title from the image name and pixel coords
	if (title == "")
	    tlab = image1 // "["//ext//"]["//str(x)//","//str(y)//"]"

	# Build an x-axis label if not specified by the user
	if (xlabel == "") {
	    if (ext == "time")
		xlab = "Sample Number"
	    else
		xlab = "Sample Time (sec)"
	}

	# Get the value of the BUNIT keyword to find out what units the
	# data are currently in
	imgets (image1//"["//ext//",1]", "BUNIT")
	bunit = strupr(imgets.value)

	# Build a y-axis label if not specified by the user
	if (ylabel == "") {
	    if (ext == "sci" || ext == "err") {
		if (plrate) {
		    ylab = "Countrate (DN/sec)"
		    if (bunit == "ELECTRONS" || bunit == "ELECTRONS/S")
			ylab = "Countrate (e/sec)"
		} else {
		    ylab = "Counts (DN)"
		    if (bunit == "ELECTRONS" || bunit == "ELECTRONS/S")
			ylab = "Counts (e)"
		}
	    } else if (ext == "dq")
		ylab = "DQ"
	    else if (ext == "samp")
		ylab = "Samples"
	    else if (ext == "time")
		ylab = "Sample Time (sec)"
	}

	# Get name for output file
	ofile = logfile
	if (ofile != "" && ofile != "STDOUT" && access (ofile))
	    error (1, "Output file already exists!")

	# Create tmp file for storing table of results
	tmpfile = mktemp ("tmp$pstack")

	# Print header lines to output file
	printf ("# %s[%s][%s,%s]\n", image1, ext, str(x), str(y), >> tmpfile)
	if (ext == "sci" || ext == "err")
	    print  ("# SAMPTIME   COUNTS  COUNTRATE", >> tmpfile)
	else if (ext == "dq")
	    print  ("# SAMPTIME    DQ", >> tmpfile)
	else if (ext == "samp")
	    print  ("# SAMPTIME   SAMPLES", >> tmpfile)
	else if (ext == "time")
	    print  ("# SAMPLE  SAMPTIME", >> tmpfile)

	# Find out how many readouts there are in the file
	imgets (image1//"[0]", "NSAMP")
	nsamp = int (imgets.value)

	# Loop over the readouts, reading the sample time and pixel values.
	# Note that we must read through the file backwards because
	# MultiAccum data are stored in reverse time order.
	for (i = nsamp-1; i > 0; i=i-1) {

	     # Get the sample time for this readout
	     image2 = image1 // "[sci,"//str(i)//"]"
	     imgets (image2, "SAMPTIME")
	     exptime = real(imgets.value)

	     # Check for null data arrays
	     image2 = image1 // "["//ext//","//str(i)//"]"
	     keypar (image2, "i_naxis", silent+)

	     # Null data array: use value of "pixvalue" keyword
	     if (keypar.value == "0") {
		 keypar (image2, "pixvalue", silent+)
		 yval = real(keypar.value)

	     # Non-null data array: get pixel value
	     } else {
		 image2 = image2 // "["//str(x)//","//str(y)//"]"
		 listpix (image2, wcs="logical", formats="", verbose-) |
			  scan (pix, yval)
	     }

	     # Print data to output file
	     if ((ext == "sci" || ext == "err") && (bunit == "COUNTS" || bunit == "ELECTRONS"))
		 printf(" %9.8g %8.6g %8.6g\n", exptime, yval, yval/exptime,
			>> tmpfile)
	     else if (ext == "sci" || ext == "err")
		 printf(" %9.8g %8.6g %8.6g\n", exptime, yval*exptime, yval,
			>> tmpfile)
	     else if (ext == "dq")
		 printf (" %9.8g %5d\n", exptime, yval, >> tmpfile)
	     else if (ext == "samp")
		 printf (" %9.8g %5d\n", exptime, (yval-1), >> tmpfile)
	     else if (ext == "time")
		 printf (" %5d   %9.8g\n", int(nsamp-i), yval, >> tmpfile)
	}

	# If requested, save data to output file
	if (ofile != "" && ofile != "STDOUT")
	    copy (tmpfile, ofile, verbose=no)

	# If requested, print the data to the terminal
	if (ofile == "STDOUT")
	    type (tmpfile, map_cc=yes, device="terminal")

	# Call sgraph to plot the data
	if (plrate)
	    inp = tmpfile // " c1 c3"
	else
	    inp = tmpfile // " c1 c2"

	sgraph (input=inp, errcolumn="", xlabel=xlab, device=device,
		ylabel=ylab, title=tlab, pointmode+, marker="box",
		szmark=0.01, wl=0, wr=0, wb=0, wt=0)

	# Delete the temporary file
	delete (tmpfile,  yes, verify=no, default=yes)

end
