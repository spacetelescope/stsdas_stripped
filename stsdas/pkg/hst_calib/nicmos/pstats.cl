# PSTATS: Plot statistics from a NICMOS or WFC3/IR MultiAccum image
#
# This script will compute statistics for a given image section for all
# the readouts in an IR MultiAccum file and plot the stats as a function
# of exposure time. The statistics are computed using "imstat", stored
# in a table, and then plotted. The output table can be saved.
#
# Version 1.0: H. Bushouse - April 1999: Initial implementation (based on
#					 pstack).
# Version 2.0: H. Bushouse - Aug 2010: Added bunit checks for data in DN or
#				       electrons (for WFC3 support).
#

procedure pstats (input)

file	input	{prompt="Input file name"}

string	extname {"sci",prompt="EXTNAME for data to plot"}
string	units	{"counts",prompt="Plot units (counts or rate)"}
string	stat	{"midpt",prompt="Statistic: mean, midpt, mode, stddev, min, max"}
file	logfile	{"",prompt="Log file for storing results"}
char	title	{"",prompt="Plot title"}
char	xlabel	{"",prompt="X-axis plot label"}
char	ylabel	{"",prompt="Y-axis plot label"}
char	device	{"stdgraph",prompt="Graphics device"}

begin

	# Declarations
	file	image, ofile, tmpfile
	int	nsamp
	string	bunit
	string	tlab, xlab, ylab
	string	image2, inp, ext
	string	root, section
	bool	plrate
	real	exptime
	real	tstat

	# Make sure the necessary packages are loaded
	if (!defpac("images")) images
	if (!defpac("imutil")) imutil
	if (!defpac("tables")) tables
	if (!defpac("stsdas")) stsdas motd-
	if (!defpac("graphics")) graphics
	if (!defpac("stplot")) stplot

	# Get the input arguments
	image = input
	root = ""
	section = ""
	sections (image, option="root") | scan (root)
	sections (image, option="section") | scan (section)
	if (strlen(section) == 0) section = "[*,*]"
	ext  = extname
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
            error (1, "Invalid extname parameter specified")

	# Determine whether the user wants the plot in units of counts
	plrate = no
	if (ext == "sci" || ext == "err") {
	    if (units == "rate" || units == "RATE")
		plrate = yes
	}

	# Build a plot title from the image name and section
	if (title == "")
	    tlab = root // "["//ext//"]"// section

	# Build an x-axis label if not specified by the user
	if (xlabel == "") {
	    if (ext == "time")
		xlab = "Sample Number"
	    else
		xlab = "Sample Time (sec)"
	}

	# Get the value of the BUNIT keyword to find out what units the
	# data are currently in
	imgets (image//"["//ext//",1]", "BUNIT")
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
	tmpfile = mktemp ("tmp$pstats")

	# Print header lines to output file
	printf ("# %s[%s]%s\n", root, ext, section, >> tmpfile)
        if (ext == "sci" || ext == "err")
            print  ("# SAMPTIME   COUNTS  COUNTRATE", >> tmpfile)
        else if (ext == "dq")
            print  ("# SAMPTIME    DQ", >> tmpfile)
        else if (ext == "samp")
            print  ("# SAMPTIME   SAMPLES", >> tmpfile)
        else if (ext == "time")
            print  ("# SAMPLE  SAMPTIME", >> tmpfile)

	# Get the value of the BUNIT keyword to find out what units the
	# data are currently in
	imgets (root//"["//ext//",1]", "BUNIT")
	bunit = imgets.value

	# Find out how many readouts there are in the file
	imgets (root//"[0]", "NSAMP")
	nsamp = int (imgets.value)

	# Loop over the readouts, reading the sample time and pixel values.
	# Note that we must read through the file backwards because
	# MultiAccum data are stored in reverse time order.
	for (i = nsamp-1; i > 0; i=i-1) {

	     # Get the sample time for this readout
	     image2 = root // "[sci,"//str(i)//"]"
	     imgets (image2, "SAMPTIME")
	     exptime = real(imgets.value)

	     # Check for null data arrays
	     image2 = root // "[" //ext//","//str(i)//"]"
	     keypar (image2, "i_naxis", silent+)

	     # Null data array: use value of "pixvalue" keyword
	     if (keypar.value == "0") {
		 keypar (image2, "pixvalue", silent+)
		 tstat = real(keypar.value)

	     # Non-null data array: compute pixel statistics
	     } else {
	         image2 = image2 // section
	         imstat (image2, fields=stat, lower=INDEF, upper=INDEF,
		         binwidth=0.1, format-) | scan (tstat)
	     }

	     # Print data to output file
             if ((ext == "sci" || ext == "err") && (bunit == "COUNTS" || bunit == "ELECTRONS"))
                 printf(" %9.8g %8.6g %8.6g\n", exptime, tstat, tstat/exptime,
                        >> tmpfile)
             else if (ext == "sci" || ext == "err")
                 printf(" %9.8g %8.6g %8.6g\n", exptime, tstat*exptime, tstat,
                        >> tmpfile)
	     else if (ext == "dq")
		 printf (" %9.8g %5.4g\n", exptime, tstat, >> tmpfile)
	     else if (ext == "samp")
		 printf (" %9.8g %5.4g\n", exptime, real(tstat-1), >> tmpfile)
	     else if (ext == "time")
		 printf (" %5d   %9.8g\n", int(nsamp-i), tstat, >> tmpfile)
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

	sgraph (input=inp, errcolumn="",xlabel=xlab, device=device,
		ylabel=ylab, title=tlab, pointmode+, marker="box",
		szmark=0.01)

	# Delete the temporary file
	delete (tmpfile,  yes, verify=no, default=yes)

end
