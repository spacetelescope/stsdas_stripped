procedure mosdisplay (input, frame)

# Displays all imsets in an IR MULTIACCUM image together 
# in one display window.

string	input	{prompt="MULTIACCUM image to display", mode="al"}
int	frame	{prompt="Frame to be written into",    mode="al"}
int	ncols	{INDEF, prompt="Number of columns",    mode="h"}
int	nrows	{INDEF, prompt="Number of rows",       mode="h"}
string	extname	{"SCI", prompt="EXTNAME for images to be displayed", mode="h"}
bool	zscale	{yes, prompt="Autoscale display?",     mode="h"}
bool	zrange	{yes, prompt="Display full image intensity range?", mode="h"}
real	z1	{INDEF, prompt="Minimum greylevel to be displayed", mode="h"}
real	z2	{INDEF, prompt="Maximum greylevel to be displayed", mode="h"}
bool	number	{no, prompt="Overlay IMSET number using TVMARK?", mode="h"}
int	tvmcolor {204, prompt="TVMARK color for IMSET label", mode="h"}
int	tvmtxsize {1, prompt="Text size for TVMARK IMSET label", mode="h"}

begin

	# Declarations
	string	img, imtstr, tmpfile, uextname
	struct	devstr
	int	iframe, utvmcolor, utvmtxsize
	int	nimset
	real	rsize, uz1, uz2
	real	incols, inrows, uncols, unrows
	real	c_center, c_incr, r_center, r_incr
	int	i
	int	nxtv=20
	int	nytv=20
	bool	doerase
	bool	uzscale, uzrange, unumber

	# Get query parameters
	img        = input
	iframe     = frame
	uncols     = ncols
	unrows     = nrows
	uextname   = extname
	uzscale    = zscale
	uzrange    = zrange
	uz1        = z1
	uz2        = z2
	unumber    = number
	utvmcolor  = tvmcolor
	utvmtxsize = tvmtxsize

	# Load necessary packages
	if (!defpac("plot")) plot
	if (!defpac("images")) images
	if (!defpac("imutil")) imutil
	if (!defpac("tv")) tv

	# Get number of IMSETS in the input file
	imgets (img//"[0]", "NEXTEND")
	nimset = int(imgets.value) / 5
	if (nimset > 25) {
	    print ("  This program will display a maximum of 25 IMSETs from")
	    print ("  an IR MULTIACCUM image. Your image has ",nimset,"IMSETs.")
	    print ("  Only the LAST 25 (chronological) readouts will be shown.")
	    print ("  Usually this means that only the zeroth read is excluded,")
	    print ("  since ordinary NICMOS images have no more than 26 IMSETs.")
	    nimset = 25
	}

	# If the number of rows and columns are not specified by the user,
	# then calculate the best square
	if (uncols == INDEF || unrows == INDEF) {
	    rsize = sqrt (nimset)
	    incols = int (rsize)
	    if (frac(rsize) > 0)
		incols += 1
	    inrows = int (rsize + 0.5)
	}

	# Else just use the specified values
	else {
	    incols = uncols
	    inrows = unrows
	}

	if (incols*inrows < nimset)
	    print ("Warning: Not enough columns/rows specified for the number of images to display!")

	# Calculate the increment between images in the display window.
	c_incr = 1.0 / incols
	i = c_incr * 100
	c_incr = i / 100.
	r_incr = 1.0 / inrows
	i = r_incr * 100
	r_incr = i / 100.

	doerase = yes

	# Loop through rows of images to display
	for (r_center = 1 - (r_incr/2.); r_center > 0.0; r_center -= r_incr) {

	     # Loop through columns of images to display
	     for (c_center = c_incr / 2.; c_center < 1.0; c_center += c_incr) {

		  if (nimset==0)
		      break

		  # Display the current image
		  display (img//"["//uextname//","//nimset//"]", iframe, 
			   bpmask="BPM", bpdisplay="none", overlay="",
			   erase=doerase, border_erase=doerase, select_frame+,
			   repeat-, fill+, zscale=uzscale, zrange=uzrange,
			   xcen=c_center, ycen=r_center, xsiz=c_incr,
			   ysiz=r_incr, xmag=1, ymag=1, order=0, z1=uz1, z2=uz2)

		  # Label the image with the IMSET number, if requested
		  if (unumber) {
		      tmpfile = mktemp("tmp$mdisp")
		      print (nxtv, nytv, nimset, >tmpfile)
		      tvmark (iframe, coords=tmpfile, logfile="", auto-, 
			      outimage="", deletions="", commands="",
			      mark="point", radii="0", lengths="0",
			      font="raster", color=utvmcolor, label+, number-, 
			      nxoff=0, nyoff=0, pointsiz=0, txsize=utvmtxsize,
			      interactive-)
		      delete (tmpfile, ver-)
		  }

		  doerase = no
		  nimset -= 1
	     }
	}

end
