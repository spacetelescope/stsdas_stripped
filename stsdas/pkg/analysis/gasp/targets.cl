procedure targets (ra, dec, width, exttab, image)

#  TARGETS -- Extract guide stars from the CD-ROM catalog in a field,
#  overlay the guide stars on an image, interactively determine a
#  plate solution and read back image coordinates.

#  Modified 6 December 1990, Z.G. Levay, STScI
#  to use NFS mounted GSC CD-ROM and simplify task structure

#  7/23/91 Removed extra arguments from display
#  and check for loaded packages.  ZGL

## 3 June 1992 added allcols parameter for compatibility with extgst.  ZGL
## 18 Nov 92 Make imtrans default "no"
##    Add savetabs argument to targets call.  ZGL
## 26 April 1993  Change package load test to reflect new path to
##    display task.  ZGL
## 18 August 1993 Toggle "update" parameter to pxcoord to update pixel
##    coords of gsc stars in table.  Make sure intrep is getting RA in
##    degrees.

real	ra	{min=0, max=24, prompt="R.A. of field center in hours"}
real	dec	{min=-90, max=90, prompt="Dec. of field center in degrees"}
real	width	{min=0,max=180, prompt="Width of field in degrees"}
file	exttab	{"extgsc", prompt="Table of extracted stars"}
file	image	{prompt="Image"}

real	mag1	{INDEF, prompt="Magnitude limit"}
real	mag2	{INDEF, prompt="Magnitude limit"}
bool	allcols	{no, prompt="Copy all table columns from GSC?"}
bool	plot	{no, prompt="Plot sky chart of selected region?"}
bool	imtrans	{no, prompt="Find coordinate transformation in input image?"}
real	crpix1	{prompt="X Reference pixel"}
real	crpix2	{prompt="Y Reference pixel"}
real	xpxsize	{prompt="X Pixel size [microns]"}
real	ypxsize	{prompt="Y Pixel size [microns]"}
real	pltscl	{prompt="Plate scale [arcsec/mm]"}
real	crota	{prompt="Rotation angle (degrees)"}
string	ctype1	{prompt="X Coordinate type"}
string	ctype2	{prompt="Y Coordinate type"}

string	mode	{"a"}

begin
	file	inimage		# Input image
	file	garbage		# Temporary output
	bool	cpallc

	# Internal parameters
	real	racen
	real	decen
	real	size
	file	gsctab

	real	xcrval
	real	ycrval
	real	xcdelt
	real	ycdelt
	real	xcrpix
	real	ycrpix

	# Make sure the appropriate tasks are loaded.
	if( !defpac("images") || !defpac("tv")) {
	    print( "Error: images and tv packages need to be loaded!" )
	    bye
	}

	# Assign internal parameters
	racen	= targets.ra
	decen	= targets.dec
	size	= targets.width
	gsctab	= targets.exttab

	cpallc  = regions.allcols

	# Temporary file names
	garbage = mktemp ("tmp$grb")

	regions (racen, decen, size, gsctab,
	    mag1=targets.mag1, mag2=targets.mag2,
	    savetabs=no, plot=targets.plot, allcols=cpallc)

	# Reference coords in degrees
	xcrval = racen * 15
	ycrval = decen

	# Increment per pixel in degrees
	xcdelt = -targets.pltscl * targets.xpxsize / 3.6e6
	ycdelt =  targets.pltscl * targets.ypxsize / 3.6e6

	# Reference pixel
	xcrpix = targets.crpix1
	ycrpix = targets.crpix2

	inimage = targets.image

	print ("Display the image")
	display (inimage, 1)

	# Find the pixel coordinates of the GSC stars
	print ("Mark the GSC positions on the image")

#	print ("CRVAL = ", xcrval, ycrval)
#	print ("CRPIX = ", xcrpix, ycrpix)
#	print ("CDELT = ", xcdelt, ycdelt)
#	print ("CROTA = ", crota, " CTYPE = ", ctype1, " ", ctype2)

	pxcoord (inimage, gsctab, tabcoord=no, update=yes,
	    imtrans=targets.imtrans,
	    crval1=xcrval, crval2=ycrval,
	    crpix1=xcrpix, crpix2=ycrpix,
	    cdelt1=xcdelt, cdelt2=ycdelt, crota=targets.crota,
	    ctype1=targets.ctype1, ctype2=targets.ctype2,
	    > garbage)

#	print (garbage)
#	type (garbage)

	# Mark the GSC stars on the image
	tvmark (1, garbage, mark="circle", radii=7, color=204,
	    interactive=no)

	delete (garbage)

	print ("Reposition the GSC positions to match the image")
#	print (gsctab, " ", inimage) 
#	print ("crpix: ", xcrpix, ycrpix)
#	print ("center: ", xcrval, decen) 
#	print ("pxsize: ", xpxsize, ypxsize)
#	print ("pltscl: ", pltscl)

	intrep (gsctab, inimage, 
	    valid=0, xcrpix=xcrpix, ycrpix=ycrpix,
	    racen=xcrval, deccen=decen, 
	    xpxsize=targets.xpxsize, ypxsize=targets.ypxsize,
	    pltscl=targets.pltscl, solout=no)
end
