include	<math.h>

#  rd2xy -- translate RA and Dec to pixel coordiante 
#
#  Description:
#  ------------
#  
#  Input CL parameters:
#  -----------------
#  "infile"		Input science data file template name
#  "ra"			right ascension
#  "dec"		declination
#  "hour"		the input ra is in hours?
#
#  Output CL parameters:
#  -----------------
#  "x"			X pixel coordinate
#  "y"			Y pixel coordinate
#
#  Date		Author			Description
#  ----		------			-----------
#  12-Aug-1991  J.-C. Hsu		design and coding
#  16-Oct-1996	W.J. Hack		Converted to read from WFPC2 FITS
#					tables
#------------------------------------------------------------------------------

procedure upp_rd2xy (infile, group, inra, indec, kx, ky)

char	infile[ARB]		# Rootname for input image
int	group			# Group number (row number from TABLE)
double	x, y			# Output position of target in pixels
bool	hour			# Are input coordinates in hours?
double	inra, indec		# Input target coordinates
real	kx, ky
double	ra, dec
	
char	tabname[SZ_FNAME]	# Name of image to access FITS table
real	cd[2, 2], crpix[2]
double	cdinv[2, 2]
double	ra0, dec0, xi, eta, det, bottom
pointer	gtab, cptr		# input file pointer
int	extnum

pointer	tbtopn()
#==============================================================================
begin

	# read input parameters
	hour = false

	# Construct full img name to access FITS table in extension 1.
	extnum = 1
	call get_fitextn(infile, extnum, tabname)

	# open input file FITS table
	gtab = tbtopn (tabname, READ_ONLY, NULL)		

	# Read in parameters from FITS table
	# Get information from each column here, from rownum = group
	# Consider adding a check to make sure the column is found	
	call tbcfnd (gtab, "CRPIX1", cptr, 1)
	call tbegtr (gtab, cptr, group, crpix[1])

	call tbcfnd (gtab, "CRPIX2", cptr, 1)
	call tbegtr (gtab, cptr, group, crpix[2])

	call tbcfnd (gtab, "CRVAL1", cptr, 1)
	call tbegtd (gtab, cptr, group, ra0)

	call tbcfnd (gtab, "CRVAL2", cptr, 1)
	call tbegtd (gtab, cptr, group, dec0)

	call tbcfnd (gtab, "CD1_1", cptr, 1)
	call tbegtr (gtab, cptr, group, cd[1,1])

	call tbcfnd (gtab, "CD1_2", cptr, 1)
	call tbegtr (gtab, cptr, group, cd[1,2])

	call tbcfnd (gtab, "CD2_1", cptr, 1)
	call tbegtr (gtab, cptr, group, cd[2,1])

	call tbcfnd (gtab, "CD2_2", cptr, 1)
	call tbegtr (gtab, cptr, group, cd[2,2])	

	# Close table
	call tbtclo(gtab) 
		
	# Perform calculations here...
	det = cd[1,1]*cd[2,2] - cd[1,2]*cd[2,1]
	if (det == 0.d0) 
	    call error (1, "singular CD matrix")

	cdinv[1,1] = cd[2,2] / det
	cdinv[1,2] = -cd[1,2] / det
	cdinv[2,1] = -cd[2,1] / det
	cdinv[2,2] = cd[1,1] / det

	# translate (ra, dec) to (x, y)
	ra0 = DEGTORAD(ra0)
	dec0 = DEGTORAD(dec0)
	if (hour) 
	    inra = inra * 15.d0
	ra = DEGTORAD(inra)
	dec = DEGTORAD(indec)

	bottom = sin(dec)*sin(dec0) + cos(dec)*cos(dec0)*cos(ra-ra0)
	if (bottom == 0.d0) 
	    call error (1, "Unreasonable RA/Dec range")

	xi = cos(dec) * sin(ra-ra0) / bottom
	eta = (sin(dec)*cos(dec0) - cos(dec)*sin(dec0)*cos(ra-ra0)) / bottom
	xi = RADTODEG(xi)
	eta = RADTODEG(eta)

	x = cdinv[1, 1] * xi + cdinv[1, 2] * eta + crpix[1]
	y = cdinv[2, 1] * xi + cdinv[2, 2] * eta + crpix[2]

	kx = real(x)
	ky = real(y)
end
