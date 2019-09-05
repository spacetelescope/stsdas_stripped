include	<imhdr.h>
include	<error.h>
include <time.h>

define	IRAF_KEY	0	# Type of wavelength axis keywords
define	ST_KEY		1
define	FITS_KEY	2

# NEWREDSHIFT -- Changes the redshift of spectra.
#
# The input spectra are given by an image template list. The output is either 
# a matching list of spectra or a directory. The number of input spectra may 
# be either one or match the number of output spectra. Image sections are 
# ignored, since the user wants a exact copy of the input, however with the
# wavelength scale modified.
#
#							Ivo Busko  7/17/89

procedure t_newredshift()

char	imtlist1[SZ_LINE]			# Input spectra list
char	imtlist2[SZ_LINE]			# Output spect. list/directory
real	oldz					# Old redshift
real	newz					# New redshift
bool	log					# log wavelength ?
int	axis					# Wavelength axis
bool	verbose					# Print operations?

char	image1[SZ_PATHNAME]			# Input image name
char	image2[SZ_PATHNAME]			# Output image name
char	dirname1[SZ_PATHNAME]			# Directory name
char	dirname2[SZ_PATHNAME]			# Directory name

int	list1, list2, root_len

int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory(), clgeti()
bool	clgetb()
real	clgetr()

begin
	# Get task parameters

	call clgstr ("input", imtlist1, SZ_LINE)
	call clgstr ("output", imtlist2, SZ_LINE)
	axis	= clgeti ("axis")
	oldz    = clgetr ("oldz")
	newz    = clgetr ("newz")
	log	= clgetb ("log")
	verbose = clgetb ("verbose")

	# If the output string is a directory, generate names for
	# the new images accordingly.

	if (isdirectory (imtlist2, dirname2, SZ_PATHNAME) > 0) {
	    list1 = imtopen (imtlist1)
	    while (imtgetim (list1, image1, SZ_PATHNAME) != EOF) {

		# Strip an eventual image section first.  Place the input 
		# image name, without a directory or image section, in string 
		# dirname1.
		call imgimage (image1, image2, SZ_PATHNAME)
		root_len = fnldir (image2, dirname1, SZ_PATHNAME)
		call strcpy (image2[root_len + 1], dirname1, SZ_PATHNAME)

		# Assemble output image name. Strip again image section from
		# input image name.
		call strcpy (dirname2, image2, SZ_PATHNAME)
		call strcat (dirname1, image2, SZ_PATHNAME)
		call imgimage (image1, image1, SZ_PATHNAME)

		# Do it.
		call newr_image (image1, image2, oldz, newz, log, 
				 axis, verbose)
	    }
	    call imtclose (list1)

	} else {

	    # Expand the input and output image lists.
	    list1 = imtopen (imtlist1)
	    list2 = imtopen (imtlist2)
	    if (imtlen (list1) != imtlen (list2)) {
	        call imtclose (list1)
	        call imtclose (list2)
	        call error (0, "Number of input and output images not the same")
	    }

	    # Do each set of input/output images. First strip any sections.
	    while ((imtgetim (list1, image1, SZ_PATHNAME) != EOF) &&
		(imtgetim (list2, image2, SZ_PATHNAME) != EOF)) {
		call imgimage (image1, image1, SZ_PATHNAME)
		call imgimage (image2, image2, SZ_PATHNAME)
		call newr_image (image1, image2, oldz, newz, log, 
				 axis, verbose)
	    }

	    call imtclose (list1)
	    call imtclose (list2)
	}
end

# NEWR_IMAGE -- Copy a spectrum, changing the wavelength scale accordingly
# to the values of oldz and newz. 
#
# No fast image copy is possible, since iki_copy for the oif format is
# not implemented. Instead, a routine based in task images.imcopy is used.
#
# If the input and output image names are equal, just 
# issue a warning message. Keywords that describe the wavelength axis are 
# looked for in the header, in the following order: first, ONEDSPEC keywords 
# W0 and WPC; second, CD keywords CRVALn and CDn_1, and at last FITS keywords 
# CRVALn and CDELTn. Here n is the value of parameter AXIS, unless the ONEDSPEC 
# header keyword DISPAXIS is found, in which case it takes precedence over the 
# parameter. Logarithmic wavelength scale is treated correctly either if the 
# LOG task parameter is set to yes or if the ONEDSPEC header keyword DC-FLAG is
# found in the header with value 1. HISTORY records are appended to the 
# header.

procedure newr_image (image1, image2, oldz, newz, logwav, axis, verbose)

char	image1[ARB]			# Input spectrum
char	image2[ARB]			# Output spectrum
real	oldz				# Old redshift
real	newz				# New redshift
bool	logwav				# Log wavelength ?
int	axis				# Wavelength axis
bool	verbose				# Print the operation
char    str[SZ_LINE]

pointer	im
int	dispaxis, dcflag, keymode
double	corre, crval2, cdelt2, crval1, cdelt1

pointer immap()
int	imgeti()
bool	imaccf(), streq()

begin
	if (streq (image1, image2))
	    call error (0, "Same input and output images.")

	# Check keywords on input image header
	im = immap (image1, READ_ONLY, 0)

	# First check which one is the dispersion axis.
	if (imaccf (im, "DISPAXIS"))
	    dispaxis = imgeti (im, "DISPAXIS")
	else
	    dispaxis = axis

	# Next check image dimensionality
	if (dispaxis < 1 || dispaxis > IM_NDIM(im)) {
	    call eprintf ("%s : non-existent axis.\n")
	        call pargstr (image1)
	    call imunmap (im)
	    return
	}

	# Check if wavelength scale is linear or logarithmic.
	# If no indication is found on the image, assume linear.
	if (imaccf (im, "DC-FLAG"))
	    dcflag = imgeti (im, "DC-FLAG")
	else
	    dcflag = 0
	if (dcflag == -1) {
	    call eprintf ("%s : not calibrated to wavelength.\n")
	        call pargstr (image1)
	    call imunmap (im)
	    return
	}

	# Detect keyword type: IRAF, ST or plain FITS.
	if (imaccf (im, "W0"))
	    keymode = IRAF_KEY
	else if (imaccf (im, "CD1_1")) {
	    keymode = ST_KEY
	} else {
	    keymode = FITS_KEY
	}

	call imunmap (im)

	# Now create and open the new image.
#	call imcopy (image1, image2)
	call newr_imcopy (image1, image2)
	im = immap (image2, READ_WRITE, 28800)

	# Update keywords with new values corrected by the redshift
	# difference between oldz and newz.

	corre = (1.d0 + newz) / (1.d0 + oldz)

	call newr_get (im, keymode, dispaxis, crval1, cdelt1)

	if ( (dcflag == 1) || (logwav) ) {
	    crval2 = crval1 + log10(corre)
	    cdelt2 = cdelt1
	} else {
	    crval2 = crval1 * corre
	    cdelt2 = cdelt1 * corre
	}

	call newr_put (im, keymode, dispaxis, crval2, cdelt2)

	# If verbose print the operation.
	if (verbose) {
	    call eprintf ("%s %32t->%40t %s\n")
	        call pargstr (image1)
	        call pargstr (image2)
	    call eprintf (" origin = %.4g %40t  origin = %.4g\n")
	        call pargd (crval1)
	        call pargd (crval2)
	    call eprintf (" step   = %.4g %40t  step   = %.4g\n")
	        call pargd (cdelt1)
	        call pargd (cdelt2)
	}

	# Update image HISTORY.
	#call smark (sp)
	#call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (str, SZ_LINE, " NEWREDSHIFT: oldz = %f, newz = %f ")
	    call pargr (oldz)
	    call pargr (newz)
	call newr_timelog (str, SZ_LINE)
	call imputh (im, "HISTORY", str)
	call sprintf (str, SZ_LINE, "input file to NEWREDSHIFT: %s ")
	    call pargstr (image1)
	call imputh (im, "HISTORY", str)
	#call sfree (sp)

	call imunmap (im)
end

# NEWR_GET -- Get header parameters which describe the wavelength axis.

procedure newr_get (im, key, axis, crval, cdelt)

pointer	im			#i: image pointer
int	key			#i: type of keyword
int	axis			#i: dispersion axis
double	crval			#o: reference wavelength
double	cdelt			#o: wavelength step

char	astring[SZ_LINE]
double	imgetd()

begin
	# Wavelength scale zero is common for ST and FITS formats.
	call sprintf (astring, SZ_LINE, "CRVAL%1d")
	    call pargi (axis)
	crval = imgetd (im, astring)

	switch (key) {

	case ST_KEY:
	    call sprintf (astring, SZ_LINE, "CD%1d_1")
	        call pargi (axis)
	    cdelt = imgetd (im, astring)

	case FITS_KEY:
	    call sprintf (astring, SZ_LINE, "CDELT%1d")
	        call pargi (axis)
	    cdelt = imgetd (im, astring)

	case IRAF_KEY:
	    crval = imgetd (im, "W0")
	    cdelt = imgetd (im, "WPC")

	}
end

# NEWR_PUT -- Put header parameters which describe the wavelength axis.

procedure newr_put (im, key, axis, crval, cdelt)

pointer	im			#i: image pointer
int	key			#i: type of keyword
int	axis			#i: dispersion axis
double	crval			#i: reference wavelength
double	cdelt			#i: wavelength step

char	astring[SZ_LINE]

begin
	switch (key) {

	case ST_KEY:
	    call sprintf (astring, SZ_LINE, "CRVAL%1d")
	        call pargi (axis)
	    call imputd (im, astring, crval)
	    call sprintf (astring, SZ_LINE, "CD%1d_1")
	        call pargi (axis)
	    call imputd (im, astring, cdelt)

	case FITS_KEY:
	    call sprintf (astring, SZ_LINE, "CRVAL%1d")
	        call pargi (axis)
	    call imputd (im, astring, crval)
	    call sprintf (astring, SZ_LINE, "CDELT%1d")
	        call pargi (axis)
	    call imputd (im, astring, cdelt)

	case IRAF_KEY:
	    call imputd (im, "W0", crval)
	    call imputd (im, "WPC", cdelt)

	}
end

# NEWR_TIMELOG -- Prepend a time stamp to the given string.
#
# For the purpose of a history logging prepend a short time stamp to the
# given string.  Note that the input string is modified.

procedure newr_timelog (str, max_char)

char	str[max_char]		# String to be time stamped
int	max_char		# Maximum characters in string

pointer	sp, time, temp
long	clktime()

begin
	call smark (sp)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (temp, max_char, TY_CHAR)

	call cnvdate (clktime(0), Memc[time], SZ_DATE)
	call sprintf (Memc[temp], max_char, "%s %s")
	    call pargstr (Memc[time])
	    call pargstr (str)
	call strcpy (Memc[temp], str, max_char)

	call sfree (sp)
end


# NEWR_IMCOPY -- Copy an image.  Use sequential routines to permit copying
# images of any dimension.  Perform pixel i/o in the datatype of the image,
# to avoid unnecessary type conversion.
#
# This routine is basicaly task images.imcopy whith verbose option and
# output image section handling removed.

procedure newr_imcopy (image1, image2)

char	image1[ARB]			# Input image
char	image2[ARB]			# Output image

int	npix, junk
pointer	buf1, buf2, im1, im2
pointer	sp, imtemp
long	v1[IM_MAXDIM], v2[IM_MAXDIM]

int	imgnls(), imgnll(), imgnlr(), imgnld(), imgnlx()
int	impnls(), impnll(), impnlr(), impnld(), impnlx()
pointer	immap()

begin
	call smark (sp)
	call salloc (imtemp, SZ_PATHNAME, TY_CHAR)

	# Map the input image.
	im1 = immap (image1, READ_ONLY, 0)

	# Get a temporary output image name and map it as a copy of the 
	# input image.
	# Copy the input image to the temporary output image and unmap
	# the images.  Release the temporary image name.

	call xt_mkimtemp (image1, image2, Memc[imtemp], SZ_PATHNAME)
	im2 = immap (image2, NEW_COPY, im1)

	# Setup start vector for sequential reads and writes.

	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	# Copy the image.

	npix = IM_LEN(im1, 1)
	switch (IM_PIXTYPE(im1)) {
	case TY_SHORT:
	    while (imgnls (im1, buf1, v1) != EOF) {
		junk = impnls (im2, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
	    }
	case TY_USHORT, TY_INT, TY_LONG:
	    while (imgnll (im1, buf1, v1) != EOF) {
		junk = impnll (im2, buf2, v2)
		call amovl (Meml[buf1], Meml[buf2], npix)
	    }
	case TY_REAL:
	    while (imgnlr (im1, buf1, v1) != EOF) {
		junk = impnlr (im2, buf2, v2)
		call amovr (Memr[buf1], Memr[buf2], npix)
	    }
	case TY_DOUBLE:
	    while (imgnld (im1, buf1, v1) != EOF) {
		junk = impnld (im2, buf2, v2)
		call amovd (Memd[buf1], Memd[buf2], npix)
	    }
	case TY_COMPLEX:
	    while (imgnlx (im1, buf1, v1) != EOF) {
	        junk = impnlx (im2, buf2, v2)
		call amovx (Memx[buf1], Memx[buf2], npix)
	    }
	default:
	    call error (1, "unknown pixel datatype")
	}

	# Unmap the images.

	call imunmap (im2)
	call imunmap (im1)
	call xt_delimtemp (image2, Memc[imtemp])
end
