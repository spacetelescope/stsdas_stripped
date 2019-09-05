include	<imhdr.h>
include	<mach.h>

define	HUE_MIN	0.0
define	HUE_MAX	360.0
define	BRT_MIN	0.0
define	BRT_MAX	1.0

procedure t_hltorgb ()

# HLTORGB -- Convert a pair of images representing brightness and hue
# into three images representing red, green, and blue.  We assume that
# the images are the same size and registered.  One can specify any
# range of input brightness and color values to map to any range of hue
# and lightness.  Any input color values outside the specified range
# should map to INDEF, which means the output color triples will have the
# same value and represent the scaled gray value of the input intensity
# image.

pointer	lfn, hfn, rgbfn, rfn, gfn, bfn
pointer sp
pointer	lim, him, rim, gim, bim
pointer	lln, hln, sln, rln, gln, bln
pointer	root, exten
real	saturation
int	szln, nmln, line
real	ihmin, ihmax, ilmin, ilmax
real	ohmin, ohmax, olmin, olmax

string	rsuff	"_r"
string	gsuff	"_g"
string	bsuff	"_b"

real	clgetr()
pointer	immap(), imgl2r(), impl2r()

begin
	call smark (sp)
	call salloc (lfn, SZ_FNAME, TY_CHAR)
	call salloc (hfn, SZ_FNAME, TY_CHAR)
	call salloc (rgbfn, SZ_FNAME, TY_CHAR)
	call salloc (rfn, SZ_FNAME, TY_CHAR)
	call salloc (gfn, SZ_FNAME, TY_CHAR)
	call salloc (bfn, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (exten, SZ_FNAME, TY_CHAR)

	call clgstr ("lightness", Memc[lfn], SZ_FNAME)
	call clgstr ("hue", Memc[hfn], SZ_FNAME)
	saturation = clgetr ("saturation")

	call clgstr ("rgbroot", Memc[rgbfn], SZ_FNAME)

	# Build the output r, g, b filenames from the root
	call ft_fname (Memc[rgbfn], rsuff, Memc[rfn], SZ_FNAME)
	call ft_fname (Memc[rgbfn], gsuff, Memc[gfn], SZ_FNAME)
	call ft_fname (Memc[rgbfn], bsuff, Memc[bfn], SZ_FNAME)

	ihmin = clgetr ("ihmin")
	ihmax = clgetr ("ihmax")
	ilmin = clgetr ("ilmin")
	ilmax = clgetr ("ilmax")

	ohmin = clgetr ("ohmin")
	if (IS_INDEFR(ohmin))
	    ohmin = HUE_MIN
	ohmax = clgetr ("ohmax")
	if (IS_INDEFR(ohmax))
	    ohmax = HUE_MAX
	olmin = clgetr ("olmin")
	if (IS_INDEFR(olmin))
	    olmin = HUE_MIN
	olmax = clgetr ("olmax")
	if (IS_INDEFR(olmax))
	    olmax = HUE_MAX

	# Map the input (HLS) images
	lim = immap (Memc[lfn], READ_ONLY, 0)
	him = immap (Memc[hfn], READ_ONLY, 0)

	# Map the output (RGB) images
	rim = immap (Memc[rfn], NEW_COPY, lim)
	gim = immap (Memc[gfn], NEW_COPY, lim)
	bim = immap (Memc[bfn], NEW_COPY, lim)

	# Force the output pixel type to real
	IM_PIXTYPE(rim) = TY_REAL
	IM_PIXTYPE(gim) = TY_REAL
	IM_PIXTYPE(bim) = TY_REAL

	# Get the actual input data ranges
	if (IS_INDEFR(ihmin) || IS_INDEFR(ihmax))
	    call dmnmxr (him, ihmin, ihmax)
	if (IS_INDEFR(ilmin) || IS_INDEFR(ilmax))
	    call dmnmxr (lim, ilmin, ilmax)

	call printf ("Intensity values [%f:%f] --> Lightness [%f:%f]\n")
	    call pargr (ilmin)
	    call pargr (ilmax)
	    call pargr (olmin)
	    call pargr (olmax)

	call printf ("Color values [%f:%f] --> Hue [%f:%f]\n")
	    call pargr (ihmin)
	    call pargr (ihmax)
	    call pargr (ohmin)
	    call pargr (ohmax)

	szln = IM_LEN(lim,1)
	nmln = IM_LEN(lim,2)

	call salloc (sln, szln, TY_REAL)
	call amovkr (saturation, Memr[sln], szln)

	do line = 1, nmln {
	    # Map a line of the input images
	    hln = imgl2r (him, line)
	    lln = imgl2r (lim, line)

	    # Scale the hue and lightness values
	    call sclhue (Memr[hln], szln, ihmin, ihmax, ohmin, ohmax)
	    call sclbrt (Memr[lln], szln, ilmin, ilmax, olmin, olmax)

	    # Map a line of the output images
	    rln = impl2r (rim, line)
	    gln = impl2r (gim, line)
	    bln = impl2r (bim, line)

	    # HLS --> RGB
	    call ahlsrgbr (Memr[hln], Memr[lln], Memr[sln],
			   Memr[rln], Memr[gln], Memr[bln],
			   szln)
	}

	call imunmap (him)
	call imunmap (lim)
	call imunmap (rim)
	call imunmap (gim)
	call imunmap (bim)

	call sfree (sp)
end


procedure ahlsrgbr (hue, light, sat, red, green, blue, size)

# AHLSRGB -- Vector transform HLS to RGB.  Assume the hue values are
# in the range [0:360] and the lightness and saturation [0:1].
 
real    hue[ARB], light[ARB], sat[ARB]
real    red[ARB], green[ARB], blue[ARB]
int     size

int     pix

begin
        do pix = 1, size {
            #  For each element
            call hls_rgb (hue[pix], light[pix], sat[pix],
               red[pix], green[pix], blue[pix])
        }
end


procedure hls_rgb (hue, light, sat, red, green, blue)

# HLS_RGB -- Transform a single HLS triple to RGB.  Assume the hue
# values lie in the range [0:360] and the lightness between [0:1].

real	hue, light, sat
real	red, green, blue

real	m1, m2
real	hueval()

bool	fp_equalr()

begin
	if (fp_equalr (sat, 0.0) || IS_INDEFR(hue)) {
	    #  Achromatic;  no hue
	    red   = light
	    green = light
	    blue  = light

	} else {
	    #  Chromatic
            if (light <= 0.5)
        	m2 = light * (1.0 + sat)
            else
        	m2 = light + sat - light * sat

            m1 = 2.0 * light - m2
 
	    red   = hueval (m1, m2, hue + 120.0)
	    green = hueval (m1, m2, hue)
	    blue  = hueval (m1, m2, hue - 120.0)
	}
end


real procedure hueval (n1, n2, hue)

real	n1, n2
real	hue
real	value

begin
	if (hue > 360.0)
		hue = hue - 360.0
	if (hue < 0.0)
		hue = hue + 360.0
	if (hue < 60.0)
		value = n1 + (n2 - n1)*hue/60.0
	else if (hue < 180.0)
		value = n2
	else if (hue < 240.0)
		value = n1 + (n2 - n1)*(240 - hue)/60.0
	else
		value = n1

	return (value)
end


procedure ft_fname (inname, ri, outname, maxch)

# FT_FNAME -- append letter to file name
# The string ri is appended to the root portion of a file name.
# The file (image) name may include a logical directory, root name,
# extension, "cluster" index (group number), and/or image section.
# The name must not include a VMS directory specification because
# that would be confused with an image section.
#
# 1990 Feb 1, Z. G. Levay, STScI
# Stolen from Phil Hodge.  I replaced imparse() with imgclust() as
# suggested by Bernie Simon.

char	inname[ARB]		# i: input image name
char	ri[ARB]			# i: "r" or "i" to be appended to name
char	outname[ARB]		# o: name with "r" or "i" appended
int	maxch

pointer sp
pointer cluster			# scratch for directory + root + extension
pointer ldir, root		# scratch for directory, root name
int	dirlen, rootlen, flen

int	fnldir(), fnroot()

begin
	call smark (sp)
	call salloc (cluster, maxch, TY_CHAR)
	call salloc (ldir, maxch, TY_CHAR)
	call salloc (root, maxch, TY_CHAR)

	# Extract the "cluster" name, which is the logical directory,
	# file name root, and extension.  Ignore image section, etc.
	call imgcluster (inname, Memc[cluster], maxch)
	dirlen  = fnldir (Memc[cluster], Memc[ldir], maxch)
	rootlen = fnroot (Memc[cluster], Memc[root], maxch)

	# inname[1:flen] includes the logical directory and root but not
	# the (optional) dot and extension or cluster index or image section.
	flen = dirlen + rootlen
	call strcpy (inname, outname, flen)
	call strcat (ri, outname, maxch)
	call strcat (inname[flen+1], outname, maxch)

	call sfree (sp)
end


procedure sclhue (hue, npix, imin, imax, omin, omax)

#  SCLHUE -- Scale the hue values between the input range to the
#  specified range of hue.
#  That is, set any values larger than the maximum to the maximum and
#  similarly for the minimum.  Or perform a floor and ceiling
#  operation.

real	hue[ARB]
int	npix
real	imin, imax
real	omin, omax

int	i
real	interc, slope

begin
	slope  = (omax - omin) / (imax - imin)
	interc = omin - slope * imin
	do i = 1, npix {
	    if (hue[i] < imin || hue[i] > imax)
		hue[i] = INDEFR
	    else
		hue[i] = slope * hue[i] + interc
	}
end


procedure sclbrt (brt, npix, imin, imax, omin, omax)

#  SCLBRT -- Scale the lightness (brightness) values to the input range.
#  That is, set any values larger than the maximum to the maximum and
#  similarly for the minimum.  Or perform a floor and ceiling
#  operation.

real	brt[ARB]
int	npix
real	imin, imax
real	omin, omax

begin
	call amapr (brt, brt, npix, imin, imax, omin, omax)
end


procedure dmnmxr (im, rmin, rmax)

#  DMNMXR -- Find the min and max values in an input image.  Replace an
#  input range if it is INDEF.

pointer	im
real	rmin, rmax

int	i, nx, ny
real	tmin, tmax
real	umin, umax

pointer	imgl2r()

begin
	nx = IM_LEN(im,1)
	ny = IM_LEN(im,2)

	umin =  MAX_REAL
	umax = -MAX_REAL

	do i = 1, ny {
	    call alimr (Memr[imgl2r (im,i)], nx, tmin, tmax)
	    umin = min (umin, tmin)
	    umax = max (umax, tmax)
	}

	if (IS_INDEFR(rmin))
	    rmin = umin

	if (IS_INDEFR(rmax))
	    rmax = umax
end
