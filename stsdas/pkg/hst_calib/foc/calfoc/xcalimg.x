include <imhdr.h>
include "calfoc.h"

# xcalimg -- perform calibration for an image
#
# D. Giaretta March 1988  Original.
# Phil Hodge,  7-Aug-1991  Reorganize; combine xcalimg with xcalspg.
# Phil Hodge, 10-Jul-1992  In xcal_geo, set chg_coords; change error
#			handling; two input images for xc_group, one
#			for coordinates and one for min & max.
# Phil Hodge, 24-Mar-1994  Call xhistory to add history info and set
#			flags to COMPLETE, etc.
# Phil Hodge,  7-Apr-1994  Change chg_coords to set_values & apply_dist
#			in the call to newgeom in xcal_geo.

procedure xcalimg (infile, outroot, caldat, phot, modestr, phot_path)

char	infile[SZ_FNAME]	# i: name of input image
char	outroot[SZ_FNAME]	# i: root name of output image
pointer	caldat			# i: cal files structure
real	phot[4]			# o: flam, zero pt, pivot wl, rms bandwidth
char	modestr[ARB]		# o: mode string to be put in output header
char	phot_path[ARB]		# o: path info for phot
#--
char	input[SZ_FNAME]		# name of input for each step
char	b_output[SZ_FNAME]	# name of output from first step (BAC, etc)
char	g_output[SZ_FNAME]	# name of GEO output
char	u_output[SZ_FNAME]	# name of UNI (or SDE) output
bool	del_temp		# true if we should delete temp image

begin
	if (FOC_IMG_MODE(caldat) == FOC_NORMAL)
	    call log_progress ("starting normal mode image processing")
	else
	    call log_progress ("starting spectrographic mode image processing")

	# We reset the value of 'input' after each stage of processing.
	call strcpy (infile, input, SZ_FNAME)		# input to BAC, etc.

	# Assign the names of the output images.
	call xoutnames (infile, outroot, caldat,
			b_output, g_output, u_output, SZ_FNAME)

	del_temp = false				# initial value

	if (DO_BAC(caldat) || DO_ITF(caldat) ||
	    DO_PXL(caldat) || DO_ABS(caldat)) {

	    # Write to a temp image and delete it later.
	    if (DO_GEO(caldat) || DO_FLAT(caldat))
		del_temp = true

	    # Do the BAC, ITF, PXL, ABS corrections.
	    iferr (call xcal_bac (input, b_output, caldat,
			phot, modestr, phot_path))
		call xc_error (caldat)

	    call strcpy (b_output, input, SZ_FNAME)	# input to GEO

	} else {
	    # Log file names and calibration flags to trailer file.
	    call x_bac_log (caldat)
	    call x_itf_log (caldat)
	    call x_pxl_log (caldat)
	    call x_abs_log (caldat)
	}

	if (DO_GEO(caldat)) {

	    # Do the GEO correction.
	    iferr (call xcal_geo (input, g_output, caldat))
		call xc_error (caldat)

	    if (del_temp) {
		# Delete temporary image, then reset the flag so
		# we can test it again in DO_FLAT section.
		call imdelete (b_output)
		del_temp = false
	    }

	    call strcpy (g_output, input, SZ_FNAME)	# input to UNI or SDE

	} else {
	    call x_geo_log (caldat)
	}

	if (DO_FLAT(caldat)) {
	    # either UNI or SDE
	    iferr (call xcal_uni (input, u_output, caldat))
		call xc_error (caldat)

	    if (del_temp) {
		call imdelete (b_output)
		del_temp = false
	    }

	    call strcpy (u_output, input, SZ_FNAME)	# input to xc_group

	} else {
	    call x_uni_log (caldat)
	    call x_sde_log (caldat)
	}

	# Write the group parameters to the .cgr file.  The coordinates
	# will be taken from 'infile', the original input image, while
	# datamin & datamax will be taken from 'input', which is the
	# output from the last stage of processing, whatever that may be.
	call xc_group (infile, input, outroot)

	if (FOC_IMG_MODE(caldat) == FOC_NORMAL)
	    call log_progress ("ending   normal mode image processing")
	else
	    call log_progress ("ending   spectrographic mode image processing")
end

# xoutnames -- assign the names of the output images

procedure xoutnames (infile, outroot, caldat,
		b_output, g_output, u_output, maxch)

char	infile[ARB]		# i: name of input image
char	outroot[ARB]		# i: root name of output image
pointer	caldat			# i: cal files structure
char	b_output[ARB]		# o: name of output from first step (BAC, etc)
char	g_output[ARB]		# o: name of GEO output
char	u_output[ARB]		# o: name of UNI (or SDE) output
int	maxch			# i: size of ..._output strings
#--
char	env_buf[SZ_FNAME]	# value of environment variable (ignored)
bool	pipeline		# are we running in the pipeline?
int	envgets()

begin
	pipeline = (envgets (PODPS_ENVIRON, env_buf, SZ_FNAME) > 0)

	# Assign the root names of the output images.
	call strcpy (outroot, b_output, maxch)
	call strcpy (outroot, g_output, maxch)
	call strcpy (outroot, u_output, maxch)

	# Append extension for output from first step (BAC, ITF, etc).
	if (DO_GEO(caldat) || DO_FLAT(caldat))
	    call strcat (BAC_EXT, b_output, maxch)	# temp file
	else if (pipeline)
	    call strcat (GEO_EXT, b_output, maxch)
	else
	    call strcat (BAC_EXT, b_output, maxch)

	# Append extensions for GEO and UNI (or SDE) output.
	if (DO_GEO(caldat) && DO_FLAT(caldat)) {
	    call strcat (GEO_EXT, g_output, maxch)
	    call strcat (UNI_EXT, u_output, maxch)
	} else if (pipeline) {
	    call strcat (GEO_EXT, g_output, maxch)
	    call strcat (UNI_EXT, u_output, maxch)
	} else {
	    call strcat (UNI_ONLY_EXT, u_output, maxch)
	    if (DO_BAC(caldat) || DO_ITF(caldat) ||
		DO_PXL(caldat) || DO_ABS(caldat)) {
		call strcat (GEO_EXT, g_output, maxch)
	    } else {
		call strcat (GEO_ONLY_EXT, g_output, maxch)
	    }
	}
end

procedure xcal_bac (input, output, caldat, phot, modestr, phot_path)

char	input[SZ_FNAME]		# i: input image name
char	output[SZ_FNAME]	# i: output image name
pointer	caldat			# i: cal files structure
real	phot[4]			# i: flam, zero pt, pivot wl, rms bandwidth
char	modestr[ARB]		# i: mode string to be put in output header
char	phot_path[ARB]		# i: path info for phot
#--
pointer	immap()
pointer	iim, oim		# imhdr pointer for input, output images
pointer	ibuf, obuf		# buffers for input & output image values
pointer	bac_buf, itf_buf	# buffers for bac & itf values

real	datamin, datamax	# min & max pixel values

long	iv[IM_MAXDIM]
long	ov[IM_MAXDIM]
long	bac_v[IM_MAXDIM]
long	itf_v[IM_MAXDIM]
int	imgnlr(), impnlr(), naxis1, naxisout, i, i2
real	exp_sc

errchk	immap, imgnlr

begin
	call log_progress ("    starting BAC and ITF processing")
	call printf ("cont        for %s\n")
	    call pargstr (output)
	call flush (STDOUT)

	iim = immap (input, READ_ONLY, NULL)
	oim = immap (output, NEW_COPY, iim)

	call amovkl (long(1), iv, IM_MAXDIM)
	call amovkl (long(1), ov, IM_MAXDIM)
	call amovkl (long(1), bac_v, IM_MAXDIM)
	call amovkl (long(1), itf_v, IM_MAXDIM)

	IM_PIXTYPE(oim)= TY_REAL
	exp_sc = EXP_TIME(caldat)
	naxis1 = IM_LEN(iim, 1)

	# If we need to de-zoom, then double the length of the first axis.
	if (DO_PXL(caldat))
	    IM_LEN(oim, 1) = 2*IM_LEN(iim, 1)

	naxisout = IM_LEN(oim, 1)

	datamax = -1.e20			# initial values
	datamin = 1.e20

	while (imgnlr (iim, ibuf, iv) != EOF &&
	       impnlr (oim, obuf, ov) != EOF) {

	    call amovr (Memr[ibuf], Memr[obuf], naxis1)

	    if (DO_BAC(caldat)) {
		if (imgnlr (BAC_FILE(caldat), bac_buf, bac_v) == EOF)
		    call error (1, "xcal_bac:  EOF from imgnlr for BAC")
		
		do i = 0, naxis1-1
		    Memr[obuf+i] = Memr[obuf+i] - exp_sc*Memr[bac_buf+i]
	    }

	    if (DO_ITF(caldat)) {
		if (imgnlr (ITF_FILE(caldat), itf_buf, itf_v) == EOF)
		    call error (1, "xcal_bac:  EOF from imgnlr for ITF")

		call amulr (Memr[obuf], Memr[itf_buf], Memr[obuf], naxis1)
	    }

	    if (DO_PXL(caldat)) {
		do i = naxis1-1, 0, -1 {
		    i2 = 2*i 
		    Memr[obuf+i2]   = Memr[obuf+i]*0.5
		    Memr[obuf+i2+1] = Memr[obuf+i2]
		}
	    }
	    do i = 0, naxisout-1 {		# update datamin & datamax
		if (Memr[obuf+i] > datamax)
		    datamax = Memr[obuf+i]
		else if (Memr[obuf+i] < datamin)
		    datamin = Memr[obuf+i]
	    }
	}

	call imputr (oim, "i_maxpixval", datamax)
	call imputr (oim, "i_minpixval", datamin)
	IM_LIMTIME(oim) = IM_MTIME(oim) + 1

	if (DO_BAC(caldat))
	    BAC_FLAG(caldat) = COMPLETE

	if (DO_ITF(caldat))
	    ITF_FLAG(caldat) = COMPLETE

	# Modify crpix1, cd1_1, cd2_1.
	if (DO_PXL(caldat)) {
	    call cal_split (oim)	# bug fix - PEH 1991 May 13
	    PXL_FLAG(caldat) = COMPLETE
	}

	# Add the header parameters which give the photometry info.
	if (DO_ABS(caldat)) {
	    call cal_p_phot (oim, phot, modestr, phot_path)
	    ABS_FLAG(caldat) = COMPLETE
	}

	# Set flags (BACCORR, etc) in output header to COMPLETE,
	# if appropriate, and add history records with calibration
	# file name, pedigree and descrip.
	call xhistory (oim, caldat)

	call imunmap (oim)
	call imunmap (iim)
	call log_progress ("    ending   BAC and ITF processing")
end

procedure xcal_geo (input, output, caldat)

char	input[SZ_FNAME]		# i: input image name
char	output[SZ_FNAME]	# i: output geom image name
pointer	caldat			# i: cal files structure
#--
pointer	iim, oim	# imhdr pointers for input & output images
bool	set_values	# set coord param based on GEO header?  true
bool	apply_dist	# modify coord param based on geo distortion?  false
bool	box_check	# set to false
bool	verbose		# set to false
pointer	immap()

errchk	immap, newgeom

begin
	call log_progress ("    starting geometric correction")
	call printf ("cont        for %s\n")
	    call pargstr (output)
	call flush (STDOUT)

	iim = immap (input, READ_ONLY, NULL)
	oim = immap (output, NEW_COPY, iim)
    
	# Perform geometric correction.
	# Newgeom sets the size of the output image.
	# Using set_values = true means that for normal image mode we
	# will assign the output CD matrix values based on the image scale
	# (from the GEO file) and orientation (from calibrated image), and
	# for spectrographic mode we replace the coordinate parameters
	# with ones describing position along the slit and wavelength.

	set_values = true
	apply_dist = false
	box_check = false
	verbose = false
	call newgeom (iim, oim, GEO_FILE(caldat), GEO_DEFV(caldat),
		set_values, apply_dist, box_check, verbose)

	# Set flags (BACCORR, GEOCORR, etc) in output header to COMPLETE,
	# if appropriate, and add history records with calibration
	# file name, pedigree and descrip.
	GEO_FLAG(caldat) = COMPLETE
	call xhistory (oim, caldat)
	
	call imunmap (oim)
	call imunmap (iim)

	call log_progress ("    ending   geometric correction")
end

# xcal_uni -- either UNI or SDE correction

procedure xcal_uni (input, output, caldat)

char	input[SZ_FNAME]		# i: input image name
char	output[SZ_FNAME]	# i: output flat-fielded image name
pointer	caldat			# i: cal files structure
#--
pointer	iim, oim		# imhdr pointer for input, output images
pointer ref_im			# imhdr pointer for UNI (or SDE) image
pointer	ibuf, obuf		# buffers for input & output image values
pointer	ref_buf			# buffer for UNI (or SDE) value
real	datamin, datamax	# min & max pixel values
long	iv[IM_MAXDIM]
long	ov[IM_MAXDIM]
long	ref_v[IM_MAXDIM]
pointer	immap()
pointer	imgnlr(), impnlr()
int	npix1, i

errchk	immap, imgnlr

begin
	if (FOC_IMG_MODE(caldat) == FOC_NORMAL) {
	    call log_progress ("    starting UNI correction")
	    ref_im = UNI_FILE(caldat)
	} else {
	    call log_progress ("    starting SDE correction")
	    ref_im = SDE_FILE(caldat)
	}
	call printf ("cont        for %s\n")
	    call pargstr (output)
	call flush (STDOUT)

	iim = immap (input, READ_ONLY, NULL)

	oim = immap (output, NEW_COPY, iim)

	npix1 = IM_LEN(iim, 1)

	datamax = -1.e20		# initial values
	datamin = 1.e20

	call amovkl (long(1), iv, IM_MAXDIM)
	call amovkl (long(1), ov, IM_MAXDIM)
	call amovkl (long(1), ref_v, IM_MAXDIM)

	while (imgnlr (iim, ibuf, iv) != EOF &&
	       impnlr (oim, obuf, ov) != EOF) {

	    if (imgnlr (ref_im, ref_buf, ref_v) == EOF)
		call error (1, "xcal_uni:  EOF from imgnlr")

	    call amulr (Memr[ibuf], Memr[ref_buf], Memr[obuf], npix1)

	    do i = 0, npix1-1 {			# update datamin & datamax
		if (Memr[obuf+i] > datamax)
		    datamax = Memr[obuf+i]
		else if (Memr[obuf+i] < datamin)
		    datamin = Memr[obuf+i]
	    }                      
	} 
	call imputr (oim, "i_maxpixval", datamax)
	call imputr (oim, "i_minpixval", datamin)
	IM_LIMTIME(oim) = IM_MTIME(oim) + 1

	# Set flags (GEOCORR, UNICORR, etc) in output header to COMPLETE,
	# if appropriate, and add history records with calibration
	# file name, pedigree and descrip.
	if (FOC_IMG_MODE(caldat) == FOC_NORMAL)
	    UNI_FLAG(caldat) = COMPLETE
	else
	    SDE_FLAG(caldat) = COMPLETE

	call xhistory (oim, caldat)

	call imunmap (oim)
	call imunmap (iim)

	if (FOC_IMG_MODE(caldat) == FOC_NORMAL)
	    call log_progress ("    ending   UNI correction")
	else
	    call log_progress ("    ending   SDE correction")
end
