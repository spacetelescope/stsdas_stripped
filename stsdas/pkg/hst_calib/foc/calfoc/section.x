include "calfoc.h"

# This file contains routines for getting the image sections appropriate
# for the background correction and flat-field (UNI or SDE) correction.
#
# Phil Hodge, 30-Aug-1991  Original

# bac_section -- create image section to be appended to name of BAC image
# Background correction is done before pixel splitting (dezooming), so
# we need a 512 x 1024 BAC file for zoomed pixels and a 1024 x 1024 BAC
# file for normal pixels.  The image section has a different starting
# point in those two cases.

procedure bac_section (i_foc, section, maxch)

pointer i_foc		# i: pointer to FOC struct
char	section[ARB]	# o: image section to append to name of BAC header
int	maxch		# i: size of section
#--
int	sampbeg		# sample number of first image pixel
int	linebeg		# line number on photocathode of first image pixel
int	naxis1, naxis2	# size of input (not dezoomed) image
int	foc_full()

begin
	# If the image covers the full photocathode, we don't need
	# an image section.
	if (foc_full (i_foc) == YES) {
	    section[1] = EOS
	    return
	}

	# Sampbeg is sample number on photocathode of first image pixel.
	sampbeg = FOC_SAMPBEG(i_foc)
	linebeg = FOC_LINEBEG(i_foc)
	naxis1  = FOC_NPIX1(i_foc)
	naxis2  = FOC_NPIX2(i_foc)

	# Convert sampbeg to sample number in BAC image, which should be
	# only 512 x 1024 for a zoomed image.
	if (FOC_ZOOM_MODE(i_foc))
	    sampbeg = (sampbeg + 1) / 2

	call sprintf (section, maxch, "[%d:%d,%d:%d]")
	    call pargi (sampbeg)
	    call pargi (sampbeg + naxis1 - 1)
	    call pargi (linebeg)
	    call pargi (linebeg + naxis2 - 1)
end

# uni_section -- create image section to be appended to name of UNI image
# Flat-field correction (UNI or SDE) is done after dezooming, so we need
# a 1024 x 1024 UNI file for normal pixels and also for zoomed pixels if
# we have dezoomed.  In the case of zoomed pixels that were not dezoomed,
# we have to halve sampbeg for the 512 x 1024 UNI file.

procedure uni_section (i_foc, caldat, section, maxch)

pointer i_foc		# i: pointer to FOC struct
pointer caldat		# i: pointer to FOC coordinate struct
char	section[ARB]	# o: image section to append to name of UNI header
int	maxch		# i: size of section
#--
int	sampbeg		# sample number of first image pixel
int	linebeg		# line number on photocathode of first image pixel
int	naxis1, naxis2	# size of image
int	foc_full()

begin
	# If the image covers the full photocathode, we don't need
	# an image section.
	if (foc_full (i_foc) == YES) {
	    section[1] = EOS
	    return
	}

	sampbeg = FOC_SAMPBEG(i_foc)
	linebeg = FOC_LINEBEG(i_foc)
	naxis1  = FOC_NPIX1(i_foc)	# size of input (not dezoomed) image
	naxis2  = FOC_NPIX2(i_foc)

	if (DO_PXL(caldat)) {		# uni file should be 1024 x 1024

	    naxis1 = 2 * naxis1		# size of dezoomed image
	    if ( ! FOC_ZOOM_MODE(i_foc))	# bad combination!
		call logmsg (
		"WARNING:  dezoom and flat field non-zoomed image?!")

	} else {				# don't dezoom

	    # Usually image would have normal pixels, but it could have
	    # zoomed pixels and we're keeping it that way, in which case
	    # we would use zoomed uni file (512 x 1024); check for that case.
	    if (FOC_ZOOM_MODE(i_foc))
		sampbeg = (sampbeg + 1) / 2	# 512 x 1024 uni file
	}

	call sprintf (section, maxch, "[%d:%d,%d:%d]")
	    call pargi (sampbeg)
	    call pargi (sampbeg + naxis1 - 1)
	    call pargi (linebeg)
	    call pargi (linebeg + naxis2 - 1)
end

# foc_full -- test if image is full photocathode
# This function returns YES if the image covers the full photocathode;
# otherwise, the function returns NO.

int procedure foc_full (i_foc)

pointer i_foc		# i: pointer to FOC struct

begin
	if (FOC_SAMPBEG(i_foc) == 1 && FOC_LINEBEG(i_foc) == 1) {
	    if (FOC_ZOOM_MODE(i_foc)) {
		if (FOC_NPIX1(i_foc) == MAX_ZSAMPS)
		    return (YES)
	    } else {				# not zoom mode
		if (FOC_NPIX1(i_foc) == MAX_NSAMPS)
		    return (YES)
	    }
	}

	return (NO)
end
