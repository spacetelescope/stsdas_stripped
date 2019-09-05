include	<imhdr.h>
include	"../fourier.h"
include	"../fterr.h"
include "powerspec.h"

# powerspec -- compute power spectrum
#
# C. D. Biemesderfer, STScI, 13 Jun 88
# Phil Hodge, 22-Jul-1988  Combine powerspec and autocorr.
# Phil Hodge,  7-Feb-1990  Remove "tmp$" from names of temporary images.
# Phil Hodge, 14-Nov-1990  Set output data type to real.
# Phil Hodge, 27-Apr-1992  Include fourth argument in call to alogr.
# Phil Hodge, 24-Jun-1992  Include verbose parameter.
# Phil Hodge,  1-Jul-1993  Do transforms of small 2-D arrays in-memory.
# Phil Hodge, 20-Jul-1993  Call ft_open_files_o for output images.
# Phil Hodge, 22-Jul-1993  Set FT_CRPIX before calling ft_small_p.
# Phil Hodge, 15-Jan-1995  Call ft_struct_open and ft_struct_close.

procedure t_powerspec()
#--
int	option		# powerspec (as opposed to autocorrelation)

begin
	option = POWERSPEC
	call ft_pspec (option)
end

# autocorr -- compute autocorrelation

procedure t_autocorr()

int	option		# autocorrelation (as opposed to powerspec)

begin
	option = AUTOCORR
	call ft_pspec (option)
end

# ft_pspec -- power spectrum or autocorrelation

procedure ft_pspec (option)

int	option			# i: autocorrelation or power spectrum
#--
pointer sp
pointer fti, fto		# FT structures for input, output files
pointer ctd[2]			# pointers to input & output CTYPE dictionaries
pointer infile, outfile
int	len_blk			# edge length of block for transposing image
bool	getreal, getimag	# real part, imaginary part exist?
bool	logpower		# take log10 of power spectrum?
bool	center			# center output?
bool	verbose			# print file names?
bool	in_memory		# do 2-D transform in-memory?
bool	crereal, creimag	# flags for creating output image
int	clgeti()
bool	clgetb()

begin
	call smark (sp)

	# Allocate Fourier structure for input & output images.
	call ft_struct_open (fti)
	call ft_struct_open (fto)

	call salloc (infile, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)

	# Get file names.
	call clgstr ("input", Memc[infile], SZ_FNAME)
	call clgstr ("output", Memc[outfile], SZ_FNAME)

	if (Memc[outfile] == EOS)
	    call ft_error (FT_NOOUTPUT, FT_FATAL)

	# Get processing options.
	getreal = clgetb ("inreal")
	getimag = clgetb ("inimag")

	center = clgetb ("center")
	verbose = clgetb ("verbose")
	len_blk = 512				# default

	if (option == POWERSPEC) {
	    logpower = clgetb ("logpower")
	    # Get "ftpairs" parameter, and read that file.
	    call ft_read_ftpairs (ctd)
	} else {
	    logpower = false
	    ctd[1] = NULL
	    ctd[2] = NULL
	}

	# Open input files as requested.
	call ft_open_files_i (fti, Memc[infile], getreal, getimag)

	# Create only a real part for the output.
	crereal = true
	creimag = false
	call ft_open_files_o (fti, fto, Memc[outfile], crereal, creimag)

	# Print file names.
	if (verbose) {
	    call ft_ri_print (fti, "input")
	    call ft_ri_print (fto, "output")
	    call flush (STDOUT)
	}

	# Perform transform.
	switch (FT_NAXIS(fti)) {
	case 1:
	    call ft_1_pspec (fti, fto, ctd, option, logpower, center)
	case 2:
	    in_memory = clgetb ("inmemory")
	    if (!in_memory) {
		len_blk = clgeti ("len_blk")
	    }
	    call ft_2_pspec (fti, fto, ctd, option,
			in_memory, len_blk, logpower, center)
	default:
	    call ft_error (FT_BADNAXIS, FT_FATAL)
	}

	# Add title and old coordinate info; update crval, etc.
	if (option == POWERSPEC) {
	    call sprintf (IM_TITLE(FT_REPT(fto)), SZ_IMTITLE,
			"Power spectrum of %s")
		call pargstr (Memc[infile])
	} else {
	    call sprintf (IM_TITLE(FT_REPT(fto)), SZ_IMTITLE,
			"Autocorrelation of %s")
		call pargstr (Memc[infile])
	}
	call save_old_info (fti, FT_REPT(fto))	# save input crval, crpix
	call save_ctstruct (fto, FT_REPT(fto))

	# Close files and clean up.
	call ft_close_files (fti)
	call ft_close_files (fto)
	call sfree (sp)
	if (option == POWERSPEC)
	    call ft_pairs_free (ctd)
	call ft_struct_close (fto)
	call ft_struct_close (fti)
end

# ft_1_pspec -- 1-dimensional power spectrum

procedure ft_1_pspec (fti, fto, ctd, option, logpower, center)

pointer fti		# i: FFT pointer; input image
pointer fto		# i: FFT pointer; output image
pointer ctd[2]		# i: pointers to input & output CTYPE dictionaries
int	option		# i: autocorrelation or power spectrum
bool	logpower	# i: take log10 of power spectrum?
bool	center		# i: center output?
#--
pointer work			# structure for work space
pointer iRe, iIm
pointer	oRe, xr, xi
real	xnpts			# = npts
int	npts
int	cax			# current axis (one)
bool	coord_shift		# no, don't shift phase for coordinates
bool	fwd			# forward transform?
pointer impl1r()
real    ft_log0()
extern	ft_log0		# error trap for log (x) if x <= 0

begin
	coord_shift = false		# coordinate shifting is a no-op

	# Assign coordinate info in fto (FT_CRPIX may be updated later).
	if (option == POWERSPEC)
	    call ft_wcs_fwd (fti, fto, ctd)
	else
	    call ft_wcs_n (fti, fto)	# just copy coordinate info

	call malloc (work, LEN_WORK, TY_STRUCT)

	N_FILES(work) = 1		# number of input files

	npts = IM_LEN(FT_IMAGE(fti),1)
	NPTS(work) = npts

	# Allocate scratch (and zero it) or get pointer to input image,
	# depending on which input files (real or imaginary parts) exist.
	call ft_malloc (fti, npts, YES, iRe, iIm)
	call ft_gl1r (FT_REPT(fti), FT_IMPT(fti), iRe, iIm)

	# Put the transform domain results in memory.  We'll do the complex
	# arithmetic into the output file.
	call malloc (xr, npts, TY_REAL)
	call malloc (xi, npts, TY_REAL)

	# For 1-D data, current axis is always 1.
	cax = 1

	# Initialization section.  The order is important here:
	# set SHIFT, then call FT_WORK_INIT.

	if (center)
	    SHIFT(work) = npts / 2	# shift pixel 1 right by this amount

	# Create scratch for NCAR FFT routine, and for centering.
	call ft_work_init (work, coord_shift, center)

	# Forward FFT.
	fwd = true
	call ft_fft (work, Memr[iRe], Memr[iIm], Memr[xr], Memr[xi], fwd)

	# Get output file pointer and compute power spectrum.
	oRe = impl1r (FT_REPT(fto))
	call amulr (Memr[xr], Memr[xr], Memr[xr], npts)
	call amulr (Memr[xi], Memr[xi], Memr[xi], npts)
	call aaddr (Memr[xr], Memr[xi], Memr[oRe], npts)
	# normalization
	xnpts = real(npts)
	call adivkr (Memr[oRe], xnpts, Memr[oRe], npts)
	# take log10
	if (logpower)
	    call alogr (Memr[oRe], Memr[oRe], npts, ft_log0)

	if (option == AUTOCORR) {
	    fwd = false				# inverse transform
	    call aclrr (Memr[xi], npts)		# no imaginary part
	    # Note that the inverse transform is done in-place.
	    call ft_fft (work, Memr[oRe], Memr[xi], Memr[oRe], Memr[xi], fwd)
	}
	if (center) {
	    call ft_center (npts, SHIFT(work),
			Memr[oRe], Memr[xi], Memr[C_COPY(work)])
	    FT_CRPIX(fto,cax) = FT_CRPIX(fto,cax) + SHIFT(work)
	}

	# Deallocate scratch for NCAR, etc (but not work itself)
	call ft_work_free (work, coord_shift, center)

	# Deallocate memory buffers if we needed any.
	call mfree (xi, TY_REAL)
	call mfree (xr, TY_REAL)
	call ft_mfree (fti, iRe, iIm)

	call mfree (work, TY_STRUCT)
end

# ft_2_pspec -- 2-dimensional power spectrum

procedure ft_2_pspec (fti, fto, ctd, option,
			in_memory, len_blk, logpower, center)

pointer fti		# i: FFT pointer; input image
pointer fto		# i: FFT pointer; output image
pointer ctd[2]		# i: pointers to input & output CTYPE dictionaries
int	option		# i: autocorrelation or power spectrum
bool	in_memory	# i: do transform in-memory?
int	len_blk		# i: edge length of block for transposing image
bool	logpower	# i: take log10 of power spectrum?
bool	center		# i: center output?
#--
pointer work		# structure for work space
pointer iRe, iIm	# input data
pointer oRe, oIm	# output data
pointer xi		# scratch for imag part
pointer Re_x, Im_x	# to imhdr struct for scratch transposed images
pointer ip		# ptr to imhdr struct for input image
real	xnpts2		# = npts1*npts2
int	npts		# length of current axis
int	cax		# current axis
int	line		# loop index for line of image
bool	coord_shift	# no, don't shift phase for coordinates
bool	fwd		# forward transform?
string	seedri "ftri"
string	seedr  "ftr"
string	seedi  "fti"
extern	ft_log0()		# error trap for log (x) if x <= 0

begin
	coord_shift = false		# coordinate shifting is a no-op

	# Assign coordinate info in fto (FT_CRPIX may be updated below).
	if (option == POWERSPEC)
	    call ft_wcs_fwd (fti, fto, ctd)
	else
	    call ft_wcs_n (fti, fto)	# just copy coordinate info

	ip = FT_IMAGE(fti)

	# Modify FT_CRPIX if output is to be centered.
	if (center) {
	    FT_CRPIX(fto,1) = 1 + IM_LEN(ip,1) / 2	# truncate
	    FT_CRPIX(fto,2) = 1 + IM_LEN(ip,2) / 2
	}

	# Do the transforms in-memory?
	if (in_memory) {
	    call ft_small_p (fti, fto, option, logpower, center)
	    return
	}

	call malloc (work, LEN_WORK, TY_STRUCT)

	N_FILES(work) = 1		# number of input files

	xnpts2 = real(IM_LEN(ip,1)) * real(IM_LEN(ip,2))

	# Both real & imaginary output files must exist while we're doing
	# the transform; create a scratch file for imaginary part.
	FT_IMAG(fto) = YES			# reset later
	call ft_map_s (seedri, 2, IM_LEN(ip,1), IM_LEN(ip,2), FT_IMPT(fto))

	# ********************
	# Begin the section for taking the transform of each line.
	# Write the results into the output file, but at this point
	# we're just using it for scratch.

	cax = 1					# first axis
	npts = IM_LEN(ip,cax)
	NPTS(work) = npts

	# Allocate scratch for input files that don't exist.
	call ft_malloc (fti, npts, YES, iRe, iIm)

	# Initialization section for work struct for an array of size npts.
	# The order is important here:  set SHIFT, call FT_WORK_INIT.

	if (center)
	    SHIFT(work) = npts / 2	# shift pixel 1 right by this amount

	# Create scratch for NCAR FFT routine and centering.
	call ft_work_init (work, coord_shift, center)

	fwd = true

	do line = 1, IM_LEN(ip,2) {		# do for each line of input

	    call ft_gl2r (FT_REPT(fti), FT_IMPT(fti), line, iRe, iIm)
	    call ft_pl2r (FT_REPT(fto), FT_IMPT(fto), line, oRe, oIm)

	    # Forward FFT.
	    call ft_fft (work, Memr[iRe], Memr[iIm], Memr[oRe], Memr[oIm], fwd)

	    # For autocorrelation we can't do the centering here because it
	    # would mess up the inverse transform.
	    if (center && option != AUTOCORR)
		call ft_center (npts, SHIFT(work),
			Memr[oRe], Memr[oIm], Memr[C_COPY(work)])
	}

	# Deallocate scratch for NCAR, etc (but not work itself).
	call ft_work_free (work, coord_shift, center)

	# Deallocate memory buffers for input if we needed any.
	call ft_mfree (fti, iRe, iIm)

	# ********************

	# Flush so we can read from output file.
	call imflush (FT_REPT(fto))
	call imflush (FT_IMPT(fto))

	# Create scratch files for the transpose of the output image.  The
	# lengths of the axes are interchanged; Re_x & Im_x are output.
	call ft_map_s (seedr, 2, IM_LEN(ip,2), IM_LEN(ip,1), Re_x)
	call ft_map_s (seedi, 2, IM_LEN(ip,2), IM_LEN(ip,1), Im_x)

	# Transpose the output file into the scratch file.
	call ft_transpose (FT_REPT(fto), Re_x, len_blk)
	call ft_transpose (FT_IMPT(fto), Im_x, len_blk)

	# Flush so we can read from transposed file.
	call imflush (Re_x)
	call imflush (Im_x)

	# ********************
	# Begin the section for taking the transform of each column.

	cax = 2					# second axis
	npts = IM_LEN(ip,cax)
	NPTS(work) = npts

	call malloc (xi, npts, TY_REAL)		# for imaginary part of result

	if (center)
	    SHIFT(work) = npts / 2

	call ft_work_init (work, coord_shift, center)

	do line = 1, IM_LEN(ip,1) {		# do for each column

	    # We're doing this in-place in the scratch (transposed) images.
	    call ft_gl2r (Re_x, Im_x, line, iRe, iIm)
	    call ft_pl2r (Re_x, Im_x, line, oRe, oIm)

	    # Forward FFT.
	    fwd = true
	    call ft_fft (work, Memr[iRe], Memr[iIm], Memr[oRe], Memr[oIm], fwd)

	    # Compute the power spectrum.
	    call amulr (Memr[oRe], Memr[oRe], Memr[oRe], npts)
	    call amulr (Memr[oIm], Memr[oIm], Memr[oIm], npts)
	    call aaddr (Memr[oRe], Memr[oIm], Memr[oRe], npts)
	    # normalization
	    call adivkr (Memr[oRe], xnpts2, Memr[oRe], npts)
	    # take log10 (not done if autocorr)
	    if (logpower)
		call alogr (Memr[oRe], Memr[oRe], npts, ft_log0)

	    if (option == AUTOCORR) {
		fwd = false				# inverse transform
		call aclrr (Memr[xi], npts)
		# Note that we use zero for the imaginary part, but we
		# save the imaginary part of the inverse transformed image.
		call ft_fft (work, Memr[oRe], Memr[xi],
				Memr[oRe], Memr[oIm], fwd)	# in-place
	    }
	    if (center)
		call ft_center (npts, SHIFT(work),
			Memr[oRe], Memr[oIm], Memr[C_COPY(work)])
	}
	# We've been writing to Re_x, but now we must read from it.
	call imflush (Re_x)
	if (option == AUTOCORR)
	    call imflush (Im_x)			# needed for autocorrelation
	else
	    call ft_del_s (Im_x)		# delete scratch image

	call ft_work_free (work, coord_shift, center)

	call mfree (xi, TY_REAL)

	# Transpose the scratch file back into the output file.
	call ft_transpose (Re_x, FT_REPT(fto), len_blk)
	call imflush (FT_REPT(fto))		# so we can read from it
	call ft_del_s (Re_x)			# delete scratch

	# For autocorrelation we have to take the inverse transform of each row.
	if (option == AUTOCORR) {

	    # Transpose the imaginary part back into the output file.
	    call ft_transpose (Im_x, FT_IMPT(fto), len_blk)
	    call imflush (FT_IMPT(fto))		# so we can read from it
	    call ft_del_s (Im_x)		# now we're done with this

	    cax = 1				# first axis again
	    npts = IM_LEN(ip,cax)
	    NPTS(work) = npts

	    if (center)
		SHIFT(work) = npts / 2

	    call ft_work_init (work, coord_shift, center)

	    fwd = false				# inverse transform
	    do line = 1, IM_LEN(ip,2) {		# do for each line of output

		# We're doing this in-place in the output images.
		call ft_gl2r (FT_REPT(fto), FT_IMPT(fto), line, iRe, iIm)
		call ft_pl2r (FT_REPT(fto), FT_IMPT(fto), line, oRe, oIm)

		call ft_fft (work, Memr[iRe], Memr[iIm],
				Memr[oRe], Memr[oIm], fwd)
		if (center)
		    call ft_center (npts, SHIFT(work),
				Memr[oRe], Memr[oIm], Memr[C_COPY(work)])
	    }
	    call ft_work_free (work, coord_shift, center)
	}

	call ft_del_s (FT_IMPT(fto))		# done with transposed imag part
	FT_IMAG(fto) = NO

	call mfree (work, TY_STRUCT)
end

# ft_log0 -- called when we take the log of zero or a negative number

real procedure ft_log0()

begin
	return (LOG_ZERO)
end
