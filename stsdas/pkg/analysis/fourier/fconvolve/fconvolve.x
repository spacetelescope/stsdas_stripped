include	<imhdr.h>
include	"../fourier.h"
include	"../fterr.h"

# fconvolve -- convolution
#
# Phil Hodge, 24-Jun-1993  Task created.
# Phil Hodge, 15-Jan-1995  Call ft_struct_open and ft_struct_close.

procedure t_fconvolve()
#--
pointer sp
pointer fti1, fti2, fto		# FT structures for input, output files
pointer infile1, infile2, outfile
bool	getreal1, getreal2, getimag1, getimag2, crereal, creimag
bool	pad			# use sum of sizes of input images?
bool	verbose			# print file names?
bool	clgetb()

begin
	call smark (sp)

	call salloc (infile1, SZ_FNAME, TY_CHAR)
	call salloc (infile2, SZ_FNAME, TY_CHAR)
	call salloc (outfile, SZ_FNAME, TY_CHAR)

	# Allocate Fourier structure for input & output images.
	call ft_struct_open (fti1)
	call ft_struct_open (fti2)
	call ft_struct_open (fto)

	# Get file names.
	call clgstr ("input1", Memc[infile1], SZ_FNAME)
	call clgstr ("input2", Memc[infile2], SZ_FNAME)
	call clgstr ("output", Memc[outfile], SZ_FNAME)

	if (Memc[outfile] == EOS)
	    call error (1, "no output specified")

	# Get processing options.
	getreal1 = clgetb ("inreal1")
	getimag1 = clgetb ("inimag1")
	getreal2 = clgetb ("inreal2")
	getimag2 = clgetb ("inimag2")
	crereal = clgetb ("outreal")
	creimag = clgetb ("outimag")

	# Should we make the size of the array for FT the sum of the
	# sizes of the input images?
	pad = clgetb ("pad")

	verbose = clgetb ("verbose")

	# Open files as requested.  The first input file is used as template
	# for creating the output file.
	call ft_open_files_i (fti1, Memc[infile1], getreal1, getimag1)
	call ft_open_files_i (fti2, Memc[infile2], getreal2, getimag2)
	call ft_open_files_o (fti1, fto, Memc[outfile], crereal, creimag)

	# Print file names.
	if (verbose) {
	    call ft_ri_print (fti1, "input1")
	    call ft_ri_print (fti2, "input2")
	    call ft_ri_print (fto, "output")
	    call flush (STDOUT)
	}

	# Perform convolution.
	switch (FT_NAXIS(fti1)) {
	case 1:
	    call ft_1_conv (fti1, fti2, fto, pad)
	case 2:
	    call ft_2_conv (fti1, fti2, fto, pad)
	default:
	    call ft_error (FT_BADNAXIS, FT_FATAL)
	}

	if (FT_REPT(fto) != NULL) {
	    call sprintf (IM_TITLE(FT_REPT(fto)), SZ_IMTITLE,
			"Re (convolution:  %s %s)")
		call pargstr (Memc[infile1])
		call pargstr (Memc[infile2])
	}

	if (FT_IMPT(fto) != NULL) {
	    call sprintf (IM_TITLE(FT_IMPT(fto)), SZ_IMTITLE,
			"Im (convolution:  %s %s)")
		call pargstr (Memc[infile1])
		call pargstr (Memc[infile2])
	}

	# Close files and clean up.
	call ft_close_files (fti1)
	call ft_close_files (fti2)
	call ft_close_files (fto)

	call ft_struct_close (fto)
	call ft_struct_close (fti1)
	call ft_struct_close (fti2)
	call sfree (sp)
end

# ft_1_conv -- 1-dimensional convolution

procedure ft_1_conv (fti1, fti2, fto, pad)

pointer fti1		# i: FFT pointer; input image
pointer fti2		# i: FFT pointer; input PSF image
pointer fto		# i: FFT pointer; output image
bool	pad		# i: use sum of sizes of input images?
#--

begin
	call ft_small_v (fti1, fti2, fto, pad)
end

# ft_2_conv -- 2-dimensional convolution

procedure ft_2_conv (fti1, fti2, fto, pad)

pointer fti1		# i: FFT pointer; input image
pointer fti2		# i: FFT pointer; input PSF image
pointer fto		# i: FFT pointer; output image
bool	pad		# i: use sum of sizes of input images?
#--

begin
	call ft_small_v (fti1, fti2, fto, pad)
end
