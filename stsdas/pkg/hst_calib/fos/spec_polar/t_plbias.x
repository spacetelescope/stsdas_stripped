include	<imhdr.h>

procedure t_plbias ()

# PLBIAS -- This routine recomputes the value of linear polarization (PL)
# in an FOS c3h dataset correcting for the bias introduced by the fact that
# Q-squared and U-squared are always positive.  The bias-corrected value
# of PL is computed as: 
#                         PL_corr = sqrt ( PL**2 - PL_err**2 );
#
# which is equivalent to: 
#                         PL_corr = sqrt ( (Q/I)**2 + (U/I)**2 - PL_err**2 ).
#
# The input image MUST be in FOS c3h file format.
#
# Dec 93: H.A.Bushouse - Original implementation. (STSDAS v1.3.1)
#
# Task parameters
char	input[SZ_LINE]		# Input image list
char	output[SZ_LINE]		# Output image list
char	wrap[SZ_LINE]		# Wrap option
bool	verbose

# Local variables
pointer	in_image, out_image
pointer iflux, oflux
pointer	root, extn
pointer	in_ptr, out_ptr
pointer	PL, PLerr
int	in_list, out_list, npix, ngroups, ig, i, j
real	datamin, datamax

# IRAF Functions
bool	clgetb(), streq()
int	imtopen(), imtgetim(), imtlen(), immap(), imgl1r(), impl1r(), imgeti()
pointer	sp

begin

	# Allocate memory for image name strings
	call smark (sp)
	call salloc (in_image, SZ_FNAME, TY_CHAR)
	call salloc (out_image, SZ_FNAME, TY_CHAR)
	call salloc (iflux, SZ_FNAME, TY_CHAR)
	call salloc (oflux, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)

	# Get input image list
	call clgstr ("input", input, SZ_LINE)

	# Get output image list
	call clgstr ("output", output, SZ_LINE)

	# Get the wrap option setting
	call clgstr ("wrap", wrap, SZ_LINE)

	verbose = clgetb ("verbose")

	# Expand the input and output image lists
	in_list  = imtopen (input)
	out_list = imtopen (output)

	# Make sure input/output lists are same length
	if (imtlen(in_list) != imtlen(out_list)) {
	    call imtclose (in_list)  
	    call imtclose (out_list)
	    call error (1, "Number of input and output images not the same")
	}

	# Loop over the list of images

        call iki_init ()
	while (imtgetim(in_list,  Memc[in_image],  SZ_FNAME) != EOF &&
	       imtgetim(out_list, Memc[out_image], SZ_FNAME) != EOF) {

	       # Open the input image
	       call iki_parse (Memc[in_image], Memc[root], Memc[extn])
	       if (streq (Memc[extn], "")) 
		   call strcat (".c3h", Memc[in_image], SZ_FNAME)

	       in_ptr = immap (Memc[in_image], READ_ONLY, 0)

	       # Get the number of pixels per spectrum and the
	       # the number of data groups in the image
	       npix    = IM_LEN (in_ptr, 1)
	       ngroups = imgeti (in_ptr, "GCOUNT")

	       # Make sure this is an appropriate polarimetry c3h image
	       if (ngroups != 56) {
		   call imunmap (in_ptr)
		   call eprintf (" %s is not an appropriate image; skipping.\n")
			call pargstr (Memc[in_image]); call flush (STDERR)
		   next 
	       }

	       # Copy the input image into the output image
	       call iki_parse (Memc[out_image], Memc[root], Memc[extn])
	       if (streq (Memc[extn], "")) 
		   call strcat (".c3h", Memc[out_image], SZ_FNAME)

	       call imcopy (Memc[in_image], Memc[out_image])

	       if (verbose) {
		   call printf ("%s -> %s\n")
			call pargstr (Memc[in_image])
			call pargstr (Memc[out_image])
		   call flush (STDOUT)
	       }

	       # Open the output image
	       out_ptr = immap (Memc[out_image], READ_WRITE, 0)

	       # Allocate dynamic memory for the spectra
	       call malloc (PL,    npix, TY_REAL)
	       call malloc (PLerr, npix, TY_REAL)

	       # Loop over the 4 sets of Stokes parameter spectra
	       do j = 1, 4, 1 {

		  # Read the PL and PLerr spectra for this set
		  ig = 14*(j-1)
		  call gf_opengr (out_ptr, ig+12, datamin, datamax, 0)
		  call amovr (Memr[imgl1r(out_ptr)], Memr[PLerr], npix)
		  call gf_opengr (out_ptr, ig+9,  datamin, datamax, 0)
		  call amovr (Memr[imgl1r(out_ptr)], Memr[PL], npix)

		  # Compute the corrected value of PL
		  do i = 0, npix-1 {
		     Memr[PL+i] = 
			 Memr[PL+i]*Memr[PL+i] - Memr[PLerr+i]*Memr[PLerr+i]

		     if (Memr[PL+i] >= 0.0) {
			 Memr[PL+i] = sqrt (Memr[PL+i])

		     } else {
			 if (streq(wrap,"positive"))
			     Memr[PL+i] = sqrt (-1.0*Memr[PL+i])
			 else if (streq(wrap,"zero"))
			     Memr[PL+i] = 0.0
			 else if (streq(wrap,"negative"))
			     Memr[PL+i] = -1.0 * sqrt (-1.0*Memr[PL+i])
		     }
		  }

		  # Write out the corrected PL spectrum for this set
		  call amovr (Memr[PL], Memr[impl1r(out_ptr)], npix)
		  call imflush (out_ptr)

	       } # Next set of Stokes spectra

	       # Deallocate dynamic memory
	       call mfree (PL,    TY_REAL)
	       call mfree (PLerr, TY_REAL)

	       # Close this pair of input and output images
	       call imunmap (in_ptr)
	       call imunmap (out_ptr)
	}

	# All done; close image lists and free memory
	call imtclose (in_list)
	call imtclose (out_list)
	call sfree (sp)

end
