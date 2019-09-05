include	<imhdr.h>

procedure t_polnorm ()

# POLNORM -- This routine normalizes the Stokes Q, U, and V parameter spectra
# in an FOS c3h image by the Stokes I parameter (flux) spectrum.  The error
# spectrum associated with each Stokes parameter is also normalized as
# follows:
#          norm_qerr = sqrt ( (Qerr/I)**2 + (Ierr*Q/I**2)**2 )
#
# and similarly for u, uerr, and v, verr.
# The input image MUST be in FOS c3h file format.
#
# Jun 93: H.A.Bushouse - Original implementation.  Pixel values that would
#         cause floating overflows in calculations are set to INDEF.
#         (STSDAS v1.3)
#
# Nov 93: HAB - Use 'stoke_norm' subroutine to calculate normalized Stokes 
#         spectra (got rid of 'stoke_err').  Replaces bad pixels with zeros, 
#         instead of INDEFs as before.  (STSDAS v1.3.1)
#
# Aug 94: HAB - Fixed bug in 'stoke_norm' that was giving incorrect
#	  normalized error values. (STSDAS v.1.3.3)

# Task parameters
char	input[SZ_LINE]		# Input image list
char	output[SZ_LINE]		# Output image list
bool	verbose

# Local variables
pointer	in_image, out_image
pointer iflux, oflux
pointer	root, extn
pointer	in_ptr, out_ptr
pointer	I, Q, U, V, Ierr, Qerr, Uerr, Verr
int	in_list, out_list, npix, ngroups, ig, j
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
	       call malloc (I, npix, TY_REAL)
	       call malloc (Q, npix, TY_REAL)
	       call malloc (U, npix, TY_REAL)
	       call malloc (V, npix, TY_REAL)
	       call malloc (Ierr, npix, TY_REAL)
	       call malloc (Qerr, npix, TY_REAL)
	       call malloc (Uerr, npix, TY_REAL)
	       call malloc (Verr, npix, TY_REAL)

	       # Loop over the 4 sets of Stokes parameter spectra
	       do j = 1, 4, 1 {

	       # Read the Stokes spectra for this set
	       ig = 14*(j-1)
	       call gf_opengr (in_ptr, ig+1, datamin, datamax, 0)
	       call amovr (Memr[imgl1r(in_ptr)], Memr[I], npix)
	       call gf_opengr (in_ptr, ig+2, datamin, datamax, 0)
	       call amovr (Memr[imgl1r(in_ptr)], Memr[Q], npix)
	       call gf_opengr (in_ptr, ig+3, datamin, datamax, 0)
	       call amovr (Memr[imgl1r(in_ptr)], Memr[U], npix)
	       call gf_opengr (in_ptr, ig+4, datamin, datamax, 0)
	       call amovr (Memr[imgl1r(in_ptr)], Memr[V], npix)
	       call gf_opengr (in_ptr, ig+5, datamin, datamax, 0)
	       call amovr (Memr[imgl1r(in_ptr)], Memr[Ierr], npix)
	       call gf_opengr (in_ptr, ig+6, datamin, datamax, 0)
	       call amovr (Memr[imgl1r(in_ptr)], Memr[Qerr], npix)
	       call gf_opengr (in_ptr, ig+7, datamin, datamax, 0)
	       call amovr (Memr[imgl1r(in_ptr)], Memr[Uerr], npix)
	       call gf_opengr (in_ptr, ig+8, datamin, datamax, 0)
	       call amovr (Memr[imgl1r(in_ptr)], Memr[Verr], npix)

	       # Normalize the Stokes QUV spectra and their errors
	       call stoke_norm (Memr[Q], Memr[Qerr], Memr[I], Memr[Ierr], npix)
	       call stoke_norm (Memr[U], Memr[Uerr], Memr[I], Memr[Ierr], npix)
	       call stoke_norm (Memr[V], Memr[Verr], Memr[I], Memr[Ierr], npix)

	       # Write out the normalized QUV spectra for this set
	       call gf_opengr (out_ptr, ig+2, datamin, datamax, 0)
	       call amovr (Memr[Q], Memr[impl1r(out_ptr)], npix)
	       call gf_opengr (out_ptr, ig+3, datamin, datamax, 0)
	       call amovr (Memr[U], Memr[impl1r(out_ptr)], npix)
	       call gf_opengr (out_ptr, ig+4, datamin, datamax, 0)
	       call amovr (Memr[V], Memr[impl1r(out_ptr)], npix)
	       call gf_opengr (out_ptr, ig+6, datamin, datamax, 0)
	       call amovr (Memr[Qerr], Memr[impl1r(out_ptr)], npix)
	       call gf_opengr (out_ptr, ig+7, datamin, datamax, 0)
	       call amovr (Memr[Uerr], Memr[impl1r(out_ptr)], npix)
	       call gf_opengr (out_ptr, ig+8, datamin, datamax, 0)
	       call amovr (Memr[Verr], Memr[impl1r(out_ptr)], npix)

	       } # Next set of Stokes spectra

	       # Deallocate dynamic memory
	       call mfree (I, TY_REAL)
	       call mfree (Q, TY_REAL)
	       call mfree (U, TY_REAL)
	       call mfree (V, TY_REAL)
	       call mfree (Ierr, TY_REAL)
	       call mfree (Qerr, TY_REAL)
	       call mfree (Uerr, TY_REAL)
	       call mfree (Verr, TY_REAL)

	       # Close this pair of input and output images
	       call imunmap (in_ptr)
	       call imunmap (out_ptr)
	}

	# All done; close image lists and free memory
	call imtclose (in_list)
	call imtclose (out_list)
	call sfree (sp)

end
