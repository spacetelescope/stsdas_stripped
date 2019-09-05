include	<imhdr.h>

procedure t_deaccum ()

# Task parameters
char	input[SZ_LINE]		# Input image list
char	output[SZ_LINE]		# Output image list
bool	verbose

# Local variables
pointer	in_image, out_image
pointer	root, extn, bunit, filtyp, ctype2
pointer flux1, flux2
pointer	expo
pointer	in_ptr, out_ptr
int	in_list, out_list, npix, ngroups, nread, j
real	datamin, datamax, mean, sigma
real	errfcn
bool	calib, fits

extern	errfcn

# IRAF Functions
bool	clgetb(), streq()
int	imtopen(), imtgetim(), imtlen(), immap(), imgl1r(), impl1r(), imgeti()
int	imaccf(), imgl2r(), impl2r()
real	imgetr()
pointer	sp

begin

	# Allocate memory for image name strings
	call smark (sp)
	call salloc (in_image, SZ_FNAME, TY_CHAR)
	call salloc (out_image, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (bunit, SZ_LINE, TY_CHAR)
	call salloc (filtyp, SZ_LINE, TY_CHAR)
	call salloc (ctype2, SZ_LINE, TY_CHAR)
	call salloc (expo, 2, TY_REAL)

	# Get input image list
	call clgstr ("input", input, SZ_LINE)

	# Get output image list
	call clgstr ("output", output, SZ_FNAME)

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

	while (imtgetim(in_list,  Memc[in_image],  SZ_FNAME) != EOF &&
	       imtgetim(out_list, Memc[out_image], SZ_FNAME) != EOF) {

	       # Open the input image
	       in_ptr = immap (Memc[in_image], READ_ONLY, 0)

	       # Get the number of pixels per spectrum and the
	       # the number of data groups in the image
	       if (imaccf(in_ptr, "GCOUNT") == YES) {
		   ngroups = imgeti (in_ptr, "GCOUNT")
		   fits = false
	       } else {
		   if (imaccf(in_ptr, "CTYPE2") == YES) {
		       call imgstr (in_ptr, "CTYPE2", Memc[ctype2], SZ_LINE)
		       if (streq(Memc[ctype2], "GROUP_NUMBER")) {
			   ngroups = IM_LEN (in_ptr, 2)
			   fits = true
		       } else {
			   call imunmap (in_ptr)
			   call eprintf (" %s is not multigroup; skipping.\n")
				call pargstr (Memc[in_image])
			        call flush(STDERR)
			   next
		       }
		   } else {
		       call imunmap (in_ptr)
		       call eprintf ("Cannot determine group format of %s; skipping.\n")
			    call pargstr (Memc[in_image])
			    call flush(STDERR)
		       next
		   }
	       }
		       
	       npix    = IM_LEN (in_ptr, 1)
	       nread   = imgeti (in_ptr, "NREAD")

	       # Skip if not an ACCUM image
	       if (ngroups==1 || nread==1 || ngroups!=nread) {
		   call imunmap (in_ptr)
		   call eprintf (" %s is not an ACCUM image; skipping.\n")
			call pargstr (Memc[in_image])
			call flush (STDERR)
		   next
	       }

	       # Skip if not flux data
	       call iki_parse (Memc[in_image], Memc[root], Memc[extn])
	       call imgstr (in_ptr, "BUNIT", Memc[bunit], SZ_LINE)
	       call imgstr (in_ptr, "FILETYPE", Memc[filtyp], SZ_LINE)

	       if (streq(Memc[extn],"cqh") || streq(Memc[filtyp],"CDQ") ||
		   streq(Memc[extn],"q0h") || streq(Memc[filtyp],"SDQ")) {
		   call imunmap (in_ptr)
		   call eprintf (" Cannot process data quality image %s; skipping.\n")
			call pargstr (Memc[in_image])
			call flush (STDERR)
		   next
	       } else if (streq(Memc[extn],"c0h") || streq(Memc[filtyp],"WAV")
		       || streq(Memc[bunit],"ANGSTROMS")) {
		   call imunmap (in_ptr)
		   call eprintf (" Cannot process wavelength image %s; skipping.\n")
			call pargstr (Memc[in_image])
			call flush (STDERR)
		   next
	       } else if (streq(Memc[bunit],"")) {
		   call imunmap (in_ptr)
		   call eprintf (" %s has unknown data units; skipping.\n")
			call pargstr (Memc[in_image])
			call flush (STDERR)
		   next
	       } else if (streq(Memc[extn],"d0h") || 
			  streq(Memc[bunit],"COUNTS")) {
		   calib = false
	       } else {
		   calib = true
	       }

	       # Copy the input image into the output image
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
	       call malloc (flux1, npix, TY_REAL)
	       call malloc (flux2, npix, TY_REAL)

	       # Loop over the spectral groups
	       do j = ngroups, 2, -1 {

		  # Read the spectra
		  if (fits) {
		      call amovr (Memr[imgl2r(out_ptr,j-1)], Memr[flux1], npix)
		      call amovr (Memr[imgl2r(out_ptr,j)],   Memr[flux2], npix)
		  } else {
		      call gf_opengr (out_ptr, j-1, datamin, datamax, 0)
		      call amovr (Memr[imgl1r(out_ptr)], Memr[flux1], npix)
		      Memr[expo]   = imgetr (out_ptr, "EXPOSURE")
		      call gf_opengr (out_ptr, j,   datamin, datamax, 0)
		      call amovr (Memr[imgl1r(out_ptr)], Memr[flux2], npix)
		      Memr[expo+1] = imgetr (out_ptr, "EXPOSURE")
		  }

		  # If data is calibrated, scale by group number
		  if (calib) {
		      call amulkr (Memr[flux2], real(j),   Memr[flux2], npix)
		      call amulkr (Memr[flux1], real(j-1), Memr[flux1], npix)
		  }

		  # Subtract; if error data, then subtract in quadrature
		  if (streq(Memc[filtyp],"ERR") || streq(Memc[extn],"c2h")) {
		      call aavgr (Memr[flux2], npix, mean, sigma)
		      call amulkr(Memr[flux2], 1./mean, Memr[flux2], npix)
		      call amulr (Memr[flux2], Memr[flux2], Memr[flux2], npix)

		      call amulkr(Memr[flux1], 1./mean, Memr[flux1], npix)
		      call amulr (Memr[flux1], Memr[flux1], Memr[flux1], npix)

		      call asubr (Memr[flux2], Memr[flux1], Memr[flux2], npix)
		      call asqrr (Memr[flux2], Memr[flux2], npix, errfcn)

		      call amulkr(Memr[flux2], mean, Memr[flux2], npix)

		  } else {
		      call asubr (Memr[flux2], Memr[flux1], Memr[flux2], npix)
		  }

		  # Clip count and countrate data at zero
		  if (streq(Memc[bunit],"COUNTS") || 
		      streq(Memc[bunit],"COUNTS/S"))
		      call amaxkr (Memr[flux2], 0.0, Memr[flux2], npix)

		  # Write out the subtracted spectrum
		  if (fits) {
		      call amovr (Memr[flux2], Memr[impl2r(out_ptr,j)], npix)
		  } else {
		      call amovr (Memr[flux2], Memr[impl1r(out_ptr)], npix)
		      call imputr (out_ptr, "EXPOSURE", Memr[expo+1]-Memr[expo])
		  }
		  call imflush (out_ptr)

	       } # Next group

	       # Deallocate dynamic memory
	       call mfree (flux1, TY_REAL)
	       call mfree (flux2, TY_REAL)

	       # Close this pair of input and output images
	       call imunmap (in_ptr)
	       call imunmap (out_ptr)
	}

	# All done; close image lists and free memory
	call imtclose (in_list)
	call imtclose (out_list)
	call sfree (sp)

end
#
# ERRZERO -- Error function to deal with division by or sqrt of invalid argument

real procedure errfcn( x )

real x

begin
   return 0.0
end
