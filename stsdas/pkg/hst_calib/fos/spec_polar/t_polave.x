include	<imhdr.h>

procedure t_polave ()
# 
# POLAVE -- This routine averages the Stokes I,Q,U,V spectra amongst
# multiple FOS c3h images.  The user has the option of either uniform
# weighting of the averaged IQUV data, or variance weighting, i.e. 
# weighting by 1/sigma**2, where sigma is the error value associated
# the IQUV data.  After computing the averaged IQUV data, the linear and
# circular polarization, and polarization position angle data are
# recomputed from the averaged Stokes spectra.
#
# Jun 93: H.A. Bushouse - Original implementation.  Set polarization
#         spectra to zero in output image.  Output image contains
#         averaged Stokes spectra only. Uniform weighting only. (STSDAS v1.3)
#
# Nov 93: HAB - Added weighting option and recalculate polarization spectra
#         from averaged Stokes spectra. (STSDAS v1.3.1)
#

# Task parameters
char	imtlist[SZ_LINE]	# Input image list
char	output[SZ_FNAME]	# Output image name
char	weight[SZ_LINE]		# Type of weighting to be used
bool	verbose

# Local variables
pointer	input, iroot, iextn, oroot, oextn
pointer	ifbuf, ofbuf, errmsg
pointer wgt, tmp
int	i, list, ngroups, npix, ig, stat, next_flx
int	flx_list[16]
real	datamin, datamax

bool	streq(), clgetb()
int	imtopen(), imtgetim(), immap(), imgl1r(), impl1r(), imgeti()
pointer	sp
real	errzero

extern	errzero

data	flx_list /1,2,3,4,15,16,17,18,29,30,31,32,43,44,45,46/

begin

	# Allocate memory for image name strings
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (iroot, SZ_FNAME, TY_CHAR)
	call salloc (oroot, SZ_FNAME, TY_CHAR)
	call salloc (iextn, SZ_FNAME, TY_CHAR)
	call salloc (oextn, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Get input image list
	call clgstr ("input", imtlist, SZ_LINE)

	# Get output image name
	call clgstr ("output", output, SZ_FNAME)
        call iki_init ()
	call iki_parse (output, Memc[oroot], Memc[oextn])

	# Get the type of weighting desired
	call clgstr ("weight", weight, SZ_LINE)

	verbose = clgetb ("verbose")

	# Expand the input image list
	list = imtopen (imtlist)

	# Get first input image name
	stat = imtgetim(list, Memc[input], SZ_FNAME)
	call iki_parse (Memc[input], Memc[iroot], Memc[iextn])
	if (streq( Memc[iextn], "")) {
	    call strcpy ("c3h", Memc[iextn], SZ_FNAME)
	    call strcat (".c3h", Memc[input], SZ_FNAME)
	}
	call strcpy (Memc[oroot], output, SZ_FNAME)
	call strcat (".", output, SZ_FNAME)
	call strcat (Memc[iextn], output, SZ_FNAME)

	# Open the first input image
	ifbuf = immap (Memc[input], READ_ONLY, 0)

	# Get the number of pixels per spectrum and the
	# the number of data groups in the images
	npix    = IM_LEN (ifbuf, 1)
	ngroups = imgeti (ifbuf, "GCOUNT")
	call imunmap (ifbuf)

	# If the number of groups is not 56, this is not a valid c3h image
	if (ngroups != 56) {
	    call imtclose (list)
	    call sprintf (Memc[errmsg], SZ_LINE, " %s is not an appropriate image")
	    call pargstr (Memc[input])
	    call error (1, Memc[errmsg])
	}

	# Copy the first input image into the output image
	call imcopy (Memc[input], output)
	if (verbose) {
	    call printf ("%s -> %s\n")
		 call pargstr (Memc[input])
		 call pargstr (output)
	    call flush (STDOUT)
	}

	# Open the output image
	ofbuf = immap (output, READ_WRITE, 0)

	# Allocate dynamic memory for temporary storage arrays
	call malloc (wgt, npix, TY_REAL)
	call malloc (tmp, npix, TY_REAL)

	# Compute the weighted flux values for the first image
	next_flx = 1
	do ig = 1, ngroups, 1 {
	     if (ig == flx_list[next_flx]) {

		 # Read the input error values and square them.
	         call gf_opengr (ofbuf, ig+4, datamin, datamax, 0)
		 call apowkr (Memr[imgl1r(ofbuf)], 2, Memr[tmp], npix)

		 # Compute weights from the error values: For variance
		 # weighting, weight=1/error**2 for good pixels, or zero
		 # for bad pixels.
		 if (streq (weight, "variance")) {
		     #call arczr (1.0, Memr[tmp], Memr[tmp], npix, errzero)
		     do i = 0, npix-1 {
			if (Memr[tmp+i] != 0.0) Memr[tmp+i] = 1.0/Memr[tmp+i]
		     }
		     call amovr (Memr[tmp], Memr[wgt], npix)

		 # For uniform weighting, weight=1 for good pixels, 0 for bad.
		 } else {
		     call amovr (Memr[tmp], Memr[wgt], npix)
		     do i = 0, npix-1 {
			if (Memr[wgt+i] != 0.0) Memr[wgt+i] = 1.0
		     }
		 }

		 # Store the errors and weights in the output image.
		 call amovr (Memr[tmp], Memr[impl1r(ofbuf)], npix)
		 call gf_opengr (ofbuf, ig+8, datamin, datamax, 0)
		 call amovr (Memr[wgt], Memr[impl1r(ofbuf)], npix)

		 # Read the input fluxes, multiply by the weights, and
		 # store results in output image.
		 call gf_opengr (ofbuf, ig, datamin, datamax, 0)
		 call amulr (Memr[imgl1r(ofbuf)], Memr[wgt], 
			     Memr[impl1r(ofbuf)], npix)
		 call imflush (ofbuf)

		 next_flx = next_flx + 1
	     }
	}

	# Loop over the list of remaining input images

	while (imtgetim(list, Memc[input], SZ_FNAME) != EOF) {

	       # Open the input image
	       call iki_parse (Memc[input], Memc[iroot], Memc[iextn])
	       if (streq( Memc[iextn], "")) {
		   call strcpy ("c3h", Memc[iextn], SZ_FNAME)
		   call strcat (".c3h", Memc[input], SZ_FNAME)
	       }
	       ifbuf = immap (Memc[input], READ_ONLY, 0)

	       # Check that each image is the proper size
	       if (IM_LEN(ifbuf,1) != npix || imgeti(ifbuf,"GCOUNT") != 56) {
		   call imunmap (ifbuf)
		   call eprintf (" %s is not the proper size; skipping.\n")
		   call pargstr (Memc[input]); call flush (STDERR)
		   next
	       }

	       if (verbose) {
		   call printf ("%s -> %s\n")
			call pargstr (Memc[input])
			call pargstr (output)
			call flush (STDOUT)
	       }

	       # Add each input group to the output image
	       next_flx = 1
	       do ig = 1, ngroups, 1 {

		    if (ig == flx_list[next_flx]) {

		    # Read the input errors and errors accumulated so far.
		    call gf_opengr (ifbuf, ig+4, datamin, datamax, 0)
		    call gf_opengr (ofbuf, ig+4, datamin, datamax, 0)
		    call apowkr (Memr[imgl1r(ifbuf)], 2, Memr[tmp], npix)

		    # Compute weights from the errors: For variance weighting
		    # weight=1/error**2 for good pixels, or 0 for bad pixels.
		    if (streq (weight, "variance")) {
			#call arczr (1.0, Memr[tmp], Memr[tmp], npix, errzero)
			do i = 0, npix-1 {
			   if (Memr[tmp+i] != 0.0) Memr[tmp+i] = 1.0/Memr[tmp+i]
			}
			call amovr (Memr[tmp], Memr[wgt], npix)

		    # For uniform weighing, weight=1 for good pixels, 0 for bad.
		    } else {
			call amovr (Memr[tmp], Memr[wgt], npix)
			do i = 0, npix-1 {
			   if (Memr[wgt+i] != 0.0) Memr[wgt+i] = 1.0
			}
		    }

		    # Add the errors and weights to the running sums in 
		    # the output image.
		    call aaddr (Memr[tmp], Memr[imgl1r(ofbuf)], 
				Memr[impl1r(ofbuf)], npix)
		    call gf_opengr (ofbuf, ig+8, datamin, datamax, 0)
		    call aaddr (Memr[wgt], Memr[imgl1r(ofbuf)],
				Memr[impl1r(ofbuf)], npix)

		    # Read the input fluxes, multiply by weights, and
		    # add results to running sum in output image.
		    call gf_opengr (ifbuf, ig, datamin, datamax, 0)
		    call gf_opengr (ofbuf, ig, datamin, datamax, 0)
		    call amulr (Memr[imgl1r(ifbuf)], Memr[wgt], Memr[tmp], npix)
		    call aaddr (Memr[tmp], Memr[imgl1r(ofbuf)], 
				Memr[impl1r(ofbuf)], npix)
		    call imflush (ofbuf)

		    next_flx = next_flx + 1
		    }
	       }

	       # Close this input image
	       call imunmap (ifbuf)

	}

	# Calculate the mean output spectra

	next_flx = 1
	do ig = 1, ngroups, 1 {

	     # Calculate weigthed flux averages and average sigmas
	     if (ig == flx_list[next_flx]) {

		 # Reload the accumulated weights and fluxes.
		 call gf_opengr (ofbuf, ig+8, datamin, datamax, 0)
		 call amovr (Memr[imgl1r(ofbuf)], Memr[wgt], npix)
		 call gf_opengr (ofbuf, ig, datamin, datamax, 0)

		 # Divide sum of weighted fluxes by sum of weights.
		 call advzr (Memr[imgl1r(ofbuf)], Memr[wgt],
			     Memr[impl1r(ofbuf)], npix, errzero)

		 # Reload accumulated errors.
		 call gf_opengr (ofbuf, ig+4, datamin, datamax, 0)
		 call amovr (Memr[imgl1r(ofbuf)], Memr[tmp], npix)

		 # Calculate average errors: For variance weighting,
		 # average error = sqrt(1/errsum).
		 if (streq (weight, "variance")) {
		     #call arczr (1.0, Memr[tmp], Memr[tmp], npix, errzero)
		     do i = 0, npix-1 {
			if (Memr[tmp+i] != 0.0) Memr[tmp+i] = 1.0/Memr[tmp+i]
		     }
		     call asqrr (Memr[tmp], Memr[impl1r(ofbuf)], npix, errzero)

		 # For uniform weighting, average error = sqrt(errsum)/n.
		 } else {
		     call asqrr (Memr[tmp], Memr[tmp], npix, errzero)
		     call advzr (Memr[tmp], Memr[wgt], Memr[impl1r(ofbuf)],
				 npix, errzero)
		 }
		 call imflush (ofbuf)

		 next_flx = next_flx + 1
	     }
	}

	# Recalculate polarization spectra from averaged Stokes spectra
	call calcpol (ofbuf)

	# Close the output image
	call imunmap (ofbuf)

	# Free memory
	call mfree (tmp, TY_REAL)
	call mfree (wgt, TY_REAL)
	call imtclose(list)
	call sfree (sp)

end
