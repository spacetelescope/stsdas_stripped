include	<imhdr.h>

procedure t_pcombine ()
#
# PCOMBINE -- This routine averages multiple 1D HST spectral files;
# flux and error spectra are averaged, and data quality values are
# also propagated.  While originally written to work on FOS c1h, c2h, cqh
# images, the task is generally applicable to any 1D data of similar
# nature.  The user has the option of either uniform weighting of the
# averaged flux data, or variance weighting, i.e. weighting by 1/sigma**2,
# where sigma is the error value associated with the flux values.
# Pixels from individual spectra may be rejected from the averaging
# process by setting a data quality rejection threshold.  Any pixel having
# a DQ value greater than or equal to this threshold will be rejected.
#
# Jun 93 H.A.Bushouse - Original implementation.  Uniform weighting only
#        and no rejection process based on DQ value.  (STSDAS 1.3)
#
# Sep 93 HAB: Fixed output buffering bug by calling imflush.
#
# Nov 93 HAB: Added weighting and rejection algorithms. (STSDAS 1.3.1)
#

char	imtlist[SZ_LINE]	# Input image list
char	output[SZ_FNAME]	# Output image name
char	weight[SZ_LINE]		# Weighting method
int	rejlim			# Data quality rejection limit
bool	verbose

pointer	input, iroot, iext, oroot, oext, tmp_name
pointer iflux, ierr, iqual, oflux, oerr, oqual
pointer	ifbuf, iebuf, iqbuf, ofbuf, oebuf, oqbuf, tbuf
pointer	flux, err, dq, wgt, flxsum, errsum, dqsum, wgtsum
int	i, list, ngroups, npix, ig, stat
real	datamin, datamax

bool	clgetb(), streq()
int	clgeti()
int	imtopen(), imtgetim(), immap(), imgl1r(), impl1r(), imgeti()
pointer	sp
real	errzero

extern	errzero

begin

	# Allocate memory for image name strings
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (iroot, SZ_FNAME, TY_CHAR)
	call salloc (iext,  SZ_FNAME, TY_CHAR)
	call salloc (oroot, SZ_FNAME, TY_CHAR)
	call salloc (oext,  SZ_FNAME, TY_CHAR)
	call salloc (iflux, SZ_FNAME, TY_CHAR)
	call salloc (ierr,  SZ_FNAME, TY_CHAR)
	call salloc (iqual, SZ_FNAME, TY_CHAR)
	call salloc (oflux, SZ_FNAME, TY_CHAR)
	call salloc (oerr,  SZ_FNAME, TY_CHAR)
	call salloc (oqual, SZ_FNAME, TY_CHAR)
	call salloc (tmp_name, SZ_FNAME, TY_CHAR)

	# Get input image list
	call clgstr ("input", imtlist, SZ_LINE)

	# Get output image root name
	call clgstr ("output", output, SZ_FNAME)
        call iki_init ()
	call iki_parse (output, Memc[oroot], Memc[oext])

	# Get the type of weighting desired
	call clgstr ("weight", weight, SZ_LINE)

	# Get the data quality rejection limit value
	rejlim = clgeti ("rejlim")

	verbose = clgetb ("verbose")

	# Expand the input image list
	list = imtopen (imtlist)

	# Get first input image name
	stat = imtgetim(list, Memc[input], SZ_FNAME)
	call iki_parse (Memc[input], Memc[iroot], Memc[iext])

	# Copy the first input image into the output image
	call strcpy (Memc[iroot], Memc[iflux], SZ_FNAME)
	call strcat (".c1h", Memc[iflux], SZ_FNAME)
	call strcpy (Memc[oroot], Memc[oflux], SZ_FNAME)
	call strcat (".c1h", Memc[oflux], SZ_FNAME)
	call imcopy (Memc[iflux], Memc[oflux])
	if (verbose) {
	    call printf ("%s -> %s\n")
		 call pargstr (Memc[iflux])
		 call pargstr (Memc[oflux])
	    call flush (STDOUT)
	}

	call strcpy (Memc[iroot], Memc[ierr], SZ_FNAME)
	call strcat (".c2h", Memc[ierr], SZ_FNAME)
	call strcpy (Memc[oroot], Memc[oerr], SZ_FNAME)
	call strcat (".c2h", Memc[oerr], SZ_FNAME)
	call imcopy (Memc[ierr], Memc[oerr])
	if (verbose) {
	    call printf ("%s -> %s\n")
		 call pargstr (Memc[ierr])
		 call pargstr (Memc[oerr])
	    call flush (STDOUT)
	}

	call strcpy (Memc[iroot], Memc[iqual], SZ_FNAME)
	call strcat (".cqh", Memc[iqual], SZ_FNAME)
	call strcpy (Memc[oroot], Memc[oqual], SZ_FNAME)
	call strcat (".cqh", Memc[oqual], SZ_FNAME)
	call imcopy (Memc[iqual], Memc[oqual])
	if (verbose) {
	    call printf ("%s -> %s\n")
		 call pargstr (Memc[iqual])
		 call pargstr (Memc[oqual])
	    call flush (STDOUT)
	}

	# Open the output images
	ofbuf = immap (Memc[oflux], READ_WRITE, 0)
	oebuf = immap (Memc[oerr],  READ_WRITE, 0)
	oqbuf = immap (Memc[oqual], READ_WRITE, 0)

	# Get the number of pixels per spectrum and the
	# the number of data groups in the images
	npix    = IM_LEN (ofbuf, 1)
	ngroups = imgeti (ofbuf, "GCOUNT")

	# Allocate dynamic memory
	call malloc (flux, npix, TY_REAL)
	call malloc (err,  npix, TY_REAL)
	call malloc (dq,   npix, TY_REAL)
	call malloc (wgt,  npix, TY_REAL)
	call malloc (flxsum, npix, TY_REAL)
	call malloc (errsum, npix, TY_REAL)
	call malloc (dqsum,  npix, TY_REAL)
	call malloc (wgtsum, npix, TY_REAL)

	# Open temporary image to store sum of weights
	call mktemp ("tmp$weights", Memc[tmp_name], SZ_FNAME)
	call strcat (".c2h", Memc[tmp_name], SZ_FNAME)
	call imcopy (Memc[oerr], Memc[tmp_name], SZ_FNAME)
	tbuf = immap (Memc[tmp_name], READ_WRITE, 0)

	# Compute the weighted flux and error values for the first image
	do ig = 1, ngroups, 1 {

	     # Read the input flux, error, and dq spectra
	     call gf_opengr (ofbuf, ig, datamin, datamax, 0)
	     call amovr (Memr[imgl1r(ofbuf)], Memr[flux], npix)
	     call gf_opengr (oebuf, ig, datamin, datamax, 0)
	     call amovr (Memr[imgl1r(oebuf)], Memr[err], npix)
	     call gf_opengr (oqbuf, ig, datamin, datamax, 0)
	     call amovr (Memr[imgl1r(oqbuf)], Memr[dq], npix)
	     call gf_opengr (tbuf, ig, datamin, datamax, 0)

	     # Square the errors
	     call amulr (Memr[err], Memr[err], Memr[err], npix)

	     # Calculate weights from the errors
	     do i = 0, npix-1 {
		
		# For variance weighting, set weight=1/error**2 for good
		# pixels and 0 for bad pixels.
		if (streq (weight, "variance")) {
		    if (Memr[err+i] != 0.0) Memr[err+i] = 1.0 / Memr[err+i]
		    Memr[wgt+i] = Memr[err+i]

		# For uniform weighting, set weight=1 for good pixels
		# and 0 for bad pixels.
		} else {
		    Memr[wgt+i] = Memr[err+i]
		    if (Memr[wgt+i] != 0.0) Memr[wgt+i] = 1.0
		}

		# Set weight and error to zero if rejecting this pixel.
		if (rejlim > 0 && Memr[dq+i] >= rejlim) {
		    Memr[wgt+i] = 0.0
		    Memr[err+i] = 0.0
		}
	     }

	     # Compute weighted fluxes and accumulate fluxes, errors,
	     # dq's, and weights.
	     call amulr (Memr[flux], Memr[wgt], Memr[flxsum], npix)
	     call amovr (Memr[err], Memr[errsum], npix)
	     call amovr (Memr[dq],  Memr[dqsum],  npix)
	     call amovr (Memr[wgt], Memr[wgtsum], npix)

	     # Save accumulations in output image.
	     call amovr (Memr[flxsum], Memr[impl1r(ofbuf)], npix)
	     call imflush (ofbuf)
	     call amovr (Memr[errsum], Memr[impl1r(oebuf)], npix)
	     call imflush (oebuf)
	     call amovr (Memr[dqsum],  Memr[impl1r(oqbuf)], npix)
	     call imflush (oqbuf)
	     call amovr (Memr[wgtsum], Memr[impl1r(tbuf)], npix)
	     call imflush (tbuf)
	}

	# Loop over the list of remaining input images

	while (imtgetim(list, Memc[input], SZ_FNAME) != EOF) {

	       # Open the input images
	       call iki_parse (Memc[input],Memc[iroot],Memc[iext])
	       call strcpy (Memc[iroot], Memc[iflux], SZ_FNAME)
	       call strcpy (Memc[iroot], Memc[ierr],  SZ_FNAME)
	       call strcpy (Memc[iroot], Memc[iqual], SZ_FNAME)
	       call strcat (".c1h", Memc[iflux], SZ_FNAME)
	       call strcat (".c2h", Memc[ierr],  SZ_FNAME)
	       call strcat (".cqh", Memc[iqual], SZ_FNAME)

	       ifbuf = immap (Memc[iflux], READ_ONLY, 0)
	       iebuf = immap (Memc[ierr],  READ_ONLY, 0)
	       iqbuf = immap (Memc[iqual], READ_ONLY, 0)

	       # Check that each input image is the proper size
	       if (IM_LEN(ifbuf,1)!=npix || imgeti(ifbuf,"GCOUNT")!=ngroups) {
		   call imunmap (ifbuf)
		   call imunmap (iebuf)
		   call imunmap (iqbuf)
		   call eprintf (" %s is not the proper size; skipping.\n")
		   call pargstr (Memc[input]); call flush (STDERR)
		   next
	       }

	       if (verbose) {
		   call printf ("%s -> %s\n")
			call pargstr (Memc[iflux])
			call pargstr (Memc[oflux])
		   call printf ("%s -> %s\n")
			call pargstr (Memc[ierr])
			call pargstr (Memc[oerr])
		   call printf ("%s -> %s\n")
			call pargstr (Memc[iqual])
			call pargstr (Memc[oqual])
			call flush (STDOUT)
	       }

	       # Accumulate the weights and weighted fluxes for each group

	       do ig = 1, ngroups, 1 {

		  # Read the input flux, error, and dq spectra.
		  call gf_opengr (ifbuf, ig, datamin, datamax, 0)
		  call amovr (Memr[imgl1r(ifbuf)], Memr[flux], npix)
		  call gf_opengr (iebuf, ig, datamin, datamax, 0)
		  call amovr (Memr[imgl1r(iebuf)], Memr[err], npix)
		  call gf_opengr (iqbuf, ig, datamin, datamax, 0)
		  call amovr (Memr[imgl1r(iqbuf)], Memr[dq], npix)

		  # Reload the flux, error, dq, and weights accumulated so far.
		  call gf_opengr (ofbuf, ig, datamin, datamax, 0)
		  call amovr (Memr[imgl1r(ofbuf)], Memr[flxsum], npix)
		  call gf_opengr (oebuf, ig, datamin, datamax, 0)
		  call amovr (Memr[imgl1r(oebuf)], Memr[errsum], npix)
		  call gf_opengr (oqbuf, ig, datamin, datamax, 0)
		  call amovr (Memr[imgl1r(oqbuf)], Memr[dqsum], npix)
		  call gf_opengr (tbuf, ig, datamin, datamax, 0)
		  call amovr (Memr[imgl1r(tbuf)], Memr[wgtsum], npix)

		  # Square the input errors.
		  call amulr (Memr[err], Memr[err], Memr[err], npix)

		  # Calculate weights from the errors.
		  do i = 0, npix-1 {

		     # For variance weighting, set weight=1/error**2 for good
		     # pixels and 0 for bad pixels.
		     if (streq (weight, "variance")) {
			 if (Memr[err+i]!=0.0) Memr[err+i]=1.0/Memr[err+i]
			 Memr[wgt+i] = Memr[err+i]

		     # For uniform weighting, set weight=1 for good pixels
		     # and 0 for bad pixels.
		     } else {
			 Memr[wgt+i] = Memr[err+i]
			 if (Memr[wgt+i] != 0.0) Memr[wgt+i] = 1.0
		     }

		     # Rejection mode on:
		     if (rejlim > 0) {

			 # This is a bad pixel; set weight and error to zero.
			 if (Memr[dq+i] >= rejlim) {
			     Memr[wgt+i] = 0.0
			     Memr[err+i] = 0.0

			     # If all points rejected so far, set dq to
			     # max dq of all points so far

			     if (Memr[dqsum+i] >= rejlim)
				 Memr[dqsum+i] = max(Memr[dqsum+i],Memr[dq+i])

			 # This is a good pixel:
			 } else {

			     # If all previous points rejected, set dq to
			     # dq value of this good point

			     if (Memr[dqsum+i] >= rejlim) {
				 Memr[dqsum+i] = Memr[dq+i] 
			     } else {

			     # If not all previous points rejected, set dq to
			     # max dq of all good points

				 Memr[dqsum+i] = max(Memr[dqsum+i],Memr[dq+i])
			     }
			 }

		     # Rejection mode off:
		     } else {

			 # Set dq to max dq of all points so far.

			 Memr[dqsum+i] = max (Memr[dqsum+i], Memr[dq+i])
		     }
		  }

		  # Accumulate current values into running sums
		  call amulr (Memr[flux], Memr[wgt], Memr[flux], npix)
		  call aaddr (Memr[flux], Memr[flxsum], Memr[flxsum], npix)
		  call aaddr (Memr[err],  Memr[errsum], Memr[errsum], npix)
		  call aaddr (Memr[wgt],  Memr[wgtsum], Memr[wgtsum], npix)

		  # Write running sums to output files
		  call amovr (Memr[flxsum], Memr[impl1r(ofbuf)], npix)
		  call imflush (ofbuf)
		  call amovr (Memr[errsum], Memr[impl1r(oebuf)], npix)
		  call imflush (oebuf)
		  call amovr (Memr[dqsum],  Memr[impl1r(oqbuf)], npix)
		  call imflush (oqbuf)
		  call amovr (Memr[wgtsum], Memr[impl1r(tbuf)],  npix)
		  call imflush (tbuf)
	       }

	       # Close this set of input images
	       call imunmap (ifbuf)
	       call imunmap (iebuf)
	       call imunmap (iqbuf)
	}

	# Calculate the mean output spectra

	do ig = 1, ngroups, 1 {

	   # First calculate average fluxes by dividing sum of weighted
	   # fluxes by sum of weights.
	   call gf_opengr (tbuf, ig, datamin, datamax, 0)
	   call amovr (Memr[imgl1r(tbuf)], Memr[wgtsum], npix)
	   call gf_opengr (ofbuf, ig, datamin, datamax, 0)
	   call advzr (Memr[imgl1r(ofbuf)], Memr[wgtsum], Memr[impl1r(ofbuf)],
		       npix, errzero)
	   call imflush (ofbuf)

	   # Now calculate average errors:
	   call gf_opengr (oebuf, ig, datamin, datamax, 0)
	   call amovr (Memr[imgl1r(oebuf)], Memr[errsum], npix)

	   # For variance weighting, calculate sqrt(1/errsum):
	   if (streq (weight, "variance")) {
	       #call arczr (1.0, Memr[errsum], Memr[errsum], npix, errzero)
	       do i = 0, npix-1 {
		  if (Memr[errsum+i]!=0.0) Memr[errsum+i]=1.0/Memr[errsum+i]
	       }
	       call asqrr (Memr[errsum], Memr[impl1r(oebuf)], npix, errzero)

	   # For uniform weighting, calculate sqrt(errsum)/n:
	   } else {
	       call asqrr (Memr[errsum], Memr[errsum], npix, errzero)
	       call advzr (Memr[errsum], Memr[wgtsum], Memr[impl1r(oebuf)],
			   npix, errzero)
	   }
	   call imflush (oebuf)
	}

	# Close the output images
	call imunmap (ofbuf)
	call imunmap (oebuf)
	call imunmap (oqbuf)
	call imunmap (tbuf)
	call imdelete (Memc[tmp_name])
	call imtclose(list)

	# Free memory
	call mfree (flux, TY_REAL)
	call mfree (err, TY_REAL)
	call mfree (dq, TY_REAL)
	call mfree (wgt, TY_REAL)
	call mfree (flxsum, TY_REAL)
	call mfree (errsum, TY_REAL)
	call mfree (dqsum, TY_REAL)
	call mfree (wgtsum, TY_REAL)
	call sfree (sp)

end
