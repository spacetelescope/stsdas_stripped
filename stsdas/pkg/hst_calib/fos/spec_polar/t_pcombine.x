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
# Jun 94 HAB: Added HISTORY records to output image headers and
#	 added capability to handle input images with NREAD > 1. (STSDAS 1.3.2)

char	imtlist[SZ_LINE]	# Input image list
char	output[SZ_FNAME]	# Output image name
char	weight[SZ_LINE]		# Weighting method
int	rejlim			# Data quality rejection limit
bool	verbose

pointer	input, iroot, iext, oroot, oext, tmp_name
pointer iflux, ierr, iqual, oflux, oerr, oqual
pointer	ifbuf, iebuf, iqbuf, ofbuf, oebuf, oqbuf, tbuf
pointer	flux, err, dq, wgt, flxsum, errsum, dqsum, wgtsum
int	i, list, ngroups, npix, ig, stat, nread, og, ogrp, mxgrp
real	datamin, datamax

bool	clgetb(), streq()
int	clgeti()
int	imtopen(), imtgetim(), immap(), imgl1r(), impl1r(), imgeti()
pointer	sp
pointer	hstr
string	history "PCOMBINE: %s"
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
	call salloc (hstr, SZ_LINE, TY_CHAR)

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
	call strcpy (Memc[iroot], Memc[iflux], SZ_FNAME)
	call strcat (".c1h", Memc[iflux], SZ_FNAME)

	# Open the first input flux image
	ifbuf = immap (Memc[iflux], READ_ONLY, 0)

	# Get the number of pixels per spectrum and the
	# the number of data groups in the images
	npix    = IM_LEN (ifbuf, 1)
	ngroups = imgeti (ifbuf, "GCOUNT")

	# Look for NREAD header keyword; if not found, set to 1.
	iferr (nread = imgeti (ifbuf, "NREAD")) nread=1
	ogrp = ngroups / nread
	call imunmap (ifbuf)

	# Allocate dynamic memory
	call malloc (flux, npix, TY_REAL)
	call malloc (err,  npix, TY_REAL)
	call malloc (dq,   npix, TY_REAL)
	call malloc (wgt,  npix, TY_REAL)
	call malloc (flxsum, npix, TY_REAL)
	call malloc (errsum, npix, TY_REAL)
	call malloc (dqsum,  npix, TY_REAL)
	call malloc (wgtsum, npix, TY_REAL)

	# Create temporary image to store sum of weights
	call mktemp ("tmp$weights", Memc[tmp_name], SZ_FNAME)
	call strcat (".c1h", Memc[tmp_name], SZ_FNAME)
	call imcopy (Memc[iflux], Memc[tmp_name], SZ_FNAME)
	tbuf  = immap (Memc[tmp_name], READ_WRITE, 0)

	# Compute the weighted flux and error values for the first image
	og = 0
	mxgrp = ogrp
	do ig = 1, ngroups, 1 {

	     # Check to see if this is a group we want to use
	     if (mod(ig,2*nread)!=0 && mod(ig+1,2*nread)!=0) next
	     og = og + 1

	     # Open the input and output images
	     if (og > 1) mxgrp = 0
	     call mkfname (Memc[iroot], "c1h", ig, 0, Memc[iflux], SZ_FNAME)
	     call mkfname (Memc[oroot], "c1h", og, mxgrp, Memc[oflux], SZ_FNAME)
	     ifbuf = immap (Memc[iflux], READ_ONLY, 0)
	     ofbuf = immap (Memc[oflux], NEW_COPY, ifbuf)
	     call mkfname (Memc[iroot], "c2h", ig, 0, Memc[ierr], SZ_FNAME)
	     call mkfname (Memc[oroot], "c2h", og, mxgrp, Memc[oerr], SZ_FNAME)
	     iebuf = immap (Memc[ierr], READ_ONLY, 0)
	     oebuf = immap (Memc[oerr], NEW_COPY, iebuf)
	     call mkfname (Memc[iroot], "cqh", ig, 0, Memc[iqual], SZ_FNAME)
	     call mkfname (Memc[oroot], "cqh", og, mxgrp, Memc[oqual], SZ_FNAME)
	     iqbuf = immap (Memc[iqual], READ_ONLY, 0)
	     oqbuf = immap (Memc[oqual], NEW_COPY, iqbuf)
	     call gf_opengr (tbuf, og, datamin, datamax, 0)

	     # Print info on first output group
	     if (og==1) {
		 call strcpy (Memc[iroot], Memc[iflux], SZ_FNAME)
		 call strcat (".c1h", Memc[iflux], SZ_FNAME)
		 call strcpy (Memc[iroot], Memc[ierr], SZ_FNAME)
		 call strcat (".c2h", Memc[ierr], SZ_FNAME)
		 call strcpy (Memc[iroot], Memc[iqual], SZ_FNAME)
		 call strcat (".cqh", Memc[iqual], SZ_FNAME)

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

		 # Add HISTORY record to output image headers
		 call sprintf (Memc[hstr], SZ_LINE, history)
		 call pargstr (Memc[iflux])
		 call imputh  (ofbuf, "HISTORY", Memc[hstr])
		 call sprintf (Memc[hstr], SZ_LINE, history)
		 call pargstr (Memc[ierr])
		 call imputh  (oebuf, "HISTORY", Memc[hstr])
		 call sprintf (Memc[hstr], SZ_LINE, history)
		 call pargstr (Memc[iqual])
		 call imputh  (oqbuf, "HISTORY", Memc[hstr])
	     }

	     # Read the input flux, error, and dq spectra
	     call amovr (Memr[imgl1r(ifbuf)], Memr[flux], npix)
	     call amovr (Memr[imgl1r(iebuf)], Memr[err], npix)
	     call amovr (Memr[imgl1r(iqbuf)], Memr[dq], npix)

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
	     call imunmap (ifbuf);call imunmap(iebuf);call imunmap(iqbuf)
	     call imunmap (ofbuf);call imunmap(oebuf);call imunmap(oqbuf)
	}

	# Reopen output images for remainder of processing
	call strcpy (Memc[oroot], Memc[oflux], SZ_FNAME)
	call strcpy (Memc[oroot], Memc[oerr],  SZ_FNAME)
	call strcpy (Memc[oroot], Memc[oqual], SZ_FNAME)
	call strcat (".c1h", Memc[oflux], SZ_FNAME)
	call strcat (".c2h", Memc[oerr],  SZ_FNAME)
	call strcat (".cqh", Memc[oqual], SZ_FNAME)
	ofbuf = immap (Memc[oflux], READ_WRITE, 0)
	oebuf = immap (Memc[oerr],  READ_WRITE, 0)
	oqbuf = immap (Memc[oqual], READ_WRITE, 0)

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
	       ngroups = imgeti(ifbuf, "GCOUNT")
	       iferr (nread = imgeti(ifbuf, "NREAD")) nread = 1
	       if (IM_LEN(ifbuf,1)!=npix || ngroups/nread!=ogrp) {
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

	       # Add HISTORY records to output image headers
               call sprintf (Memc[hstr], SZ_LINE, history)
               call pargstr (Memc[iflux])
               call imputh (ofbuf, "HISTORY", Memc[hstr])
               call sprintf (Memc[hstr], SZ_LINE, history)
               call pargstr (Memc[ierr])
               call imputh (oebuf, "HISTORY", Memc[hstr])
               call sprintf (Memc[hstr], SZ_LINE, history)
               call pargstr (Memc[iqual])
               call imputh (oqbuf, "HISTORY", Memc[hstr])

	       # Accumulate the weights and weighted fluxes for each group

	       og = 0
	       do ig = 1, ngroups, 1 {

		  # Check to see if this is a group we want to use
		  if (mod(ig,2*nread)!=0 && mod(ig+1,2*nread)!=0) next
		  og = og + 1

		  # Read the input flux, error, and dq spectra.
                  call gf_opengr (ifbuf, ig, datamin, datamax, 0)
                  call amovr (Memr[imgl1r(ifbuf)], Memr[flux], npix)
                  call gf_opengr (iebuf, ig, datamin, datamax, 0)
                  call amovr (Memr[imgl1r(iebuf)], Memr[err], npix)
                  call gf_opengr (iqbuf, ig, datamin, datamax, 0)
                  call amovr (Memr[imgl1r(iqbuf)], Memr[dq], npix)

		  # Reload the flux, error, dq, and weights accumulated so far.
                  call gf_opengr (ofbuf, og, datamin, datamax, 0)
                  call amovr (Memr[imgl1r(ofbuf)], Memr[flxsum], npix)
                  call gf_opengr (oebuf, og, datamin, datamax, 0)
                  call amovr (Memr[imgl1r(oebuf)], Memr[errsum], npix)
                  call gf_opengr (oqbuf, og, datamin, datamax, 0)
                  call amovr (Memr[imgl1r(oqbuf)], Memr[dqsum], npix)
                  call gf_opengr (tbuf, og, datamin, datamax, 0)
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

	do og = 1, ogrp, 1 {

	   # First calculate average fluxes by dividing sum of weighted
	   # fluxes by sum of weights.
	   call gf_opengr (tbuf, og, datamin, datamax, 0)
	   call amovr (Memr[imgl1r(tbuf)], Memr[wgtsum], npix)
	   call gf_opengr (ofbuf, og, datamin, datamax, 0)
	   call advzr (Memr[imgl1r(ofbuf)], Memr[wgtsum], Memr[impl1r(ofbuf)],
		       npix, errzero)
	   call imflush (ofbuf)

	   # Now calculate average errors:
	   call gf_opengr (oebuf, og, datamin, datamax, 0)
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

	# Update NREAD header keyword if necessary
	iferr (nread = imgeti(ofbuf, "NREAD")) nread = 1
	if (nread > 1) {
	    call imputi (ofbuf, "NREAD", 1)
	    call imputi (oebuf, "NREAD", 1)
	    call imputi (oqbuf, "NREAD", 1)
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
