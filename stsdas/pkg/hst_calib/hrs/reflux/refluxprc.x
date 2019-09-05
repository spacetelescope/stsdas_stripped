define  SZ_KEYWORD      18              # max chars in POPDS keyword value

include <imhdr.h>
include <error.h>
include <syserr.h>

# REFLUX_PRC -- Correct flux calibrated HRS spectrum for new absolute 
# sensitivities.
#
# Remove the previous absolute sensitivity correction from an HRS spectrum
# and apply the new absolute sensitivity correction. Task uses absolute
# sensitivity and wavelength net reference files, CZABSR and CZNETR, as
# described in ICD-47. Task assumes that observation to be corrected
# hras a corresponding wavelength (c0h) image with the same rootname.
# Task creates a new flux image with a new rootname. Task processes all groups.
#
# S. Hulbert 	Jul91	Original

procedure reflux_prc (flux0_file, flux1_file, abs1_file, net1_file, verbose)

char	flux0_file[SZ_PATHNAME]	#I: input flux image name (from expanded input template list)
char	flux1_file[SZ_PATHNAME]	#I: output flux image name (from expanded output template list)
char	abs1_file[SZ_PATHNAME]	#I: new absolute sensitivity image name
char	net1_file[SZ_PATHNAME]	#I: new wavelength net image name
bool	verbose			#I: print operations?

char	wave_file[SZ_PATHNAME], abs0_file[SZ_PATHNAME], net0_file[SZ_PATHNAME]
pointer	im_flx, im_wav, im_out
pointer	im_abs0, im_net0, im_abs1, im_net1
pointer	abs1, net1, abs0, net0
pointer	flux0, flux1, wave
pointer	temp
int	npix_abs1, npix_abs0, npix_flx, npix_wav
int	ngroup_flx, ngroup_wav
int	sclamp, binid1
int	off0, off1
int	group
char	grat_abs1[SZ_KEYWORD], grat_flx[SZ_KEYWORD]
char	aper_flx[SZ_KEYWORD] , grat_abs0[SZ_KEYWORD]
real	datamax, datamin
char	errmsg[SZ_LINE]

bool	streq()
int	sp, gf_gstfval(), imgeti()
pointer	immap(), imgl1r(), impl1r()

begin

	call smark (sp)

	# make sure we have the new sensitivities before proceeding
	if (abs1_file[1] == EOS || net1_file[1] == EOS) 
	    call error (1, "must supply new sensitivity and wavelength image names")

	# map new sensitivity images
	iferr (im_abs1 = immap (abs1_file, READ_ONLY, 0)) {
	    call sprintf (errmsg, SZ_LINE, "unable to open new sensitivity image %s")
		call pargstr (abs1_file)
	    call error (1, errmsg)
	}

	iferr (im_net1 = immap (net1_file, READ_ONLY, 0)) {
	    call sprintf (errmsg, SZ_LINE, "unable to open new wavelength net image %s")
	        call pargstr (net1_file)
	    call error (1, errmsg)
	}

	# read new sensitivity data and do consistency check
	call get_abs (im_abs1, im_net1, abs1, net1, npix_abs1, grat_abs1)

	# process images
	if (verbose) {
	    call eprintf ("%s -> %s\n")
	        call pargstr (flux0_file)
	        call pargstr (flux1_file)
	}

	# construct wavelength file from flux_file
	call make_wave (flux0_file, wave_file, SZ_PATHNAME)

	# map the input flux image
	iferr (im_flx = immap (flux0_file, READ_ONLY, 0)) {
	    call sprintf (errmsg, SZ_LINE, "unable to open input flux image %s")
		call pargstr (flux0_file)
	    call error (1, errmsg)
	}

	# map the corresponding wavelength image
	iferr (im_wav = immap (wave_file, READ_ONLY, 0)) {
	    call sprintf (errmsg, SZ_LINE, "unable to open input wavelength image %s")
		call pargstr (wave_file)
	    call error (1, errmsg)
	}

	# allocate temp buffer for output mapping
	call salloc (temp, SZ_PATHNAME, TY_CHAR)

	# map output flux image
	iferr {
	    call xt_mkimtemp (flux0_file, flux1_file, Memc[temp], SZ_PATHNAME)
	    im_out = immap (flux1_file, NEW_COPY, im_flx)
	} then {
	    call sprintf (errmsg, SZ_LINE, "unable to open output image %s")
	        call pargstr (flux1_file)
	    call error (1, errmsg)
	}
		
	# read GRATING from header and check
	call imgstr (im_flx, "GRATING", grat_flx, SZ_KEYWORD) 
        if (!streq (grat_flx, grat_abs1)) {
	    call sprintf (errmsg, SZ_LINE, "new absolute sensitivity data are for grating %s while input flux data were obtained with grating %s")
	        call pargstr (grat_abs1)
	        call pargstr (grat_flx)
	    call error (1, errmsg)
	}

	# determine APERTURE from flux header
	iferr {
	    call imgstr (im_flx, "APERTURE", aper_flx, SZ_KEYWORD)
	} then {
	    sclamp = imgeti (im_flx, "SCLAMP")
	    if (sclamp > 0) {
                call error (1, "processing not permitted for spectral cal lamp data")
	    } else {
	        binid1 = imgeti (im_flx, "BINID(1)")
		if (binid1 == 1)
		    call strcpy ("SSA", aper_flx, SZ_KEYWORD)
		else if (binid1 == 2)
		    call strcpy ("LSA", aper_flx, SZ_KEYWORD)
		else
                    call error (1, "aperture must be SSA or LSA")
	    }
	}

	# read abs0_file and net0_file from flux header
	iferr {
	    call imgstr (im_flx, "ABSHFILE", abs0_file, SZ_PATHNAME) 
	    call imgstr (im_flx, "NETHFILE", net0_file, SZ_PATHNAME) 
	} then
	    call error (1, "Unable to read reference file keywords from flux header")

	if (verbose) {
	    call eprintf ("    old reference: %s / %s\n")
	        call pargstr (abs0_file)
	        call pargstr (net0_file)
	    call eprintf ("    new reference: %s / %s\n")
	        call pargstr (abs1_file)
	        call pargstr (net1_file)
	}

	if (abs0_file[1] == EOS || net0_file[1] == EOS) 
	    call error (1, "one or both of the old sensitivity image names is missing")

	# map the old sensitivities
	iferr (im_abs0 = immap (abs0_file, READ_ONLY, 0)) {
	    call sprintf (errmsg, SZ_LINE, "unable to open old sensitivity image %s")
	        call pargstr (abs0_file)
	    call error (1, errmsg)
	}
	iferr (im_net0 = immap (net0_file, READ_ONLY, 0)) {
	    call sprintf (errmsg, SZ_LINE, "unable to open old wavelength net image %s")
	        call pargstr (net0_file)
	    call error (1, errmsg)
	}

	# read old sensitivity data and do consistency check
	call get_abs (im_abs0, im_net0, abs0, net0, npix_abs0, grat_abs0)

	# check that old sensitivity file is consistent with observation
        if (!streq (grat_flx, grat_abs0)) {
	    call sprintf (errmsg, SZ_LINE, "old absolute sensitivity data are for grating %s while input flux data data were obtained with grating %s")
	        call pargstr (grat_abs0)
		call pargstr (grat_flx)
            call error (1, errmsg)
	}

	# check for good flux and wavelength data
	npix_flx = IM_LEN(im_flx, 1)
	npix_wav = IM_LEN(im_wav, 1)
	if (npix_wav != npix_flx)
	        call error (1, "Flux and wavelength images are different sizes")

	# check number of groups
        ngroup_flx = gf_gstfval (im_flx, "GCOUNT")
        ngroup_wav = gf_gstfval (im_wav, "GCOUNT")
	if (ngroup_wav != ngroup_flx)
	        call error (1, "Flux and wavelength images have a different number of groups")

	if (verbose) {
	        call printf ("    %d groups processed\n")
		    call pargi (ngroup_flx)
	}

	# set the output number of groups
        call gf_pstfval (im_out, "GCOUNT", ngroup_flx)

	# set up reusable buffers
        call realloc (flux0, npix_flx, TY_REAL)
        call realloc (wave, npix_wav, TY_REAL)
        call realloc (flux1, npix_flx, TY_REAL)

	# determine offset into sensitivity files based on aperture
	# SSA data stored in first part of buffer
	# LSA data stored in last part of buffer
	if (streq(aper_flx, "SSA")) {
		off0 = 0
		off1 = 0
	} else {
		off0 = npix_abs0
		off1 = npix_abs1
	}

        # loop on number of groups
        do group = 1, ngroup_flx {

            # point to next group 
	    iferr (call gf_opengr (im_flx, group, datamin, datamax, 0)) {
	            call sprintf (errmsg, SZ_LINE, "changing to group %d in input image %s")
	                call pargi (group)
	                call pargstr (abs0_file)
	            call error (1, errmsg)
	    }
            iferr (call gf_opengr (im_wav, group, datamin, datamax, 0)) {
	            call sprintf (errmsg, SZ_LINE, "changing to group %d in input image %s")
	                call pargi (group)
	                call pargstr (net0_file)
	            call error (1, errmsg)
	    }
                iferr (call gf_opengr (im_out, group, datamin, datamax, im_flx)) {
	            call sprintf (errmsg, SZ_LINE, "changing to group %d in output image %s")
	                call pargi (group)
	                call pargstr (abs1_file)
	            call error (1, errmsg)
	    }

	    # get the input data 
            call amovr (Memr[imgl1r(im_flx)], Memr[flux0], npix_flx)
            call amovr (Memr[imgl1r(im_wav)], Memr[wave], npix_flx)
	
	    # remove previous flux calibration and apply new one
	    call fluxcal (Memr[flux0], Memr[wave], npix_flx, 
		Memr[abs0+off0], Memr[net0+off0], npix_abs0, 
		Memr[abs1+off1], Memr[net1+off1], npix_abs1, 
		Memr[flux1])

	    # put new flux in new flux image
            call amovr (Memr[flux1], Memr[impl1r(im_out)], npix_flx)


	}

	# update header keywords in output flux header
	iferr {
	    call impstr (im_out, "ABSHFILE", abs1_file, SZ_PATHNAME) 
	    call impstr (im_out, "NETHFILE", net1_file, SZ_PATHNAME) 
	} then
	    call error (1, "Unable to update reference file keywords in output flux header")


	    # do some clean up
	    call imunmap (im_abs0)
	    call imunmap (im_net0)
	    call imunmap (im_flx)
	    call imunmap (im_wav)
	    call imunmap (im_out)
	    call xt_delimtemp (flux1_file, Memc[temp])

	call mfree(abs0, TY_REAL)
	call mfree(net0, TY_REAL)
        call mfree (flux0, TY_REAL)
        call mfree (wave, TY_REAL)
        call mfree (flux1, TY_REAL)
	call mfree(abs1, TY_REAL)
	call mfree(net1, TY_REAL)
	call imunmap (im_abs1)
	call imunmap (im_net1)
	call sfree (sp)

end
