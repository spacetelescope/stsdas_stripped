# generate the paper product plot page for FOS polarimetry observations.
# model after ypaccrapid, JC Hsu 6/30/98.
# remove the NREAD dependency of grp, fgrp, qgrp, ugrp, also limit the Q/U vs.
# wavelength plot to between -100% to +100%  4/5/99 JC Hsu

procedure yppolar (root, tmproot, igi_list, imtype)

char	root	{prompt="Rootname of observation"}
char	tmproot	{prompt="Rootname for temporary files"}
char	igi_list {prompt="Name of the file containing the igi script names"}
char	imtype	{prompt="Image type of root (FITS or GEIS)"}

begin

	# Declarations
	char	c0h, c3h		# Input data images.
	char	c0h_s, c3h_f
	char	flux_unit		# Unit of flux.
        int     gcount                  # Number of groups in images.
	char	lroot, ltmp, ligi, tmptab, tmptabp
	char	script			# Igi script.
	char	tmp			# Local temps.
	char	wave_unit		# Unit of wavelengths.
	char	ffile
	real	ymin, ymax, minwave, maxwave
	int	i, j, nr, nread, nplot, grp, fgrp
	int	pleft, pright, pstart, pend
	real	wleft, wright, wstart, wend
	char	ytext
	bool	c0_exist
	int	nxsteps, npix	
	char	ftype

	int	tmplen
	char	tmpdummy

	# Get interactive parameters
	lroot = root
	ltmp = tmproot
	ligi = igi_list
	ftype = imtype

	# Create file names.
	tmpdummy = mktemp("")
	tmplen = strlen(tmpdummy)
	tmp = ltmp//"PO"//substr(tmpdummy,tmplen-1,tmplen)

	script = tmp//"_po.igi"
	tmptab = tmp//"tb"
	tmptabp = tmp//"tbp"

	if (ftype == "geis") {
	    c0h = lroot//".c0h"
	    c3h = lroot//".c3h"
	} else {
	    c0h = lroot//"_c0f.fits"
	    c3h = lroot//"_c3f.fits"
	}

	# if c0h is missing so is c3h
	c0_exist = access (c0h)

	# For FITS files, access primary header
	if (ftype == "fits") {
	    c0h = lroot//"_c0f.fits[0]"
	    c3h = lroot//"_c3f.fits[0]"
	}		

	if (c0_exist) {
	    if (ftype == "geis") {
                keypar (c3h, "gcount", silent=yes)
                gcount = int (keypar.value)
	    } else {

		# FITS files don't have the 'gcount' keyword...
		keypar (c3h, "naxis2", silent=yes)
		gcount = int (keypar.value)
	    }
	}

	# In polarimetry there is no obj-obj observations
	nplot = 1

	j = 1
	while (j <= nplot) {

	    #
	    # Page 1: Flux vs. wavelength
	    #
	    if (c0_exist) {

	        # Start a new page.
	        print ("erase", >> script)
	        ypbanner (lroot, script, ftype)

		ytext = ""

	        keypar (c0h, "nread", silent=yes)
	        nread = int(keypar.value)
	        keypar (c0h, "nxsteps", silent=yes)
	        nxsteps = int(keypar.value)
    
	        # like ACCUM mode, use the last readout
	        grp = 1
	        fgrp = 43
		npix = nxsteps * 16

	        keypar (c0h, "minwave", silent=yes)
	        minwave = real(keypar.value)
	        keypar (c0h, "maxwave", silent=yes)
	        maxwave = real(keypar.value)
		if (ftype == "geis") {
	            pixloc (c0h, minwave, maxwave, maxvals=100000, > tmptab)
		} else {
		    pixloc (c0h//"[*,"//grp//"]", minwave,
				maxwave, maxvals=100000, > tmptab)
		}
		tinfo (tmptab, ttout=no)
		nr = tinfo.nrows

		# decide the pixel range which corresponds to the wavelength 
		# range
	        tabpar (tmptab, "c1", 1)
	        pleft = int(tabpar.value)
	        tabpar (tmptab, "c1", nr)
	        pright = int(tabpar.value)
	        tabpar (tmptab, "c2", 1)
	        wleft = int(tabpar.value)
	        tabpar (tmptab, "c2", nr)
	        wright = int(tabpar.value)

		if (wleft > wright) {
		    pstart = pright
		    pend = pleft
		    wstart = wright
		    wend = wleft
		} else {
		    pstart = pleft
		    pend = pright
		    wstart = wleft
		    wend = wright
		}
	
		if (ftype == "geis") {
	            c0h_s = c0h//"["//grp//"]"//"["//pstart//":"//pend//"]"
	            c3h_f = c3h//"["//fgrp//"]"//"["//pstart//":"//pend//"]"
        	} else {
	            c0h_s = c0h//"["//pstart//":"//pend//","//grp//"]"
	            c3h_f = c3h//"["//pstart//":"//pend//","//fgrp//"]"
		}

	        # Draw calibrated spectrum in linear space.
	        printf ("ysection %s\n", c3h_f, >> script)
	        printf ("xsection %s\n", c0h_s, >> script)
	        printf ("expand .6; location .15 .9 .55 .925\n", >> script)
	        printf ("limits ; box; connect; expand .8\n", >> script)

	        keypar (c0h, "bunit", silent=yes)
	        wave_unit = keypar.value
	    
	        printf ("xlabel '%s'\n", wave_unit, >> script)
	        keypar (c3h, "bunit", silent=yes)
	        flux_unit = keypar.value
	        printf ("justify 5; vmove 0.05 0.75; angle 90; label '%s'\n",
		        flux_unit, >> script)
	        printf ("angle 0; title 'Flux [c3h] vs. Wavelength [c0h] (group %d) %s'\n", fgrp, ytext, >> script)

	        # Draw calibrated spectrum in log space.
                # First, check if the y-array is all negative (JC Hsu, 7/22/96)
                gstatistics (c3h_f, masks="", groups=fgrp, g_accum=no, 
		    fields="min,max", lower=INDEF, upper=INDEF, >& "dev$null")
	        printf ("expand 0.6; location .15 .9 .075 .42\n", >> script)
                if (gstpar.max > 0.) {
	            printf ("ylog; limits; box; connect; expand 0.8\n", 
				>> script)
	            printf ("xlabel '%s'\n", wave_unit, >> script)
	            printf ("justify 5; vmove 0.05 0.25; angle 90\n", >> script)
		    printf ("label 'Log (%s)'\n", flux_unit, >> script)
	            printf ("angle 0; title 'Log (Flux [c3h]) vs. Wavelength [c0h] (group %d) %s'\n", fgrp, ytext, >> script)
                } else {
                    printf ("xlabel 'Flux is negative.'\n", >> script)
                }

	    #
	    # Page 2: Stokes Q and U vs. wavelength
	    #
	        # Start a new page.
	        print ("erase", >> script)
	        ypbanner (lroot, script, ftype)
    
		# compute mean and standard deviation for wavelength, Q, and U
		ypp_polerr (c0h, c3h, tmptabp, ftype, pstart, pend, npix, nread)

	        # Draw Sokes parameter Q

		# determine the limits of the plot, considering the error bars
		tcalc (tmptabp, "c7", "c3+1.1*c4", datatype = "real")
		tcalc (tmptabp, "c8", "c3-1.1*c4", datatype = "real")
		tstat (tmptabp, "c7", outtable = "", lowlim = INDEF, 
			highlim = INDEF)
		ymax = tstat.vmax
		ymax = min (ymax, 100.)
		tstat (tmptabp, "c8", outtable = "", lowlim = INDEF, 
			highlim = INDEF)
		ymin = tstat.vmin
		ymin = max (ymin, -100.)

	        printf ("data %s\n", tmptabp, >> script)
	        printf ("xcol 1\n", >> script)
	        printf ("ycol 3\n", >> script)
		
	        printf ("expand .6; location .15 .9 .075 .42\n", >> script)
	        printf ("limits %0.4g %0.4g %0.4g %0.4g\n", minwave, maxwave, 
				ymin, ymax, >> script)
	        printf ("box; expand .8\n", >> script)

		# draw error bars
	        printf ("ecol 4\n", >> script)
	        printf ("etype 2; errorbar 2; errorbar -2\n", >> script)
	        printf ("ecol 2\n", >> script)
	        printf ("errorbar 1; errorbar -1\n", >> script)

	        printf ("xlabel '%s'\n", wave_unit, >> script)
	        printf ("justify 5; vmove 0.05 0.25; angle 90; label '%s'\n",
		        "Q/I (%)", >> script)
	        printf ("angle 0; title 'Stokes Q/I vs. Wavelength [c0h] %s'\n", ytext, >> script)

	        # Draw Sokes parameter U

		# determine the limits of the plot, considering the error bars
		tcalc (tmptabp, "c9", "c5+1.1*c6", datatype = "real")
		tcalc (tmptabp, "c10", "c5-1.1*c6", datatype = "real")
		tstat (tmptabp, "c9", outtable = "", lowlim = INDEF, 
			highlim = INDEF)
		ymax = tstat.vmax
		ymax = min (ymax, 100.)
		tstat (tmptabp, "c10", outtable = "", lowlim = INDEF, 
			highlim = INDEF)
		ymin = tstat.vmin
		ymin = max (ymin, -100.)
	        printf ("ycol 5\n", >> script)
		
	        printf ("expand .6; location .15 .9 .55 .925\n", >> script)
	        printf ("limits %0.4g %0.4g %0.4g %0.4g\n", minwave, maxwave, 
				ymin, ymax, >> script)
	        printf ("box; expand .8\n", >> script)

		# draw error bars
	        printf ("ecol 6\n", >> script)
	        printf ("etype 2; errorbar 2; errorbar -2\n", >> script)
	        printf ("ecol 2\n", >> script)
	        printf ("errorbar 1; errorbar -1\n", >> script)

	        printf ("xlabel '%s'\n", wave_unit, >> script)
	        printf ("justify 5; vmove 0.05 0.75; angle 90; label '%s'\n",
		        "U/I (%)", >> script)
	        printf ("angle 0; title 'Stokes U/I vs. Wavelength [c0h] %s'\n", ytext, >> script)


	    } else
	        wave_unit = "Pixels"

	    j = j + 1
	}

	# Add the script to the list.
	print (script, >> ligi)
end
