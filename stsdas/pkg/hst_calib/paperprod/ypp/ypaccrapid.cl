procedure ypaccrapid (root, tmproot, igi_list, opmode, imtype)
char	root	{prompt="Rootname of observation"}
char	tmproot	{prompt="Rootname for temporary files"}
char	igi_list {prompt="Name of the file containing the igi script names"}
char	opmode	{prompt="opmode (ACCUM or RAPID)"}
char	imtype	{prompt="Image type of root (FITS or GEIS)"}

begin
	# Declarations
	char	d0h, c0h, c1h, c5h, c7h	# Input data images.
	char	c0h_s, c1h_s, c5h_s, c7h_s	
	real	exposure		# Exposure time.
	char	flux_unit		# Unit of flux.
	real	fmax, fmin		# Correct flux min/max.
        int     gcount                  # Number of groups in images.
	char	grlist			# Group list.
	char	hsel			# hselect output.
	char	lroot, ltmp, ligi, tmptab, lmode
	char	script			# Igi script.
	char	sum, sum2, sum3, sum4, sum5, sum6, sum7, sumx	# Sum table.
	char	tmp			# Local temps.
        real    vl, vr, vb, vt          # Viewport.
	char	dummy
	char	wave_unit		# Unit of wavelengths.
	char	ffile
	real	x1, x2
	real	minwave, maxwave
	int	i, j, nr, nplot, grp
	int	dum1, dum2, dum3, dum4
	int	pleft, pright, pstart, pend, pmiddle
	real	wleft, wright, wstart, wend, wmiddle
	char	ystep1, ystep2, ytext, gtext, wtext
	bool	c0_exist, c5_exist, objobj, c7_exist, sct_exist
	real	back_mean, sct_val	# background mean and sct_val
	real	vmax, maxcheck, npix	
	real	exp_grp
	char	ftype

	int	tmplen
	char	tmpdummy

	# Get interactive parameters
	lroot = root
	ltmp = tmproot
	ligi = igi_list
	lmode = opmode
	ftype = imtype

	# Create file names.
	tmpdummy = mktemp("")
	tmplen = strlen(tmpdummy)
	tmp = ltmp//"AR"//substr(tmpdummy,tmplen-1,tmplen)
	#printf("tmplen = %d, tmpdummy = %s, tmp = %s\n",tmplen,tmpdummy,tmp)

	script = tmp//"_ar.igi"
	grlist = tmp//"gl"
	hsel = tmp//"hs"
	sum = tmp//"s"
	sum2 = tmp//"s2"
	sum3 = tmp//"s3"
	sum4 = tmp//"s4"
	sum5 = tmp//"s5"
	sum6 = tmp//"s6"
	sum7 = tmp//"s7"
	tmptab = tmp//"tb"

	if (ftype == "geis") {
		d0h = lroot//".d0h"
		c1h = lroot//".c1h"
		c0h = lroot//".c0h"
		c5h = lroot//".c5h"
		c7h = lroot//".c7h"
	} else {
		d0h = lroot//"_d0f.fits"
		c1h = lroot//"_c1f.fits"
		c0h = lroot//"_c0f.fits"
		c5h = lroot//"_c5f.fits"
		c7h = lroot//"_c7f.fits"
	}

	# JC Hsu, 8/2/96, if c0h is missing so will c1h
	c0_exist = access (c0h)
	c5_exist = access (c5h)
	c7_exist = access (c7h)
	sct_exist = yes

	# For FITS files, access primary header
	if (ftype == "fits") {
		d0h = lroot//"_d0f.fits[0]"
		c1h = lroot//"_c1f.fits[0]"
		c0h = lroot//"_c0f.fits[0]"
		c5h = lroot//"_c5f.fits[0]"
		c7h = lroot//"_c7f.fits[0]"
	}		


	# read YSTEP1 and YSTEP2 in d0h, if both are OBJ, nplot = 2
        keypar (d0h, "ystep1", silent=yes)
        ystep1 = keypar.value
        keypar (d0h, "ystep2", silent=yes)
        ystep2 = keypar.value
	if (ystep1 == "OBJ" && ystep2 == "OBJ") {
	    objobj = yes
	    nplot = 2
	} else {
	    objobj = no
	    nplot = 1
	}

	if (c0_exist) {
		if (ftype == "geis") {
                    keypar (c1h, "gcount", silent=yes)
                    gcount = int (keypar.value)
		} else {
		# FITS files don't have the 'gcount' keyword...
		    keypar (c1h, "naxis2", silent=yes)
		    gcount = int (keypar.value)
		}

		# read in Exposure time / group here
		keypar (c0h, "exposure", silent=yes)
		exp_grp = real (keypar.value)
	}
	j = 1
	while (j <= nplot) {

	    #
	    # Page 1:
	    #
	    if (c0_exist) {

    
	        # Start a new page.
	        print ("erase", >> script)
	        ypbanner (lroot, script, ftype)
    
	        # for ACCUM mode do the last group, RAPID mode do the first 
		# group
	        if (lmode == "ACCUM") grp = gcount+1-j
	        else grp = j

		if (objobj) {
		    if (ftype == "geis") {
                    	keypar (c0h//"["//grp//"]", "ypos", silent=yes)
                   	ytext = "(YPOS = "//keypar.value//")"
		    } else {
			# FITS files use tables in [1] to store group parameters
                    	tabpar (lroot//"_c0f.fits[1]","ypos", grp)
			ytext = "(YPOS = "//tabpar.value//")"
	  	    }
 		} else
		    ytext = ""

	        keypar (c0h, "minwave", silent=yes)
	        minwave = real(keypar.value)
	        keypar (c0h, "maxwave", silent=yes)
	        maxwave = real(keypar.value)
		if (ftype == "geis") {
	        	pixloc (c0h, minwave, maxwave, maxvals=100000, > tmptab)
		} else {
			pixloc (lroot//"_c0f.fits[0][*,"//grp//"]", minwave,
				maxwave, maxvals=100000, > tmptab)
		}
		tinfo (tmptab, ttout=no)
		nr = tinfo.nrows

	        tabpar (tmptab, "c1", 1)
	        pleft = int(tabpar.value)
	        tabpar (tmptab, "c1", nr)
	        pright = int(tabpar.value)
	        tabpar (tmptab, "c2", 1)
	        wleft = int(tabpar.value)
	        tabpar (tmptab, "c2", nr)
	        wright = int(tabpar.value)
	        tabpar (tmptab, "c2", (nr+1)/2)
	        wmiddle = int(tabpar.value)

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
	            c1h_s = c1h//"["//grp//"]"//"["//pstart//":"//pend//"]"
	            c5h_s = c5h//"["//grp//"]"//"["//pstart//":"//pend//"]"
        	} else {
	            c0h_s = c0h//"["//pstart//":"//pend//","//grp//"]"
	            c1h_s = c1h//"["//pstart//":"//pend//","//grp//"]"
	            c5h_s = c5h//"["//pstart//":"//pend//","//grp//"]"
		}

	        # Draw calibrated spectrum in linear space.
	        printf ("ysection %s\n", c1h_s, >> script)
	        printf ("xsection %s\n", c0h_s, >> script)
	        printf ("expand .6; location .15 .9 .55 .925\n", >> script)
	        printf ("limits ; box; connect; expand .8\n", >> script)

	        keypar (c0h, "bunit", silent=yes)
	        wave_unit = keypar.value
	    
	        printf ("xlabel '%s'\n", wave_unit, >> script)
	        keypar (c1h, "bunit", silent=yes)
	        flux_unit = keypar.value
	        printf ("justify 5; vmove 0.05 0.75; angle 90; label '%s'\n",
		        flux_unit, >> script)
	        printf ("angle 0; title 'Flux [c1h] vs. Wavelength [c0h], group %d of %d %s'\n", grp, gcount, ytext, >> script)

	        # Draw calibrated spectrum in log space.
                # First, check if the y-array is all negative (JC Hsu, 7/22/96)
                gstatistics (c1h_s, masks="", groups=grp, g_accum=no, 
		    fields="min,max", lower=INDEF, upper=INDEF, >& "dev$null")
	        printf ("expand 0.6; location .15 .9 .075 .42\n", >> script)
                if (gstpar.max > 0.) {
	            printf ("ylog; limits; box; connect; expand 0.8\n", >> script)
	            printf ("xlabel '%s'\n", wave_unit, >> script)
	            printf ("justify 5; vmove 0.05 0.25; angle 90\n", >> script)
		    printf ("label 'Log (%s)'\n", flux_unit, >> script)
	            printf ("angle 0; title 'Log (Flux [c1h]) vs. Wavelength [c0h], group %d of %d %s'\n", grp, gcount, ytext, >> script)
                } else {
                    printf ("xlabel 'Flux is negative.'\n", >> script)
                }
	    } else
	        wave_unit = "Pixels"

	    #
	    # Page 2: Instrument-corrected counts.
	    # 	      if c5h exists, c0h always exists
	    #
	    if (c5_exist) {
	        print ("erase", >> script)
	        ypbanner (lroot, script, ftype)

		if (!c0_exist) {
		    wtext = "Pixels"
		    keypar (c5h, "i_naxis1", silent=yes)
		    pstart = 1
		    pend = int (keypar.value)

		if (ftype == "geis") {
                    keypar (c5h, "gcount", silent=yes)
		} else {
		# FITS files don't have the 'gcount' keyword...
		    keypar (c5h, "naxis2", silent=yes)
		}
		    gcount = int (keypar.value)

	            if (lmode == "ACCUM") grp = gcount+1-j
	            else grp = j

		    if (objobj) {
		      if (ftype == "geis") {
                    	keypar (c5h//"["//grp//"]", "ypos", silent=yes)
                   	ytext = "(YPOS = "//keypar.value//")"
		      } else {
			# FITS files use tables in [1] to store group parameters
                    	tabpar (lroot//"_c5f.fits[1]","ypos", grp)
			ytext = "(YPOS = "//tabpar.value//")"
	  	      }
		    } else
		        ytext = ""
		} else
		    wtext = "Wavelength[c0h]"

		if (ftype == "geis") {
		        keypar (c5h//"["//grp//"]", "exposure", silent=yes)
	        	exposure = real (keypar.value)
		} else {
		        tabpar (lroot//"_c5f.fits[1]", "exposure", grp)
			exposure = real (tabpar.value)
		}			

	        pmiddle = (pstart + pend) / 2
		if (ftype == "geis") {
	            c0h_s = c0h//"["//grp//"]"//"["//pstart//":"//pend//"]"
	            c5h_s = c5h//"["//grp//"]"//"["//pstart//":"//pend//"]"
		} else {
	            c0h_s = c0h//"["//pstart//":"//pend//","//grp//"]"
	            c5h_s = c5h//"["//pstart//":"//pend//","//grp//"]"		
		}

	        gstatistics (c5h_s, masks="", groups=grp, g_accum=no, 
			    fields="min,max", lower=INDEF, upper=INDEF, 
			    >& "dev$null")
	        fmin = gstpar.min * exposure 
	        fmax = gstpar.max * exposure

	        keypar (c5h, "fgwa_id", silent=yes)
	        if (keypar.value == "PRI") {

		    # plot the upper half only
		    if (ftype == "geis") {
	            	printf ("ysection %s[%d][%d:%d]\n", c5h, grp, 
				pstart, pend, >> script)
		    } else {
			printf ("ysection %s[%d:%d,%d]\n", c5h,
				pstart, pend, grp, >> script)
		    }

	            # if no wavelength file, use pixels

		    # X range
	            if (c0_exist) {
			if (ftype == "geis") {
	                    printf ("xsection %s[%d][%d:%d]\n", c0h, grp, 
				pstart, pend, >> script)
			} else {
	                    printf ("xsection %s[%d:%d,%d]\n", c0h,
				pstart, pend, grp, >> script)
			}

	                x1 = minwave
	                x2 = maxwave
	            } else {
			if (ftype == "geis") {
	                    printf ("xsection %s[%d][%d:%d]\n", c5h, grp, 
				pstart, pend, >> script)
			} else {
	                    printf ("xsection %s[%d:%d,%d]\n", c5h,
				pstart, pend, grp, >> script)
			}

                        printf ("xeval r + %d - 1\n", pstart, >> script)
	                x1 = pstart
	                x2 = pend
	            }
                    printf ("yeval y * %g \n", exposure, >> script)
	            printf ("limits %g %g %g %g\n", x1, x2, fmin, 
				fmax, >> script)
	            printf ("expand 0.6; location 0.15 0.9 0.575 0.95\n", 
				>> script)
	            printf ("box; connect; expand .8\n", >> script)
	            printf ("xlabel '%s'\n", wave_unit, >> script)
	            printf ("ylabel 'Corrected Counts'\n", >> script)
	            printf ("title 'Instrument-Corrected Counts vs. %s, group %d of %d %s'\n", wtext, grp, gcount, ytext, >> script)

	            printf ("vmove 0.5 0.; justify 8; expand 0.6\n", >> script)
	            printf ("label 'Instrument Corrected Counts = [c5h] * exposure'\n", >> script)
	        } else {

		    # plot the upper half
		    if (ftype == "geis") {
	                printf ("ysection %s[%d][%d:%d]\n", c5h, grp, 
				pstart, pmiddle, >> script)
		    } else {
	                printf ("ysection %s[%d:%d,%d]\n", c5h, 
				pstart, pmiddle, grp, >> script)
		    }

	            # if no wavelength file, use pixels
	            if (c0_exist) {
			if (ftype == "geis") {
	  	              printf ("xsection %s[%d][%d:%d]\n", c0h, grp, 
				pstart, pmiddle, >> script)
			} else {
	  	              printf ("xsection %s[%d:%d,%d]\n", c0h,
				pstart, pmiddle, grp, >> script)
			}
		    } else {
			if (ftype == "geis") {
	                    printf ("xsection %s[%d][%d:%d]\n", c5h, grp, 
				pstart, pmiddle, >> script)
			} else {
	                    printf ("xsection %s[%d:%d,%d]\n", c5h,
				pstart, pmiddle, grp, >> script)
			}
                        printf ("xeval r + %d - 1\n", pstart, >> script)
		    }
    
                    printf ("yeval y * %g \n", exposure, >> script)
	            printf ("expand 0.6; location 0.15 0.9 0.575 0.95\n", 
				>> script)

		    # X range
	            if (c0_exist) {
	                x1 = wstart
	                x2 = wmiddle
	            } else {
	                x1 = pstart
	                x2 = pmiddle
	            }
	            printf ("limits %g %g %g %g\n", x1, x2, fmin, fmax, 
				>> script)
	            printf ("box; connect; expand .8\n", >> script)
	            printf ("xlabel '%s'\n", wave_unit, >> script)
	            printf ("ylabel 'Corrected Counts'\n", >> script)
	            printf ("title 'Instrument-Corrected Counts vs. %s, group %d of %d %s'\n", wtext, grp, gcount, ytext, >> script)


		    # plot the lower half
		    if (ftype == "geis") {
	                printf ("ysection %s[%d][%d:%d]\n", c5h, grp, 
				pmiddle, pend, >> script)
		    } else {
	                printf ("ysection %s[%d:%d,%d]\n", c5h,
				pmiddle, pend, grp, >> script)
		    }

	            # if no wavelength file, use pixels
	            if (c0_exist) {
			if (ftype == "geis") {
		            printf ("xsection %s[%d][%d:%d]\n", c0h, grp, 
				pmiddle, pend, >> script)
			} else {
		            printf ("xsection %s[%d:%d,%d]\n", c0h,
				pmiddle, pend, grp, >> script)
			}
		    } else {
			if (ftype == "geis") {
	                    printf ("xsection %s[%d][%d:%d]\n", c5h, grp, 
				pmiddle, pend, >> script)
			} else {
	                    printf ("xsection %s[%d:%d,%d]\n", c5h,
				pmiddle, pend, grp, >> script)
			}
                        printf ("xeval r + %d - 1\n", pmiddle, >> script)
		    }

                    printf ("yeval y * %g \n", exposure, >> script)
	            printf ("expand 0.6; location 0.15 0.9 0.1 0.475\n", 
				>> script)
    
		    # X range
	            if (c0_exist) {
	                x1 = wmiddle
	                x2 = wend
	            } else {
	                x1 = pmiddle
	                x2 = pend
	            }
	            printf ("limits %g %g %g %g\n", x1, x2, fmin, fmax, 
				>> script)
	            printf ("box; connect; expand .8\n", >> script)
	            printf ("xlabel '%s'\n", wave_unit, >> script)
	            printf ("ylabel 'Corrected Counts'\n", >> script)

	            printf ("vmove 0.5 0.; justify 8; expand 0.6\n", >> script)
	            printf ("label 'Instrument Corrected Counts = [c5h] * exposure'\n", >> script)
	        }
	    }

	    #
	    # Page 3: Instrument Corrected Counts vs. Group.
	    #
    
	    # if c5h does not exist, use d0h
	    if (c5_exist) 
	        ffile = c5h
	    else {
	        ffile = d0h
	    }

	    if (ftype == "geis") {
	    	keypar (ffile, "gcount", silent=yes)
	    	gcount = int (keypar.value)
	    } else {
	    # FITS files don't have the 'gcount' keyword...
	    	keypar (ffile, "naxis2", silent=yes)
	    	gcount = int (keypar.value)
	    }

	    # No need for this page if there is only one group
	    if (gcount > 1) {

	    if (lmode == "ACCUM") grp = gcount+1-j
	    else grp = j

	    if (objobj) {
		if (ftype == "geis") {
                keypar (ffile//"["//grp//"]", "ypos", silent=yes)
                ytext = "(YPOS = "//keypar.value//")"
		} else {
		    # FITS files use tables in [1] to store group parameters
		    if (c5_exist) {
                    	tabpar (lroot//"_c5f.fits[1]","ypos", grp)
		    } else {
                    	tabpar (lroot//"_d0f.fits[1]","ypos", grp)
		    }
		    ytext = "(YPOS = "//tabpar.value//")"
		}
	    } else
	        ytext = ""

	    if (objobj) {
		
		# for ACCUM mode, do the even first, RAPID do odd first
		if (lmode == "ACCUM") gtext = (3-j)//"-"//gcount//"x2"
		else gtext = j//"-"//gcount//"x2"
	    } else gtext = "*"

	    print ("erase", >> script)
	    ypbanner (lroot, script, ftype)

	    if (c5_exist) {
		keypar (ffile, "exposure", silent=yes)
		exposure = real (keypar.value)
	    } else
	        exposure = 1.

	    # Added 10/11/96 JC Hsu, to only include "usable data", especially
	    # for prism 
	    if (!c0_exist) {
		keypar (ffile, "i_naxis1", silent=yes)
		pleft = 1
		pright = int (keypar.value)
	    }
	    if (ftype == "geis") {
	    	gstatistics (ffile//"["//pleft//":"//pright//"]", masks="", 
			groups=gtext, g_accum=no, fields="sum", lower=INDEF, 
			upper=INDEF, > sum)
	    } else {
		i = 1
		delete(tmptab,verify=no,>& "dev$null")

		while (i <= gcount) {
	 	  gstatistics (ffile//"["//pleft//":"//pright//","//i//"]", 
			masks="", groups=gtext, g_accum=no, 
			fields="npix,sum", lower=INDEF, 
			upper=INDEF, >> tmptab)
		  i = i + 1
		}
        	tprint (tmptab, prparam=no, prdata=yes, pwidth=80, plength=0,
                	showrow=yes, showhdr=no, showunits=no, columns="c2",
                	rows="-", option="plain", align=no, sp_col="", 
			lgroup=0, > sum)
	    }
		
	    npix = gstpar.npix

	    if (lmode == "ACCUM") {
		if (j == 1) sumx = sum6
		else sumx = sum7

		if (ftype == "geis") {
        		grlist (ffile, gtext, inclusive=no, > grlist)
        		hselect ("@"//grlist, fields="exposure", expr="yes", 
				> hsel)
		} else {
			if (c5_exist) 
				grlist = lroot//"_c5f.fits[1]"
			else
				grlist = lroot//"_d0f.fits[1]"

			tdump (grlist, cdfile="dev$null",pfile="dev$null",
				datafile=hsel,columns="exposure", 
				rows="-",pwidth=80,>& "dev$null")
		}
		
        	tmerge (sum//","//hsel, sum2, "merge", allcols=yes, 
			tbltype="text", allrows=100, extracol=0)

        	tcalc (sum2, "c4", "c2 * c3", datatype="double", colunits="",
               		colfmt="")
        	tproject (sum2, sum3, "c1,c4", uniq=no)
        	tprint (sum3, prparam=no, prdata=yes, pwidth=80, plength=0,
                	showrow=no, showhdr=no, showunits=no, columns="c1,c2",
                	rows="2-", option="plain", align=no, sp_col="", 
			lgroup=0, > sum4)
        	tmerge (sum4//","//sum3, sum5, option="merge", allcols=yes,
                	tbltype="text", allrows=100, extracol=0)
        	tcalc (sum5, "c5", "c2-c4", datatype="double", colunits="",
               		colfmt="")
        	tinfo (sum5, ttout=no)
        	i = tinfo.nrows - 1
        	tprint (sum3, prparam=no, prdata=yes, pwidth=80, plength=0,
                	showrow=no, showhdr=no, showunits=no, columns="c1,c2",
                	rows="1", option="plain", align=no, sp_col="", 
			lgroup=0, > sumx)
        	tprint (sum5, prparam=no, prdata=yes, pwidth=80, plength=0,
                	showrow=no, showhdr=no, showunits=no, columns="c1,c5",
                	rows="1-"//i, option="plain", align=no, sp_col="", 
			lgroup=0, >> sumx)
        	tstat (sumx, "c2", outtable="", lowlim=INDEF, highlim=INDEF, 
			rows="-")
	        printf ("data %s\n", sumx, >> script)
	        printf ("xcolumn c1; ycolumn c2\n", >> script)

	        if (c7_exist) {
		  if (ftype == "geis") {
	   	  	gstatistics (c7h, masks="", 
		    	    groups=gcount, g_accum=no, fields="npix,mean",
			    lower=INDEF, upper=INDEF, >& "dev$null")
		   } else {
			 gstatistics (c7h//"[*,"//gcount//"]", masks="", 
			    groups="*", g_accum=no, fields="npix,mean",
			    lower=INDEF, upper=INDEF, >& "dev$null")
		   }
		  back_mean = gstpar.mean / gcount * gstpar.npix * exp_grp
	        } else
		  back_mean = 0.

	        if (c0_exist) {
		  if (ftype == "geis") {
	    	  	gstatistics (c0h, masks="", groups=gcount, g_accum=no,
		    	    fields="npix", lower=INDEF, upper=INDEF, 
			    >& "dev$null")
		  } else {
	    	  	gstatistics (c0h//"[*,"//gcount//"]", masks="",
			    groups="*", g_accum=no, fields="npix", lower=INDEF,
			    upper=INDEF, >& "dev$null")
		  } 
	    	  keypar (c0h, "sct_val", silent=yes)
		  if (keypar.value != "") {
	    	      sct_val = real(keypar.value)/gcount*gstpar.npix*exp_grp
		  } else { 
		      sct_exist = no
		      sct_val = 0.
		  }

	        } else
		  sct_val = 0.

	    } else {
		
	        dummy = "c2 * " // exposure
	        tcalc (sum, "c3", dummy, datatype="double", colunits="",
	               	colfmt="")
                tstat (sum, "c3", outtable="", lowlim=INDEF, highlim=INDEF, 
			rows="-")
	        printf ("data %s\n", sum, >> script)
	        printf ("xcolumn c1; ycolumn c3\n", >> script)

		# initialize the value of back_mean for loop and 
		# act as default value should there be no valid
		# data.
		back_mean = 0.
       		if (c7_exist) {
		    i=1
		    while (i <= gcount) {
		    if (ftype == "geis") {
	   	    	gstatistics (c7h, masks="", 
		       	    groups=i, g_accum=no, fields="mean", lower=INDEF, 
		            upper=INDEF, >& "dev$null")
		    } else {
	   	    	gstatistics (c7h//"[*,"//i//"]", masks="", 
		      	    groups="*", g_accum=no, fields="mean", lower=INDEF, 
		      	    upper=INDEF, >& "dev$null")
		    }
		    back_mean = back_mean + gstpar.mean
		    i = i + 1
		    }
		    back_mean = back_mean / gcount * npix * exp_grp
	        } 
		# initialize the value of sct_val for loop and 
		# act as default value should there be no valid
		# data.
		sct_val = 0.
		if (c0_exist) {
		    i=1
		    while ( i <= gcount) {
			if (ftype == "geis") {		
		            keypar (c0h//"["//i//"]", "sct_val", silent=yes)
		 	    if (keypar.value != "") {       
				sct_val = (real (keypar.value) * exp_grp ) + sct_val
			    } else {
				sct_exist = no
				sct_val = 0.
			    }
			} else {
			    tabpar(lroot//"_c0f.fits[1]", "sct_val", i)
			    if (tabpar.value != "") {
				sct_val= (real (tabpar.value) * exp_grp ) + sct_val
			    } else { 
				sct_exist = no
				sct_val = 0.
			    }
			}
			i = i + 1
		    }
		    sct_val = sct_val / gcount * npix
		}

	    }
 
	    # determine the upper limit from comparing the data
	    # background mean and scattered light value for plotting
		maxcheck = max(back_mean, sct_val)
		vmax = max(tstat.vmax, maxcheck)	
	    
		
	    # determine the window for the percentage plot
            vb = min (vmax * 0.9, tstat.vmin)
            x = abs (vmax - vb) * 0.05
            vb = vb - x
            vt = vmax + x
            x = tstat.nrows * nplot * 0.05
            vl = 1. - x
            vr = (tstat.nrows) * nplot + x
    
	    #printf ("Top and Bottom of plot is at: %.6g and %.6g\n",vt, vb)

	    printf ("expand 0.6; location 0.15 0.9 0.075 0.925\n", >> script)
            printf ("limits %.6g %.6g %.6g %.6g\n", vl, vr, vb, vt,
                    >> script)
            printf ("angle 90; axis %.6g %.6g 0 0 0.15 0.075 .85 2 1\n",
                    vb, vt, >> script)
    	
	    # if max is 0, do not plot the percentage
	    if (vmax > 0.) {
                x = vb / vmax
                y = vt / vmax
                x = x * 100.
                y = y * 100.
                printf ("axis %.6g %.6g 0 0 0.9 0.075 0.85 2 0\n", x, y, >> script)
                printf ("angle -90; vmove .95 .5\n", >> script)
	        printf ("putlabel 6 'Percent of Maximum Value'\n", >> script)
	    } else {
                printf ("angle 90; axis %.6g %.6g 0 0 0.9 0.075 .85 0 0\n",
                	    vb, vt, >> script)
	    }
            printf ("angle 0\n", >> script)

            # figure out the best way to draw ticks
	    dum1 = 10**(int(log10(gcount)))
	    dum2 = int(gcount/dum1) * dum1
	    dum3 = max(int(dum1/2), 1)
	    dum4 = max(int(dum1/10), 1)
            printf ("axis %.6g %.6g %d %d 0.15 0.075 .75 1 0\n",
                            vl, vr, dum4, dum3, >> script)
            printf ("axis %.6g %.6g %d %d 0.15 0.925 .75 0 1\n",
                            vl, vr, dum4, dum3, >> script)
    
	    printf ("points; expand .8\n", >> script)
	    printf ("xlabel 'Group'\n", >> script)
	    if (c5_exist) {
                printf ("angle 90; vmove 0.03 .5; putlabel 5 'Corrected Counts'\n",
                	    >> script)
	        printf ("title 'Total Instrument-Corrected Counts vs. Group %s'\n", ytext, >> script)
	    } else {
                printf ("angle 90; vmove 0.03 .5\n", >> script)
	        printf ("putlabel 5 'Uncorrected Counts'\n", >> script)
	        printf ("title 'Total Uncorrected Counts [d0h] vs. Group %s'\n",
			    ytext, >> script)
	    }

	    if (c7_exist && back_mean > vb) {
	    	# plot a solid line across the plot for the background mean
	    	printf ("angle 0;expand 0.6; justify 6; ltype 0\n", >> script)
	    	printf ("move %.6g %.6g; draw %.6g %.6g\n", vl, back_mean, vr, back_mean, >> script)
		printf ("vmove 0.0 0.2; ltype 0; vdraw 0.03 0.2\n", >> script)
		printf ("label ' Dark Cts'\n", >> script)
	    }
	    if (c0_exist && sct_val > vb && sct_exist) {
	    	# plot a dashed line across the plot for the mean scattered light
	    	printf ("angle 0; expand 0.6 ;justify 6\n", >> script)
	    	printf ("ltype 2; move %.6g %.6g; draw %.6g %.6g\n",vl,sct_val,vr,sct_val, >> script)
		printf ("vmove 0.0 0.25; vdraw 0.03 0.25\n", >> script)
		printf ("ltype 0; label ' Sct. Light'\n", >> script)

	    }
		printf ("justify 1\n", >> script)

	    }
	    j = j + 1
	}

	# Add the script to the list.
	print (script, >> ligi)
end
