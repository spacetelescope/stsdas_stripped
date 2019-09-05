include	<imhdr.h>
include	<gset.h>

procedure t_polplot ()

# POLPLOT - Plots FOS polarimetry "c3h" data sets.  Plots the Stokes I
# (intensity), circular polarization, linear polarization, and polarization
# position angle spectra as a stacked plot.  The user has the option of
# whether or not to include the circular polarization spectrum in the plot,
# and also whether or not to overplot error bars on the polarization spectra.
# There is also the choice of plotting against wavelength or inverse wave.
#
# The flux (intensity) spectrum is plotted as a continuous line for all
# points with sigma > 0. The polarization spectra are plotted as individual 
# points (with or without error bars).
#
# May 93: H.A.Bushouse - Original implementation.  Based in large part on
#         Fortran programs "LINPOLPLOT" and "POLARPLOT" written by J.R. Walsh, 
#         ST-ECF, ESO.  (STSDAS v1.3)
#
# Sep 93: HAB - Add "ptype" parameter and options to plot in point, continuous 
#         line, or histogram modes.
# Sep 93: HAB - Add "spectra" parameter and capability of plotting either 
#          Stokes Q,U,V or polarimetry spectra.
# Oct 93: HAB - Remove use of data quality vector; instead key on zero values 
#         of sigma vector to detect bad points.
# Nov 93: HAB - Modify continuous plot mode to skip over points with sigma=0.
#         (STSDAS v1.3.1)
# May 94: HAB - Added check for reversed wavelengths for AMBER detector.
#
# Jun 94: HAB - Modified style of histogram plots. Also added dashed line
#	  drawn at zero in polarization spectra panels. (STSDAS v1.3.2)

# Task parameters
pointer	imname		# Spectral image name
int	set		# Dataset number to plot
pointer	spectra		# Stokes vs. polarimetry spectra switch
pointer	wavname		# Wavelength image name
bool	lrev		# Inverse wavelength switch
bool	errors		# Error bar inclusion switch
bool	circ		# Circular polarization switch
pointer	title		# Plot title string

# Local variables
pointer	imptr, wvptr
pointer	root, extn
pointer	wave, flux, sigma, pol1, pol2, pol3, pol1err, pol2err, pol3err

int	npix, ngroups, ig
real	rad1, datamin, datamax

bool	streq(), clgetb()
int	immap(), imgl1r(), gf_gstfval(), clgeti(), stridx()
pointer	sp

begin

	# Allocate memory for local variables
	call smark (sp)
	call salloc (imname,  SZ_FNAME, TY_CHAR)
	call salloc (wavname, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (extn, SZ_FNAME, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (spectra, SZ_LINE, TY_CHAR)

	# Get the flux image name
	call clgstr ("image", Memc[imname], SZ_FNAME)
        call iki_init ()
	call iki_parse (Memc[imname], Memc[root], Memc[extn])
	if (streq (Memc[extn], "")) call strcat (".c3h", Memc[imname], SZ_FNAME)

	# Get other task parameters
	set      = clgeti ("set")
	lrev     = clgetb ("invlam")
	errors   = clgetb ("errors")
	circ     = clgetb ("circpol")
	call clgstr( "spectra", Memc[spectra], SZ_LINE)
	call strlwr (Memc[spectra])
	
	# Get the wavelength file name
	call clgstr ("wave", Memc[wavname], SZ_FNAME)
	if (streq (Memc[wavname], ""))
	    call strcpy (Memc[root], Memc[wavname], SZ_FNAME)
	call iki_parse (Memc[wavname], Memc[root], Memc[extn])
	if (streq (Memc[extn], "")) 
	    call strcat (".c0h", Memc[wavname], SZ_FNAME)

	# Open the input data files
	imptr = immap (Memc[imname],  READ_ONLY, 0)
	wvptr = immap (Memc[wavname], READ_ONLY, 0)

	# Get the number of pixels in the input spectra and the
	# number of groups in the flux data file
	npix    = IM_LEN (imptr, 1)
	ngroups = gf_gstfval (imptr, "GCOUNT")

	# If ngroups is not equal to 56, this is not a true "c3h" file
	if (ngroups != 56) {
	    call imunmap (imptr)
	    call imunmap (wvptr)
	    call error (1, "Input data file does not have c3h format")
	}

	# Make sure the wavelength image has same number
	# pixels as the polarization spectra
	if (IM_LEN(wvptr,1) != npix) {
	    call imunmap (imptr)
	    call imunmap (wvptr)
	    call error (1, "Wavelength image not same size as spectral data")
	}

	# Allocate memory for the spectra to be plotted
	call malloc (wave,    npix, TY_REAL)
	call malloc (flux,    npix, TY_REAL)
	call malloc (sigma,   npix, TY_REAL)
	call malloc (pol1,    npix, TY_REAL)
	call malloc (pol2,    npix, TY_REAL)
	call malloc (pol3,    npix, TY_REAL)
	call malloc (pol1err, npix, TY_REAL)
	call malloc (pol2err, npix, TY_REAL)
	call malloc (pol3err, npix, TY_REAL)

	# Read the polarization spectra
	ig = 14*(set-1) + 1
	call gf_opengr (imptr, ig, datamin, datamax, 0)
	call amovr (Memr[imgl1r(imptr)], Memr[flux], npix)
	call gf_opengr (imptr, ig+4, datamin, datamax, 0)
	call amovr (Memr[imgl1r(imptr)], Memr[sigma], npix)

	if (stridx("pol",Memc[spectra]) > 0) 
	    ig = ig + 8
	else
	    ig = ig + 1

	call gf_opengr (imptr, ig, datamin, datamax, 0)
	call amovr (Memr[imgl1r(imptr)], Memr[pol1], npix)
	ig = ig + 1
	call gf_opengr (imptr, ig, datamin, datamax, 0)
	if (stridx("pol",Memc[spectra]) > 0)
	    call amovr (Memr[imgl1r(imptr)], Memr[pol3], npix)
	else
	    call amovr (Memr[imgl1r(imptr)], Memr[pol2], npix)
	ig = ig + 1
	call gf_opengr (imptr, ig, datamin, datamax, 0)
	if (stridx("pol",Memc[spectra]) > 0)
	    call amovr (Memr[imgl1r(imptr)], Memr[pol2], npix)
	else
	    call amovr (Memr[imgl1r(imptr)], Memr[pol3], npix)

	if (stridx("pol",Memc[spectra]) > 0) 
	    ig = ig + 1
	else
	    ig = ig + 2

	call gf_opengr (imptr, ig, datamin, datamax, 0)
	call amovr (Memr[imgl1r(imptr)], Memr[pol1err], npix)
	ig = ig + 1
	call gf_opengr (imptr, ig, datamin, datamax, 0)
	if (stridx("pol",Memc[spectra]) > 0)
	    call amovr (Memr[imgl1r(imptr)], Memr[pol3err], npix)
	else
	    call amovr (Memr[imgl1r(imptr)], Memr[pol2err], npix)
	ig = ig + 1
	call gf_opengr (imptr, ig, datamin, datamax, 0)
	if (stridx("pol",Memc[spectra]) > 0)
	    call amovr (Memr[imgl1r(imptr)], Memr[pol2err], npix)
	else
	    call amovr (Memr[imgl1r(imptr)], Memr[pol3err], npix)

	# Read the wavelength spectra
	ig = 1
	if (set == 2) ig = 2
	call gf_opengr (wvptr, ig, datamin, datamax, 0)
	call amovr (Memr[imgl1r(wvptr)], Memr[wave], npix)
	
	# Convert linear and circular polarization and errors to %,
	# and convert polarization position angle and error to degrees.
	if (stridx("pol",Memc[spectra]) > 0) {
	    rad1 = 180.0 / 3.141592654
	    call amulkr (Memr[pol1], 100.0, Memr[pol1], npix)
	    call amulkr (Memr[pol2], rad1,  Memr[pol2], npix)
	    call amulkr (Memr[pol1err], 100.0, Memr[pol1err], npix)
	    call amulkr (Memr[pol2err], rad1,  Memr[pol2err], npix)
	    if (circ) {
		call amulkr (Memr[pol3], 100.0, Memr[pol3], npix)
		call amulkr (Memr[pol3err], 100.0, Memr[pol3err], npix)
	    }
	}

	# If plot is to be against inverse wavelength, then convert wave
	if (lrev) call arcpr (1.0e4, Memr[wave], Memr[wave], npix)

	# Get label for plot
	call clgstr ("title", Memc[title], SZ_LINE)
	if (streq (Memc[title], "imtitle")) {
	    call strcpy (Memc[imname], Memc[title], SZ_LINE)
	    call strcat (": ", Memc[title], SZ_LINE)
	    call strcat (IM_TITLE(imptr), Memc[title], SZ_LINE)
	}

	# Draw the plot
	call polplt (Memr[wave],Memr[flux],Memr[sigma],Memr[pol3],Memr[pol3err],
		     Memr[pol1],Memr[pol1err],Memr[pol2],Memr[pol2err],npix,
		     lrev,errors,circ,Memc[title],set,Memc[spectra])
     
	# All done; deallocate memory and close images
	call mfree (wave,    TY_REAL)
	call mfree (flux,    TY_REAL)
	call mfree (sigma,   TY_REAL)
	call mfree (pol1,    TY_REAL)
	call mfree (pol2,    TY_REAL)
	call mfree (pol3,    TY_REAL)
	call mfree (pol1err, TY_REAL)
	call mfree (pol2err, TY_REAL)
	call mfree (pol3err, TY_REAL)
	call sfree (sp)
	call imunmap (imptr)
	call imunmap (wvptr)

end


procedure polplt (wave, flux, sigma, pol3, pol3err, pol1, pol1err, pol2,
		  pol2err, n, lrev, err, circ, label, set, spectra)

# POLPLT - Plots the polarization spectra as a stacked plot.

int	n, set
real	wave[n], flux[n], sigma[n], pol1[n], pol1err[n], pol2[n], pol2err[n],
	pol3[n], pol3err[n]
bool	lrev, err, circ
char	label[ARB], spectra[ARB]

int	i
real	wmin, wmax, imax, imin, cmax, cmin, lmax, lmin, pamax, pamin
real	bot, top, intv, wx1, wx2, wy1, wy2
pointer	device, xlabel, flabel, clabel, llabel, palabl, ptype

int	stridx()
real	clgetr()
pointer	sp, gp, gopen()
bool	dumb

begin

	# Allocate memory for local variables
	call smark (sp)
	call salloc (device, SZ_LINE, TY_CHAR)
	call salloc (xlabel, SZ_LINE, TY_CHAR)
	call salloc (flabel, SZ_LINE, TY_CHAR)
	call salloc (clabel, SZ_LINE, TY_CHAR)
	call salloc (llabel, SZ_LINE, TY_CHAR)
	call salloc (palabl, SZ_LINE, TY_CHAR)
	call salloc (ptype, SZ_FNAME, TY_CHAR)

	# Get the wavelength range to plot. If min=max, then plot entire range.
	wmin = clgetr ("wmin")
	wmax = clgetr ("wmax")
	if (wmin == wmax) {
	    wmin = wave[1]
	    wmax = wave[n]

	    # wavelength vector is reversed for AMBER detector data
	    if (!lrev && wmin>wmax) {
		wmin = wave[n]
		wmax = wave[1]
	    }
	}

	# Get y-axis plot limits for flux, circular polarization (%),
	# linear polarization (%), and position angle (degrees) spectra.
	# If min=max, then autoscale.

	imin = clgetr ("imin")
	imax = clgetr ("imax")
	if (imin == imax) {
	    dumb = false
	    call limits (wave,flux,sigma,n,wmin,wmax,lrev,dumb,imin,imax)
	}

	if (circ) {
	    cmin = clgetr ("cmin")
	    cmax = clgetr ("cmax")
	    if (cmin == cmax) 
		call limits (wave,pol3,pol3err,n,wmin,wmax,lrev,err,cmin,cmax)
	}

	lmin = clgetr ("lmin")
	lmax = clgetr ("lmax")
	if (lmin == lmax)
	    call limits (wave,pol1,pol1err,n,wmin,wmax,lrev,err,lmin,lmax)

	pamin = clgetr ("pamin")
	pamax = clgetr ("pamax")
	if (pamin == pamax)
	    call limits (wave,pol2,pol2err,n,wmin,wmax,lrev,err,pamin,pamax)

	# Set-up titles for x-axis, flux, circular and linear polarization,
	# and position angle plots
	if (!lrev) {
	    if (wave[1] > 1000.0) 
		call strcpy ("Wavelength (A)", Memc[xlabel], SZ_LINE)
	    else
		call strcpy ("Channels", Memc[xlabel], SZ_LINE)

	} else 
	    call strcpy ("1/lambda (1/micron)", Memc[xlabel], SZ_LINE)

	if (imax < 1.0) 
	    call strcpy ("Flux", Memc[flabel], SZ_LINE)
	else
	    call strcpy ("Counts", Memc[flabel], SZ_LINE)

	if (stridx("pol",spectra) > 0) {
	    call strcpy ("Lin. Pol (%)", Memc[llabel], SZ_LINE)
	    call strcpy ("Pol. PA (Deg)", Memc[palabl], SZ_LINE)
	    call strcpy ("Circ. Pol (%)", Memc[clabel], SZ_LINE)
	} else {
	    call strcpy ("Q", Memc[llabel], SZ_LINE)
	    call strcpy ("U", Memc[palabl], SZ_LINE)
	    call strcpy ("V", Memc[clabel], SZ_LINE)
	}
	
	# Get the graphics device
	call clgstr ("device", Memc[device], SZ_LINE)

	# Open the graphics device
	gp = gopen (Memc[device], NEW_FILE, STDGRAPH)
	call gsetr (gp, G_YTICKLABELSIZE, 0.8)

	# Calculate the window coordinates
	bot = 0.12
	top = 0.90
	intv = (top-bot)/3.0
	if (circ) intv = (top-bot)/4.0

	# Plot the data: First, the flux spectrum
	call gsview (gp, 0.17, 0.96, bot, bot+intv)
	call gswind (gp, wmin, wmax, imin, imax)
	call glabax (gp, "", Memc[xlabel], Memc[flabel])

	# Only join points that have flux-err non-zero
	do i = 1, n-1 {
	   if (sigma[i]>0 && sigma[i+1]>0)
	       call gline (gp, wave[i], flux[i], wave[i+1], flux[i+1])	       
	}

	# Turn off x-axis tick labels for remaining plots
	call gseti (gp, G_XLABELTICKS, NO)

	# Find out what plot style to use
	call clgstr("ptype", Memc[ptype], SZ_FNAME)

	# Next, do the circular polarization plot (if requested)
	if (circ) 
	    call plotit (gp, bot, intv, wmin, wmax, cmin, cmax, Memc[clabel],
			 pol3, pol3err, wave, n, err, Memc[ptype])

	# Next, do the linear polarization plot
	call plotit (gp, bot, intv, wmin, wmax, lmin, lmax, Memc[llabel],
		     pol1, pol1err, wave, n, err, Memc[ptype])

	# Finally, do the polarization position angle plot
	call plotit (gp, bot, intv, wmin, wmax, pamin, pamax, Memc[palabl],
		     pol2, pol2err, wave, n, err, Memc[ptype])

	# Plot title
	call ggview (gp, wx1, wx2, wy1, wy2)
	call gseti  (gp, G_WCS, 0)
	bot = min (wy2+.07, 0.99)
	call gtext  (gp, (wx1+wx2)/2.0, bot, label, "h=c;v=t;f=b;s=1.0")
	if (set==1) call strcpy ("Pass direction: 1", label, SZ_LINE)
	if (set==2) call strcpy ("Pass direction: 2", label, SZ_LINE)
	if (set==3) call strcpy ("Pass direction: 1+2 combined", label, SZ_LINE)
	if (set==4) call strcpy ("Pass direction: 1+2 combined and corrected",
				 label, SZ_LINE)
	bot = min (wy2+.03, 0.99)
	call gtext  (gp, (wx1+wx2)/2.0, bot, label, "h=c;v=t;f=b;s=.8")

	# Flush the graphics buffer
	call gflush (gp)

	# Close the graphics device
	call gclose (gp)

	# Free memory
	call sfree (sp)

end


procedure limits (x, y, yerr, n, lox, hix, lrev, errors, plmin, plmax)

# LIMITS - Calculate the min and max y-axis plot limits for data
# within specified x range.

int	n
real	x[n], y[n], yerr[n]
real	lox, hix, plmin, plmax
bool	lrev, errors

int	i
real	miny, maxy

begin

 	miny =  1.0e32
 	maxy = -1.0e32

	if (lrev) {
	    do i = 1, n {
	       # Only examine data within specified x limits
	       if (x[i]<=lox && x[i]>=hix) {
		   # Include error bars
		   if (errors) {
		       # Only examine data points with non-zero errors
		       if (yerr[i]!=0.0) {
			   miny = min (miny, y[i]-abs(yerr[i]))
			   maxy = max (maxy, y[i]+abs(yerr[i]))
		       }
		   # Without error bars
		   } else {
		       if (yerr[i]!=0.0) {
			   miny = min (miny, y[i])
			   maxy = max (maxy, y[i])
		       }
		   }
		}
	     }
	} else {
	    do i = 1, n {
	       if (x[i]>=lox && x[i]<=hix) {
		   if (errors) {
		       if (yerr[i]!=0.0) {
			   miny = min (miny, y[i]-abs(yerr[i]))
			   maxy = max (maxy, y[i]+abs(yerr[i]))
		       }
		   } else {
		       if (yerr[i]!=0.0) {
			   miny = min (miny, y[i])
			   maxy = max (maxy, y[i])
		       }
		   }
	        }
	    }
	}

	# Add 5% margin on top and bottom
	plmin = miny - 0.05*abs(maxy-miny)
	plmax = maxy + 0.05*abs(maxy-miny)

end

procedure plotit (gp, bot, intv, wmin, wmax, dmin, dmax, label,
		  dpol, dpolerr, wave, n, err, ptype)

pointer	gp
int	n
real	bot, intv, wmin, wmax, dmin, dmax
real	dpol[n], dpolerr[n], wave[n]
char	label[ARB], ptype[ARB]
bool	err

int	i, j, stridx()
real	dx1, dx2, dxsv
real	sz, x
bool	first

begin
	    bot=bot+intv
	    call gsview (gp, 0.17, 0.96, bot, bot+intv)
	    call gswind (gp, wmin, wmax, dmin, dmax)
	    call glabax (gp, "", "", label)

	    # Draw dashed line at zero
	    call gseti (gp, G_PLTYPE, 2)
	    call gline (gp, wmin, 0.0, wmax, 0.0)
	    call gseti (gp, G_PLTYPE, 1)

	    call strlwr( ptype )

	    # Point plot
	    if (stridx("p", ptype) > 0) {

	    do i = 1, n {
	       if (dpolerr[i]!=0.0) {
		   call gmark (gp, wave[i], dpol[i], GM_PLUS, 2.0, 2.0)
		   if (err) {
		       sz = 2. * dpolerr[i]
		       call gmark (gp, wave[i], dpol[i], GM_VLINE, 1.0, -sz)
		   }
	       }
	    }

	    # Continuous line plot
	    } else if (stridx("c", ptype) > 0) {

		do i = 1, n-1 {
		   if (dpolerr[i]!=0.0 && dpolerr[i+1]!=0.0)
		       call gline (gp, wave[i], dpol[i], wave[i+1], dpol[i+1])
		}
		if (err) {
		    do i = 1, n {
		       sz = 2. * dpolerr[i]
		       if (dpolerr[i]!=0.0)
			   call gmark (gp, wave[i], dpol[i], GM_VLINE, 1., -sz)
		    }
		}

	    # Histogram plot
	    } else if (stridx("h", ptype) > 0) {

		first = true
		dxsv = 0.0
		do i = 1, n-1 {
		   if (dpolerr[i]!=0.0) {
			if (first) {
			    call gamove (gp, wave[i], dpol[i])
			    first = false
			}
			j = i + 1
			while (j <= n && dpolerr[j]==0.0) j = j + 1
			j = min (j, n)
			if (j==n && dpolerr[j]==0.0) break

			#x = (wave[j] + wave[i]) / 2.0

			dx1 = (wave[j] - wave[i])/2.0
			if (j!=n) {
			    dx2 = (wave[j+1] - wave[j])/2.0
			    dx1 = min(abs(dx1),abs(dx2))
			} else {
			    dx2 = dx1
			    dx1 = abs(dx1)
			}
			if (dxsv != 0.0 && dxsv < dx1) {
			    if (dx2 > 0) 
			        x = wave[i] + dxsv
			    else
			        x = wave[i] - dxsv
			    dxsv = abs(wave[j] - x)
			} else {
			    if (dx2 > 0)
				x = wave[j] - dx1
			    else
				x = wave[j] + dx1
			    dxsv = dx1
			}

			call gadraw (gp, x, dpol[i])
			call gadraw (gp, x, dpol[j])
			call gadraw (gp, wave[j], dpol[j])
		   }
		}
		if (err) {
		    do i = 1, n {
		       if (dpolerr[i]!=0.0) {
			   sz = 2. * dpolerr[i]
			   call gmark (gp, wave[i], dpol[i], GM_VLINE, 1., -sz)
		       }
		    }
		}
	    }

end
