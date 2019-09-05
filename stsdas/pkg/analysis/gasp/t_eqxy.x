include <math.h>

# EQXY -- Procedure to calculate pixel coordinates from a set
#	  of (ra,dec) in radians, given a plate solution from program
#         'pltsol' or the solution from GASP.
# Feb 1995 NZ
# The CNPIX's values for the gasp plates is measures from
# the lower left corner of the pixel rather than the
# middle point in the case of IRAF convention.
# So correct by 1/2 pixel.
# A new offset value was introduced to count for this 1/2 pixels.
# Now the user can choose which convention the x,y are going to be output
# by this program.

procedure  t_eqxy()


pointer im, fd, fxy
pointer format[2],fmt,sp		# Output format for X and Y
					# Parameters from the original frame
double  plate_cen_x, plate_cen_y	# pixel reference (microns)
double  plate_cen_ra, plate_cen_dec	# equatorial reference position (deg)
double	plate_scale			# [arcs/mm]
int	ra_h, ra_m
double	ra_s
char	dec_sign[1], xfmt[64], yfmt[64]
int	dec_d, dec_m, axis
double	dec_s
double	x_pixel_size	# x coordinate pixel size (mm)
double	y_pixel_size	# y coordinate pixel size (mm)
double	amdx[20]	# Original Plate Solution coeff. in X
double	amdy[20]	# "			            Y

			# Reference parameters for the new solution
double	crpix1, crpix2	# pixel position for reference point (pixel)
double	crval1, crval2	# equatorial coordinates of reference point (degrees)
double	namdx[20]	# New Plate Solution from 'pltsol' in X
double	namdy[20]	# "				      Y
double	icds[4]		# CD matrix values

double	x, y		# output pixel coordinates
double  gx, gy, ix, iy
double  mag, col	# input mag and color (Not use in here)
double	xcorner, ycorner	# input lower left corner of frame w/r to 
				# original image (scannen plate)

double	ra, dec			# input (ra,dec) from an ascii table

bool	iminfo, iamd, icd, namd, hrs
int	icol[2]
int	clgeti()
bool	clgetb()
double	imgetd()
int	imgeti()
char	line[SZ_LINE], input[SZ_FNAME], rdfile[SZ_FNAME]
char	parname[8], pix_center[4]
double	dcrval1, dcrval2, mcrpix1, mcrpix2
real	offset
int	immap(), open(), getline(), nowhite(), imaccf(), strcmp()
int	i, nch, nskip, strncmp()


begin

	# Allocate memory for output formats
        call smark (sp)
        do axis = 1, 2 {
            call salloc (format[axis], SZ_FNAME, TY_CHAR)
        }
        call salloc (fmt, SZ_FNAME, TY_CHAR)

	# If there is an image with the plate solution
	iminfo = clgetb ("iminfo")

	# If so get image name, otherwise get ascii file with coefficients
	if (iminfo)
	   call clgstr ("image", input, SZ_LINE)
	else
	   call clgstr ("coeffile", input, SZ_LINE)

	# input ascii table with (ra,dec) positions
	call clgstr ("rdfile", rdfile, SZ_LINE)
	call clgstr ("pix_center", pix_center, 4)
	hrs = clgetb ("ra_hours")
	if (strncmp (rdfile, "STDIN",5) != 0) {
	   icol(1) = clgeti ("rcolnum")
	   icol(2) = clgeti ("dcolnum")
	   nskip = clgeti ("nskip")
	} else {
           icol[1] = 1
	   icol[2] = 2
	   nskip = 0
	}

		
	# Do we want to use original plate solution	
	#
	iamd = clgetb ("original")

	# Do we want to use new plate solution
	#
	namd = clgetb ("new")

	# Do we want to use CD matrix
	#
	icd = clgetb ("cdmatx")

        # Get user output formats for x and y

        call clgstr ("xformat", Memc[format[1]], SZ_FNAME)
        call clgstr ("yformat", Memc[format[2]], SZ_FNAME)
        if (nowhite (Memc[format[1]], xfmt, SZ_FNAME) == 0)
                call strcpy("%8.2f",xfmt)
        if (nowhite (Memc[format[2]], yfmt, SZ_FNAME) == 0)
                call strcpy("%8.2f",yfmt)

        call sprintf(Memc[fmt], SZ_FNAME, "%s %s")
                call pargstr (xfmt)
                call pargstr (yfmt)

	
	if (iminfo) {
	   im = immap (input, READ_WRITE, 0)
	   fd = -1
 	} else {
	   fd = open (input, READ_ONLY, TEXT_FILE)
	   im = -1	# No image is open
	}

	# Get reference parameter and solution.
	if (namd || icd || im == -1)
	   call rd_aslf (im, fd, namd, icd, crpix1, crpix2, 
		     crval1, crval2, x_pixel_size, y_pixel_size, 
		     plate_scale, namdx, namdy)

	# open input ascii table with (ra,dec) values
	fxy = open (rdfile, READ_ONLY, TEXT_FILE)
	
	if (!iminfo) {
	   iamd = false
	   icd = false
	   namd = true
	}
	if (im == -1 && iamd)
	   call error (13, "No input image to get original solution")
	if (im == -1 && icd)
	   call error (13, "No input image to get CD matrix values")	 

	if (icd) {
	   icds[1] = imgetd (im, "CD1_1")
	   icds[2] = imgetd (im, "CD1_2")
	   icds[3] = imgetd (im, "CD2_1")
	   icds[4] = imgetd (im, "CD2_2")
	}
	
	# Get reference values to use the original solution
	if (iamd) {
           # See if the new (dss) CNPIX keywords are in the header,
	   # otherwise ask for them.
	   if (imaccf (im, "CNPIX1") == NO)
	   call error (13,
	    "CNPIX1 keyword not found (X lower left corner w/r original frame)")
	   xcorner = imgetd (im, "CNPIX1")
	   ycorner = imgetd (im, "CNPIX2")

	   plate_cen_x = imgetd (im, "PPO3    ")
	   plate_cen_y = imgetd (im, "PPO6    ")
	   x_pixel_size = imgetd (im, "XPIXELSZ")
	   y_pixel_size = imgetd (im, "YPIXELSZ")
	   plate_scale = imgetd (im, "PLTSCALE")
	   ra_h = imgeti (im, "PLTRAH  ")
	   ra_m = imgeti (im, "PLTRAM  ")
	   ra_s = imgetd (im, "PLTRAS  ")
	   call imgstr (im, "PLTDECSN", dec_sign, 1)
	   dec_d = imgeti (im, "PLTDECD ")
	   dec_m = imgeti (im, "PLTDECM ")
	   dec_s = imgetd (im, "PLTDECS ")

	   plate_cen_ra = DEGTORAD ((ra_h + ra_m/60.0d0 + ra_s/3600.0)*15.0d0)
	   plate_cen_dec = DEGTORAD (dec_d + dec_m/60.0d0 + dec_s/3600.0d0)
	   if (dec_sign[1] == '-')
	      plate_cen_dec = -plate_cen_dec
 
	   do i = 1, 20 {
	      call sprintf (parname, SZ_LINE, "AMDX%d")
	           call pargi(i)
	      amdx[i] = imgetd (im, parname)
	   }
	   do i = 1, 20 {
	      call sprintf (parname, SZ_LINE, "AMDY%d")
	           call pargi(i)
	      amdy[i] = imgetd (im, parname)
	   }
	}
	if (iminfo)
	    call imunmap (im)


	# Read pairs of (ra,dec) and calculate (x,y) with one or more 
	# plate solution
	#
	if (namd || icd) {
	   mcrpix1 = crpix1 * x_pixel_size
	   mcrpix2 = crpix2 * y_pixel_size
	   dcrval1 = DEGTORAD (crval1)
	   dcrval2 = DEGTORAD (crval2)
	}
	mag = 0.0
	col = 0.0
	do i = 1, nskip
	   nch = getline (fxy,line)
	# Title
	if (namd && iamd && icd) {
	   call printf ("%2t RA(%s) %14t DEC(deg) %26t X_nsol %35t Y_nsol \
%43t X_osol %52t Y_osol %62t X_cd %71t Y_cd\n") 
	} else if (namd && !iamd && icd) {
	   call printf ("%2t RA(%s) %14t DEC(deg) %26t X_nsol %35t Y_nsol \
%45t X_cd %54t Y_cd\n")
	} else if (namd && iamd && !icd) {
	   call printf ("%2t RA(%s) %14t DEC(deg) %26t X_nsol %35t Y_nsol \
%43t X_osol %52t Y_osol\n") 
	} else if (namd && !iamd && !icd) {
	   call printf ("%2t RA(%s) %14t DEC(deg) %26t X_nsol %35t Y_nsol\n")
	} else if (!namd && iamd && icd) {
	   call printf ("%2t RA(%s) %14t DEC(deg) %26t X_osol %35t Y_osol \
%45t X_cd %54t Y_cd\n")
	} else if (!namd && iamd & !icd) {
	   call printf ("    RA(%s)     DEC(deg)  X_osol   Y_osol\n")
	} else if (namd && !iamd & !icd) {
	   call printf ("    RA(%s)     DEC(deg)  X_nsol   Y_nsol\n")
	} else if (!namd && !iamd & icd) {
	   call printf ("    RA(%s)     DEC(deg)    X_cd     Y_cd\n")
	}
	if (hrs)
	   call pargstr("hrs")
	else
	   call pargstr("deg")
	    
	while (getline (fxy, line) != EOF) {
           #Get ra and dec in degrees
           call rdxy (line, icol, ra, dec)
	   call printf ("%11.2h %11.2h ")
		call pargd (ra)	
		call pargd (dec)	

	   # Convert to Radians
	   if (hrs)
	      ra = DEGTORAD (ra*15.0)
	   else
	      ra = DEGTORAD (ra)
	   dec = DEGTORAD (dec)

	   if (namd) {
	      # Get x and y for (ra,dec) giving the calculated plate solution
	      call ccgsxy (dcrval1, dcrval2, mcrpix1, mcrpix2, 
     	                  x_pixel_size, y_pixel_size, plate_scale,
     	                  namdx, namdy,              
     	                  x, y, mag, col,
     	                  ra, dec)

	      offset = -0.5
	      if (strcmp (pix_center, "dss") == 0)
		   offset = 0.0
	      else if (strcmp (pix_center, "cos") == 0)
	         offset = -1.0
	      call printf (Memc[fmt])
	      x = x + offset
	      y = y + offset
		 call pargd (x)
		 call pargd (y)
	      if (!iamd && !icd) {
		 call printf ("\n")
		 call flush (STDOUT)
	      }
	   }

	   if (iamd ) {
	      # calculate x and y for the (ra,dec) position giving the complete
	      # plate solution.
	      call ccgsxy (plate_cen_ra, plate_cen_dec,
    	      	          plate_cen_x, plate_cen_y,
     	                  x_pixel_size, y_pixel_size, plate_scale,
     	                  amdx, amdy,              
     	                  gx, gy, mag, col,
     	                  ra, dec)

              # The CNPIX's values for the gasp plates is measures from
	      # the lower left corner of the pixel rather than the
	      # middle point in the case of STSDAS convention.
	      # So correct by 1/2 pixel.
	      #

	      offset = 0.5
	      if (strcmp (pix_center, "dss") == 0)
		   offset = 1.0
	      else if (strcmp (pix_center, "cos") == 0)
	         offset = 0.0
	      x = gx - xcorner + offset
	      y = gy - ycorner + offset

	      call printf (Memc[fmt])
		 call pargd (x)
		 call pargd (y)
	      if (namd && !icd || !namd && !icd) {
		 call printf ("\n")
	         call flush (STDOUT)
	      }
	   }	

	   if (icd) {
	      # Get x and y for (ra,dec) giving the input image CD values
	      # Eqtopix takes ra and dec in radians.
              call eqtopix (dcrval1, dcrval2, crpix1, crpix2, 
	   	            icds, ra, dec, ix, iy)

	      offset = 0.0
	      if (strcmp (pix_center, "dss") == 0)
		   offset = 0.5
	      else if (strcmp (pix_center, "cos") == 0)
	         offset = -0.5
	      ix = ix + offset
	      iy = iy + offset

	      call printf (Memc[fmt])
		 call pargd (ix)
		 call pargd (iy)
	      call printf("\n")
	      call flush (STDOUT)
	   }
	}
	call sfree (sp)

end
