include <math.h>

# XYEQ -- Procedure to calculate equatorial coordinates from a set
#	  of (x,y) in pixel, given a plate solution from program
#         'pltsol' or the solution from GASP.

procedure t_xyeq()


pointer im, fd, fxy
					# Parameters from the original frame
pointer format[2], fmt, sp              # Output format for RA and DEC
double  plate_cen_x, plate_cen_y	# pixel reference (microns)
double  plate_cen_ra, plate_cen_dec	# equatorial reference position (deg)
double	plate_scale			# [arcs/mm]
int	ra_h, ra_m, axis
double	ra_s
char	dec_sign[1], ra_fmt[64], dec_fmt[64]
int	dec_d, dec_m
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

double	x, y		# input pixel coordinates
double  mag, col	# input mag and color (Not use in here)
double	xcorner, ycorner	# input lower left corner of frame w/r to 
				# original image (scannen plate)

double  iamd_ra, iamd_dec	# output (ra,dec) from the original solution
double	namd_ra, namd_dec	# output (ra,dec) from the new plate solution
double	icd_ra, icd_dec		# output (ra,dec) from the CD matrix values

bool	iminfo, iamd, icd, namd, hrs
int	icol[2]
int	clgeti()
bool	clgetb()
double	imgetd()
int	imgeti()
char	line[SZ_LINE], input[SZ_FNAME], xyfile[SZ_FNAME]
char	parname[8], pix_center[4]
real   offset
double	dcrval1, dcrval2, mcrpix1, mcrpix2
double  objx, objy

int	immap(), open(), getline(), strncmp(), nowhite(), imaccf()
int	i, nch, nskip, strcmp()


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

	# input ascii table with (x,y) positions
	call clgstr ("xyfile", xyfile, SZ_LINE)
	call clgstr ("pix_center", pix_center, 4)
	if (strncmp (xyfile, "STDIN", 5) != 0) {
	   icol(1) = clgeti ("xcolnum")
	   icol(2) = clgeti ("ycolnum")
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

	# Do we want RA in hours

	hrs = clgetb ("ra_hours")

	# Get user output formats for RA and DEC

        call clgstr ("ra_format", Memc[format[1]], SZ_FNAME)
        call clgstr ("dec_format", Memc[format[2]], SZ_FNAME)
	if (nowhite (Memc[format[1]], ra_fmt, SZ_FNAME) == 0)
		call strcpy("%11.2h",ra_fmt)
	if (nowhite (Memc[format[2]], dec_fmt, SZ_FNAME) == 0)
		call strcpy("%11.2h",dec_fmt)

	call sprintf(Memc[fmt], SZ_FNAME, "%s %s ")
		call pargstr (ra_fmt)
		call pargstr (dec_fmt)
	
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

	# open input ascii table with (x,y) values
	fxy = open (xyfile, READ_ONLY, TEXT_FILE)

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
	   iferr (call imgstr (im, "PLTDECSN", dec_sign, 1))
	      call error(13,"Keyword PLTDECSN not found in image header")
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


	# Read pairs of (x,y) and calculate (ra,dec) with one or more 
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
	   call printf ("%4t X %13t Y %20t RA_nsol %31t DEC_nsol \
%44t RA_osol %55t DEC_osol %68t RA_cd %79t DEC_cd\n") 
           call printf ("%22t(%s) %33t(deg)\n")
	} else if (namd && !iamd && icd) {
	   call printf ("%4t X %13t Y %20t RA_nsol %31t DEC_nsol \
%44t RA_cd %55t DEC_cd\n")
           call printf ("%22t(%s) %33t(deg)\n")
	} else if (namd && iamd && !icd) {
	   call printf ("%4t X %13t Y %20t RA_nsol %31t DEC_nsol \
%44t RA_osol %55t DEC_osol\n") 
           call printf ("%22t(%s) %33t(deg)\n")
	} else if (namd && !iamd && !icd) {
	   call printf ("%4t X %13t Y %20t RA_nsol %31t DEC_nsol\n")
           call printf ("%22t(%s) %33t(deg)\n")
	} else if (!namd && iamd && icd) {
	   call printf ("%4t X %13t Y %20t RA_osol %31t DEC_osol \
%44t RA_cd %55t DEC_cd\n")
           call printf ("%22t(%s) %33t(deg)\n")
	} else if (!namd && iamd & !icd) {
	   call printf ("%4t X %13t Y %20t RA_osol %31t DEC_osol\n")
           call printf ("%22t(%s) %33t(deg)\n")
	} else if (!namd && !iamd & icd) {
	   call printf ("%4t X %13t Y %20t RA_cd %31t DEC_cd\n")
           call printf ("%22t(%s) %33t(deg)\n")
	}
	if (hrs)
	   call pargstr("hrs")
        else
	   call pargstr("deg")

	while (getline (fxy, line) != EOF) {
	   call rdxy (line, icol, x, y)
	   call printf ("%8.2f %8.2f ")
		call pargd (x)	
		call pargd (y)	

	   if (namd) {
	      # Get ra and dec for (x,y) giving the calculated plate solution
	      offset = 0.5
	      if (strcmp (pix_center, "dss") == 0)
		 offset = 0.0
	      else if (strcmp (pix_center, "cos") == 0)
		 offset = 1.0
	      objx = x + offset
	      objy = y + offset
	      call ccgseq (dcrval1, dcrval2, mcrpix1, mcrpix2, 
     	                  x_pixel_size, y_pixel_size, plate_scale,
     	                  namdx, namdy,              
     	                  objx, objy, mag, col,
     	                  namd_ra, namd_dec)


	      call printf (Memc[fmt])
		 if (hrs)
		    call pargd (RADTODEG(namd_ra)/15.0)
		 else
		    call pargd (RADTODEG(namd_ra))
		 call pargd (RADTODEG(namd_dec))
	      if (!iamd && !icd) {
		 call printf ("\n")
		 call flush (STDOUT)
	      }
	   }

	   if (iamd ) {
	      # calculate ra and dec for the (x,y) position giving the complete
	      # plate solution.
	      offset = -0.5
	      if (strcmp (pix_center, "dss") == 0)
		 offset = -1.0
	      else if (strcmp (pix_center, "cos") == 0)
		 offset = 0.0
	      objx = x + xcorner + offset
	      objy = y + ycorner + offset
	      call ccgseq (plate_cen_ra, plate_cen_dec,
    	      	          plate_cen_x, plate_cen_y,
     	                  x_pixel_size, y_pixel_size, plate_scale,
     	                  amdx, amdy,              
     	                  objx, objy, mag, col,
     	                  iamd_ra, iamd_dec)

              call printf (Memc[fmt])
		 if (hrs)
		    call pargd (RADTODEG(iamd_ra)/15.0)
		 else
		    call pargd (RADTODEG(iamd_ra))
		 call pargd (RADTODEG(iamd_dec))
	      if (namd && !icd || !namd && !icd) {
		 call printf ("\n")
	         call flush (STDOUT)
	      }
	   }	

	   if (icd) {
	      # Get ra and dec for (x,y) giving the input image CD values
	      offset = 0.0
	      if (strcmp (pix_center, "dss") == 0)
		 offset = -0.5
	      else if (strcmp (pix_center, "cos") == 0)
		 offset = 0.5
              objx = x + offset
	      objy = y + offset
	      call pixtoeq (dcrval1, dcrval2, crpix1, crpix2, 
	   	            icds, objx, objy, icd_ra, icd_dec)

              call printf (Memc[fmt])

		 if (hrs)
		    call pargd (RADTODEG(icd_ra)/15.0)
		 else
		    call pargd (RADTODEG(icd_ra))
		 call pargd (RADTODEG(icd_dec))
	      call printf("\n")
	      call flush (STDOUT)
	   }
	}
        call sfree (sp)

end
