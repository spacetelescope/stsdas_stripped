# WCOEFF -- Procedure to write the new plate coeeficients obtained
#	    by the plate solution algorithm onto the image header.

define	SZ_KEYWORD	8
define	NTERMS_MODEL	20

procedure wcoeff (im, fd, xcoeff, ycoeff, ncoef, plate_model, crpix1, crpix2,
		  crval1, crval2, x_pixel_size, y_pixel_size, plate_scale)

pointer	im		# Image descriptor
pointer	fd		# Output ascii file descriptor
double	xcoeff[ncoef]	# coefficient in x
double	ycoeff[ncoef]	# coefficient in y
int	ncoef		# number of coefficients
int	plate_model[NTERMS_MODEL]	# plate model flags
double	crpix1, crpix2	# Reference pixel
double	crval1, crval2	# Equatorial coordinates for reference point
real	x_pixel_size	# Pixel size in micron in X 
real	y_pixel_size	# Pixel size in micron in Y 
double	plate_scale	# [arcs/mm]
pointer sp, kp
int	i, j

char	stime[SZ_LINE]
int	clktime()

begin
	call smark(sp)
	call salloc (kp, SZ_KEYWORD, TY_CHAR)
	if (im != -1) {
	   j = 1
	   do i = 1, NTERMS_MODEL {
	      if (plate_model(i) == 1) {
	         call sprintf (Memc[kp], SZ_KEYWORD, "NAMDX%d")
	            call pargi (i)
	         call imaddd (im, Memc[kp], xcoeff[j])	
	         j = j + 1	
	      }
	   }	 

	   j = 1
	   do i = 1, NTERMS_MODEL {
	      if (plate_model(i) == 1) {
	         call sprintf (Memc[kp], SZ_KEYWORD, "NAMDY%d")
	            call pargi (i)
	         call imaddd (im, Memc[kp], ycoeff[j])	
	         j = j + 1	
	      }
	   }	 

	call sfree (sp)
	}

	# write to output file if needed
	if (fd != -1) {
	   call cnvtime (clktime(0), stime, SZ_LINE)
	   call fprintf (fd, "Astrometic solution file  %s\n\n")
	       call pargstr (stime)

	   call fprintf (fd, "CRPIX1     %g\n")
	       call pargd (crpix1)
	   call fprintf (fd, "CRPIX2     %g\n")
	       call pargd (crpix2)
	   call fprintf (fd, "CRVAL1     %g\n")
	       call pargd (crval1)
	   call fprintf (fd, "CRVAL2     %g\n")
	       call pargd (crval2)
	   call fprintf (fd, "XPIXELSZ   %g\n")
	       call pargr (x_pixel_size)
	   call fprintf (fd, "YPIXELSZ   %g\n")
	       call pargr (y_pixel_size)
	   call fprintf (fd, "PLATESCL   %g\n")
	       call pargd (plate_scale)

	   j = 1
	   do i = 1, NTERMS_MODEL {
	      if (plate_model(i) == 1) {
	         call fprintf (fd, "XCOEFF%-2d   %g\n") 
		     call pargi (i)
		     call pargd (xcoeff(j))
	         j = j + 1	
	      }
	   }	 
  	   j = 1
	   do i = 1, NTERMS_MODEL {
	      if (plate_model(i) == 1) {
	         call fprintf (fd, "YCOEFF%-2d   %g\n")
		     call pargi (i)
		     call pargd (ycoeff(j))
	         j = j + 1	
	      }
	   }
	}
end
