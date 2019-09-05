# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	<gset.h>
include	<error.h>
include <imio.h>
include <math.h>
include "iraf$pkg/xtools/icfit/icfit.h"
include  "pls.h"

# List of colon commands.
define	CMDS "|model|errors|vshow|cds|wcoeff"

define	MODEL		1	# Set or show plate model
define	ERRORS		2	# Show errors of fit
define	VSHOW		3	# Show verbose information
define	CDS		4	# show Coefficients in [degrees/pixels]
define  WCOEFF		5	# Write coefficient to input image or file

# COLON -- Processes colon commands.  The common flags 
# signal changes in fitting parameters or the need to redraw the graph.

procedure colon (ic, im, cmdstr, gp, gt, pn, xi_res, eta_res, 
		 xcoeff, ycoeff, xsig, ysig, xchisqr, ychisqr,
		 crpix1, crpix2, crval1, crval2, plate_scale,
		 plate_model, nterm, npts,x_pixel_size,y_pixel_size)

pointer	ic				# ICFIT pointer
pointer	im				# image pointer
char	cmdstr[ARB]			# Command string
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
pointer pn
double	crpix1, crpix2, crval1, crval2, plate_scale
double  ncrpix1, ncrpix2
double	xi_res[npts], eta_res[npts]
double	xcoeff[nterm], ycoeff[nterm]
double  mag,col, dxpz, dypz
real	xsig[nterm], ysig[nterm]
real	x_pixel_size, y_pixel_size, xf, yf
int	plate_model[NTERMS_MODEL]
int	npts, nterm

char	cmd[SZ_LINE], image[SZ_FNAME]
double	xchisqr, ychisqr
pointer	fd, bas
int	ncmd, ival1, ival2, i, jm, save_im

int	nscan(), strdic(), open(), immap()
errchk  ic_show

begin
	# Use formated scan to parse the command string.
	# The first word is the command and it may be minimum match
	# abbreviated with the list of commands.

	call sscan (cmdstr)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	switch (ncmd) {
	case MODEL: # :model- List or set the fitting function.
	    call malloc (bas, TERML*NTERMS_MODEL, TY_CHAR)
	    call get_model (bas)
	    call gargi (ival1)
	    call gargi (ival2)
	    if (nscan() == 1) {
	        call gdeactivate (gp, AW_CLEAR)
		call printf ("No  Term            value\n")
	        do i = 1, NTERMS_MODEL {
	           call printf("%d  %s %22t%d\n")
		       call pargi(i)
		       call pargstr(Memc[bas+TERML*(i-1)])
		       call pargi(plate_model[i])
	        }
	        call greactivate (gp, AW_PAUSE)
	    } else if (nscan() == 2) {
		call printf ("%d  %s value= %d\n")
		    call pargi(ival1)
		    call pargstr(Memc[bas+TERML*(ival1-1)])
		    call pargi(plate_model[ival1])
	    } else {
		call printf ("%d  %s value= %d\n")
		    call pargi(ival1)
		    call pargstr(Memc[bas+TERML*(ival1-1)])
		    plate_model[ival1] = ival2
		    call pargi(plate_model[ival1])
	    }
	    call mfree (bas, TY_CHAR)

	case ERRORS: # :errors - print errors analysis of fit
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1) {
		call gdeactivate (gp, AW_CLEAR)
		call pls_error (ic, "STDOUT", nterm, npts, plate_model,
		     xi_res, eta_res, xcoeff, ycoeff, xsig, ysig,
		     xchisqr, ychisqr)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr {
		    call ic_show (ic, cmd, gt)
		     call pls_error (ic, cmd, nterm, npts, plate_model,
		     xi_res, eta_res, xcoeff, ycoeff, xsig, ysig,
		     xchisqr, ychisqr)
		} then
		    call erract (EA_WARN)
	    }
	case VSHOW: # :vshow - Verbose list of the fitting parameters. 
	    call gargstr (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
		call gdeactivate (gp, AW_CLEAR)
		call pls_vshow (ic, "STDOUT", pn, nterm, npts, plate_model,
		     xi_res, eta_res, xcoeff, ycoeff, xsig, ysig,
		     xchisqr, ychisqr)
		call greactivate (gp, AW_PAUSE)
	    } else {
		iferr {
		call pls_vshow (ic, cmd, pn, nterm, npts, plate_model,
		     xi_res, eta_res, xcoeff, ycoeff, xsig, ysig,
		     xchisqr, ychisqr)
		} then 
		    call erract (EA_WARN)
	    }
	case CDS: # Show coefficients in units of degrees/pixels.
		  # or write them into an image header.
	    xf = x_pixel_size*1.0e-5/36.
	    yf = y_pixel_size*1.0e-5/36.

	    # Calculate the new CRPIX(1,2) based on the new solution.
	    mag = 0.0
	    col = 0.0
	    dxpz = x_pixel_size
	    dypz = y_pixel_size
	    call ccgsxy (DEGTORAD(crval1), DEGTORAD(crval2), 
			 crpix1*x_pixel_size, crpix2*y_pixel_size,
			  dxpz, dypz, plate_scale,
			  xcoeff, ycoeff, ncrpix1, ncrpix2, 
			  mag, col, DEGTORAD(crval1), DEGTORAD(crval2))

	    # No image name given, print to STDOUT.
	    fd = open ("STDOUT", APPEND, TEXT_FILE)
	    call gdeactivate (gp, AW_CLEAR)
	    call gargwrd (image, SZ_FNAME)

	    if (nscan() == 1) {
	       call fprintf (fd,
	       "CRPIX1: %13.8g   #New CRPIX1 based on the new solution.\n")
		    call pargd(ncrpix1)
	       call fprintf (fd,
	       "CRPIX2: %13.8g   #New CRPIX2 based on the new solution.\n")
		    call pargd(ncrpix2)
	       call fprintf (fd,"CRVAL1: %13.8g\n")
		    call pargd(crval1)
	       call fprintf (fd,"CRVAL2: %13.8g\n")
		    call pargd(crval2)
	       call fprintf (fd,"CD1_1: %13.8g    ")
		    call pargd(-1.0*xcoeff(1)*xf)
	       call fprintf (fd,
			      "# Change in RA per pixel along the first axis.")
	       call fprintf (fd,"\nCD1_2: %13.8g    ")
		    call pargd(xcoeff(2)*xf)
	       call fprintf (fd,"# same along the second axis.")
	       call fprintf (fd,"\nCD2_2: %13.8g    ")
		    call pargd(ycoeff(1)*xf)
	       call fprintf (fd,
			    "# Change in DEC per pixel along the second axis.")
	       call fprintf (fd,"\nCD2_1: %13.8g    ")
		    call pargd(-1.0*ycoeff(2)*xf)
	       call fprintf (fd,"# same along the first axis.\n")
	    }else {
               if (im == -1)
		  jm = immap (image, READ_WRITE, 0)
	       else
		  jm = im
	       call imastr (jm, "CTYPE1", "RA---TAN")
	       call imastr (jm, "CTYPE2", "DEC--TAN")
	       call imaddd (jm, "CRPIX1", ncrpix1)
	       call imaddd (jm, "CRPIX2", ncrpix2)
	       call imaddd (jm, "CRVAL1", crval1)
	       call imaddd (jm, "CRVAL2", crval2)
	       call imaddd (jm, "CD1_1", -1.0*xcoeff(1)*xf )
	       call imaddd (jm, "CD1_2", xcoeff(2)*xf )
	       call imaddd (jm, "CD2_2", ycoeff(1)*xf )
	       call imaddd (jm, "CD2_1", -1.0*ycoeff(2)*xf )
	
	       call fprintf (fd,
		  "\n\n... WCS keywords written into: '%s' header\n\n")
		  call pargstr(IM_NAME(jm))
	       if (im == -1) 
		  call imunmap(jm)
	    }
	    call close (fd)
	    call greactivate (gp, AW_PAUSE)
	case WCOEFF: # Write coefficients to image or to output ascii file
	    call gargwrd (cmd, SZ_LINE)
	    fd = -1
	    if (nscan() == 1) 
	       fd = -1
	    else { 
	       # We have a filename, open as image or as file.
	       save_im = im
	       if (save_im == -1) {
		  # There is no image open
		  iferr(im = immap (cmd, READ_WRITE, 0)){
			# Is not image type, try text file
			fd = open (cmd, APPEND, TEXT_FILE)
			im = -1
	          }
	       } else {
		  # There is an image open, so just open text file.
		  fd = open (cmd, APPEND, TEXT_FILE)
	       }
	    }
	    # Calculate the new CRPIX(1,2) based on the new solution.
	    mag = 0.0
	    col = 0.0
	    dxpz = x_pixel_size
	    dypz = y_pixel_size
	    call ccgsxy (DEGTORAD(crval1), DEGTORAD(crval2), 
			 crpix1*x_pixel_size, crpix2*y_pixel_size,
			  dxpz, dypz, plate_scale,
			  xcoeff, ycoeff, ncrpix1, ncrpix2, 
			  mag, col, DEGTORAD(crval1), DEGTORAD(crval2))

	    call wcoeff (im, fd, xcoeff, ycoeff, nterm, plate_model, 
			 ncrpix1, ncrpix2, crval1, crval2, x_pixel_size, 
			 y_pixel_size, plate_scale)
	    if (save_im == -1 && im != -1) {
		call imunmap(im)
		im = save_im
	    }

	    if (fd != -1)
		call close (fd)	
	default:
	    call eprintf("\007Warning: command `%s' not supported\n")
		 call pargstr(cmd)
	}
end
# GET_MODEL --
procedure get_model(bas)

pointer	bas
begin
	call strcpy ("x",	     Memc[bas], TERML)
	call strcpy ("y",	     Memc[bas+TERML], TERML)
	call strcpy ("1",	     Memc[bas+TERML*2], TERML)
	call strcpy ("x2",	     Memc[bas+TERML*3], TERML)
	call strcpy ("xy",	     Memc[bas+TERML*4], TERML)
	call strcpy ("y2",	     Memc[bas+TERML*5], TERML)
	call strcpy ("x2 + y2",	     Memc[bas+TERML*6], TERML)
	call strcpy ("x3",	     Memc[bas+TERML*7], TERML)
	call strcpy ("xxy",	     Memc[bas+TERML*8], TERML)
	call strcpy ("xyy",	     Memc[bas+TERML*9], TERML)
	call strcpy ("y3",	     Memc[bas+TERML*10], TERML)
	call strcpy ("x(x2 + y2)",   Memc[bas+TERML*11], TERML)
	call strcpy ("x(x2 + y2)2",  Memc[bas+TERML*12], TERML)
	call strcpy ("mag",	     Memc[bas+TERML*13], TERML)
	call strcpy ("mag2",	     Memc[bas+TERML*14], TERML)
	call strcpy ("mag3",	     Memc[bas+TERML*15], TERML)
	call strcpy ("mag*x",	     Memc[bas+TERML*16], TERML)
	call strcpy ("mag(x2 + y2)", Memc[bas+TERML*17], TERML)
	call strcpy ("mag*x(x2+y2)", Memc[bas+TERML*18], TERML)
	call strcpy ("color", 	     Memc[bas+TERML*19], TERML)
end
