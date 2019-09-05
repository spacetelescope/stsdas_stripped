include	<imhdr.h>
include	"ypp.h"

# Plot a gray scale image for FOS ACQ mode or IMAGE mode with MIRROR

procedure ypp_image ()

char	rootname[SZ_FNAME]
char	output[SZ_FNAME]
char	ftype[SZ_FNAME]

int	fd
pointer	imdat
char	rootid[SZ_FNAME]
char	fname[SZ_FNAME]
pointer	d0h
char	d0h_ext[SZ_EXT]
char	linenum[SZ_LINENUM]
char	propid[SZ_LINE]
real	yoff
int	naxis1, naxis2
real	zmin, zmax
real	pscale, cd1_1, cd1_2
real	orientat
real	width, vscale
bool	mir_revr, neg_img

real	xcomp, ycomp, compsize
real	xaxes, yaxes

pointer	immap()
int	open()
real	imgetr()
bool	imgetb()
pointer	imgs2r()
bool	streq()

begin
	# read parameters
	call clgstr ("rootname", rootname, SZ_LINE)
	call clgstr ("output", output, SZ_LINE)
	call clgstr ("fits", ftype, SZ_FNAME)

	# construct necessary file name extensions
	if (streq(ftype,"fits")) {
	    call strcpy ("_d0f.fits[0]", d0h_ext, SZ_EXT) 
	} else {
	    call strcpy (".d0h", d0h_ext, SZ_EXT) 
	}

	# construct file names
	call strcpy (rootname, fname, SZ_FNAME)
	call strcat (d0h_ext, fname, SZ_FNAME)
	d0h = immap (fname, READ_ONLY, 0)

	# read keywords
        call imgstr (d0h, "LINENUM", linenum, SZ_LINENUM)
        call imgstr (d0h, "PROPOSID", propid, SZ_LINE)
        call imgstr (d0h, "ROOTNAME", rootid, SZ_LINE)
	naxis1 = IM_LEN(d0h, 1)
	naxis2 = IM_LEN(d0h, 2)
	orientat = imgetr (d0h, "ORIENTAT")
	mir_revr = imgetb (d0h, "MIR_REVR")
	cd1_1 = imgetr (d0h, "CD1_1")
	cd1_2 = imgetr (d0h, "CD1_2")

	# read the image to find the maximun and minimum
	imdat = imgs2r (d0h, 1, naxis1, 1, naxis2)
	call alimr (Memr[imdat], naxis1*naxis2, zmin, zmax)

	# close files
	call imunmap (d0h)

	# open the output file
	fd = open (output, NEW_FILE, TEXT_FILE)

	# start a new page
	call pp_erase (fd)

	# draw the banner
	call obs_banner (fd, linenum, rootid, propid, "FOS", yoff)
	call fprintf (fd, "reset\n")
	call fprintf (fd, "vpage 0.0 1.0 0.0 1.0\n")

	# plot the gray scale image
	if (naxis1 == 510) {
	    width = 0.9
	    vscale = 0.9/510.
	    call fprintf (fd, "zsection '%s'\n")
	        call pargstr (fname)
	    call fprintf (fd, "fitpix 0.05 %g 0.65 0.85 \n")
		call pargr (0.05+width)
	    call fprintf (fd, "limits; xflip \n")
	    call fprintf (fd, "zrange %g %g \n")
	        call pargr (zmax)
	        call pargr (zmin)
	    call fprintf (fd, "pixmap \n")
	    call fprintf (fd, "ticksize 10 100 5 20\n")
	    call fprintf (fd, "box \n")

	    #call fprintf (fd, "zsection '%s%s'\n")
	        #call pargstr (fname)
	        #call pargstr ("[201:300,*]")
	    #call fprintf (fd, "fitpix 0.05 0.75 0.25 0.60 \n")
	    #call fprintf (fd, "limits; xflip \n")
	    #call fprintf (fd, "zrange %g %g \n")
	        #call pargr (zmax)
	        #call pargr (zmin)
	    #call fprintf (fd, "pixmap \n")
	    #call fprintf (fd, "ticksize 5 20 1 5\n")
	    #call fprintf (fd, "box \n")
	} else if (naxis1 == 96) {
	    width = 0.6
	    vscale = 0.6/96.
	    call fprintf (fd, "zsection '%s'\n")
	        call pargstr (fname)
	    call fprintf (fd, "fitpix 0.15 %g 0.25 0.85 \n")
		call pargr (0.15+width)
	    call fprintf (fd, "limits; xflip \n")
	    call fprintf (fd, "zrange %g %g \n")
	        call pargr (zmax)
	        call pargr (zmin)
	    call fprintf (fd, "pixmap \n")
	    call fprintf (fd, "ticksize 5 20 5 20\n")
	    call fprintf (fd, "box \n")
	} else
	    call error (1, "Unknown FOS image dimension length")

	# add the grey scale bar here...
	neg_img = TRUE
	call pp_gsbar (fd, 0.15, 0.45, 0.87, 0.9, zmin, zmax, 0.7, neg_img)
    
	# draw the compass
	if (mir_revr ) call eprintf ("Mirror reverse is TRUE\n")
	mir_revr = TRUE
	xcomp = 0.87
	ycomp = 0.45
	compsize = 0.065
	xaxes = xcomp - compsize
	yaxes = ycomp + (compsize * 1.5)
	call pp_compass (fd, xcomp, ycomp, compsize, orientat, mir_revr)
	call fprintf (fd, "expand 0.7; move %.3g %.3g\n")
		call pargr(xcomp)
		call pargr(ycomp)
	call fprintf (fd, "draw %.3g %.3g\n")
		call pargr(xaxes)
		call pargr(ycomp)
	xaxes = xaxes - (compsize / 5.)
	call fprintf (fd, "move %.3g %.3g; label 'X'\n")
		call pargr (xaxes)
		call pargr (ycomp)
	
	call fprintf (fd, "expand 0.7; move %.3g %.3g\n")
		call pargr(xcomp)
		call pargr(ycomp)
	call fprintf (fd, "draw %.3g %.3g\n")
		call pargr(xcomp)
		call pargr(yaxes)
	xaxes = xcomp + (compsize / 5.)
	yaxes = yaxes + (compsize / 5.)
	call fprintf (fd, "move %.3g %.3g; label 'Y'\n")
		call pargr(xaxes)
		call pargr(yaxes)
	
	# calculate the plate scale in arc seconds
	pscale = sqrt(cd1_1**2 + cd1_2**2) * 3600.

	# draw the plate scale
	call pp_pscale (fd, 0.87, 0.25, 0.1, pscale, vscale)

	call close (fd)
end
