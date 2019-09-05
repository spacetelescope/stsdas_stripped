include	<imhdr.h>
include	"ypp.h"

# Plot FOS IMAGE mode with a dispersion element (i.e. not mirror)

procedure ypp_imdsp ()

char	rootname[SZ_FNAME]
char	output[SZ_FNAME]
char	ftype[SZ_FNAME]

int	fd
#pointer	imdat
char	rootid[SZ_FNAME]
char	fname[SZ_FNAME]
pointer	c5h
char	c5h_ext[SZ_EXT]
char	linenum[SZ_LINENUM]
char	propid[SZ_LINE]
real	yoff
real	total_ht, ht, width
real	ymin, ymax
#real	gmin, gmax
real	exptm, ypos[MAX_YPOS]
int	naxis1
int	i, j, grp, gcount
int	label, nbox, npages
real	lf, rt, bt, tp

pointer	immap()
int	open()
int	imgeti()
real	imgetr()
#pointer	imgl1r()
bool	streq()

begin
	# maximum number of boxes in one page
	nbox = 5

	# view port
	lf = 0.1
	rt = 0.9
	bt = 0.05
	tp = 0.9

	width = rt - lf
	total_ht = tp - bt

	# read parameters
	call clgstr ("rootname", rootname, SZ_LINE)
	call clgstr ("output", output, SZ_LINE)
	call clgstr ("fits", ftype, SZ_FNAME)

	# construct necessary file name extensions
	if (streq(ftype,"fits") ) {
	    call strcpy ("_c5f.fits[0]", c5h_ext, SZ_EXT) 
	} else {
	    call strcpy (".c5h", c5h_ext, SZ_EXT) 
	}

	# construct file names
	call strcpy (rootname, fname, SZ_FNAME)
	call strcat (c5h_ext, fname, SZ_FNAME)
	c5h = immap (fname, READ_ONLY, 0)

	# read keywords
        call imgstr (c5h, "LINENUM", linenum, SZ_LINENUM)
        call imgstr (c5h, "PROPOSID", propid, SZ_LINE)
        call imgstr (c5h, "ROOTNAME", rootid, SZ_LINE)
	naxis1 = IM_LEN(c5h, 1)
	exptm = imgetr (c5h, "EXPOSURE")
	if (streq(ftype,"geis") ){
		gcount = imgeti (c5h, "GCOUNT")
	} else {
		gcount = IM_LEN(c5h,2)
	}

	# open the output file
	fd = open (output, NEW_FILE, TEXT_FILE)

	# Determine the overall MAX and MIN from all the groups, and
	# 	YPOS of each group
	call ypp_minmax (c5h, fname, ftype, gcount, naxis1, ymin, ymax, ypos)

	# determine how many pages
	npages = (gcount-1)/nbox + 1
	
	# determine the height and width of each box
	if (gcount < nbox) ht = total_ht / real(gcount)
	else ht = total_ht / real(nbox)

	grp = 1

	# plot each page
	do j = 1, npages {

	    # start a new page
	    call pp_erase (fd)

	    # draw the banner
	    call obs_banner (fd, linenum, rootid, propid, "FOS", yoff)
	    call fprintf (fd, "reset; fontset hard\n")
	    call fprintf (fd, "vpage 0.0 1.0 0.0 1.0\n")

	    # draw the top x-axis
	    call fprintf (fd, "angle 0\n")
	    call fprintf (fd, "axis 1 %d 0 0 %g %g %g 0 1\n")
		call pargi (naxis1)
		call pargr (lf)
		call pargr (tp)
		call pargr (width)

	    # draw the labels
             call fprintf (fd, "angle 90; vmove %g .5; putlabel 5 'Corrected Counts'\n")
		call pargr (lf-0.09)
            call fprintf (fd, "angle 0; vmove 0.5 %g; putlabel 5 'Pixel'\n")
		call pargr (bt-0.04)
	    call fprintf (fd, "expand 0.7\n")

	    # plot each box
	    do i = 1, nbox {
		if (i == nbox || grp == gcount) label = 1
		else label = 0	

		# draw the x-axis
	        call fprintf (fd, "angle 0\n")
	    	call fprintf (fd, "axis 1 %d 0 0 %g %g %g %d 0\n")
		    call pargi (naxis1)
		    call pargr (lf)
		    call pargr (tp-ht*i)
		    call pargr (width)
		    call pargi (label)

		# draw the y-axes
	        call fprintf (fd, "angle 90\n")
	    	call fprintf (fd, "axis %g %g 0 0 %g %g %g 2 1\n")
		    call pargr (ymin*exptm)
		    call pargr (ymax*exptm)
		    call pargr (lf)
		    call pargr (tp-ht*i)
		    call pargr (ht)
	    	call fprintf (fd, "axis %g %g 0 0 %g %g %g 0 0\n")
		    call pargr (ymin*exptm)
		    call pargr (ymax*exptm)
		    call pargr (rt)
		    call pargr (tp-ht*i)
		    call pargr (ht)

                call fprintf (fd, "angle 0; vmove %g %g; putlabel 6 'Group %d'\n")
		    call pargr (rt+0.01)
		    call pargr (tp-(i-0.85)*ht)
		    call pargi (grp)
                call fprintf (fd, "angle 0; vmove %g %g; putlabel 6 'YPOS = %g'\n")
		    call pargr (rt+0.01)
		    call pargr (tp-(i-0.85)*ht-0.02)
		    call pargr (ypos[grp])

	   	# draw the curve
		if (streq(ftype,"geis")) {
			call fprintf (fd, "ysection %s[%d]\n")
			    call pargstr (fname)
			    call pargi (grp)
		} else {
			call fprintf (fd, "ysection %s[*,%d]\n")
			    call pargstr (fname)
			    call pargi (grp)
		}		
		call fprintf (fd, "location %g %g %g %g\n")
		    call pargr (lf)
		    call pargr (rt)
		    call pargr (tp-ht*i)
		    call pargr (tp-ht*(i-1))
		call fprintf (fd, "limits 1 %d %g %g\n")
		    call pargi (naxis1)
		    call pargr (ymin)
		    call pargr (ymax)
		call fprintf (fd, "connect\n")

		grp = grp + 1
		if (grp > gcount) break
	    }
	}
	
	# close files
	call imunmap (c5h)
	call close (fd)
end


procedure ypp_minmax ( img, fname, ftype, gcount, naxis1, ymin, ymax, ypos)

pointer	img
char	fname[ARB]
char	ftype[ARB]
real	ymin, ymax
real	ypos[ARB]
int	gcount 
int	naxis1

int	grp
pointer	imdat
real	gmin, gmax
pointer	gtab, cptr
int	extnum
char	tabname[SZ_FNAME]

pointer	imgl1r()
pointer	imgl2r()
bool	streq()
real	imgetr()
pointer	tbtopn()

begin



	    # Read in the image line and YPOS value ...
	if(streq(ftype,"geis")) {
	    do grp = 1, gcount {
		#if (grp != 1)
			call gf_opengr (img, grp, gmin, gmax, 0)

		# read the image to find the maximum and minimum
		imdat = imgl1r(img)

		ypos[grp] = imgetr (img, "YPOS")
	   	# Perform statistics on image line...
	    	call alimr (Memr[imdat], naxis1, gmin, gmax)

	    	if (grp == 1) {
			ymin = gmin
			ymax = gmax
	    	} else {
			if (gmin < ymin) ymin = gmin
			if (gmax > ymax) ymax = gmax
	    	}
	    }

	} else {
	    
	    extnum = 1
	    # We only need to open the table once for a FITS file...
	    call get_fitextn(fname, extnum, tabname)
	    gtab = tbtopn(tabname, READ_ONLY, NULL)
	    call tbcfnd (gtab, "YPOS", cptr, 1)

	    do grp = 1, gcount {
	    imdat = imgl2r (img, grp)		
	    # Determine YPOS for each group here...
	    	if (cptr != NULL) {
		    call tbegtr (gtab, cptr, grp, ypos[grp])
	    	} else {
		    ypos[grp] = 0.
	    	}
	    # Perform statistics on image line...
	    call alimr (Memr[imdat], naxis1, gmin, gmax)
	    	if (grp == 1) {
			ymin = gmin
			ymax = gmax
	    	} else {
			if (gmin < ymin) ymin = gmin
			if (gmax > ymax) ymax = gmax
	    	}


	    }
	    # Now close the FITS table...
	    call tbtclo(gtab)

	}

end
