include <mach.h>
include <imhdr.h>
include	"streakflat.h"

#  flat_do -- Perform streak flat field calculations
#
#  Description:
#  ------------
#  This is mostly a file housekeeping routine for the streak flat task.
#  It takes care of input/output files open/close, check for dimensions, read/
#  write data from/to files, allocate memory spaces etc.
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Mar-1992  J.-C. Hsu		design and coding
#  16-Jan-1994  J.-C. Hsu		modify to run WFPC2 data
#------------------------------------------------------------------------------

procedure flat_do (fin, fout, iter, hwidth, ngood, verbose)

pointer	fin 			# input: file template pointer
char	fout[SZ_FNAME]		# input: output file name
int	iter			# input: number of iterations
int	hwidth[MAX_ITER]	# input: half widths of 1-D smoothing
int	ngood			# input: min good points needed
bool	verbose
 
char	foutmask[SZ_FNAME]	# output mask file name
char	gfout[SZ_FNAME], gfoutmask[SZ_FNAME]
pointer	ipin[MAX_FILES], ipmask[MAX_FILES]
pointer	iptmp[MAX_FILES]
pointer	ipout, ipoutmask
real	strpa[MAX_FILES], chippa[MAX_FILES], pa_wf[NGROUPS], pa_pc[NGROUPS]
real	pa_wfpc2[NGROUPS]
real	dpa
real	minval, maxval

char	fdata[SZ_FNAME, MAX_FILES], fmask[SZ_FNAME, MAX_FILES]
char	ftmp[SZ_FNAME, MAX_FILES]
char	extname[SZ_FNAME]
char	camera[SZ_LINE]
char	dumch[1], dumch4[4]
char	append[SZ_FNAME]
int	nf			# number of existing files
int	ngrp
int	i, j, k, n, grp, indx, ipx
int	npix
int	extlen, rootlen
real	xmin, xmax
real	medavg, picavg
pointer sp, median, pic, mask, pic2, mask2
char	text[SZ_LINE], tstring[SZ_LINE]
	
pointer	immap()
pointer	imgs2r(), imps2r()
pointer	imgs2s(), imps2s()
int	fnextn()
int	strlen()
int	itoc()
long	clktime()
bool	streq()
#int	imgeti()

data	pa_wf/224.09, 314.68, 45.00, 134.89/
data	pa_pc/270., 0., 90., 180./
# from Colin Cox SOB Reprot 94-10-21 for WFPC2
data	pa_wfpc2/224.70, 314.22, 44.52, 135.09/
#==============================================================================
begin

	npix = DIM_X * DIM_Y

	# allocate array space
	call malloc (pic, npix, TY_REAL)
	call malloc (mask, npix, TY_SHORT)
	call malloc (median, npix, TY_REAL)

	call cnvtime (clktime(0), tstring, SZ_LINE)
	call sprintf (text, SZ_LINE, 
		"Task STREAKFLAT, version %s, starts at %s")
	    call pargstr (VERSION)
	    call pargstr (tstring)

	# open input files and temporary files, check the parameters
	call flat_check (fin, fdata, fmask, ftmp, ipin, ipmask, 
				nf, camera, ngrp, strpa)
	
	# get the root name and extension name of the output file
	extlen = fnextn (fout, extname, SZ_FNAME)
	if (extlen == 0) 
	    call strcat (".", fout, SZ_FNAME)
	rootlen = strlen(fout)-extlen
	call strcpy (fout, foutmask, rootlen)

	# if there is no extension, use the default (.r6h) extension
	if (extlen == 0)
	    call strcat ("r6h", fout, SZ_FNAME)
	call strcat ("b6h", foutmask, SZ_FNAME)

	# loop all groups
	do grp = 1, ngrp {

	    # determine the position angle offset
	    if (streq(camera, "WF")) 
		#dpa = pa_wf[mod(imgeti(ipin[1],"DETECTOR"), 4)]
		dpa = pa_wf[grp]
	    else if (streq(camera, "PC")) 
		#dpa = pa_pc[mod(imgeti(ipin[1],"DETECTOR"), 4)]
		dpa = pa_pc[grp]
	    else if (streq(camera, "WFPC2")) 
		dpa = pa_wfpc2[grp]
	    else
		call error (1, "illegal instrument/camera")

	    if (verbose) {
		call printf ("Begin processing group %d\n")
		call pargi (grp)
		call flush (STDOUT)
	    }

	    # loop all (existing) files
	    do n = 1, nf {

	        # read data from the input data and data quality files
		call gf_opengr (ipin[n], grp, xmin, xmax, 0)
		call gf_opengr (ipmask[n], grp, xmin, xmax, 0)

		chippa[n] = strpa[n] - dpa
		while (chippa[n] > 90.) chippa[n] = chippa[n] - 180.
		while (chippa[n] <= -90.) chippa[n] = chippa[n] + 180.
	
	    	if (verbose) {
		    call printf ("  streak angle for file %s is %7.3f\n")
		    	call pargstr (fdata[1,n])
		    	call pargr (chippa[n])
		    call flush (STDOUT)
	    	}
	    }

	    # open output file/mask
	    call strcpy (fout, gfout, SZ_FNAME)
	    call strcpy (foutmask, gfoutmask, SZ_FNAME)

	    if (grp == 1) {
		call strcpy ("[1/", append, SZ_FNAME)
		k = itoc (ngrp, dumch, 1)
		call strcat (dumch, append, SZ_FNAME)
		call strcat ("]", append, SZ_FNAME)
	        call strcat (append, gfout, SZ_FNAME)
	        call strcat (append, gfoutmask, SZ_FNAME)
		ipout = immap (gfout, NEW_COPY, ipin[1])	
		ipoutmask = immap (gfoutmask, NEW_COPY, ipmask[1])	

		call imputh (ipout, "HISTORY", text)
		call imputh (ipoutmask, "HISTORY", text)

		# record the boxcar half widths in the header
		call sprintf (text, SZ_LINE, 
			"Boxcar filter half widths (in pixels) are:")
		do i = 1, iter {
		    k = itoc (hwidth[i], dumch4, SZ_LINE)
		    call strcat (" ", text, SZ_LINE)
		    call strcat (dumch4, text, SZ_LINE)
		}
		call imputh (ipout, "HISTORY", text)
		call imputh (ipoutmask, "HISTORY", text)

	    } else {
		k = itoc (grp, dumch, 1)
		call strcat ("[", gfout, SZ_FNAME)
		call strcat (dumch, gfout, SZ_FNAME)
		call strcat ("]", gfout, SZ_FNAME)
		call strcat ("[", gfoutmask, SZ_FNAME)
		call strcat (dumch, gfoutmask, SZ_FNAME)
		call strcat ("]", gfoutmask, SZ_FNAME)
	    	call gf_opengr (ipout, grp, minval, maxval, ipin[1])
	    	call gf_opengr (ipoutmask, grp, real(OKVAL), real(MASKVAL), 
					ipmask[1])
	    }

	    # write history of input file names and streak angles to output file
	    call sprintf (text, SZ_LINE, "group %d streak angles:")
		call pargi (grp)
	    call imputh (ipout, "HISTORY", text)
	    call imputh (ipoutmask, "HISTORY", text)
	    do n = 1, nf {
		call sprintf (text, SZ_LINE, "  %s  %8.3f")
		    call pargstr (fdata[1,n])
		    call pargr (chippa[n])
		call imputh (ipout, "HISTORY", text)
		call imputh (ipoutmask, "HISTORY", text)
	    }

	    # calculate the median flat to be used as first estimate of the 
	    # flat field
	    call flat_median (ipin, ipmask, nf, ngood, Memr[median])
	    if (verbose)
		call printf ("  median statistics:")
	    call getavg (Memr[median], CENT_X1, CENT_X2, CENT_Y1, CENT_Y2, 
				medavg, verbose)

	    # loop over different half widths of boxcar filtering
	    do k = 1, iter {
	        if (verbose) {
		    call printf ("  Begin 1-D smoothing, iteration %d\n")
		    call pargi (k)
		    call flush (STDOUT)
	        }
		do n = 1, nf {
		    pic2 = imgs2r (ipin[n], 1, DIM_X, 1, DIM_Y)
		    mask2 = imgs2s (ipmask[n], 1, DIM_X, 1, DIM_Y)

	    	    # open temporary files to store chip streak images
		    # (12/29/92 JCH, transplanted from flat_check.x)
		    if (grp == 1 && k == 1) {
	    	    	iferr (iptmp[n] = immap (ftmp[1,n], NEW_COPY, ipin[1])) {
			    call printf ("can not create temporary file %s\n")
		    	    	call pargstr (ftmp[1,n])
			    call flush (STDOUT)
			    call error (1, "")
			}
		    } else
	    	    	iptmp[n] = immap (ftmp[1,n], READ_WRITE, 0)

		    do ipx = 0, npix-1 {
			Mems[mask+ipx] = Mems[mask2+ipx]
			if (Mems[mask2+ipx] != OKVAL || 
			    	Memr[pic2+ipx] == BADVAL ||
			    	Memr[median+ipx] == BADVAL)
			    Memr[pic+ipx] = BADVAL
			else {
			    Memr[pic+ipx] = Memr[pic2+ipx] * medavg /
						 Memr[median+ipx]
			}
		    }
		    call flat_smooth (Memr[pic], Mems[mask], chippa[n], 
					hwidth[k])
		    if (verbose)
			call printf ("    smoothed streak statistics: ")

	    	    call getavg (Memr[pic], CENT_X1, CENT_X2, CENT_Y1, CENT_Y2, 
					picavg, verbose)

		    # divide raw data by the smoothed streak
		    do ipx = 0, npix-1 {
			if (Memr[pic+ipx] == 0.) {
				call printf ("pic at %d, %d is zero\n")
				call pargi(i)
				call pargi(j)
				call flush(STDOUT)
			}
			if (Memr[pic+ipx] != BADVAL)
			    Memr[pic+ipx] = Memr[pic2+ipx]*picavg/Memr[pic+ipx]
		    }

		    # put this to a temporary file
	    	    sp = imps2r(iptmp[n], 1, DIM_X, 1, DIM_Y)
	    	    call amovr (Memr[pic], Memr[sp], npix)
		    call imunmap (iptmp[n])
		}

		# reopen tmp files to save memory space (12/29/92 JCH)
		do n = 1, nf
	    	    iptmp[n] = immap (ftmp[1,n], READ_ONLY, 0)

	        call flat_median (iptmp, ipmask, nf, ngood, Memr[median])

		do n = 1, nf
	    	    call imunmap (iptmp[n])
	    }
	
	    # put the result to the output file/mask
	    #call getavg (Memr[median], CENT_X1, CENT_X2, CENT_Y1, CENT_Y2, 
					#medavg, verbose)
	    do i = 1, npix {
		indx = median+i-1
		if (Memr[indx] != BADVAL && Memr[indx] != 0.) {

		    # do NOT normalize nor invert
		    #Memr[indx] = medavg / Memr[indx]
		    Mems[mask+i-1] = OKVAL
		} else {
		    Memr[indx] = 1.
		    Mems[mask+i-1] = MASKVAL
		}
	    }

	    # update minmax for the output files
	    call alimr (Memr[median], npix, minval, maxval)
	    if (grp == ngrp) {
	    	IM_MIN(ipout) = minval
	    	IM_MAX(ipout) = maxval
	    	IM_MIN(ipoutmask) = OKVAL
	    	IM_MAX(ipoutmask) = MASKVAL
	    	IM_LIMTIME(ipout) = clktime(long(0))
	    	IM_LIMTIME(ipoutmask) = clktime(long(0))
	    }

	    sp = imps2r(ipout, 1, DIM_X, 1, DIM_Y) 
	    call amovr (Memr[median], Memr[sp], npix)
	    call amovs (Mems[mask], 
			Mems[imps2s(ipoutmask, 1, DIM_X, 1, DIM_Y)], npix)
	}

	# write history of ending time to output file
	call cnvtime (clktime(0), tstring, SZ_LINE)
	call sprintf (text, SZ_LINE, "Task STREAKFLAT stops at %s")
	    call pargstr (tstring)
	call imputh (ipout, "HISTORY", text)
	call imputh (ipoutmask, "HISTORY", text)

	# blank(zero) out certain header parameters in the output files
	call impstr (ipout, "ROOTNAME", "")
	call impstr (ipoutmask, "ROOTNAME", "")

	# close input and temporary files
	do n = 1, nf {
	    call imunmap (ipin[n])
	    #call imunmap (iptmp[n])		# not needed (12/29/92 JCH)
	    call imunmap (ipmask[n])
	    call imdelete (ftmp[1,n])
	}

	# close output files
	call imunmap (ipout)
	call imunmap (ipoutmask)

	# print out message of which files been created
	call printf ("STREAKFLAT: output flat field file %s is created\n")
	    call pargstr (fout)
	call printf ("STREAKFLAT: output flat field mask %s is created\n")
	    call pargstr (foutmask)

	call mfree (pic, TY_REAL)
	call mfree (mask, TY_SHORT)
	call mfree (median, TY_REAL)
end


#  getavg -- calculate the average of a specified region of an image
#------------------------------------------------------------------------------

procedure getavg (pic, x1, x2, y1, y2, avg, verbose)

real	pic[DIM_X, DIM_Y]
int	x1, x2, y1, y2
real	avg	
bool	verbose

real	stddev, x
double	sum
double	sumsq
int	npts
int	i, j
#==============================================================================
begin

	if (x1 < 1 || y1 < 1 || x2 > DIM_X || y2 > DIM_Y)
	    call error (1, "getavg out of boundary")

	sum = 0.d0
	sumsq = 0.d0
	npts = 0

	do j = y1, y2 {
	    do i = x1, x2 {
		x = pic[i,j]
		if (x != BADVAL) {
		    sum = sum + x
		    sumsq = sumsq + x**2
		    npts = npts + 1
		}
	    }
	}

	if (npts == 0)
	    call error (1, "no valid points for averaging")
	avg = sum / double(npts)
	stddev = sumsq/double(npts) - avg**2
	if (stddev <= EPSILON)
	    stddev = 0.
	else
	    stddev = sqrt(stddev)

	if (verbose) {
	    call printf ("avg= %0.1f +/- %0.1f, npts= %d\n")
	    call pargr (avg)
	    call pargr (stddev)
	    call pargi (npts)
	    call flush (STDOUT)
	}
end
