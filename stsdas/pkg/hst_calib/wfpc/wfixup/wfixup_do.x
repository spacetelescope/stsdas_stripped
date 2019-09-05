#  wfixup_do -- Perform flagging of extremely high or low flat field pixels
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  18-Jan-1993  J.-C. Hsu		design and coding
#  05-Jul-1995  J.-C. Hsu		use selective DQF value
#------------------------------------------------------------------------------
procedure wfixup_do(tpin, tpinmask, tpout, maxgap, fillval, dqval)

pointer	tpin, tpinmask, tpout
int	maxgap
real	fillval
short	dqval
 
char	gfout[SZ_FNAME]
char	append[SZ_FNAME]
pointer	ipin, ipinmask
pointer	ipout

pointer	pic, mask
char	fout[SZ_FNAME]
char	dumch[1]
bool	same
int	ngrp
int	i, j, k, n, grp
int	ngap, gap1, gap2, ipx
int	dim_x, dim_y
real	xmin, xmax
char	text[SZ_LINE], tstring[SZ_LINE]
int	npix
short	szero
	
pointer	gf_map()
pointer	imgs2r()
pointer	imgs2s(), imps2r()
short	ands()
int	itoc()
int	imtlen()
long	clktime()
#==============================================================================
begin
	szero = short(0)

	# loop all input files
	do n = 1, imtlen(tpin) {

	    # open input files, check the parameters
	    call wfixup_check(tpin, tpinmask, tpout, ipin, ipinmask, 
				fout, ngrp, dim_x, dim_y, same)
	
	    # allocate tempory space for the new data pixels
	    npix = dim_x * dim_y

	    # loop all groups 
	    do grp = 1, ngrp {

		# if only one group, no need to open the input again
		if (ngrp > 1) {
		    call gf_opengr(ipin, grp, xmin, xmax, 0)
		    call gf_opengr(ipinmask, grp, xmin, xmax, 0)
		}

	        # read input data
		pic = imgs2r(ipin, 1, dim_x, 1, dim_y)
		mask = imgs2s(ipinmask, 1, dim_x, 1, dim_y)

		# do the interpolation row by row
		do j = 1, dim_y {

		    # reset the gap width counter
	    	    ngap = 0		
		    
		    # go through each pixel in one row
		    do i = 1, dim_x {	
			ipx = mask + (j-1) * dim_x + (i-1)

			# use selective DQF value, 7/5/95 JC Hsu.
			if (ands(Mems[ipx], dqval) != szero) {

			    # increment the gap width counter 
			    ngap = ngap + 1 		

			    # if the row begins with a gap
			    if (ngap == 1) gap1 = i

			    # if the row ends with a gap
			    if (i == dim_x) {
				gap2 = i
				call fix_intrp(gap1, gap2, maxgap, fillval, 
						pic, j, dim_x)
			    }
		  	} else {

			    # do the interpolation for every gap in one row
			    if (ngap != 0) {
				gap2 = i-1
				call fix_intrp(gap1, gap2, maxgap, fillval, 
						pic, j, dim_x)

				# reset the gap width counter
				ngap = 0
			    }
			}
		    }
		}

		# construct the group designation for file name
	        if (grp == 1) {
		    call strcpy("[1/", append, SZ_FNAME)
		    k = itoc(ngrp, dumch, 1)
	        } else {
		    call strcpy("[", append, SZ_FNAME)
		    k = itoc(grp, dumch, 1)
		}
		call strcat(dumch, append, SZ_FNAME)
		call strcat("]", append, SZ_FNAME)

	        # open output file
		if (same)
		    ipout = ipin
		else {
	            call strcpy(fout, gfout, SZ_FNAME)
	            call strcat(append, gfout, SZ_FNAME)
	            if (grp == 1)
		        ipout = gf_map(gfout, NEW_COPY, ipin)	
	            call gf_opengr(ipout, grp, xmin, xmax, ipin)
		}

		# write the result to the output file
	        call amovr(Memr[pic], 
			Memr[imps2r(ipout, 1, dim_x, 1, dim_y)], npix)
	    }
	
	    # write history to the output file
	    call cnvtime(clktime(0), tstring, SZ_LINE)
	    call sprintf(text, SZ_LINE, "WFIXUP completed at %s")
	        call pargstr(tstring)
	    call gf_iputh(ipout, "HISTORY", text)

	    # close input files
	    call gf_unmap(ipin)
	    call gf_unmap(ipinmask)

	    # close output file
	    if (!same)
	        call gf_unmap(ipout)

	    # print out message of which files been created
	    if (!same) {
	    	call printf( "A new output data file %s is generated\n")
	            call pargstr(fout)
	    } else {
	        call printf("The input data file %s is updated\n")
	            call pargstr(fout)
	    }
	}
end

procedure fix_intrp(gap1, gap2, maxgap, fillval, pic, row, dim_x)

int	gap1			# index of the first gap pixel
int	gap2			# index of the last gap pixel
int	maxgap			# maximum gap width allowed
real	fillval			# fill value for gap wider than maxgap
				# if = INDEF, do not interpolate or fill 
pointer	pic
int	row			# which row in the image is processed
int	dim_x			# x dimension of the image
 
real	p0, dp
int	i
int	ngap, ipx
#==============================================================================
begin
	ngap = gap2 - gap1 + 1

	# if no left or right end point (i.e. gap goes to the boundary)
	if (gap1 == 1 || gap2 == dim_x) {
	    do i = gap1, gap2 {
		if (!IS_INDEFR(fillval)) {
		    ipx = pic + (row-1) * dim_x + (i-1)
		    Memr[ipx] = fillval
		}
	    }
	} else {
	
	    # for wide gap, fill in the fill value unless fillval=INDEF
	    if (ngap > maxgap) {
	    	do i = gap1, gap2 {
		    if (!IS_INDEFR(fillval)) {
		        ipx = pic + (row-1) * dim_x + (i-1)
		        Memr[ipx] = fillval
		    }
	        }

	    # for gaps within maxgap, do a simple linear interpolation using
	    # pixels values of the two end points just outside the gap
	    } else {
		p0 = Memr[pic + (row-1)*dim_x + gap1-2]
		dp = (Memr[pic + (row-1)*dim_x + gap2] - p0) / real(ngap+1)
		do i = gap1, gap2 {
		    ipx = pic + (row-1) * dim_x + (i-1)
		    Memr[ipx] = p0 + dp * real(i-gap1+1)
		}
	    }
	}
end
