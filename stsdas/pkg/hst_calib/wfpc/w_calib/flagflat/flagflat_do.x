define	OKVAL		0
define	BADMASK 	2

#  flagflat_do -- Perform flagging of extremely high or low flat field pixels
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  29-Dec-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure flagflat_do (tpin, tpinmask, tpoutmask, boxsize, sigma)

pointer	tpin, tpinmask, tpoutmask
int	boxsize
real	sigma
 
char	gfoutmask[SZ_FNAME]
char	append[SZ_FNAME]
pointer	ipin, ipinmask
pointer	ipoutmask

pointer	pic, mask, numask
char	foutmask[SZ_FNAME]
char	dumch[1]
bool	samemask
int	ngrp
int	i, j, k, n, grp
int	ii, jj, ipx
int	dim_x, dim_y
int	left, right
real	xmin, xmax
char	text[SZ_LINE], tstring[SZ_LINE]
real	sum, ave, hi, lo
int	npix, nptot
	
pointer	immap()
pointer	imgs2r()
pointer	imgs2s(), imps2s()
int	itoc()
int	imtlen()
long	clktime()
#==============================================================================
begin
	# loop all input files
	do n = 1, imtlen(tpin) {

	    # open input files, check the parameters
	    call fflat_check (tpin, tpinmask, tpoutmask, ipin, ipinmask, 
				foutmask, ngrp, dim_x, dim_y, samemask)
	
	    # allocate tempory space for the new data quality pixels
	    npix = dim_x * dim_y
	    call malloc (numask, npix, TY_SHORT)

	    # loop all groups 
	    do grp = 1, ngrp {
		call gf_opengr (ipin, grp, xmin, xmax, 0)
		call gf_opengr (ipinmask, grp, xmin, xmax, 0)

	        # read input data
		pic = imgs2r (ipin, 1, dim_x, 1, dim_y)
		mask = imgs2s (ipinmask, 1, dim_x, 1, dim_y)
		call amovs (Mems[mask], Mems[numask], npix)

		# each successive calculation shift one row to the top
		do j = 1, dim_y-boxsize+1 {

		    # do the leftmost box first
	    	    sum = 0.
	    	    nptot = 0
		    do jj = j, j+boxsize-1 {
			do ii = 1, boxsize {
			    ipx = (jj-1)*dim_x+(ii-1)
			    if (Mems[mask+ipx] == OKVAL) {
			    	sum = sum + Memr[pic+ipx]
			    	nptot = nptot + 1
			    }
			}
		    }
		   
		    # each successive calculation shift one column to the right
		    do i = 1, dim_x-boxsize+1 {

		  	# skip the first time because the averaging is 
			# already done
			if (i != 1) {
			   
			    # add the (new) right column, subtract the (old) 
			    # left column to get the new box average
			    do jj = j, j+boxsize-1 {
				right = (jj-1)*dim_x+(i-1+boxsize)-1
				left = (jj-1)*dim_x+(i-1)-1
				if (Mems[mask+right] == OKVAL) {
			    	    sum = sum + Memr[pic+right]
				    nptot = nptot + 1
				}
				if (Mems[mask+left] == OKVAL) {
			    	    sum = sum - Memr[pic+left]
				    nptot = nptot - 1
				}
			    }
			}

			# exclude the extreme flatfield pixels
			if (nptot > 0) {
			    ave = sum / real(nptot)
			    lo = ave / sigma
			    hi = ave * sigma
			    do jj = j, j+boxsize-1 {
			        do ii = i, i+boxsize-1 {
				    ipx = (jj-1)*dim_x+ii-1
				    if (Mems[numask+ipx] == OKVAL) {
				        if (Memr[pic+ipx] > hi || 
					        Memr[pic+ipx] < lo)
					    Mems[numask+ipx] = BADMASK
				    }
				}
			    }
			}
		    }
		}

		# construct the group designation
	        if (grp == 1) {
		    call strcpy ("[1/", append, SZ_FNAME)
		    k = itoc (ngrp, dumch, 1)
	        } else {
		    call strcpy ("[", append, SZ_FNAME)
		    k = itoc (grp, dumch, 1)
		}
		call strcat (dumch, append, SZ_FNAME)
		call strcat ("]", append, SZ_FNAME)

	        # open output mask
		if (samemask)
		    ipoutmask = ipinmask
		else {
	            call strcpy (foutmask, gfoutmask, SZ_FNAME)
	            call strcat (append, gfoutmask, SZ_FNAME)
	            if (grp == 1)
		        ipoutmask = immap (gfoutmask, NEW_COPY, ipinmask)	
	            call gf_opengr (ipoutmask, grp, xmin, xmax, ipinmask)
		}

		# write to the output mask
	        call amovs (Mems[numask], 
			Mems[imps2s(ipoutmask, 1, dim_x, 1, dim_y)], npix)
	    }
	
	    # write history to output file
	    call cnvtime (clktime(0), tstring, SZ_LINE)
	    call sprintf (text, SZ_LINE, "FLAGFLAT completed at %s")
	        call pargstr (tstring)
	    call imputh (ipoutmask, "HISTORY", text)

	    # close input files
	    call imunmap (ipin)
	    call imunmap (ipinmask)

	    # close output file
	    if (!samemask)
	        call imunmap (ipoutmask)
	    call mfree (numask, TY_SHORT)

	    # print out message of which files been created
	    if (!samemask) {
	    	call printf (
			"A new flat field data quality file %s is generated\n")
	            call pargstr (foutmask)
	    } else {
	        call printf ("The flat field data quality file %s is updated\n")
	            call pargstr (foutmask)
	    }
	}
end
