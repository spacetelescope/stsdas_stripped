include	"normclip.h"

#  norm_do -- Perform flat field clipping and normalization between the 4 chips.
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  24-Aug-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure norm_do (tpin, tpinmask, tpout, tpoutmask, flatmin, flatmax, verbose)

pointer	tpin, tpinmask, tpout, tpoutmask
real	flatmin, flatmax
bool	verbose
 
char	gfout[SZ_FNAME], gfoutmask[SZ_FNAME]
char	append[SZ_FNAME]
pointer	ipin, ipinmask
pointer	ipout, ipoutmask

real	pic[DIM_X]
short	mask[DIM_X]
char	fout[SZ_FNAME], foutmask[SZ_FNAME]
char	dumch[1]
bool	samefile, samemask
int	ngrp
int	i, j, k, n, grp
real	xmin, xmax
char	text[SZ_LINE], tstring[SZ_LINE]
double	sum, totavg
int	nptot
	
pointer	immap()
pointer	imgl2r(), impl2r()
pointer	imgl2s(), impl2s()
int	itoc()
int	imtlen()
long	clktime()
#==============================================================================
begin
	# loop all input files
	do n = 1, imtlen(tpin) {

	    # open input files, check the parameters
	    call norm_check (tpin, tpinmask, tpout, tpoutmask, ipin, ipinmask, 
				fout, foutmask, ngrp, samefile, samemask)
	
	    # loop all groups to get the overall average count
	    sum = 0.d0
	    nptot = 0
	    do grp = 1, ngrp {
		call gf_opengr (ipin, grp, xmin, xmax, 0)
		call gf_opengr (ipinmask, grp, xmin, xmax, 0)

	        # read data line by line
		do j = 1, DIM_Y {
		    call amovr (Memr[imgl2r (ipin, j)], pic, DIM_X)
		    call amovs (Mems[imgl2s (ipinmask, j)], mask, DIM_X)

		    do i = 1, DIM_X {
			if (mask[i] == OKVAL) {
			    sum = sum + pic[i]
			    nptot = nptot + 1
			}
		    }
		}
	    }
	    totavg = sum / double(nptot)
	    call printf ("average value = %0.5g, total good points = %d\n")
		call pargd (totavg)
		call pargi (nptot)
	    call flush(STDOUT)

	    # Now normalize each chip to the average value and do the inversion
	    do grp = 1, ngrp {
		call gf_opengr (ipin, grp, xmin, xmax, 0)
		call gf_opengr (ipinmask, grp, xmin, xmax, 0)

	        if (grp == 1) {
		    call strcpy ("[1/", append, SZ_FNAME)
		    k = itoc (ngrp, dumch, 1)
	        } else {
		    call strcpy ("[", append, SZ_FNAME)
		    k = itoc (grp, dumch, 1)
		}
		call strcat (dumch, append, SZ_FNAME)
		call strcat ("]", append, SZ_FNAME)

	        # open output file/mask
		if (samefile)
		    ipout = ipin
		else {
	            call strcpy (fout, gfout, SZ_FNAME)
	            call strcat (append, gfout, SZ_FNAME)
	            if (grp == 1)
		        ipout = immap (gfout, NEW_COPY, ipin)	
	            call gf_opengr (ipout, grp, xmin, xmax, ipin)
		}
		if (samemask)
		    ipoutmask = ipinmask
		else {
	            call strcpy (foutmask, gfoutmask, SZ_FNAME)
	            call strcat (append, gfoutmask, SZ_FNAME)
	            if (grp == 1)
		        ipoutmask = immap (gfoutmask, NEW_COPY, ipinmask)	
	            call gf_opengr (ipoutmask, grp, xmin, xmax, ipinmask)
		}

		do j = 1, DIM_Y {
		    call amovr (Memr[imgl2r (ipin, j)], pic, DIM_X)
		    call amovs (Mems[imgl2s (ipinmask, j)], mask, DIM_X)

		    do i = 1, DIM_X {
			if (mask[i] == OKVAL && pic[i] != 0.) {
			    pic[i] = totavg / pic[i]
			    if (pic[i] > flatmax || pic[i] < flatmin) {
				pic[i] = FILLVAL
				mask[i] = BADMASK
			    }
			} else {
			    pic[i] = FILLVAL
			    mask[i] = BADMASK
			}
		    }

		    # write to the output
	            call amovr (pic, Memr[impl2r(ipout, j)], DIM_X)
	            call amovs (mask, Mems[impl2s(ipoutmask, j)], DIM_X)
		}
	    }
	
	    # write history to output file
	    call cnvtime (clktime(0), tstring, SZ_LINE)
	    call sprintf (text, SZ_LINE, "Normalization completed at %s")
	        call pargstr (tstring)
	    call imputh (ipout, "HISTORY", text)
	    call imputh (ipoutmask, "HISTORY", text)

	    # close input files
	    call imunmap (ipin)
	    call imunmap (ipinmask)

	    # close output files
	    if (!samefile)
	        call imunmap (ipout)
	    if (!samemask)
	        call imunmap (ipoutmask)
	}

	# print out message of which files been created
	call printf ("Generated normalized flat field %s and its mask %s\n")
	    call pargstr (fout)
	    call pargstr (foutmask)
end
