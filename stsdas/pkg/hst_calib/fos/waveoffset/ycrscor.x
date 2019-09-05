procedure ycrscor (pix1, pix2, npts, nbin, maxd, offset, counts, cc)

# YCRSCOR -- cross-correlation for FOS wave offset task

real	pix1[ARB]
real	pix2[ARB]
int	npts
int	nbin
int	maxd
real	offset[nbin]			#OUTPUT
real	counts[nbin]			#OUTPUT
real	cc[maxd * 2 + 1, nbin]		#OUTPUT

int	i, j, k, n, off1, off2, width
real	mean1, mean2, sigma1, sigma2, sumsq1, sumsq2, sum12, denom, rk, maxc
pointer	sp, spix1, spix2, spix12, c

real	assqr(), asumr()

begin
 	call smark (sp)

	width = maxd * 2 + 1
	n = (npts - width) / nbin

	call salloc (spix1, n, TY_REAL)
	call salloc (spix2, n, TY_REAL)
	call salloc (spix12, n, TY_REAL)
	call salloc (c, width, TY_REAL)
	
	# for each bin
	do i = 1, nbin {

	    # offset for bin (points to first good pixel)
	    off2 = (i - 1) * n + (width / 2)

	    # average of second spectrum for this bin
	    call aavgr (pix2[off2 + 1], n, mean2, sigma2)
	    counts[i] =  asumr (pix2[off2 + 1], n) 

	    # within the search width
	    do j = 1, width {

		# offset for first spectrum (points to first good pixel)
	        off1 = off2 - maxd + j - 1

	        # average of first spectrum for this bin
	        call aavgr (pix1[off1 + 1], n, mean1, sigma1)

		# subract average value from each bin
		call asubkr (pix1[off1 + 1], mean1, Memr[spix1], n)
		call asubkr (pix2[off2 + 1], mean2, Memr[spix2], n)

		# collect sum of squares
		sumsq1 = assqr (Memr[spix1], n)
		sumsq2 = assqr (Memr[spix2], n)

		# get sum of product
		call amulr (Memr[spix1], Memr[spix2], Memr[spix12], n)
		sum12 = asumr (Memr[spix12], n)

		# build correlation matrix
		denom = sqrt(sumsq1) * sqrt(sumsq2)

		# look for trouble
		if (denom == 0.0) {
	            Memr[c + j - 1] = 0.
		} else {
	            Memr[c + j - 1] = sum12 / denom
		}

	    }

	    # move to output images
	    call amovr (Memr[c], cc[1,i], width)

	    # find the max of the correlation matrix and position of max
	    maxc = 0.0
	    k = 0
	    do j = 1, width {
		if (Memr[c + j - 1] > maxc) {
		    maxc = Memr[c + j - 1]
		    k = j
		}
	    }
	    
	    # use if max is not on edge
	    if (k > 1 && k < width) {

		# use quadratic to compute offset
		rk = (Memr[c + k - 2] - Memr[c + k - 1]) / 
    		     (Memr[c + k - 2] + Memr[c + k] - 2.0 * 
		      Memr[c + k - 1]) - 0.5 

		# update
		offset[i] = maxd - k - rk + 1.0

	    } else {
		
		offset[i] = INDEF 

	    }

	}
		
	call sfree (sp)

end
