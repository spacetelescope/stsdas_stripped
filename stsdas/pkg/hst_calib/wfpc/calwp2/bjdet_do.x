define	MAX_BINS	500

procedure bjdet_do (imdes, imout, thresh, nbins, ncomp, x1, x2, dim_y, 
			fname, grp, ngrp, outfmt)

# inputs:
pointer	imdes		# input image descripter
pointer	imout		
real	thresh		# criterion of bias jump detection
int	nbins		# Number of bins
int	ncomp		# number of neighbors to be compared
int	x1, x2		# X range of usable data
int	dim_y		
char	fname[SZ_FNAME]
int	grp, ngrp	# which group 
char	outfmt[SZ_LINE]

# local:
real	ave[MAX_BINS]
real	aveerr[MAX_BINS]
int	y1[MAX_BINS]
int	y2[MAX_BINS]
pointer	imdat
int	i, k, npts
int	indx1, indx2, a, b
real	bjlevel
real	sum
double	sumsq

pointer	imgs2r()
int	imgeti()
bool	streq()
#==============================================================================
begin
	if (nbins < 2) 
	    call error (1, "must have at least two bins.")
	if (nbins > MAX_BINS) 
	    call error (1, "Too many bins.")

	do k = 1, nbins {

	    # decide the beginning and ending points of each bin
	    if (k == 1) y1[k] = 1 
	    else y1[k] = y2[k-1] + 1
	    y2[k] = nint (real(dim_y*k)/real(nbins))
	
	    imdat = imgs2r (imdes, x1, x2, y1[k], y2[k])
	
	    sum = 0.
	    sumsq = 0.
	    npts = (x2-x1+1)*(y2[k]-y1[k]+1)

	    do i = 1, npts {
		sum = sum + Memr[imdat+i-1]
		sumsq = sumsq + (Memr[imdat+i-1])**2
	    }
	    ave[k] = sum / real(npts)
	    aveerr[k] = sqrt((sumsq/real(npts) - (ave[k])**2)/real(npts-1))
	}
	call bjdet_loop (ave, thresh, nbins, ncomp, indx1, indx2, bjlevel)

	if (indx2 > 1) {
	    if ((indx2-indx1) >= 2) {
		a = y1[indx1+1]
		b = y2[indx2-1]
	    } else {
		a = y1[indx1]
		b = y2[indx2]
	    }
	} else {
	    a = 0
	    b = 0
	}

	# print out the result
	if (streq(outfmt, "calwp2")) 
	    grp = imgeti (imdes, "DETECTOR")
	call bjdet_out (imout, ave, aveerr, nbins, a, b, bjlevel, 
			fname, grp, ngrp, outfmt)
end

procedure bjdet_loop (ave, thresh, nbins, ncomp, indx1, indx2, bjlevel)

# inputs:
real	ave[ARB]	# input average DN array
real	thresh		# criterion of bias jump detection
int	nbins		# Size of the inpt array
int	ncomp		# number of neighbors to be compared

# outputs:
int	indx1, indx2	# array indices of the end points where the jump occurs
real	bjlevel		# the detected bias jump level

# local:
int	i, j, k
real	max, diff
#==============================================================================
begin
	if (ncomp < 1) 
	    call error (1, "must compare to at least one neighboring block.")

	# initialize
	max = 0.
	indx1 = 0
	indx2 = 0

	# do the comparisons, only need to do it "forward".
	do i = 1, nbins-1 {
	    do j = 1, ncomp {
		k = i + j
		if (k > nbins) next
		diff = abs(ave[i] - ave[k])
		if (diff > thresh && diff > max) {
		    indx1 = i
		    indx2 = k
		    max = diff
		}
	    }
	}

	bjlevel = max
end

procedure bjdet_out (imout, ave, aveerr, nbins, a, b, bjlevel, 
			fname, grp, ngrp, outfmt)

# inputs:
pointer	imout
real	ave[ARB]	# input average DN array
real	aveerr[ARB]	# mean error of ave
int	nbins		# number of bins
int	a, b		# array indices of the end points where the jump occurs
real	bjlevel		# the detected bias jump level
char	fname[SZ_FNAME]
int	grp, ngrp
char	outfmt[ARB]

# local:
int	i
char	text[SZ_LINE]
char	gname[SZ_LINE]

bool	streq()
#==============================================================================
begin
	# for the WFPC2 pipeline case
	if (streq(outfmt, "calwp2")) {
	    if (a != 0) {
		call ugrpname (grp, gname)
		if (bjlevel < 0.5) {
		    call sprintf (text, SZ_LINE, 
				  "%s: bias jump level ~%0.3f DN.")
		} else {
		    call sprintf (text, SZ_LINE, 
				  "WARNING: %s: bias jump level ~%0.3f DN.")
		}
		#call printf ( "%s: bias jump occurs between pixels %3d and %3d, jump level ~%0.3f DN.\n")
	   	    call pargstr (gname)
	    	    #call pargi (a)
	    	    #call pargi (b)
	    	    call pargr (bjlevel)

		call printf ("%s\n")
		    call pargstr (text)
		if (imout != NULL) call gf_iputh (imout, "HISTORY", text)
	    }
	} else {
	    if (ngrp == 1) {
	        call printf ("%s: ")
		    call pargstr (fname)
	    } else {
		call printf ("%s[%d]: ")
		    call pargstr (fname)
	    	    call pargi (grp)
	    }
	    if (a == 0) {
	        call printf ("no bias jump.\n")
	    } else {
	        call printf ("bias jump occurs between pixels %d and %d, jump level ~%0.3f DN.\n")
	    	    call pargi (a)
	    	    call pargi (b)
	    	    call pargr (bjlevel)
	    }
	    if (streq(outfmt, "detail")) {
	        call printf ("# bin  average  mean error\n")
	        do i = 1, nbins {
	            call printf (" %3d   %0.3f    %0.3f\n")
		        call pargi (i)
		        call pargr (ave[i])
		        call pargr (aveerr[i])
	        }
	    }
	}
end

procedure ugrpname (grp, gname)

int	grp
char	gname[ARB]

begin
	switch (grp) {
	    case 1:
		call strcpy ("PC1", gname, SZ_LINE)
	    case 2:
		call strcpy ("WF2", gname, SZ_LINE)
	    case 3:
		call strcpy ("WF3", gname, SZ_LINE)
	    case 4:
		call strcpy ("WF4", gname, SZ_LINE)
	}
end
