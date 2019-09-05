include	<pkg/center1d.h>

# Memory management.
define	Cc			Memr[cc+$1-1]

#---------------------------------------------------------------------------
.help xc.x 16Mar95 source
.ih
NAME
.nf
.fi
.endhelp
#---------------------------------------------------------------------------
real procedure xc_shift (in1, in2, npix, mshift)

real	in1[npix]		# I:  Input array 1.
real	in2[npix]		# I:  Input array 2.
int	npix			# I:  Size of the arrays.
int	mshift			# I:  Maximum shift.

# Declarations
pointer	cc			# Correlation funciton.
int	ccn			# Size of the correlation function.
real	center1d()		# Find the peak.
int	hlen			# Point of zero shift.
int	i			# Generic.
real	m			# Maximum correlation.
real	p			# Pixel position of center of correlation.
real	rx			# Generic.

begin
	# Do the cross correlation.
	ccn = mshift*2+1
	hlen = mshift+1
	call malloc (cc, mshift*2+1, TY_REAL)
	call xcor (in1, in2, npix, 1, mshift, Cc(1))

	# Find the maximum in the correlation.
	m = Cc(1)
	rx = 1
	do i = 2, ccn
	    if (Cc(i) > m) {
		m = Cc(i)
		rx = i
	    }
	if (rx > 1 && rx < ccn)
	    p = center1d (rx, Cc(1), ccn, 3., EMISSION, 5., 0.)

        if (IS_INDEFR(p))
            p = rx

	# That's all folks.
	call mfree (cc, TY_REAL)
	return (p - hlen)
end
#---------------------------------------------------------------------------
# End of xc_shift
#---------------------------------------------------------------------------
procedure xcor (a1, a2, length, n_sections, maxd, cc)

real	a1[length]                      # I:  First input array.
real	a2[length]                      # I:  Second input array.
int     length                          # I:  Length of input arrays.
int     n_sections                      # I:  Number of sections to correlate.
int     maxd                            # I:  Maximum offsets to consider.
real	cc[maxd*2+1, n_sections]        # O:  The correlation functions.

# Size of correlation and padded arrays.
int     na1_len                         # Length of padded array.
int     slen                            # Size of each section.
int     so                              # Section offset into original arrays.
int     width                           # Size of correlation window.

# Scalar results.
real  a1_mean                         # Mean of shifted a1 array.
real  a1_sumsq                        # Sum of squares of shifted a1_norm array
real  a2_mean                         # Mean of array a2.
real  a2_sumsq                        # Sum of squares of a2_norm array.
real  assqr()                         # Calculate sum of squares.
real  asumr()                         # Sum an array.
real  mul_sum                         # Sum of multiplication.

# Result arrays.
pointer a1_norm                         # Shifted a1 minus its mean.
pointer a2_norm                         # a2 minums its mean.
pointer mul_result                      # Multiplication of the normalized arrays
pointer na1                             # Padded array for array a1.

# Misc.
int     s                               # Current section.
real  dx                              # Generic.
int     i                               # Generic.
pointer sp                              # Stack pointer.

begin
        call smark (sp)

        # Calculate the size of the correlation.  This should be the same
        # as the fastest changing dimension of the correlation result array
        # cc.
        width = maxd * 2 + 1

        # Determine size of each section.
        slen = length / n_sections

        # Sanity check.
        if (width >= slen)
            call error (1, "Search width is larger than data sections, decrease either search or sections")

        # Determine size of the shifted array.
        na1_len = slen + width

        # Allocate the working arrays.
        call salloc (a1_norm, slen, TY_REAL)
        call salloc (a2_norm, slen, TY_REAL)
        call salloc (mul_result, slen, TY_REAL)
        call salloc (na1, na1_len, TY_REAL)
        
        # Determine correlation for each section.
        do s = 1, n_sections {

            # Compute the offset for each section.
            so = slen * (s - 1) + 1

            # Create new array for the second vector, duplicating the end
            # values to minimize edge effects.  This isn't zero padded due to
            # the mean calculations later one.
            call amovr (a1[so], Memr[na1+maxd], slen)
            call amovkr (a1[so], Memr[na1], maxd)
            call amovkr (a1[so+slen-1], memr[na1+maxd+slen], maxd)
            
            # Make basic statistical calculations with the a2 array.
            call aavgr (a2[so], slen, a2_mean, dx)
            call asubkr (a2[so], a2_mean, Memr[a2_norm], slen)
            a2_sumsq = assqr (Memr[a2_norm], slen)
            
            # Now compute the correlation coefficient for each shift.
            do i = 1, width {
                
                # Calculate mean and difference and sum squared difference for
                # this offset of the first array.
                call aavgr (Memr[na1+i-1], slen, a1_mean, dx)
                call asubkr (Memr[na1+i-1], a1_mean, Memr[a1_norm], slen)
                a1_sumsq = assqr (Memr[a1_norm], slen)
                
                # Multiply the normalized arrays and sum result.
                call amulr (Memr[a1_norm], Memr[a2_norm], Memr[mul_result], slen)
                mul_sum = asumr (Memr[mul_result], slen)
                
                # Build the correlation matrix.
                dx = sqrt (a1_sumsq) * sqrt (a2_sumsq)
                if (dx == 0.d0)
                    cc[i,s] = 0.d0
                else
                    cc[i,s] = mul_sum / dx
            }
        }

        # That's all folks.
        call sfree (sp)
end
#---------------------------------------------------------------------------
# End of xcor
#---------------------------------------------------------------------------
