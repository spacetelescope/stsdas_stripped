#---------------------------------------------------------------------------
.help wo_correlate Feb93 source
.ih
NAME
wo_correlate -- Cross correlate two one-dimensional data vectors.
.ih
USAGE
call wo_correlate (a1, a2, length, n_sections, maxd, cc)
.ih
ARGUMENTS
.ls a1 (I: double[length])
First array of values to correlate.
.le
.ls a2 (I: double[length])
The second array of values to correlate against a1.
.le
.ls length (I: int)
The lengths of the input arrays a1 and a2.
.le
.ls n_sections (I: int)
Number of sections to splint the arrays into.  Each section will be
equal in size and will be correlated seperately.
.le
.ls maxd (I: int)
The maximum pixel shift to find.
.le
.ls cc (O: double[maxd*2+_1,n_sections])
The resulting correlation functions.  Each section is correlated and
each correlation is length maxd * 2 + 1.  The pixel maxd+1 represents
the correlation function at 0 pixel shift.
.le
.ih
DESCRIPTION
The correlation function used is:

.nf
		    TOTAL{(x[i] - xmean) * (y[i - k] - ymean)}
	corr[k] = ------------------------------------------------
		    SQRT(TOTAL{(x[i] - xmean)**2}) *
		         SQRT(TOTAL{(y[i - k] - ymean)**2})
 
	where:
		corr[k] - is the measure of correlation at offset k
		   x[i] - are the values in the first spectrum
		  xmean - is the mean value of X in the region
			  	being correlated
		   y[i] - are the values in the second spectrum
		  ymean - is the mean value of Y in the region being
			 	correlated
	       TOTAL{ } - is a sum over i
.fi
.endhelp
#---------------------------------------------------------------------------
procedure wo_correlate (a1, a2, length, n_sections, maxd, cc)

double  a1[length]                      # I:  First input array.
double  a2[length]                      # I:  Second input array.
int     length                          # I:  Length of input arrays.
int     n_sections                      # I:  Number of sections to correlate.
int     maxd                            # I:  Maximum offsets to consider.
double  cc[maxd*2+1, n_sections]        # O:  The correlation functions.

# Size of correlation and padded arrays.
int     na1_len                         # Length of padded array.
int     slen                            # Size of each section.
int     so                              # Section offset into original arrays.
int     width                           # Size of correlation window.

# Scalar results.
double  a1_mean                         # Mean of shifted a1 array.
double  a1_sumsq                        # Sum of squares of shifted a1_norm array
double  a2_mean                         # Mean of array a2.
double  a2_sumsq                        # Sum of squares of a2_norm array.
double  assqd()                         # Calculate sum of squares.
double  asumd()                         # Sum an array.
double  mul_sum                         # Sum of multiplication.

# Result arrays.
pointer a1_norm                         # Shifted a1 minus its mean.
pointer a2_norm                         # a2 minums its mean.
pointer mul_result                      # Multiplication of the normalized arrays
pointer na1                             # Padded array for array a1.

# Misc.
int     s                               # Current section.
double  dx                              # Generic.
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
        call salloc (a1_norm, slen, TY_DOUBLE)
        call salloc (a2_norm, slen, TY_DOUBLE)
        call salloc (mul_result, slen, TY_DOUBLE)
        call salloc (na1, na1_len, TY_DOUBLE)
        
        # Determine correlation for each section.
        do s = 1, n_sections {

            # Compute the offset for each section.
            so = slen * (s - 1) + 1

            # Create new array for the second vector, duplicating the end
            # values to minimize edge effects.  This isn't zero padded due to
            # the mean calculations later one.
            call amovd (a1[so], Memd[na1+maxd], slen)
            call amovkd (a1[so], Memd[na1], maxd)
            call amovkd (a1[so+slen-1], memd[na1+maxd+slen], maxd)
            
            # Make basic statistical calculations with the a2 array.
            call aavgd (a2[so], slen, a2_mean, dx)
            call asubkd (a2[so], a2_mean, Memd[a2_norm], slen)
            a2_sumsq = assqd (Memd[a2_norm], slen)
            
            # Now compute the correlation coefficient for each shift.
            do i = 1, width {
                
                # Calculate mean and difference and sum squared difference for
                # this offset of the first array.
                call aavgd (Memd[na1+i-1], slen, a1_mean, dx)
                call asubkd (Memd[na1+i-1], a1_mean, Memd[a1_norm], slen)
                a1_sumsq = assqd (Memd[a1_norm], slen)
                
                # Multiply the normalized arrays and sum result.
                call amuld (Memd[a1_norm], Memd[a2_norm], Memd[mul_result], slen)
                mul_sum = asumd (Memd[mul_result], slen)
                
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
# End of wo_correlate
#---------------------------------------------------------------------------
