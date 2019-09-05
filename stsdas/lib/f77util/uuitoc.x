include <iraf77.h>
include	<mach.h>
include	<ctype.h>

# UUITOC -- Integer to character string.  We do not resolve this into a call
# to GLTOC for reasons of efficiency.

procedure uuitoc (ival, w, f77str, istat)

int	ival
%	character*(*)	f77str
int	w		# field width to output
			# w == 0   Output is left justified. 
			# w > 0    Output is right justified. 
			# w < 0    Output is zero padded.
int	istat

char	str[SZ_LINE]
char	buf[MAX_DIGITS]
int	b_op, s_op, num, temp, maxch, nch, nc, nuw
int	gstrcpy(), len()

begin	
	istat = ER_OK
%	maxch = len (f77str)
	if (maxch < abs(w)) {
	   istat = ER_NOSPACE
	   return
	}
	s_op = 1

	if (IS_INDEFI (ival)) {
	    nch = gstrcpy ("INDEF", str, maxch)	
	    call f77pak (str, f77str, SZ_LINE)	    
	    return 
	} else if (ival < 0) {
	    if (w <= 0) {
	       str[1] = '-'
	       s_op = 2
	    } 
	    num = -ival
	} else
	    num = ival

	# Encode nonnegative number in BUF, least significant digits first.

	b_op = 0
	repeat {
	    temp = num / 10
	    b_op = b_op + 1
	    buf[b_op] = TO_DIGIT (num - temp * 10)
	    num = temp
	} until (num == 0)

	# Copy encoded number to output string, reversing the order of the
	# digits so that the most significant digits are first.

	nc = w
	nuw = b_op
	if (ival < 0 && w < 0) nuw = nuw + 1    # allow for minus sign
	if (w > 0 && w <= b_op && ival < 0)     # field too narrow, will
	    s_op = s_op + w    			# bomb
						      
	if (w == 0)    		# left justified
	   while (b_op > 0) {
	       if (s_op > maxch) {
		  nch = gstrcpy ("**********", str, maxch)
	          call f77pak (str, f77str, SZ_LINE)	
		  return 
	       }
	       str[s_op] = buf[b_op]
	       s_op = s_op + 1
	       b_op = b_op - 1
	   }
	else	       		# rigth justified
	   while (b_op > 0) {
	       if (s_op > maxch || s_op > abs(w)) {
	          nch = gstrcpy ("**********", str, maxch)
	          call f77pak (str, f77str, SZ_LINE)	
	          return 
	       }

	       if (nc > nuw) { 			      # blank padded
	          str[s_op] = ' '
	          nc = nc - 1
	          if (nc == nuw && ival < 0)
		     str[s_op] = '-'
	       } else if (w < 0 && abs(nc) > nuw) {   # zero padded
	          str[s_op] = '0'
	          nc = nc + 1
	       } else {
	          str[s_op] = buf[b_op]
	          b_op = b_op - 1
	       }
	       s_op = s_op + 1
	   }
	
	str[s_op] = EOS
	call f77pak (str, f77str, SZ_LINE)	
	return
end
