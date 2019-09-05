# 5 procedures shared by t_imconvb.x, t_irfftesb.x, t_irme0b.x

# Move real ain[n1in,n2in] to aout[n1out,n2out]. ain[1,1] => aout[1,1]

procedure move_array (ain, n1in, n2in, aout, n1out, n2out)

real	ain[n1in,n2in], aout[n1out,n2out]  # Input and output real arrays
int	n1in, n2in, n1out, n2out	   # Array sizes

int	i, j

begin
	if (n2in >= n2out) 
	    do j = 1, n2out
	        do i = 1, n1out
	            aout[i,j] = ain[i,j]
	else {
	    do j = 1, n2in {
	        do i = 1, n1in
	            aout[i,j] = ain[i,j]
	        do i = n1in + 1, n1out
	            aout[i,j] = 0.0
	    }
	    do j =  n2in + 1, n2out
	        do i = 1, n1out
	            aout[i,j] = 0.0
	}
end

# Find the peak value and its location of a real array

procedure arrpeak (a, n1, n2, pval, ploc)

real	a[n1,n2]
int	n1, n2
real	pval		# Peak value
int	ploc[2]		# Peak location

real	ptmp	
int	i, j 

real	ahivr()

begin

	    pval = ahivr (a[1,1], n1)
	    ploc[2] = 1
	    do j = 2, n2 {
	        ptmp = ahivr (a[1,j], n1)
	        if (ptmp > pval) {
	            pval = ptmp
	            ploc[2] = j
	        }
	    }
	    ploc[1] = 1
	    do i = 1, n1 {
	        if (a[i,ploc[2]] == pval) {
	            ploc[1] = i
	            break
	        }
	    }
end


# Shift cyclically 1-D array ain[n1] by sh1, resulting aout[n1]. 
# ain and aout must be distinct arrays.

procedure lnshift (ain, aout, n1, sh1)

real	ain[n1], aout[n1]	# Input and output arrays
int	n1, sh1

int	shabs		# abs (sh1)
int	nx		# n1 - abs(sh1)

begin
 
	if (sh1 > 0) {
	    nx = n1 - sh1
	    call amovr (ain, aout[sh1+1], nx)
	    call amovr (ain[nx+1], aout, sh1)
	} else if (sh1 < 0) {
	    shabs = abs (sh1)
	    nx = n1 - shabs
	    call amovr (ain, aout[nx+1], shabs)
	    call amovr (ain[shabs+1], aout, nx)
	} else {
	    call amovr (ain, aout, n1)
	}
end

# Multiply two complex arrays a1 and a2, resulting output in a1

procedure amulxx (a1, a2, n)

complex	a1[n], a2[n]
int	n 
	         
int	i

begin
	do i = 1, n
	    a1[i] = a1[i] * a2[i]
end

