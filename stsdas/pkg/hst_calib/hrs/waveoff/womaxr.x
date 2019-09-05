#---------------------------------------------------------------------------
.help wo_maxcorr Feb93 source
.ih
NAME
wo_maxcorr -- Find shift using quadratic fitting through maximum correlation
.ih
USAGE
call wo_maxcorr (corr, hlen, n, nsig, niter, guess, delta, verbose, shift, value)
.ih
RETURNS
The subpixel shift represented by the correlation functions.  Returns
INDEFD if no clear shift can be determined.
.ih
ARGUMENTS
.ls corr (I: double[len,n])
The correlation functions.
.le
.ls hlen (I: int)
The half-length of each correlation.  The total length of the correlation is
hlen*2+1.
.le
.ls n (I: int)
The number of correlation functions present.
.le
.ls nsig (I: real)
When more than one correlation is present, shifts are found for each
function.  The average is taken, and those shifts beyond nsig sigma of
the average are rejected.  The average is then recomputed.  This
iteration continues until no shifts are rejected, or niter iterations
have been performed.
.le
.ls niter (I: int)
The maximum number of rejection iterations to perform when multiple
correlation functions are present.  If INDEFI, no limit is imposed.
.le
.ls guess (I: int)
The initial guess at the shift.  If INDEFI, no guess is assumed and
the maximum throughout the correlation is found.  If present, then
only shifts with delta pixels of the guess are examined for a maximum.
.le
.ls delta (I: int)
The window around the guess to search for a maximum.  Thus a maximum
is searched in a window guess-delta to guess+delta pixels.  If guess
is INDEFI, this argument is unused.
.le
.ls verbose (I: bool)
If TRUE, progress on the iterative rejection and final shift
determination is printed to standard output.
.le
.ls shift (O: double)
The shift of the correlation.
.le
.ls value (O: double)
Value of the correlation at the shift.
.le
.ih
DESCRIPTION
This routine returns the subpixel shift represented in the input
correlation functions.

The shift of a correlation function is nominally where the maximum of
the function is located.  However, if a guess is given, then a local
maximum around that guess value is searched for.  The shift is refined
by using quadratic refinement.  If the pixel resides near the edge of
the function or edge of the window around the guess, the routine
returns INDEFD, indicating that the shift cannot be calculated.

The shift for each function is determined and averaged together.
Then, if niter > 0, shifts are rejected if the following condition is
true:

.nf
        abs (shift - mean) > nsig * standard deviation of mean
.fi

The mean is then recalculated.  This iterative process continues until
no shifts are rejected or the number of iterations specified by niter
has occured.
.endhelp
#---------------------------------------------------------------------------
procedure wo_maxcorr (corr, hlen, n, nsig, niter, guess, delta, verbose,
		      mean, value)

double  corr[hlen*2+1,n]	# I:  The correlation functions.
int     hlen			# I:  Half-Length of correlations.
int     n                       # I:  Number of correlations.
real    nsig                    # I:  Sigma deviation- INDEF if not specified.
int     niter                   # I:  Maximum iterations, INDEF if unknown.
int     guess                   # I:  Guess at the shift, INDEF if unknown.
int     delta                   # I:  Half-width around guess to search.
bool    verbose                 # I:  TRUE to write messages.
double  mean                    # O:  Mean of the shifts.
double	value			# O:  Value of correlation at shift.

# Shifts.
int     len				# Length of the correlations.
int     iter                            # Current iteration.
int     ns, nns                         # Number of workable shifts.
pointer s                               # The shifts.
double  sigma                           # Sigma of the shifts.
pointer	v				# Value at shift.

# Misc.
double	dx				# Generic.
int     i, j                            # Generic.
int     min_window, max_window          # Area of correlation to search.
int     mp                              # Pixel containing maximum value.
double  mv                              # Maximum value of correlation.
int     sp                              # Stack pointer.

begin
        call smark(sp)
        call salloc (s, n, TY_DOUBLE)
	call salloc (v, n, TY_DOUBLE)

        # First find the shifts by quadrature fitting of nearest neighbors.
        ns = 0
	len = hlen * 2 + 1
        do i = 1, n {

            # Find maximum.  If a guess is given, just search around that
            # guess.  If not, search the whole function.
            if (IS_INDEFI(guess)) {
                min_window = 1
                max_window = len
            } else {
                min_window = max (1, hlen - guess - delta)
                max_window = min (len, hlen - guess + delta)
                if (max_window <= min_window)
                    call error (1, "poffsets: window too small to search correlation")
            }
            mv = corr[min_window,i]
            mp = min_window
            do j = min_window+1, max_window
                if (corr[j,i] > mv) {
                    mv = corr[j,i]
                    mp = j
                }

            # If the maximum is not "on-edge", find shift.
            if (mp > 1 && mp < len) {
                ns = ns + 1
                Memd[s+ns-1] = ((corr[mp-1,i] - corr[mp,i]) /
                                     (corr[mp-1,i] + corr[mp+1,i] -
                                      2.d0 * corr[mp,i])) - 0.5d0
                Memd[s+ns-1] = hlen - mp - Memd[s+ns-1] + 1.d0
		Memd[v+ns-1] = mv

                if (verbose) {
                    call printf ("    section %d has shift %g.\n")
                    call pargi (i)
                    call pargd (Memd[s+ns-1])
                }
            }
        }

        # Weed out unacceptable shifts.
        call aavgd (Memd[s], ns, mean, sigma)
	call aavgd (Memd[v], ns, value, dx)
        nns = ns
        do iter = 1, niter {
            ns = nns
            if (verbose) {
                call printf ("    shift average = %g, sigma = %g\n")
                call pargd (mean)
                call pargd (sigma)
            }
            nns = 0
            do j = 1, ns
                if (abs (Memd[s+j-1] - mean) <= nsig * sigma) {
                    nns = nns + 1
                    Memd[s+nns-1] = Memd[s+j-1]
		    Memd[v+nns-1] = Memd[v+j-1]
                } else {
                    call printf ("        removing shift %g\n")
                    call pargd (Memd[s+j-1])
                }
	    call aavgd (Memd[v], nns, value, dx)
            call aavgd (Memd[s], nns, mean, sigma)
            if (ns == nns)
                break
        }
        
        # Return the shift.
        if (nns > 0) {
            if (verbose) {
                call printf ("    Final average = %g, sigma = %g.\n")
                call pargd (mean)
                call pargd (sigma)
            }
        } else {
	    mean = INDEFD
	    value = INDEFD
	}

	# That's all folks.
        call sfree(sp)
end
#---------------------------------------------------------------------------
# End of wo_maxcorr
#---------------------------------------------------------------------------
