#* HISTORY *
#* D.Ball	18-Apr-88	from the ASURV routine of the same name
#* B.Simon	13-Aug-92	revised to use asurv 1.2

# KMESTM -- Compute the PL estimator, mean and error for the x variable

procedure kmestm (ind, x, jcol, ncol, ntot, binstart, binsize, nbin, 
		  diff, verbose)

int	ind[ntot] 	# i: indicator of censoring
double	x[ncol,ntot]	# i: data array
int	jcol		# i: which column to process
int	ncol		# i: number of data columns
int	ntot		# i: number of data values
double	binstart	# i: starting value of the first bin
double	binsize		# i: width of the bin
int	nbin		# i: number of bins
bool	diff		# i: compute differential estimator?
bool	verbose		# i: print out long form of output?
#--
int	isign, iu, ic, ltot, ichange, nchange, ntemp
double	smean, error, bstart, bsize
pointer	sp, zu, zc, risk, atie, sx, vx, fx, lx, bs, bl, dx

begin
	# Allocate work arrays

	call smark (sp)
	call salloc (zu, ntot, TY_DOUBLE)
	call salloc (zc, ntot, TY_DOUBLE)
	call salloc (risk, ntot, TY_DOUBLE)
	call salloc (atie, ntot, TY_DOUBLE)
	call salloc (sx, ntot, TY_DOUBLE)
	call salloc (vx, ntot, TY_DOUBLE)
	call salloc (fx, ntot, TY_DOUBLE)
	call salloc (lx, ntot, TY_INT)
	call salloc (bs, nbin, TY_DOUBLE)
	call salloc (bl, nbin, TY_DOUBLE)
	call salloc (dx, nbin, TY_DOUBLE)

	# Sort data into increasing order

	call km_sort (ind, x, jcol, ncol, ntot)

	# Separate censored data from uncensored data
	# The output arrays risk and atie are not use in this procedure

	call xvar (ind, x, jcol, ncol, ntot, isign, Memd[zu], Memd[zc], 
		   iu, ic, Memd[risk], Memd[atie], ltot)

	# Reversing the output arrays sorts them if they were
	# multiplied by -1

	if (isign < 0) {
	    call revrsd (Memd[zu], iu)
	    call revrsd (Memd[zc], ic)
	}

	# Compute the Kaplan-Meier estimator

	call plestm (Memd[zu], Memd[zc], iu, ic, Memd[sx], Memd[vx], 
		ntot, smean, error, ichange, nchange, Memi[lx])

	smean = isign * smean

	# Adjust the product limit estimator back to the original 
	# censoring pattern and remove ties

       call kmadj (Memd[zu], Memd[zc], ntot, iu, ic, Memd[sx], isign, 
		   ntemp, Memd[fx], Memd[vx])

	if (diff) {
	    call setbin (x[jcol,1], x[jcol,ntot], binstart, binsize, nbin, 
			 bstart, bsize)

	    call kmdif (Memd[sx], Memd[zu], iu, ntot, bstart, bsize,
		       nbin, Memd[fx], Memd[bs], Memd[bl], Memd[dx])
	}

	# Report the K-M estimator, differential estimator,
	# percentiles, mean and error

	call kmprint (verbose, diff, isign, ichange, smean, error, Memd[zu], 
		      Memd[zc], ntot, ntemp, iu, ic, Memd[sx], Memd[vx],
		      nbin, Memd[bs], Memd[bl], Memd[dx])
			
	call sfree (sp)
end
