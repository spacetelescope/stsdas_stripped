include	<imhdr.h>
include	<iraf77.h>

# UUMNMX -- Determine science data extrema
#
# C. Biemesderfer, STScI, 29-Jan-88
# N Zarate Rewritten May 1990

procedure uumnmx (im, datamin, datamax, istat)

pointer	im		# i: Science data image structure
real	datamin, datamax 	# o: Data min and max values
int	istat		# o: Return status

int	npix
long	v[IM_MAXDIM], clktime()
int	imgnll(), imgnlr(),imgnls()

pointer	buf
bool	first_line
short	minval_s, maxval_s
long	minval_l, maxval_l
real	minval_r, maxval_r

begin
	istat = ER_OK

	# Flush first to insure that the data is available
	call imflush(im)

	call amovkl (long(1), v, IM_MAXDIM)		# start vector
	first_line = true
	datamin = 0
	datamax = -1

	npix = IM_LEN(im,1)
	switch (IM_PIXTYPE(im)) {
	case TY_SHORT:
	    while (imgnls (im, buf, v) != EOF) {
		call alims (Mems[buf], npix, minval_s, maxval_s)
		if (first_line) {
		    datamin = minval_s
		    datamax = maxval_s
		    first_line = false
		} else {
		    if (minval_s < datamin)
			datamin = minval_s
		    if (maxval_s > datamax)
			datamax = maxval_s
		}
	    }
	case TY_USHORT, TY_INT, TY_LONG:
	    while (imgnll (im, buf, v) != EOF) {
		call aliml (Meml[buf], npix, minval_l, maxval_l)
		if (first_line) {
		    datamin = minval_l
		    datamax = maxval_l
		    first_line = false
		} else {
		    if (minval_l < datamin)
			datamin = minval_l
		    if (maxval_l > datamax)
			datamax = maxval_l
		}
	    }
	default:
	    while (imgnlr (im, buf, v) != EOF) {
		call alimr (Memr[buf], npix, minval_r, maxval_r)
		if (first_line) {
		    datamin = minval_r
		    datamax = maxval_r
		    first_line = false
		} else {
		    if (minval_r < datamin)
			datamin = minval_r
		    if (maxval_r > datamax)
			datamax = maxval_r
		}
	    }
	}
	IM_MIN(im) = datamin
	IM_MAX(im) = datamax
	IM_LIMTIME(im) = clktime(long(0))
end
