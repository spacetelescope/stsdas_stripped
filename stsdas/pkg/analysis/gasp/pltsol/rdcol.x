include <math.h>
include <mach.h>
include <config.h>

define	MAX_NCOL	6
# RD_COL -- Procedure to decode character buffer into specified
#	    tokens.

procedure rd_col (buf, icol, x, y, dra, ddec, mag, color)

char buf[ARB]		# input buffer
int  icol[MAX_NCOL]	# input column order
real x, y		# output pixel coordinates
double	dra, ddec	# output equatorial coordinates in radians
real	mag		# output magnitude
real	color		# output color

pointer sp, pp
int	ic, ip, i, maxcol, ctor(), ctod(), ctowrd()
int	nchar
begin

	# find the right most column to read from buffer
	maxcol = 1
	do i = 1, MAX_NCOL
	   maxcol = max (maxcol, abs(icol(i)))
	
	call smark (sp)
	call salloc (pp, maxcol*MAX_DIGITS, TY_CHAR)

	ip = 1
	ic = pp
	do i = 1, maxcol {
	   nchar = ctowrd (buf, ip, Memc[ic], MAX_DIGITS)
	   # If there are blanks lines at the end (nchar ==0) return.
	   if (nchar == 0)
	      call error(0,"")
	   # store word in equal length array
	   call amovkc (" ", Memc[ic+nchar], MAX_DIGITS-nchar)
	   Memc[ic+MAX_DIGITS] = EOS
	   ic = ic + MAX_DIGITS+1
	}

	# get the output parameters

	nchar = ctor (Memc[pp], (icol(1)-1)*(MAX_DIGITS+1)+1, x)
	nchar = ctor (Memc[pp], (icol(2)-1)*(MAX_DIGITS+1)+1, y)
	nchar = ctod (Memc[pp], (abs(icol(3))-1)*(MAX_DIGITS+1)+1, dra)
	if (icol(3) < 0)
	   dra = DEGTORAD (dra*15)	# input is in hour
	else
	   dra = DEGTORAD (dra)
	nchar = ctod (Memc[pp], (icol(4)-1)*(MAX_DIGITS+1)+1, ddec)
	ddec = DEGTORAD (ddec)
	nchar = ctor (Memc[pp], (icol(5)-1)*(MAX_DIGITS+1)+1, mag)
	nchar = ctor (Memc[pp], (icol(6)-1)*(MAX_DIGITS+1)+1, color)
	
	call sfree (sp)
end
