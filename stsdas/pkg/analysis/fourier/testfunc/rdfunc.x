include	<error.h>
include	<ctype.h>

define	SZ_BUF	1024

# RDFUNC -- Read a list of two dimensional data pairs into two type
# real arrays in memory.  Return pointers to the arrays and a count of the
# number of pixels.

# Ripoff of GG_RDLIST2

int procedure rdfunc (fname, x, y)

char	fname		# i: File to read data from
pointer	x, y		# o: Pointers to x and y vectors

int	buflen, n, fd, ncols, lineno
pointer	sp, lbuf, ip
real	xval, yval
int	getline(), nscan(), open()
errchk	open, sscan, getline, malloc

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	fd = open (fname, READ_ONLY, TEXT_FILE)

	buflen = SZ_BUF
	iferr {
	    call malloc (x, buflen, TY_REAL)
	    call malloc (y, buflen, TY_REAL)
	} then
	    call erract (EA_FATAL)

	n = 0
	ncols = 0
	lineno = 0

	while (getline (fd, Memc[lbuf]) != EOF) {
	    # Skip comment lines and blank lines.
	    lineno = lineno + 1
	    if (Memc[lbuf] == '#')
		next
	    for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '\n' || Memc[ip] == EOS)
		next

	    # Decode the points to be plotted.
	    call sscan (Memc[ip])
		call gargr (xval)
		call gargr (yval)

	    # The first line determines whether we have an x,y list or a
	    # y-list.  It is an error if only one value can be decoded when
	    # processing a two column list.

	    if (ncols == 0 && nscan() > 0)
		ncols = nscan()
	    
	    switch (nscan()) {
	    case 0:
		call eprintf ("no args; %s, line %d: %s\n")
		    call pargstr (fname)
		    call pargi (lineno)
		    call pargstr (Memc[lbuf])
		next
	    case 1:
		if (ncols == 2) {
		    call eprintf ("only 1 arg; %s, line %d: %s\n")
			call pargstr (fname)
			call pargi (lineno)
			call pargstr (Memc[lbuf])
		    next
		} else {
		    yval = xval
		    xval = n + 1.0
		}
	    }

	    n = n + 1
	    if (n > buflen) {
		buflen = buflen + SZ_BUF
		call realloc (x, buflen, TY_REAL)
		call realloc (y, buflen, TY_REAL)
	    }

	    Memr[x+n-1] = xval
	    Memr[y+n-1] = yval
	}

	call realloc (x, n, TY_REAL)
	call realloc (y, n, TY_REAL)

	call close (fd)
	call sfree (sp)
	return (n)
end
