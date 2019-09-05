include	<imhdr.h>
include	<error.h>
include <ctype.h>
include	<math/curfit.h>
include	<tbset.h>
include	<time.h>

include	"../nlfit/nlfit.h"
include	"../lib/curfitdef.h"

define	IMAGE		1
define	IMAGE2		4
define	TABLE		2
define	TABLE3		5
define	LIST_FILE	3
define  UNKNOWN		NULL
define	INIT_SIZE	2048

define	LINEAR		1
define	NL1		2
define	NL2		3
define	POLY		4

# FUNCTION -- Evaluate a function of the pixel coordinates from a
# image section; or a function of values taken from a table column, 
# table array cell or ASCII list.
#
#
# 16-Sep-97 (I.Busko) Add support for 3-D tables with selectors.
#

procedure t_function ()

char	input1[SZ_LINE]			# input file
char	input2[SZ_LINE]			# fit table
char	output[SZ_LINE]			# output file
int	row				# row with function
bool	xandy				# for list file - output x and f(x)?

char	line[SZ_LINE]
char	column[SZ_COLNAME]
char	root[SZ_FNAME], colselect[SZ_FNAME], rowselect[SZ_FNAME]
char	str[SZ_LINE]
int	i, j, npix, nps, i_in_line, axis, irow, nrow
int	b_size, ctype, dcflag
real	bscale, bzero, crval, crpix, cdelt
real	rval
pointer	in, out				# IMIO pointers
pointer	cv				# CURFIT pointer
pointer	nl				# NLFIT pointer
pointer i_buf, o_buf			# i/o buffer pointers
pointer	pscoef				# power-series coefficients
pointer	n_buf, colptr, x, sp, pcode

int	file_type(), getline(), tbalen()
int	tbpsta(), ctor(), clgeti(), getaxis()
real	nl_zeval()
bool	clgetb(), trseval()
pointer	open(), immap(), impl1r(), impl2r(), trsopen()

begin
	call smark (sp)

	# Get input and output files and check for redirection of input/output

#	if (fstati (STDIN, F_REDIR) == YES)	# deleted by PEH 1995 Mar 1
	call clgstr ("input1", input1, SZ_LINE)

	call clgstr ("input2", input2, SZ_LINE)

	call clgstr ("output", output, SZ_LINE)

	# Get task parameters.
	row  = clgeti ("row")

 	# initialize the curve fitting package
	call f1d_gdb( input2, row, ctype, cv, nl, pscoef, nps )

	# switch on the file type:
	switch ( file_type( input1, in ) ) {

	case IMAGE:
	    if ((ctype != NL1) && (ctype != LINEAR) && (ctype != POLY))
	        call error (0, "Two-variable function is not compatible with image section.")

	    # Open output image and get input header keywords
	    out	= immap (output, NEW_COPY, in)
	    call imgsection (input1, str, SZ_LINE)
	    axis = getaxis (str)
	    call rc_imparam (input1, axis, npix, bscale, bzero, crval, crpix, 
			     cdelt, dcflag)

	    # Generate x array in physical units.
	    call salloc (x, npix, TY_REAL)
	    call malloc( o_buf, npix, TY_REAL)	# To satisfy rc_unit call
	    call amovkr (0., Memr[o_buf], npix) # only.
	    call rc_unit (Memr[x], Memr[o_buf], npix, bscale, bzero, crval, 
			  crpix, cdelt, dcflag, long(1), long(1))
	    call mfree( o_buf, TY_REAL)

	    # Evaluate the line in output image.
	    o_buf = impl1r (out, 1)
	    call f1_eval (ctype, cv, nl, Memr[x], Memr[o_buf], npix, 
			  pscoef, nps )

	    # Update image HISTORY.
	    call func_timelog (str, SZ_LINE)
	    call sprintf (str[SZ_DATE+1], SZ_LINE, 
			 " FUNCTION input: %.15s, Axis: %d")
	        call pargstr (input1)
	        call pargi (axis)
	    call imputh (out, "HISTORY", str)
	    call imunmap (in)
	    call imunmap (out)

	case IMAGE2:
	    if (ctype != NL2)
	        call error (0, "One-variable function is not compatible with image section.")

	    # Open output image
	    out = immap (output, NEW_COPY, in)

	    # Evaluate line by line in output image.
	    do j = 1, IM_LEN(out, 2) {
	        o_buf = impl2r (out, j)
	        do i = 1, IM_LEN(out, 1)
	            Memr[o_buf+i-1] = nl_zeval (nl, real(i), real(j))
	    }

	    # Update image HISTORY.
	    call func_timelog (str, SZ_LINE)
	    call sprintf (str[SZ_DATE+1], SZ_LINE, 
	    		 " FUNCTION input: %.15s")
	        call pargstr (input1)
	    call imputh (out, "HISTORY", str)
	    call imunmap (in)
	    call imunmap (out)

	case TABLE:
	    npix	= tbpsta( in, TBL_NROWS )
	    call salloc( i_buf, npix, TY_REAL )
	    call salloc( n_buf, npix, TY_BOOL )
	    call salloc( o_buf, npix, TY_REAL )
	    call clgstr( "column", column, SZ_COLNAME )
	    call tbcfnd( in, column, colptr, 1)
	    if (colptr == NULL) {		# added by PEH on 1995 Mar 1
		call sprintf (str, SZ_LINE,
		"Column `%s' of values of independent variable not found")
		    call pargstr (column)
		call error (1, str)
	    }
	    call tbcgtr( in, colptr, Memr[i_buf], Memb[n_buf], 1, npix)
	    call f1_eval( ctype, cv, nl, Memr[i_buf], Memr[o_buf], npix,
			  pscoef, nps )
	    colptr = NULL
	    call tbcfnd( in, output, colptr, 1)
	    if ( colptr == NULL )
		call tbcdef( in, colptr, output, "", "", TY_REAL, 0, 1)

	    call tbcptr( in, colptr, Memr[o_buf], 1, npix )
	    call tbtclo( in )

	case TABLE3:

	    # Decode selectors. Only one row can be selected. Column
	    # selectors are ignored.
	    call rdselect (input1, root, rowselect, colselect, SZ_FNAME)
	    call fn_check (root, rowselect)

	    # Find which row matches the selctor.
	    nrow = tbpsta (in, TBL_NROWS)
	    pcode = trsopen (in, rowselect)
	    do i = 1, nrow {
	        if (trseval (in, i, pcode)) {
	            irow = i
	            break
	        }
	    }
	    call trsclose (pcode)

	    # Find the input column and get its array size.
	    call clgstr( "column", column, SZ_COLNAME )
	    call tbcfnd( in, column, colptr, 1)
	    if (colptr == NULL) {
		call sprintf (str, SZ_LINE,
		"Column `%s' of values of independent variable not found")
		    call pargstr (column)
		call error (1, str)
	    }
	    npix = tbalen (colptr)

	    # Alloc buffers.
	    call salloc (i_buf, npix, TY_REAL)
	    call salloc (o_buf, npix, TY_REAL)

	    # Read independent variable.
	    call tbagtr (in, colptr, irow, Memr[i_buf], 1, npix)

	    # Compute function.
	    call f1_eval( ctype, cv, nl, Memr[i_buf], Memr[o_buf], npix,
			  pscoef, nps )

	    # Write output.
	    colptr = NULL
	    call tbcfnd( in, output, colptr, 1)
	    if ( colptr == NULL )
		call tbcdef (in, colptr, output, "", "", TY_DOUBLE, npix, 1)
	    call tbaptr (in, colptr, irow, Memr[o_buf], 1, npix )

	    call tbtclo( in )

	case LIST_FILE:
	    # Note:  this option cannot be used because a list file
	    # opens as a table.
	    xandy	= clgetb( "xandy")
	    call malloc( i_buf, INIT_SIZE, TY_REAL )
	    b_size	= INIT_SIZE
	    npix	= 0
	    while ( getline(in, line) != EOF ) {
		if (line[1] != '#' ) {
		    i_in_line = 1
		    i = ctor (line, i_in_line, rval)
		    if ( npix > b_size ) {
			b_size	= b_size + INIT_SIZE
			call realloc( i_buf, b_size, TY_REAL)
		    }
		    Memr[i_buf + npix ]	= rval
		    npix		= npix + 1
		}
	    }
	    call close ( in )
	    call malloc( o_buf, npix, TY_REAL)
	    call f1_eval( ctype, cv, nl, Memr[i_buf], Memr[o_buf], npix,
			  pscoef, nps )

	    out	= open( output, NEW_FILE, TEXT_FILE )

	    # print either just f(x), or x and f(x)
	    if ( !xandy ) {
	    	for ( i = 1; i<= npix; i=i+1 ) {
		    call fprintf ( out, " %g \n" )
		    call pargr( Memr[o_buf+i-1] )
	    	}
	    } else {
	    	for ( i = 1; i<= npix; i=i+1 ) {
		    call fprintf ( out, " %g   %g \n" )
		    call pargr( Memr[i_buf+i-1] )
		    call pargr( Memr[o_buf+i-1] )
	    	}
	    }

	    call close( out )
	    call mfree( i_buf, TY_REAL)
	    call mfree( o_buf, TY_REAL)
	}

	if (ctype == POLY)
	    call mfree (pscoef, TY_REAL)
	call cvfree( cv)
	call sfree (sp)
end


# F1D_GDB -- read fit details from table and initialize appropriate
# curve fitting package. In case of POLY function, pointer pscoef
# must be freed by the caller.

procedure f1d_gdb( input, row, ctype, cv, nl, pscoef, nps )

char	input[ARB]	# i: fit table name
int	row		# i: table row
int	ctype		# o: curve type
pointer	cv		# o: curfit pointer
pointer	nl		# o: nlfit pointer
pointer	pscoef		# o: power-series coefficients
int	nps		# o: number of p-s coefficients

char	sfunction[SZ_LINE], unit[SZ_LINE]
char	str[SZ_LINE]
int	i, j, func, xunit, npar
real	aux, xmin, xmax
pointer	sp, coef, coef1, err, cflag
pointer	pp

int	strdic(), strlen()
pointer	clopset()

errchk	getfit

begin
	call smark (sp)

	# read table line
	call getfit (input, row, str, str, sfunction, unit, SZ_LINE, 
		     npar, i, aux, aux, xmin, xmax, coef, err, cflag)

	# get function type
	i = strdic (sfunction, str, SZ_LINE, FUNCTIONS)
	j = strdic (sfunction, str, SZ_LINE, NLFUNCTIONS)

	# non-linear function
	if (j != 0) {
	    if (j < NLTWODSEP)
	        ctype = NL1
	    else
	        ctype = NL2
	    func = j
	    xunit = strdic (unit, str, SZ_LINE, NLUNITS)
	    # if user function, get function string from userpars pset
	    if (func == USER) {
	        pp = clopset ("userpars")
	        call clgpset (pp, "function", sfunction, SZ_LINE) 
	        if (strlen(sfunction) == 0)
	            call error (0, "Undefined user function")
	        call clcpset (pp)
	    }
	    # open nlfit package.
	    call nl_init (nl, func, Memr[coef], Memr[cflag], npar, npar+1)
	    call nl_puti (nl, "units", xunit)
	    if (func == USER)
	        call nl_iuser (nl, sfunction, SZ_LINE)

	# linear function
	} else if (i != 0) {
	    ctype = LINEAR
	    call salloc (coef1, npar+4, TY_REAL)
	    do j = 0, npar-1
	        Memr[coef1+j+4] = Memr[coef+j]
	    Memr[coef1]   = real (i)				# curve type
	    if (i == CV_SP1)
	        npar = npar - 1
	    if (i == CV_SP3)
	        npar = npar - 3
	    Memr[coef1+1] = real (npar)				# order
	    Memr[coef1+2] = xmin				# xmin
	    Memr[coef1+3] = xmax				# xmax
	    # restore full set of coefficients
	    call cvrestore( cv, Memr[coef1] )

	# power-series polynomial
	} else {
	    ctype = POLY
	    call malloc (pscoef, npar, TY_REAL)
	    nps = npar
	    call amovr (Memr[coef], Memr[pscoef], npar)
	}

	call sfree (sp)
end


# F1_EVAL -- evaluate a function for a vector

procedure f1_eval( ctype, cv, nl, in, out, npix, pscoef, nps )

int	ctype		# i: curve type
pointer	cv		# i: CURVEFIT pointer
pointer	nl		# i: NLFIT pointer
real	in[ARB]		# i: input vector
real	out[ARB]	# o: output evaluated points
int	npix		# i: number of points
pointer	pscoef		# i: power-series coefficients
int	nps		# i: number of p-s coefficients

int	i, j
real	temp, cveval(), nl_zeval()

begin
	do i = 1, npix {
	    temp = in[i]
	    if ( IS_INDEFR( temp ) ) {
		out[i] = INDEFR
	    } else {
	        switch (ctype) {
	        case LINEAR:
	            out[i] = cveval( cv, temp )
	        case NL1:
	            out[i] = nl_zeval (nl, temp, 0. )
	        case POLY:
	            out[i] = 0.
	            do j = 0, nps-1
	                out[i] = out[i] + Memr[pscoef+j] * (temp ** j)
	        }
	    }
	}
end


# FILE_TYPE -- find out whether a given file is an image, table or text file
# This is done by trying to open it as an image, table and then text file -
# in that order. The first open that works will be used. In case of image,
# get dimensionality.

int procedure file_type( input, in )

char	input[ARB]		# i: input file name
pointer	in			# o: file pointer

#--
char	root[SZ_FNAME], rowselect[SZ_FNAME], colselect[SZ_FNAME]
int	f_type			# file type indicator
pointer	immap(), tbtopn(), open()

begin
	call rdselect (input, root, rowselect, colselect, SZ_FNAME)

	in	= NULL
	f_type	= UNKNOWN

	iferr ( in = immap( root, READ_ONLY, 0) ) {
	    in	= NULL
	}

	if ( in == NULL ) {
	    iferr ( in = tbtopn( root, READ_WRITE, 0 ) ) {
		in = NULL
	    }
	} else {
	    if (IM_NDIM( in ) == 1)
	        f_type = IMAGE
	    else if (IM_NDIM( in ) == 2)
	        f_type = IMAGE2
	    else
	        call error (0, "Invalid image dimensionality.")
	}

	if ( in == NULL ) {
	    iferr ( in = open( root, READ_ONLY, TEXT_FILE) ) {
		in = NULL
	    }
	} else if ( f_type == UNKNOWN ) {
	    f_type = TABLE
	    if ( rowselect[1] != EOS )
	        f_type = TABLE3
	}

	if ( in != NULL && f_type == UNKNOWN )
	    f_type = LIST_FILE

	if ( in == NULL || f_type == UNKNOWN )
	    call error ( 0, " Input must be an image, table or text file")

	return ( f_type )
end
                                                        

# FUNC_TIMELOG -- Prepend a time stamp to the given string.
#
# For the purpose of a history logging prepend a short time stamp to the
# given string.  Note that the input string is modified.

procedure func_timelog (str, max_char)

char	str[max_char]		# String to be time stamped
int	max_char		# Maximum characters in string

pointer	sp, time, temp
long	clktime()

begin
	call smark (sp)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (temp, max_char, TY_CHAR)

	call cnvdate (clktime(0), Memc[time], SZ_DATE)
	call sprintf (Memc[temp], max_char, "%s %s")
	    call pargstr (Memc[time])
	    call pargstr (str)
	call strcpy (Memc[temp], str, max_char)

	call sfree (sp)
end



# FN_CHECK --  Checks to see if the row selector actually selects
#              one single row.
#
# Installed: 16-Sep-97 - I.Busko

procedure fn_check (table, rowselect)

char	table[ARB]
char	rowselect[ARB]

#--
int	selrows()
pointer	tp, tbtopn()

begin
	tp = tbtopn (table, READ_ONLY, NULL)
	if (selrows (tp, rowselect) > 1)
	    call error (0, "Only one row can be selected.")
	else if (selrows (tp, rowselect) < 1)
	    call error (0, "No rows selected.")
	call tbtclo (tp)
end
