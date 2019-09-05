include <mach.h>	# for SZB_CHAR, same on both machines
include <tbset.h>
include "tconvert.h"

# tvs_data -- subroutines for copying data between VAX & Sun
# There are two copies of this file, with minor differences between them.
# One version is for a Sun, and the other version is for a VAX.  Portions
# of the code that differ between the two versions are indicated by a
# comment beginning with "# ***".
#
# *** This version is for a VAX.
#
# Phil Hodge, 11-Oct-1989  Subroutines created.
# Phil Hodge, 27-Apr-1990  Use ieepak[rd], ieeupk[rd].
# Phil Hodge, 15-Apr-1993  Include short datatype.
# Phil Hodge, 11-May-1994  Remove tests on INDEFI and INDEFS.

# tvs_data_r -- copy data (row-ordered table)
# Copy the data portion of a row-ordered table from VAX format to Sun format.

procedure tvs_data_r (itp, ifd, ofd)

pointer itp			# i: pointer to descriptor for input table
int	ifd			# i: fd for input table file
int	ofd			# i: fd for output table file
#--
char	cbuf[SZ_LINE]		# a buffer for each data type

char	inbuf[4]		# large enough to hold a double
double	inbufd
real	inbufr
int	inbufi[2]
short	inbufs
equivalence (inbufd, inbufr, inbufi, inbufs, inbuf)

pointer cptr			# pointer to column descriptor
int	dtype			# data type of column
int	nrows			# number of rows in table
int	row			# loop index for row number
int	ncols			# number of columns in table
int	col			# column number (not pointer)
int	clen			# number of char to read or write
pointer tbcnum()
int	read()
int	tbcigi(), tbpsta()

# These are used if the allocated row length > row length used
pointer sp
pointer dummy			# buffer for writing end of row
int	len_extra		# number of char at end of line
int	k			# loop index
bool	extra			# true if row length > row length used
long	offset			# offset to beginning of row
long	tbeoff()

begin
	call smark (sp)

	nrows = tbpsta (itp, TBL_NROWS)
	ncols = tbpsta (itp, TBL_NCOLS)

	len_extra = tbpsta (itp, TBL_ROWLEN_CHAR) -
		    tbpsta (itp, TBL_ROWLEN_CHAR_USED)
	if (len_extra > 0) {
	    extra = true
	    call salloc (dummy, len_extra, TY_CHAR)
	    do k = 0, len_extra-1
		Memc[dummy+k] = 0
	} else {
	    extra = false
	}

	do row = 1, nrows {

	    do col = 1, ncols {

		cptr = tbcnum (itp, col)
		dtype = tbcigi (cptr, TBL_COL_DATATYPE)

		# We must seek in the input table because we have not read
		# the extra (allocated but not used) portion of the row.
		if ((col == 1) && extra) {
		    offset = tbeoff (itp, cptr, row)
		    call seek (ifd, offset)
		}

		switch (dtype) {
		case TY_REAL:
		    if (read (ifd, inbuf, SZ_REAL) < SZ_REAL)
			call error (1,
				"tvs_data_r:  error reading real element")
		    if (IS_INDEFR (inbufr)) {
			inbufi[1] = TVS_SUN_INDEFR	# ***
		    } else {
			call ieepakr (inbufr)		# ***
		    }
		    call write (ofd, inbuf, SZ_REAL)

		case TY_DOUBLE:
		    if (read (ifd, inbuf, SZ_DOUBLE) < SZ_DOUBLE)
			call error (1,
				"tvs_data_r:  error reading double element")
		    if (IS_INDEFD (inbufd)) {
			inbufi[1] = TVS_SUN_INDEFD1	# ***
			inbufi[2] = TVS_SUN_INDEFD2	# ***
		    } else {
			call ieepakd (inbufd)		# ***
		    }
		    call write (ofd, inbuf, SZ_DOUBLE)

		case TY_INT:
		    if (read (ifd, inbuf, SZ_INT) < SZ_INT)
			call error (1,
				"tvs_data_r:  error reading integer element")
		    call bswap4 (inbufi, 1, inbufi, 1, SZ_INT*SZB_CHAR)
		    call write (ofd, inbuf, SZ_INT)

		case TY_SHORT:
		    if (read (ifd, inbuf, SZ_SHORT) < SZ_SHORT)
			call error (1,
				"tvs_data_r:  error reading short element")
		    call bswap2 (inbufs, 1, inbufs, 1, SZ_SHORT*SZB_CHAR)
		    call write (ofd, inbuf, SZ_SHORT)

		case TY_BOOL:
		    # read into integer buffer
		    if (read (ifd, inbuf, SZ_BOOL) < SZ_BOOL)
			call error (1,
				"tvs_data_r:  error reading boolean element")
		    call bswap4 (inbufi, 1, inbufi, 1, SZ_BOOL*SZB_CHAR)
		    call write (ofd, inbuf, SZ_BOOL)

		default:
		    if (dtype > 0)
			call error (1, "invalid data type in input table")
		    clen = COL_LEN(cptr)
		    if (read (ifd, cbuf, clen) < clen)
			call error (1,
				"tvs_data_r:  error reading text element")
		    call write (ofd, cbuf, clen)
		}
	    }

	    # Every column in the current row has been copied, but the
	    # allocated size of the row may be larger than this.  If so,
	    # fill out the extra portion with zeroes.
	    if (extra)
		call write (ofd, Memc[dummy], len_extra)
	}
	call sfree (sp)
end


# tvs_data_c -- copy data (column-ordered table)
# Copy the data portion of a column-ordered table from VAX format to Sun format.

procedure tvs_data_c (itp, ifd, ofd)

pointer itp			# i: pointer to descriptor for input table
int	ifd			# i: fd for input table file
int	ofd			# i: fd for output table file
#--
char	cbuf[SZ_LINE]		# a buffer for each data type

char	inbuf[4]		# large enough to hold a double
double	inbufd
real	inbufr
int	inbufi[2]
short	inbufs
equivalence (inbufd, inbufr, inbufi, inbufs, inbuf)

pointer cptr			# pointer to column descriptor
int	dtype			# data type of column
int	allrows			# number of rows allocated in table
int	row			# loop index for row number
int	ncols			# number of columns in table
int	col			# column number (not pointer)
int	clen			# number of char to read or write
pointer tbcnum()
int	read()
int	tbcigi(), tbpsta()

begin
	allrows = tbpsta (itp, TBL_ALLROWS)
	ncols = tbpsta (itp, TBL_NCOLS)

	do col = 1, ncols {

	    cptr = tbcnum (itp, col)
	    dtype = tbcigi (cptr, TBL_COL_DATATYPE)

	    switch (dtype) {
	    case TY_REAL:

		do row = 1, allrows {
		    if (read (ifd, inbuf, SZ_REAL) < SZ_REAL)
			call error (1,
				"tvs_data_c:  error reading real element")
		    if (IS_INDEFR (inbufr)) {
			inbufi[1] = TVS_SUN_INDEFR	# ***
		    } else {
			call ieepakr (inbufr)		# ***
		    }
		    call write (ofd, inbuf, SZ_REAL)
		}

	    case TY_DOUBLE:
		do row = 1, allrows {
		    if (read (ifd, inbuf, SZ_DOUBLE) < SZ_DOUBLE)
			call error (1,
				"tvs_data_c:  error reading double element")
		    if (IS_INDEFD (inbufd)) {
			inbufi[1] = TVS_SUN_INDEFD1	# ***
			inbufi[2] = TVS_SUN_INDEFD2	# ***
		    } else {
			call ieepakd (inbufd)		# ***
		    }
		    call write (ofd, inbuf, SZ_DOUBLE)
		}

	    case TY_INT:
		do row = 1, allrows {
		    if (read (ifd, inbuf, SZ_INT) < SZ_INT)
			call error (1,
				"tvs_data_c:  error reading integer element")
		    call bswap4 (inbufi, 1, inbufi, 1, SZ_INT*SZB_CHAR)
		    call write (ofd, inbuf, SZ_INT)
		}

	    case TY_SHORT:
		do row = 1, allrows {
		    if (read (ifd, inbuf, SZ_SHORT) < SZ_SHORT)
			call error (1,
				"tvs_data_c:  error reading short element")
		    call bswap2 (inbufs, 1, inbufs, 1, SZ_SHORT*SZB_CHAR)
		    call write (ofd, inbuf, SZ_SHORT)
		}

	    case TY_BOOL:
		do row = 1, allrows {
		    # read into integer buffer
		    if (read (ifd, inbuf, SZ_BOOL) < SZ_BOOL)
			call error (1,
				"tvs_data_c:  error reading boolean element")
		    call bswap4 (inbufi, 1, inbufi, 1, SZ_BOOL*SZB_CHAR)
		    call write (ofd, inbuf, SZ_BOOL)
		}

	    default:
		if (dtype > 0)
		    call error (1, "invalid data type in input table")
		do row = 1, allrows {
		    clen = COL_LEN(cptr)
		    if (read (ifd, cbuf, clen) < clen)
			call error (1,
				"tvs_data_c:  error reading text element")
		    call write (ofd, cbuf, clen)
		}
	    }
	}
end


# tsv_data_r -- copy data (row-ordered table)
# Copy table data from Sun to VAX format.

procedure tsv_data_r (itp, ifd, ofd)

pointer itp			# i: pointer to descriptor for input table
int	ifd			# i: fd for input table file
int	ofd			# i: fd for output table file
#--
char	cbuf[SZ_LINE]		# a buffer for each data type

char	inbuf[4]		# large enough to hold a double
int	inbufi[2]
double	inbufd
real	inbufr
short	inbufs
equivalence (inbufd, inbufr, inbufi, inbufs, inbuf)

pointer cptr			# pointer to column descriptor
int	dtype			# data type of column
int	nrows			# number of rows in table
int	row			# loop index for row number
int	ncols			# number of columns in table
int	col			# column number (not pointer)
int	clen			# number of char to read or write
pointer tbcnum()
int	read()
int	tbcigi(), tbpsta()

# These are used if the allocated row length > row length used
pointer sp
pointer dummy			# buffer for writing end of row
int	len_extra		# number of char at end of line
int	k			# loop index
bool	extra			# true if row length > row length used
long	offset			# offset to beginning of row
long	tbeoff()

begin
	call smark (sp)

	nrows = tbpsta (itp, TBL_NROWS)
	ncols = tbpsta (itp, TBL_NCOLS)

	len_extra = tbpsta (itp, TBL_ROWLEN_CHAR) -
		    tbpsta (itp, TBL_ROWLEN_CHAR_USED)
	if (len_extra > 0) {
	    extra = true
	    call salloc (dummy, len_extra, TY_CHAR)
	    do k = 0, len_extra-1
		Memc[dummy+k] = 0
	} else {
	    extra = false
	}

	do row = 1, nrows {

	    do col = 1, ncols {

		cptr = tbcnum (itp, col)
		dtype = tbcigi (cptr, TBL_COL_DATATYPE)

		# We must seek in the input table because we have not read
		# the extra (allocated but not used) portion of the row.
		if ((col == 1) && extra) {
		    offset = tbeoff (itp, cptr, row)
		    call seek (ifd, offset)
		}

		switch (dtype) {
		case TY_REAL:
		    if (read (ifd, inbuf, SZ_REAL) < SZ_REAL)
			call error (1,
				"tsv_data_r:  error reading real element")
		    if (inbufi[1] == TVS_SUN_INDEFR) {		# ***
			inbufr = INDEFR
		    } else {
			call ieeupkr (inbufr)			# ***
		    }
		    call write (ofd, inbuf, SZ_REAL)

		case TY_DOUBLE:
		    if (read (ifd, inbuf, SZ_DOUBLE) < SZ_DOUBLE)
			call error (1,
				"tsv_data_r:  error reading double element")
		    if ((inbufi[1] == TVS_SUN_INDEFD1) &&
			(inbufi[2] == TVS_SUN_INDEFD2))		# ***
			inbufd = INDEFD
		    else
			call ieeupkd (inbufd)			# ***
		    call write (ofd, inbuf, SZ_DOUBLE)

		case TY_INT:
		    if (read (ifd, inbuf, SZ_INT) < SZ_INT)
			call error (1,
				"tsv_data_r:  error reading integer element")
		    call bswap4 (inbufi, 1, inbufi, 1, SZ_INT*SZB_CHAR)
		    call write (ofd, inbuf, SZ_INT)

		case TY_SHORT:
		    if (read (ifd, inbuf, SZ_SHORT) < SZ_SHORT)
			call error (1,
				"tsv_data_r:  error reading short element")
		    call bswap2 (inbufs, 1, inbufs, 1, SZ_SHORT*SZB_CHAR)
		    call write (ofd, inbuf, SZ_SHORT)

		case TY_BOOL:
		    # read into integer buffer
		    if (read (ifd, inbuf, SZ_BOOL) < SZ_BOOL)
			call error (1,
				"tsv_data_r:  error reading boolean element")
		    call bswap4 (inbufi, 1, inbufi, 1, SZ_BOOL*SZB_CHAR)
		    call write (ofd, inbufi, SZ_BOOL)

		default:
		    if (dtype > 0)
			call error (1, "invalid data type in input table")
		    clen = COL_LEN(cptr)
		    if (read (ifd, cbuf, clen) < clen)
			call error (1,
				"tsv_data_r:  error reading text element")
		    call write (ofd, cbuf, clen)
		}
	    }

	    # Every column in the current row has been copied, but the
	    # allocated size of the row may be larger than this.  If so,
	    # fill out the extra portion with zeroes.
	    if (extra)
		call write (ofd, Memc[dummy], len_extra)
	}
	call sfree (sp)
end


# tsv_data_c -- copy data (column-ordered table)
# Copy table data from Sun to VAX format.

procedure tsv_data_c (itp, ifd, ofd)

pointer itp			# i: pointer to descriptor for input table
int	ifd			# i: fd for input table file
int	ofd			# i: fd for output table file
#--
char	cbuf[SZ_LINE]		# a buffer for each data type

char	inbuf[4]		# large enough to hold a double
int	inbufi[2]
double	inbufd
real	inbufr
short	inbufs
equivalence (inbufd, inbufr, inbufi, inbufs, inbuf)

pointer cptr			# pointer to column descriptor
int	dtype			# data type of column
int	allrows			# number of rows allocated in table
int	row			# loop index for row number
int	ncols			# number of columns in table
int	col			# column number (not pointer)
int	clen			# number of char to read or write
pointer tbcnum()
int	read()
int	tbcigi(), tbpsta()

begin
	allrows = tbpsta (itp, TBL_ALLROWS)
	ncols = tbpsta (itp, TBL_NCOLS)

	do col = 1, ncols {

	    cptr = tbcnum (itp, col)
	    dtype = tbcigi (cptr, TBL_COL_DATATYPE)

	    switch (dtype) {
	    case TY_REAL:

		do row = 1, allrows {
		    if (read (ifd, inbuf, SZ_REAL) < SZ_REAL)
			call error (1,
				"tsv_data_c:  error reading real element")
		    if (inbufi[1] == TVS_SUN_INDEFR)		# ***
			inbufr = INDEFR
		    else
			call ieeupkr (inbufr)			# ***
		    call write (ofd, inbuf, SZ_REAL)
		}

	    case TY_DOUBLE:
		do row = 1, allrows {
		    if (read (ifd, inbuf, SZ_DOUBLE) < SZ_DOUBLE)
			call error (1,
				"tsv_data_c:  error reading double element")
		    if ((inbufi[1] == TVS_SUN_INDEFD1) &&
			(inbufi[2] == TVS_SUN_INDEFD2))		# ***
			inbufd = INDEFD
		    else
			call ieeupkd (inbufd)
		    call write (ofd, inbuf, SZ_DOUBLE)
		}

	    case TY_INT:
		do row = 1, allrows {
		    if (read (ifd, inbuf, SZ_INT) < SZ_INT)
			call error (1,
				"tsv_data_c:  error reading integer element")
		    call bswap4 (inbufi, 1, inbufi, 1, SZ_INT*SZB_CHAR)
		    call write (ofd, inbuf, SZ_INT)
		}

	    case TY_SHORT:
		do row = 1, allrows {
		    if (read (ifd, inbuf, SZ_SHORT) < SZ_SHORT)
			call error (1,
				"tsv_data_c:  error reading short element")
		    call bswap2 (inbufs, 1, inbufs, 1, SZ_SHORT*SZB_CHAR)
		    call write (ofd, inbuf, SZ_SHORT)
		}

	    case TY_BOOL:
		do row = 1, allrows {
		    # read into integer buffer
		    if (read (ifd, inbuf, SZ_BOOL) < SZ_BOOL)
			call error (1,
				"tsv_data_c:  error reading boolean element")
		    call bswap4 (inbufi, 1, inbufi, 1, SZ_BOOL*SZB_CHAR)
		    call write (ofd, inbuf, SZ_BOOL)
		}

	    default:
		if (dtype > 0)
		    call error (1, "invalid data type in input table")
		do row = 1, allrows {
		    clen = COL_LEN(cptr)
		    if (read (ifd, cbuf, clen) < clen)
			call error (1,
				"tsv_data_c:  error reading text element")
		    call write (ofd, cbuf, clen)
		}
	    }
	}
end
