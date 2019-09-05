include <imhdr.h>
include <mach.h>	# for definition of SZB_CHAR

# rbinary -- copy a binary file into an image
# This task creates an output image, reads from a binary file, and writes
# to the output image.  A specified number of bytes may be skipped at the
# beginning of the input file.  The allowed data types are real, integer,
# and short integer.
#
# Phil Hodge,  3-Jul-1989  Task created.
# Phil Hodge, 13-Feb-1990  Rename from rdbin to rbinary.
# Phil Hodge,  3-Mar-1993  Change error messages for I/O error.

procedure rbinary()

pointer sp
pointer fname		# scratch for name of input binary file
pointer imname		# scratch for name of output image
pointer buf		# buffer for I/O
pointer im, x		# pointer to image descriptor, output data
char	dimpar[15]	# buffer for name of dimen parameters dimen[001], etc
char	chdtype[15]	# data type as a char string (r, i)
int	fd		# fd for input file
int	naxis
int	dimen[IM_MAXDIM]
int	dtype		# data type
int	nbyte_skip	# number of bytes to skip at beginning of file
int	nskip		# number of char to skip at beginning of file
int	nchar		# number of char read from file
int	nlines		# number of lines in the image (=dimen[2]*dimen[3]* ...)
int	k		# loop index
int	v[IM_MAXDIM]	# for sequentially writing to output image
pointer immap()
int	clgeti(), open(), read(), impnlr(), impnli(), impnls()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	call clgstr ("input", Memc[fname], SZ_FNAME)
	call clgstr ("output", Memc[imname], SZ_FNAME)

	naxis = clgeti ("naxis")
	do k = 1, naxis {
	    call sprintf (dimpar, 15, "dimen[%03d]")
		call pargi (k)
	    dimen[k] = clgeti (dimpar)
	    if (dimen[k] < 1)
		call error (1, "zero or negative dimension is invalid")
	}

	call clgstr ("datatype", chdtype, 15)
	if (chdtype[1] == 'r')
	    dtype = TY_REAL
	else if (chdtype[1] == 'i')
	    dtype = TY_INT
	else if (chdtype[1] == 's')
	    dtype = TY_SHORT

	nbyte_skip = clgeti ("offset")
	nskip = nbyte_skip / SZB_CHAR
	if (nbyte_skip != nskip * SZB_CHAR) {
	    call eprintf ("The offset must be divisible by %d.\n")
		call pargi (SZB_CHAR)
	    call error (1, "")
	}

	# Open the input file.
	fd = open (Memc[fname], READ_ONLY, BINARY_FILE)

	# Create the output image, and specify the data type and dimensions.
	im = immap (Memc[imname], NEW_FILE, NULL)
	IM_PIXTYPE(im) = dtype
	IM_NDIM(im) = naxis
	do k = 1, naxis
	    IM_LEN(im,k) = dimen[k]

	nlines = 1
	do k = 2, naxis
	    nlines = nlines * dimen[k]

	# Find out how large a buffer we need, and allocate it.
	if (dtype == TY_REAL)
	    nchar = dimen[1] * SZ_REAL
	else if (dtype == TY_INT)
	    nchar = dimen[1] * SZ_INT
	else
	    nchar = dimen[1] * SZ_SHORT

	call salloc (buf, nchar, TY_CHAR)

	# Skip the specified number of bytes at the beginning of the file.
	if (nskip > 0)
	    call seek (fd, nskip+1)

	# For each line of the image, read from the file, get a pointer to
	# the image, and copy to the image.

	do k = 1, IM_MAXDIM
	    v[k] = 1

	if (dtype == TY_REAL) {

	    do k = 1, nlines {
		if (read (fd, Memc[buf], nchar) < nchar) {
		    call close (fd)
		    call error (1, "input file is too short")
		}
		if (impnlr (im, x, v) < dimen[1]) {
		    call close (fd)
		    call error (1, "error writing to output image")
		}
		call amovr (Memc[buf], Memr[x], dimen[1])
	    }

	} else if (dtype == TY_INT) {

	    do k = 1, nlines {
		if (read (fd, Memc[buf], nchar) < nchar) {
		    call close (fd)
		    call error (1, "input file is too short")
		}
		if (impnli (im, x, v) < dimen[1]) {
		    call close (fd)
		    call error (1, "error writing to output image")
		}
		call amovi (Memc[buf], Memi[x], dimen[1])
	    }

	} else if (dtype == TY_SHORT) {

	    do k = 1, nlines {
		if (read (fd, Memc[buf], nchar) < nchar) {
		    call close (fd)
		    call error (1, "input file is too short")
		}
		if (impnls (im, x, v) < dimen[1]) {
		    call close (fd)
		    call error (1, "error writing to output image")
		}
		call amovs (Memc[buf], Mems[x], dimen[1])
	    }
	}

	# Close image and file.
	call imunmap (im)
	call close (fd)

	call sfree (sp)
end
