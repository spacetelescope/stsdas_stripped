.help upropen
.nf

The procedures in this file allow operations on header keywords
to be done transparently on images or tables. The method is to simply
check what kind of file we have before opening it and then use an if
statement to call the appropriate procedure. The file type 
determination is done by is_image(), which is in the selector 
sublibrary of tbtables. Since changes or additions to the table or 
image formats may break this procedure, it should be the first thing 
to check if things go wrong.

.endhelp --------------------------------------------------------------

include	<imhdr.h>

# Definition of the structure used to hold the file descriptor
# and its type

define	LEN_UPRSTRUCT	2

define	UPR_POINTER	Memi[$1]
define	UPR_ISIMAGE	Memi[$1+1]

#----------------------------------------------------------------------

# UPR_OPEN -- Open a file as either an image or table
#
# B.Simon	01-Feb-99	Original

pointer procedure upr_open (input)

char	input[ARB]	# i: file name
#--
int	type
pointer	sp, upr, errmsg

string	badtype  "Cannot determine file type (%s)"
string	badtable "Cannot open table (%s)"
string	badimage "Cannot open image (%s)"
string	badcode  "Don't understand code returned by is_image (%d)"

int	is_image()
pointer	tbtopn(), immap()

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	call malloc (upr, LEN_UPRSTRUCT, TY_INT)

	type = is_image (input)

	switch (type) {
	case ERR:
	    UPR_POINTER(upr) = NULL
	    call sprintf (Memc[errmsg], SZ_LINE, badtype)
	    call pargstr (input)
	    call error (1, Memc[errmsg])
	case NO:
	    iferr {
		UPR_POINTER(upr) = tbtopn (input, READ_WRITE, NULL)
	    } then {
		call sprintf (Memc[errmsg], SZ_LINE, badtable)
		call pargstr (input)
		call error (1, Memc[errmsg])
	    }
	case YES:
	    iferr {
		UPR_POINTER(upr) = immap (input, READ_WRITE, NULL)
	    } then {
		call sprintf (Memc[errmsg], SZ_LINE, badimage)
		call pargstr (input)
		call error (1, Memc[errmsg])
	    }
	default:
	    call sprintf (Memc[errmsg], SZ_LINE, badcode)
	    call pargi (type)
	    call error (1, Memc[errmsg])
	}

	UPR_ISIMAGE(upr) = type

	call sfree (sp)
	return (upr)
end

# UPR_CLOSE -- Close a file opened as an image or table

procedure upr_close (upr)

pointer	upr		# u: file descriptor
#--
errchk	tbtclo, imunmap

begin
	if (UPR_ISIMAGE(upr) == NO) {
	    call tbtclo (UPR_POINTER(upr))
	} else {
	    call imunmap (UPR_POINTER(upr))
	}

	call mfree (upr, TY_INT)
end

# UPR_GETKEY -- Read a header keyword

procedure upr_getkey (upr, keyword, value, maxch)

pointer	upr		# i: file descriptor
char	keyword[ARB]	# i: keyword name
char	value[ARB]	# o: keyword value
int	maxch		# i: max length of keyword
#--
errchk	tbhgtt, imgstr

begin
	if (UPR_ISIMAGE(upr) == NO) {
	    call tbhgtt (UPR_POINTER(upr), keyword, value, maxch)
	} else {
	    call imgstr (UPR_POINTER(upr), keyword, value, maxch)
	}
end

# UPR_PUTKEY -- Write a header keyword

procedure upr_putkey (upr, keyword, value)

pointer	upr		# i: file descriptor
char	keyword[ARB]	# i: keyword name
char	value[ARB]	# i: keyword value
#--
bool	bval
double	dval
int	ic, nc, dtype, ival

errchk	tbhptt, imastr

int	datatype(), ctoi(), ctod()

begin
	# Determine data type of value so we know how to write it

	ic = 1
	dtype = datatype (value)

	# Write the header keyword using the prcedure for 
	# the approriate file and data type

	if (UPR_ISIMAGE(upr) == NO) {
	    switch (dtype) {
	    case TY_BOOL:
		bval = (value[1] == 'T' || value[1] == 't')
		call tbhptb (UPR_POINTER(upr), keyword, bval)
	    case TY_CHAR:
		call tbhptt (UPR_POINTER(upr), keyword, value)
	    case TY_INT:
		nc = ctoi (value, ic, ival)
		call tbhpti (UPR_POINTER(upr), keyword, ival)
	    case TY_DOUBLE:
		nc = ctod (value, ic, dval)
		call tbhptd (UPR_POINTER(upr), keyword, dval)
	    }

	} else {
	    switch (dtype) {
	    case TY_BOOL:
		bval = (value[1] == 'T' || value[1] == 't')
		call imaddb  (UPR_POINTER(upr), keyword, bval)
	    case TY_CHAR:
		call imastr (UPR_POINTER(upr), keyword, value)
	    case TY_INT:
		nc = ctoi (value, ic, ival)
		call imaddi  (UPR_POINTER(upr), keyword, ival)
	    case TY_DOUBLE:
		nc = ctod (value, ic, dval)
		call imaddd  (UPR_POINTER(upr), keyword, dval)
	    }
	}
end

# UPR_DELKEY -- Delete a header keyword

procedure upr_delkey (upr, keyword)

pointer	upr		# i: file descriptor
char	keyword[ARB]	# i: keyword name
#--
int	keynum
errchk	tbhfkw, tbhdel, imdelf

begin
	if (UPR_ISIMAGE(upr) == NO) {
	    call tbhfkw (UPR_POINTER(upr), keyword, keynum)
	    call tbhdel (UPR_POINTER(upr), keynum)
	} else {
	    call imdelf (UPR_POINTER(upr), keyword)
	}
end

# UPR_GETNAME -- Get the file name of the file being modified

procedure upr_getname (upr, fname, maxch)

pointer	upr		# i: file descriptor
char	fname[ARB]	# o: file name
int	maxch		# i: max length of file name
#--

begin
	if (UPR_ISIMAGE(upr) == NO) {
	    call tbtnam (UPR_POINTER(upr), fname, maxch)
	} else {
	    call strcpy (IM_HDRFILE(UPR_POINTER(upr)), fname, maxch)
	}
end
