include	<ctotok.h>
include	<error.h>
include	<evexpr.h>
include	<imhdr.h>
include <tbset.h>
include	<pkg/dttext.h>
include "bu_obj.h"

# Constants
define	MAX_DIM			7
define	MAX_RECURSE		20

# Memory management.
define	Xp			Memr[xp+$1-1]
define	Yp			Memr[yp+$1-1]

#---------------------------------------------------------------------------
pointer	procedure bu_alloc (def_file, items)

char	def_file[ARB]		# I:  Name of the UDL definition table.
char	items[ARB]		# I:  List of items that will be extracted.

# Declarations
pointer	dtmap()			# Open a text database.
char	err1[SZ_LINE]		# Error message.
char	err2[SZ_LINE]		# Error message.
int	errget()		# Retrieve current error message.
int	i			# Generic.
pointer	o			# The centerflux results object.

errchk	malloc 

begin
	# Allocate the memory structure.
	call malloc (o, BU_SIZE, TY_STRUCT)

	# Open the definition database.
	iferr (BU_DB(o) = dtmap (def_file, READ_ONLY)) {
	    i = errget (err1, SZ_LINE)
	    call sprintf (err2, SZ_LINE, "could not read definition file '%s':%s")
	    call pargstr (def_file)
	    call pargstr (err1)
	    call error (1, err2)
	}

	# Initialize item list.
	call bu_set_items (o, items)

	# Initialize the value arrays.
	call malloc (BU_VALUE_ARRAY_PTR(o), BU_N_ITEMS(o), TY_POINTER)
	BU_MAX_VALUES(o) = 0
	BU_N_VALUES(o) = 0
	call malloc (BU_IS_NULL_PTR(o), BU_N_ITEMS(o), TY_INT)
	call amovki (NO, BU_IS_NULL(o,1), BU_N_ITEMS(o))

	# Initialize the file name array.
	BU_FNAME_PTR(o) = NULL

	# Loop through initializations.
	do i = 1, BU_N_ITEMS(o) {

	    # No arrays have been allocated.
	    BU_VALUE_ARRAY(o,i) = NULL
	}

        # That's all folks.
        return (o)
end
#---------------------------------------------------------------------------
# end of bu_alloc
#---------------------------------------------------------------------------
procedure bu_free (o)

#
# 25 June 1998  M. De La Pena: corrected an error in use of BU_N_VALUES.
# do j loop used BU_N_VALUES -> now BU_N_VALUES(o)

pointer	o			# IO: SP sp object, NULL on return.

int	i, j			# Generic.

errchk	 dtunmap, mfree

begin
	# Close the definition database.
	call dtunmap (BU_DB(o))
	
	# Handle loop free.
	if (BU_MAX_VALUES(o) > 0)
	    do i = 1, BU_N_ITEMS(o) {
		do j = 1, BU_N_VALUES(o)
		    call mfree (BU_VALUE(o,i,j), TY_STRUCT)
		call mfree (BU_VALUE_ARRAY(o, i), TY_POINTER)
	    }

	# Free the file name array.
	if (BU_FNAME_PTR(o) != NULL)
	    call mfree (BU_FNAME_PTR(o), TY_CHAR)
	
	# Handle individual frees.
	call mfree (BU_LIST_PTR(o), TY_CHAR)
	call mfree (BU_VALUE_ARRAY_PTR(o), TY_POINTER)
	call mfree (o, TY_STRUCT)
end
#---------------------------------------------------------------------------
# end of bu_free
#---------------------------------------------------------------------------
procedure bu_grow (o, size)

pointer	o			# I:  Object.
int	size			# I:  Size to grow to.

int	i			# Generic.
bool	new_max			# True to reallocate the arrays.

errchk	realloc

begin
	# See if growth is needed.
	new_max = false
	while (BU_MAX_VALUES(o) <= size) {
	    new_max = true
	    BU_MAX_VALUES(o) = BU_MAX_VALUES(o) + BU_GROW
	}

	# If the max has changed, reallocate the arrays.
	if (new_max) {
	    do i = 1, BU_N_ITEMS(o)
		call realloc (BU_VALUE_ARRAY(o,i), BU_MAX_VALUES(o),
			      TY_POINTER)

	    # Reallocate the file name array.
	    call realloc (BU_FNAME_PTR(o),
			  BU_MAX_VALUES(o)*(SZ_PATHNAME+1),
			  TY_CHAR)
	}

	# Reset the size.
	BU_N_VALUES(o) = size
end
#---------------------------------------------------------------------------
# End of bu_grow
#---------------------------------------------------------------------------
procedure bu_dump (o)

pointer	o			# I:  SP object.

begin
	call printf ("Number of names = %d.\n")
	call pargi (BU_N_ITEMS(o))
	call printf ("Number of values = %d, max values = %d.\n")
	call pargi (BU_N_VALUES(o))
	call pargi (BU_MAX_VALUES(o))
end
#---------------------------------------------------------------------------
# End of bu_dump
#---------------------------------------------------------------------------
procedure bu_read_image (o, name, fmt)

pointer	o			# I:  Object.
char	name[ARB]		# I:  Name of the image.
bool	fmt			# I:  Format the values?

# Declarations
pointer	data			# Data array.
pointer	bu_extract()		# Get value.
pointer	bu_format()		# Format a value.
char	err[SZ_LINE]		# Error string.
int	errget()		# Retrieve the current error message.
int	i, j, k			# Generic.
pointer	im			# Image descriptor.
pointer	immap()			# Open an image.
char	item_name[SZ_COLNAME]	# Name of the item.
pointer	imgl1i()		# Get image data.
pointer	v			# Temporary value holder.
int	word_find()		# Extract word from list.

errchk	bu_grow, imgl1i, immap

begin
	# Open the image.
	im = immap (name, READ_ONLY, NULL)
	
	# Get the data array.
	data = imgl1i (im)

	# Grow the associated arrays.
	call bu_grow (o, BU_N_VALUES(o)+1)

	# Set the file name.
	call strcpy (name, BU_FNAME(o, BU_N_VALUES(o)), SZ_PATHNAME)
	
	# For each item, find it in the data and fill the proper array.
	do i = 1, BU_N_ITEMS(o)
	    if (BU_IS_NULL(o,i) != YES) {
		j = word_find (i, BU_LIST(o), item_name, SZ_COLNAME)
		iferr (BU_VALUE(o,i,BU_N_VALUES(o)) =
		       bu_extract (o, Memi[data], IM_LEN(im,1), item_name)) {
		    k = errget (err, SZ_LINE)
		    call eprintf ("%s\n")
		    call pargstr (err)
		    call eprintf ("Warning: Could not get value for item '%s'.\n    Item will be ignored.\n")
		    call pargstr (item_name)
		    BU_IS_NULL(o,i) = YES
		}

		# If formatting, do it now
		if (fmt && BU_IS_NULL(o,i) != YES) {
		    iferr (v = bu_format (o, item_name,
					  Memi[data], IM_LEN(im,1),
					  BU_VALUE(o,i,BU_N_VALUES(o)))) {
			k = errget (err, SZ_LINE)
			call eprintf ("%s\n")
			call pargstr (err)
			call eprintf ("Warning: Could not format value for item '%s'.\n")
			call pargstr (item_name)
		    } else {
			call mfree (BU_VALUE(o,i,BU_N_VALUES(o)), TY_STRUCT)
			BU_VALUE(o,i,BU_N_VALUES(o)) = v
		    }
		}
	    }

	# That's all folks.
	call imunmap (im)
end
#---------------------------------------------------------------------------
# End of bu_read_image
#---------------------------------------------------------------------------
pointer procedure bu_extract (o, data, n, item)

pointer	o			# I:  The object.
int	data[n]			# I:  Data array.
int	n			# I:  Size of data array.
char 	item[ARB]		# I:  Item to retrieve.

# Declarations.
include	"bu_obj.com"

pointer	bu_expr()		# Get expression from database.
extern	bu_functions()		# Special functions for expression.
extern	bu_get_op()		# Get operator for expression.
pointer	evexpr()		# Evaluate arbitrary expression.
pointer	expr			# Expression to evaluate.
pointer locpr()			# Get pointer to a function.
int	locva()			# Get pointer to a variable.
pointer	result			# Result operand of expression.

errchk	bu_expr, evexpr, locpr, locva

begin
	# Get the extraction expression.
	expr = bu_expr (BU_DB(o), item, BU_EXTRACT)

	# Evaluate the expression
	data_ptr = (locva (data) - locva (Memc))/ SZ_INT + 1
	n_pts = n
	f_val = NULL
	result = evexpr (Memc[expr], locpr (bu_get_op), locpr (bu_functions))

	# That's all folks
	call mfree (expr, TY_CHAR)
	return (result)
end
#---------------------------------------------------------------------------
# End of bu_extract
#---------------------------------------------------------------------------
procedure bu_write_table (o, name, mode, template)

pointer	o			# I:  Object to save.
char	name[ARB]		# I:  Name of table to write.
int	mode			# I:  Mode to open table with.
pointer	template		# I:  Template table descriptor.

# Declarations.
pointer	col			# Column descriptor.
int	dtlocate()		# Find a named record.
int	i, j			# Generic.
char	item_name[SZ_COLNAME]	# Item name.
pointer	out			# Output table descriptor.
int	size			# Maximum size of a string result.
int	strlen()		# Get length of string.
pointer	tbtopn()		# Open a table.
char	units[SZ_COLUNITS]	# Units the column should be representing.
int	word_find()		# Find a word in a list.

errchk	bu_format, mfree, tbcdef, tbcptb, tbcpti, tbcptr,
errchk	tbcptt, tbpset, tbtclo, tbtcre, tbtopn

begin
	# Open the table.
	out = tbtopn (name, mode, template)
	call tbpset (out, TBL_ALLROWS, BU_N_VALUES(o))
	call tbpset (out, TBL_MAXCOLS, BU_N_ITEMS(o)+1)
	call tbpset (out, TBL_WHTYPE, TBL_TYPE_S_COL)
	call tbtcre (out)

	# Write the file names.
	call tbcdef (out, col, "file", "", "", -1*SZ_PATHNAME, 1, 1)
	call tbcptt (out, col, BU_FNAME(o,1), SZ_PATHNAME, 1,
		     BU_N_VALUES(o))

	# Write the values out.
	do i = 1, BU_N_ITEMS(o) {

	    # See if there are any values for this particular item.
	    if (BU_IS_NULL(o,i) != YES) {

		#  Get the record name.
		j = word_find (i, BU_LIST(o), item_name, SZ_COLNAME)
		
		# Get units if possible.
		iferr (call dtgstr (BU_DB(o),
				    dtlocate (BU_DB(o), item_name),
				    BU_UNITS, units, SZ_COLUNITS))
		    call strcpy ("", units, SZ_COLUNITS)

		# If the type of the current column is a character, find
		# the largest string size in the values.
		size = 0
		if (O_TYPE(BU_VALUE(o,i,1)) == TY_CHAR)
		    do j = 1, BU_N_VALUES(o)
			size = max(size, strlen(O_VALC(BU_VALUE(o,i,j))))
		    
		# Create the column
		if (O_TYPE(BU_VALUE(o,i,1)) == TY_CHAR)
		    call tbcdef (out, col, item_name, units, "",
				 -1*size, 1, 1)
		else
		    call tbcdef (out, col, item_name, units, "",
				 O_TYPE(BU_VALUE(o,i,1)), 1, 1)

		# Write the data out.
		do j = 1, BU_N_VALUES(o)
		    switch (O_TYPE(BU_VALUE(o,i,j))) {
		    case TY_REAL:
			call tbeptr (out, col, j, O_VALR(BU_VALUE(o,i,j)))
		    case TY_BOOL:
			call tbeptb (out, col, j, O_VALB(BU_VALUE(o,i,j)))
		    case TY_CHAR:
			call tbeptt (out, col, j, O_VALC(BU_VALUE(o,i,j)))
		    default:
			call tbepti (out, col, j, O_VALI(BU_VALUE(o,i,j)))
		    }
	    }
	}

	# That's all folks.
	call tbtclo (out)
end
#---------------------------------------------------------------------------
# End of bu_write_table
#---------------------------------------------------------------------------
procedure bu_get_op (operand, op)

char	operand[ARB]		# I:  Name of the operand to get value for.
pointer	op			# I:  Operand object to contain the value.

# Declarations
include "bu_obj.com"

int	bs			# Where dimension bracket starts.
int	dim[MAX_DIM]		# Array indicies.
int	ndim			# Dimensionality of variable.
int	stridx()		# Get index of character in string.
int	strdic()		# Get dictionary index.
int	strlen()		# Get string length.
char 	sx[SZ_LINE]		# Generic string.
char 	var[SZ_LINE]		# Pointer to variable name.

errchk	xev_initop

begin
	# Get the array element
	bs = stridx ("[", operand)
	if (bs > 0) {
	    call strcpy (operand, var, bs-1)
	    call bu_get_dim (operand[bs+1], dim, ndim)
	} else {
	    call strcpy (operand, var, strlen (operand))
	    ndim = 0
	}

	# Now decide which variable is being retreived.
	switch (strdic (var, sx, SZ_LINE, BU_VARIABLES)) {
	case BU_VAR_D:
	    if (data_ptr == NULL)
		call error (1, "d array has not been defined")
	    if (ndim > 1)
		call error (1, "d variable is only one dimensional")
	    if (ndim == 0)
		dim[1] = 1
	    if (dim[1] > n_pts)
		call error (1, "Index to variable d out of range")
	    call xev_initop (op, 0, TY_INT)
	    O_VALI(op) = Memi[data_ptr+dim[1]-1]

	case BU_VAR_V:
	    if (f_val == NULL)
		call error (1, "v variable has not been define")
	    if (ndim > 0)
		call error (1, "v variable is a scalar")
            call bu_copy_op (f_val, op)

	default:
	    call sprintf (sx, SZ_LINE, "no such item '%s'")
	    call pargstr (Var)
	    call error (1, sx)
	}
end
#---------------------------------------------------------------------------
# End of bu_get_op
#---------------------------------------------------------------------------
procedure bu_functions (name, args, nargs, op)

char	name[ARB]		# I:  Name of function to execute
pointer	args[nargs]		# I:  Arguments to the function.
int	nargs			# I:  Number of arguments.
pointer	op			# I:  Operand to contain the result

# Declarations
include "bu_obj.com"

real	bu_lin()		# Linear interpolate.
int	i			# Generic.
real	rx			# Generic.
int	strdic()		# Get dictionary index.
int	strlen()		# Length of string.
char 	sx[SZ_LINE]		# Generic strings.
int	word_find()		# Find a word in a list.
int shifti(), andi()
errchk	xev_initop, bu_lin

begin
	# Execute the appropriate function.
	switch (strdic (name, sx, SZ_LINE, BU_FUNCTIONS)) {
	case BU_FUNC_AND:
	    if (nargs != 2)
		call error (1, "function 'and' requires 2 arguments")
	    if (O_TYPE(args[1]) != TY_INT ||
		O_TYPE(args[2]) != TY_INT)
		call error (1, "function 'and' requires integer arguments")

	    call xev_initop (op, 0, TY_INT)
	    O_VALI(op) = andi (O_VALI(args[1]), O_VALI(args[2]))
	    
	case BU_FUNC_SHIFT:
	    if (nargs != 2)
		call error (1, "function 'shift' requires 2 arguments")
	    if (O_TYPE(args[1]) != TY_INT ||
		O_TYPE(args[2]) != TY_INT)
		call error (1, "function 'shift' requires integer arguments")

	    call xev_initop (op, 0, TY_INT)
	    O_VALI(op) = shifti (O_VALI(args[1]), O_VALI(args[2]))

	case BU_FUNC_WRDFND:
	    if (nargs != 2)
		call error (1, "'word_find' function takes 2 arguments")
	    if (O_TYPE(args[1]) != TY_INT)
		call error (1, "first arg to 'switch' must be an integer")

	    # Find the word.
	    if (word_find (O_VALI(args[1])+1, O_VALC(args[2]), sx, SZ_COMMAND) >
		0) {
		i = strlen (sx)
		call xev_initop (op, i, TY_CHAR)
		call strcpy (sx, O_VALC(op), i)
	    } else {
		call xev_initop (op, 1, TY_CHAR)
		call strcpy ("", O_VALC(op), 1)
	    }

	case BU_FUNC_LIN:
	    if (nargs != 2)
		call error (1, "'lin' function takes 2 arguments")
	    if (O_TYPE(args[1]) != TY_INT && O_TYPE(args[1]) !=	TY_REAL)
		call error (1, "first arg to 'lin' must be numeric")
	    if (O_TYPE(args[2]) != TY_CHAR)
		call error (1, "second arg to 'lin' must be a string")

	    if (O_TYPE(args[1]) == TY_INT)
		rx = O_VALI(args[1])
	    else
		rx = O_VALR(args[1])

	    call xev_initop (op, 0, TY_REAL)
	    O_VALR(op) = bu_lin (rx, O_VALC(args[2]))

	default:
	    call sprintf (sx, SZ_LINE, "unknown function '%s'")
	    call pargstr (name)
	    call error (1, sx)
	}
end
#---------------------------------------------------------------------------
# End of bu_functions
#---------------------------------------------------------------------------
procedure bu_get_dim (strval, dim, ndim)

char	strval[ARB]		# I:  The string to decode.
int	dim[MAX_DIM]		# O:  Array indicies.
int	ndim			# O:  Acutall dimensionality

# Declarations
int	ctoi()			# Character to integer.
int	i, j			# Generic.
char	sx[SZ_LINE]		# Generic string.
int	word_count()		# Get number of words in list.
int	word_find()		# Get i'th word from list.

begin
	ndim = word_count (strval)
	do i = 1, ndim {
	    if (word_find (i, strval, sx, SZ_LINE) > 0) {
		j = 1
		if (ctoi (sx, j, dim[i]) < 0)
		    dim[i] = 0
	    } else {
		ndim = i - 1
		break
	    }
	}
end
#---------------------------------------------------------------------------
# End of bu_get_dim
#---------------------------------------------------------------------------
pointer procedure bu_format (o, item, data, n, value)

pointer	o			# I:  The BU object.
char 	item[ARB]		# I:  The item to format.
int	data[n]			# I:  Data array.
int	n			# I:  Size of data array.
pointer	value			# I: The value to be formatted.

# Declarations
include "bu_obj.com"

pointer	bu_expr()		# Get expression from database.
extern	bu_functions()		# Functions for expressions.
extern	bu_get_op()		# Get operand for expressions.
bool	bu_indef()		# Is value indef?
pointer	evexpr()		# Evaluate a generic expression.
pointer	expr			# Expression to evaluate.
pointer	locpr()			# Location of functions.
int	locva()			# Location of variables.
pointer	result			# Result.

errchk	bu_expr, evexpr, locpr, locva, malloc

begin
	# Get the formatting expression
	expr = bu_expr (BU_DB(o), item, BU_FORMAT)

	# If the value is not defined, then just return that value.
	if (bu_indef (value)) {
	    call malloc (result, LEN_OPERAND, TY_STRUCT)
	    call bu_copy_op (value, result)

	# Else, evaluate the format expression.
	} else {
	    data_ptr = (locva (data) - locva (Memc))/ SZ_INT + 1
	    n_pts = n
	    f_val = value
	    result = evexpr (Memc[expr], locpr (bu_get_op),
			     locpr (bu_functions))
	}
	
	# That's all folks
	call  mfree (expr, TY_CHAR)
	return (result)
end
#---------------------------------------------------------------------------
# End of bu_format
#---------------------------------------------------------------------------
procedure bu_copy_op (op1, op2)

pointer	op1, op2

begin
	call amovi (Memi[op1], Memi[op2], LEN_OPERAND)
end
#---------------------------------------------------------------------------
# End of bu_copy_op
#---------------------------------------------------------------------------
procedure amovb (a, b, n)

bool	a[n], b[n]
int	i, n

begin
	do i = 1, n
	    b[i] = a[i]
end
#---------------------------------------------------------------------------
# End of amovb
#---------------------------------------------------------------------------
procedure bu_set_items (o, pat)

pointer	o			# I:  BU object
char	pat[ARB]		# I:  Pattern to find items for.

# Declarations
char	apat[SZ_COMMAND]	# A single pattern to find.
int	i, ic, j		# Generic.
char	patbuf[SZ_COMMAND]	# A compiled pattern.
int	patmake()		# Complie a pattern.
int	patmatch()		# Find a pattern.
pointer	sb			# String buffer
pointer	sb_open()		# Open a string buffer.
pointer	sb_string()		# Get the string in the buffer.
int	word_fetch()		# Get next word from pattern.

begin
	sb = sb_open()
	BU_N_ITEMS(o) = 0
	ic = 1

	# Find all the records that match the list of patterns.
	while (word_fetch (pat, ic, apat, SZ_COMMAND) > 0) {
	    do i = 1, DT_NRECS(BU_DB(o)) {
		j = patmake (apat, patbuf,  SZ_COMMAND)
		if (patmatch (DT_NAME(BU_DB(o), i), patbuf) > 0) {
		    BU_N_ITEMS(o) = BU_N_ITEMS(o) + 1
		    if (BU_N_ITEMS(o) > 1)
			call sb_cat (sb, ",")
		    call sb_cat (sb, DT_NAME(BU_DB(o),i))
		}
	    }
	}

	if (BU_N_ITEMS(o) == 0)
	    call error (1, "pattern string does not match any items in the database")

	BU_LIST_PTR(o) = sb_string (sb)
	call sb_close (sb)
end
#---------------------------------------------------------------------------
# End of bu_set_items
#---------------------------------------------------------------------------
real procedure bu_lin (x, pairs)

real	x			# I:  X value to interpolate for.
char	pairs[ARB]		# I:  String of x/y pairs defining function.

# Declarations
int	ctor()			# String to real.
int	i			# Generic.
int	n			# Number of pairs.
real	rx			# Generic.
pointer	sp			# Stack pointer.
pointer	xp			# X value of pairs.
real	y			# Interpolated/extrapolated Y value.
pointer	yp			# Y value of pairs.

begin
	call smark (sp)
	
	# Determine how many pairs are in the string.
	i = 1
	n = 0
	while (ctor (pairs, i, rx) > 0)
	    n = n + 1

	# If two pairs have not been defined, abort.
	if (n < 4)
	    call error (1, "lin: need to define at least two pairs of values")

	# Read in the values.
	call salloc (xp, n, TY_REAL)
	call salloc (yp, n, TY_REAL)
	i = 1
	n = 0
	while (ctor (pairs, i, Xp(n+1)) > 0) {
	    if (ctor (pairs, i, Yp(n+1)) <= 0)
		break
	    n = n + 1
	}

	# Find which pair the X value falls in.
	if (x < Xp(1))
	    i = 1
	else if ( x >= Xp(n))
	    i = n
	else {
	    i = 1
	    while (x >= Xp(i+1))
		i = i + 1
	}

	# Interpolate/extrapolate
	y = Yp(i) + ((x - Xp(i)) * (Yp(i+1) - Yp(i)) / (Xp(i+1) - Xp(i)))

	# That's all folks.
	call sfree (sp)
	return (y)
end
#---------------------------------------------------------------------------
# End of bu_lin
#---------------------------------------------------------------------------
pointer procedure bu_expr (db, item, field)

pointer	db			# I:  Text database object.
char	item[ARB]		# I:  Item to get expression for.
char	field[ARB]		# I:  Field to get expression from.

# Declarations
bool	again			# Reparse the expression again.
int	ctotok()		# Get next token.
pointer	e			# Final expression.
char	expr[BU_SZ_EXPR]	# Expression to evaluate.
char	expr2[BU_SZ_EXPR]	# Subexpression to imbedd in expression.
int	ip			# Character index.
int	recurse			# Number of recursions which have occured.
pointer	sb			# String buffer.
pointer	sb_open()		# Create a string buffer.
pointer	sb_string()		# Convert string buffer to string.
int	t			# Token type.
char	token[SZ_LINE]		# Token.

errchk	bu_field
errchk	sb_cat, sb_open, sb_string

begin
	# Find the basic expression
	call bu_field (db, item, field, expr, BU_SZ_EXPR)

	# Now parse through the string
	again = true
	recurse = 0
	call malloc (e, BU_SZ_EXPR, TY_CHAR)
	call strcpy (expr, Memc[e], BU_SZ_EXPR)
	
	while (again) {
	    again = false
	    ip = 1
	    sb = sb_open()
	    while (true) {
		t = ctotok (Memc[e], ip, token, SZ_LINE)
		switch (t) {

		case TOK_IDENTIFIER:
		    
		    # If an identifier, see if it is an item in the database.
		    # If so, get its "extract" expression and insert it into
		    # the string.  This is non-recursive.
		    iferr (call bu_field (db, token, BU_EXTRACT,
					  expr2, BU_SZ_EXPR)) {
			call sb_cat (sb, token)
		    } else {
			if (!again) {
			    if (recurse > MAX_RECURSE)
				call error (1, "infinite loop in expression")
			    recurse = recurse + 1
			    again = true
			}
			call sb_cat (sb, "(")
			call sb_cat (sb, expr2)
			call sb_cat (sb, ")")
		    }

		case TOK_STRING:
		    
		    # If it is a string, make sure the quotes are still
		    # there when placed back in the expression.
		    call sb_cat (sb, "\"")
		    call sb_cat (sb, token)
		    call sb_cat (sb, "\"")

		case TOK_EOS:

		    # If end of string, then break.
		    break

		default:
		    
		    # Just concatenate the extracted token.
		    call sb_cat (sb, token)
		}
	    }

	    # Replace expression with new expression.
	    call mfree (e, TY_CHAR)
	    e = sb_string (sb)
	    call sb_close (sb)
	}
	
	# That's all folks.
	return (e)
end
#---------------------------------------------------------------------------
# End of bu_expr
#---------------------------------------------------------------------------
procedure bu_field (db, rec, field, value, size)

pointer	db			# I:  Text database to search.
char	rec[ARB]		# I:  Record to retrieve.
char	field[ARB]		# I:  Field of record to retrieve.
char	value[size]		# O:  Value of field in record.
int	size			# I:  Maximum length of value string.

# Declarations
int	dtlocate()		# Find record in database.
char	err1[SZ_LINE]		# Error line.
char	err2[SZ_LINE]		# Error line.
int	errget()		# Retrieve the current error message.
int	i			# Generic.

begin
	iferr (i = dtlocate (db, rec)) {
	    i = errget (err1, SZ_LINE)
	    call sprintf (err2, SZ_LINE, "could not find record '%s':%s")
	    call pargstr (rec)
	    call pargstr (err1)
	    call error (1, err2)
	}
	    
	iferr (call dtgstr (db, i, field, value, size)) {
	    i = errget (err1, SZ_LINE)
	    call sprintf (err2, SZ_LINE, "could not find field '%s' for record '%s':%s")
	    call pargstr (field)
	    call pargstr (rec)
	    call pargstr (err1)
	    call error (1, err2)
	}
end
#---------------------------------------------------------------------------
# End of bu_field
#---------------------------------------------------------------------------
bool procedure bu_indef (v)

pointer	v			# I:  EVEXPR operand

# Declarations
bool	r			# Result

begin
	switch (O_TYPE(v)) {
	case TY_INT:
	    r = IS_INDEFI(O_VALI(v))

	case TY_REAL:
	    r = IS_INDEFR(O_VALR(v))

	default:
	    r = false
	}

	return (r)
end
#---------------------------------------------------------------------------
# End of bu_indef
#---------------------------------------------------------------------------
