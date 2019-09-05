include	<tbset.h>

define	PTR_FILE	Memi[$1]
define	ASCII_FILE	Memi[$1+1]

define	FIELDCHAR	':'
define	NOT_DELIM	(($1) > ' ' && ($1) != ',')

# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# FILE -- Routines which transparently manipulate list files or tables
#
# B.Simon	31-May-1990	Original

# CLS_FILE -- Close the file and release structure

procedure cls_file (fi)

pointer	fi		# i: File structure
#--

begin
	if (ASCII_FILE(fi) == NO) {
	    call tbtclo (PTR_FILE(fi))
	} else {
	    call close (PTR_FILE(fi))
	}

	call mfree (fi, TY_STRUCT)
end

# NROW_FILE -- Get number of rows in file

int procedure nrow_file (fi)

pointer	fi		# i: File structure
#--
int	ic, junk, nrow
pointer	sp, value, line

int	tbpsta(), getlongline(), word_fetch()

begin
	if (ASCII_FILE(fi) == NO) {
	    nrow = tbpsta (PTR_FILE(fi), TBL_NROWS)

	} else {
	    call smark (sp)
	    call salloc (value, SZ_FNAME, TY_CHAR)
	    call salloc (line, SZ_COMMAND, TY_CHAR)

	    nrow = 0
	    call seek (PTR_FILE(fi), BOF)

	    # Read each logical line. Don't count blank lines

	    while (getlongline (PTR_FILE(fi), Memc[line], 
				SZ_COMMAND, junk) != EOF) {
		ic = 1
		if (word_fetch (Memc[line], ic, Memc[value], SZ_FNAME) > 0)
		    nrow = nrow + 1
	    }

	    call sfree (sp)
	}

	return (nrow)

end

# OPN_FILE -- Open file in read only mode and create associated structure

int procedure opn_file (fname)

char	fname[ARB]	# i: File name
#--
bool	istext
pointer	fi

int	access(), fstdfile(), btoi()
pointer	open(), tbtopn()

begin
	call malloc (fi, 2, TY_STRUCT)

	istext = access (fname, READ_ONLY, TEXT_FILE) == YES ||
		 fstdfile (fname, PTR_FILE(fi)) == YES
	ASCII_FILE(fi) = btoi (istext)

	if (ASCII_FILE(fi) == NO) {
	    iferr (PTR_FILE(fi) = tbtopn (fname, READ_ONLY, NULL))
		return (ERR)
	} else {
	    iferr (PTR_FILE(fi) = open (fname, READ_ONLY, TEXT_FILE))
		return (ERR)
	}

	return (fi)
end

# RDCOL_FILE -- Read a column from the file

int procedure rdcol_file (fi, colname, coltype, vector, maxvec)

pointer	fi		# i: File structure
char	colname[ARB]	# i: Column name or number
int	coltype		# i: Column type
pointer	vector		# o: Pointer to column values
int	maxvec		# i: Declared length of output vector
#--
int	type, len, ncol, irow, icol, jcol, ic, jc, junk
pointer	sp, name, value, line, col, nilvec, ch

string	yorn       "|yes|no|true|false|"
string	badcoltyp  "Illegal column type (%s)"

int	word_count(), word_fetch(), gstrcpy(), getlongline()
int	ctowrd(), ctoi(), ctor(), ctod(), strdic()

begin
	call smark (sp)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_COMMAND, TY_CHAR)

	# Break SDAS column type into type and length variables

	if (coltype >= 0) {
	    type = coltype
	    len = 1
	} else {
	    type = TY_CHAR
	    len =  - coltype
	}

	# Count the number of columns in the list of column names
	# Multiple columns are only legal if the type is character

	ncol = max (1, word_count (colname))
	if (ncol > 1 && type != TY_CHAR)
	    return (ERR)

	call salloc (col, ncol, TY_INT)

	if (ASCII_FILE(fi) == NO) {

	    # Use the existing routine to get a single column from
	    # an SDAS table

	    if (ncol == 1) {
		call malloc (nilvec, maxvec, TY_BOOL)
		call tbcfnd (PTR_FILE(fi), colname, Memi[col], 1)
		if (Memi[col] == NULL)
		    return (ERR)

	        switch (type) {
		case TY_BOOL:
		    call tbcgtb (PTR_FILE(fi), Memi[col], Memb[vector],
				 Memb[nilvec], 1, maxvec)
		case TY_CHAR:
		    call tbcgtt (PTR_FILE(fi), Memi[col], Memc[vector],
				 Memb[nilvec], len, 1, maxvec)
		case TY_SHORT, TY_INT, TY_LONG:
		    call tbcgti (PTR_FILE(fi), Memi[col], Memi[vector],
				 Memb[nilvec], 1, maxvec)
		case TY_REAL:
		    call tbcgtr (PTR_FILE(fi), Memi[col], Memr[vector],
				 Memb[nilvec], 1, maxvec)
		case TY_DOUBLE:
		    call tbcgtd (PTR_FILE(fi), Memi[col], Memd[vector],
				 Memb[nilvec], 1, maxvec)
		default:
		    call pixerror (badcoltyp, colname)
		}

		# Null values are not allowed in the column

		do irow = 1, maxvec {
		    if (Memb[nilvec+irow-1])
			return (ERR)
		}
		call mfree (nilvec, TY_BOOL)

	    } else {

		# Create a list of SDAS column pointers

		ic = 1
		icol = 1
		while (word_fetch (colname, ic, Memc[name], SZ_FNAME) > 0) {
		     call tbcfnd (PTR_FILE(fi), Memc[name], 
				  Memi[col+icol-1], 1)

		    if (Memi[col+icol-1] == NULL)
			return (ERR)

		    icol = icol + 1
		}

		# Fetch multiple columns from an SDAS table a row at a time
		# Concatenate each column into a single string

		do irow = 1, maxvec {
		    ch = vector + (len + 1) * (irow - 1)
		    do icol = 1, ncol {
			call tbegtt (PTR_FILE(fi), Memi[col+icol-1], irow,
				     Memc[value], len)

			# Check for null value

			if (Memc[value] == EOS)
			    return (ERR)

			ch = ch + gstrcpy (Memc[value], Memc[ch], len)
			Memc[ch] = FIELDCHAR
			ch = ch + 1
		    }
		    Memc[ch-1] = EOS
		}
	    }

	} else {

	    # Read columns from a text file
	    # Handle the single column case first

	    call seek (PTR_FILE(fi), BOF)
	    if (ncol == 1) {

		# Convert column name to number
		# Return if it is not a number

		ic = 1
		junk = ctoi (colname, ic, Memi[col])
		if (NOT_DELIM(colname[ic]) || Memi[col] <= 0)
		    return (ERR)

		# Read the logical line into a temporary variable

		irow = 0
		while (getlongline (PTR_FILE(fi), Memc[line], 
				    SZ_COMMAND, junk) != EOF) {
		    irow = irow + 1
		    if (irow > maxvec)
			break

		    # Extract the column we want

		    ic = 1
		    icol = 1
		    while (ctowrd(Memc[line], ic, Memc[value], SZ_FNAME) > 0 &&
			   icol < Memi[col]) {
			icol = icol + 1
		    }

		    # Convert to output type and copy to output vector

		    ic = 1
		    if (icol == Memi[col]) {
			switch (type) {
			case TY_BOOL:
			    call strlwr (Memc[value])
			    ic = strdic (Memc[value], Memc[value], 
					 SZ_FNAME, yorn)
			    Memb[vector+irow-1] = mod (ic, 2) == 1
			case TY_CHAR:
			    ch = vector + (len + 1) * (irow - 1)
			    call strcpy (Memc[value], Memc[ch], len)
			case TY_SHORT, TY_INT, TY_LONG:
			    junk = ctoi (Memc[value], ic, Memi[vector+irow-1])
			case TY_REAL:
			    junk = ctor (Memc[value], ic, Memr[vector+irow-1])
			case TY_DOUBLE:
			    junk = ctod (Memc[value], ic, Memd[vector+irow-1])
			default:
			    call pixerror (badcoltyp, colname)
			}

		    } else if (icol > 0) {
			return (ERR)
		    }
		}

	    } else {

		# Multiple character columns in a text file
		# First, get the column numbers

		ic = 1
		do icol = 1, ncol {
		    junk = word_fetch (colname, ic, Memc[name], SZ_FNAME)
		    jc = 1
		    junk = ctoi (Memc[name], jc, Memi[col+icol-1])
		    if (NOT_DELIM(Memc[name+jc-1]) || Memi[col+icol-1] <= 0)
			return (ERR)

		}

		# Read each logical line into a temporary variable

		irow = 0		
		while (getlongline (PTR_FILE(fi), Memc[line], 
				    SZ_COMMAND, junk) != EOF) {
		    irow = irow + 1
		    if (irow > maxvec)
			break

		    # Extract each column from the line

		    jcol = ncol + 1
		    ch = vector + (len + 1) * (irow - 1)
		    do icol = 1, ncol {

			# Start reading again from the beginning of the
			# line if the next column is less than the last

			if (jcol > icol) {
			    ic = 1
			    jcol = 1
			}

			# Get the column we want

			while (ctowrd(Memc[line], ic, 
			       Memc[value], SZ_FNAME) > 0 &&
			       jcol < Memi[col+icol-1]) {
				jcol = jcol + 1
		    	}

			# Concatenate all columns in the output string

			if (jcol == Memi[col+icol-1]) {
			    ch = ch + gstrcpy (Memc[value], Memc[ch], len)
			    Memc[ch] = FIELDCHAR
			    ch = ch + 1

			} else if (icol > 0) {
			    return (ERR)
			}
		    }
		    Memc[ch-1] = EOS
		}
	    }
	}

	call sfree (sp)
	return (OK)

end

# UNIT_FILE -- Retrieve units for a column

int procedure unit_file (fi, colname, units, maxch)

pointer	fi		# i: File structure
char	colname[ARB]	# i: Column name
char	units[ARB]	# o: Units string
int	maxch		# i: Declared length of units string
#--
pointer	col

begin

	if (ASCII_FILE(fi) == NO) {
	    call tbcfnd (PTR_FILE(fi), colname, col, 1)
	    if (col == NULL)
		return (ERR)

	    call tbcigt (col, TBL_COL_UNITS, units, maxch)	    
	} else {
	    units[1] = EOS
	}

	return (OK)
end
