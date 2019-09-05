include <ctype.h>
include <imio.h>
include	<evexpr.h>
include "hcheck.h"

# FHEADER -- Evaluate the additional functions for evexpr

procedure fheader (func, argptr, nargs, op)

char	func[ARB]	# i: Function name
pointer	argptr[ARB]	# i: Pointers to function arguments
int	nargs		# i: Number of function arguments
pointer	op		# o: Structure containing function value
#--
include "hcheck.com"

bool	numflag, typeflag, valflag
int	fnum, type, iarg
int	fntype[NFUNC], argnum[NFUNC], argtype[NFUNC]
pointer	sp, ch, errmsg

data	fntype  / TY_BOOL, TY_BOOL, TY_BOOL, TY_BOOL, TY_BOOL, 
		  TY_BOOL, TY_BOOL, TY_CHAR, TY_CHAR /

data	argnum	/ -1, 1, 1, 1, 1, 1, 1, 1, 1 /

data	argtype / TY_SAME, TY_BOOL, TY_CHAR, TY_INT, TY_REAL,
		  TY_CHAR, TY_CHAR, TY_CHAR, TY_CHAR /

string	flist   FSTRING
string	badarg	"Incorrect number of arguments for %s"
string	badtyp	"Invalid argument type in %s"
string	badfun	"Unknown function name (%s)"

bool	streq()
int	word_match()

begin
	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	fnum = word_match (func, flist)
	if (fnum == 0) {
	    # unrecognized function name
	    call sprintf (Memc[errmsg], SZ_LINE, badfun)
	    call pargstr (func)
	    call error (LOCAL, Memc[errmsg])
	}

	# Check number of arguments

	if (argnum[fnum] != -1) {
	    numflag = nargs == argnum[fnum]
	} else {
	    numflag = nargs > 1
	}

	if (! numflag) {
	    call sprintf (Memc[errmsg], SZ_LINE, badarg)
	    call pargstr (func)
	    call error (LOCAL, Memc[errmsg])
	}

	# Check type of arguments

	if (argtype[fnum] != TY_SAME) {
	    typeflag = O_TYPE(argptr[1]) == argtype[fnum]

	} else {
	    typeflag = true
	    type = O_TYPE(argptr[1])

	    do iarg = 2, nargs {
		if (type != O_TYPE(argptr[iarg])) {
		    typeflag = false
		    break
	        }
	    }
	}

	if (! typeflag) {
	    if  (fnum  < 2 || fnum > 5) {
		call sprintf (Memc[errmsg], SZ_LINE, badtyp)
		call pargstr (func)
		call error (LOCAL, Memc[errmsg])
	    }
	}

	# Evaluate function

	if (fntype[fnum] == TY_CHAR) {
	    call xev_initop (op, SZ_FNAME, TY_CHAR)
	} else {
	    call xev_initop (op, 0, fntype[fnum])
	}

	switch (fnum) {
	case 1:
	    # does first argument equal one of the rest (match)
	    valflag = false

	    switch (type) {
	    case TY_BOOL:
		if (O_VALB(argptr[1])) {
		    do iarg = 2, nargs {
			if (O_VALB(argptr[iarg])) {
			    valflag = true
			    break
			}
		    }
		} else {
		    do iarg = 2, nargs {
			if (! O_VALB(argptr[iarg])) {
			    valflag = true
			    break
			}
		    }
		}
	    case TY_CHAR:
		do iarg = 2, nargs {
		    if (streq (O_VALC(argptr[1]), O_VALC(argptr[iarg]))) {
			valflag = true
			break
		    }
		}
	    case TY_SHORT,TY_INT,TY_LONG:
		do iarg = 2, nargs {
		    if (O_VALI(argptr[1]) == O_VALI(argptr[iarg])) {
			valflag = true
			break
		    }
		}
	    case TY_REAL:
		do iarg = 2, nargs {
		    if (O_VALR(argptr[1]) == O_VALR(argptr[iarg])) {
			valflag = true
			break
		    }
		}
	    }
	    O_VALB(op) = valflag

	case 2,3,4,5:
	    # Is argument of correct type ?
	    O_VALB(op) = typeflag

	case 6:
	    # is string in lower case (islower)
	    valflag = true
	    for (ch = O_VALP(argptr[1]); Memc[ch] != EOS; ch = ch + 1) {
		if (IS_UPPER(Memc[ch])) {
		    valflag = false
		    break
		}
	    }
	    O_VALB(op) = valflag

	case 7:
	    # is string in upper case (isupper)
	    valflag = true
	    for (ch = O_VALP(argptr[1]); Memc[ch] != EOS; ch = ch + 1) {
		if (IS_LOWER(Memc[ch])) {
		    valflag = false
		    break
		}
	    }
	    O_VALB(op) = valflag

	case 8:
	    # convert string to lower case (tolower)
	    call strcpy (O_VALC(argptr[1]), O_VALC(op), SZ_FNAME)
	    call strlwr (O_VALC(op))

	case 9:
	    # convert string to upper case (toupper)
	    call strcpy (O_VALC(argptr[1]), O_VALC(op), SZ_FNAME)
	    call strupr (O_VALC(op))
	}

	call sfree (sp)
end

# VHEADER -- Retrieve a keyword from an image header for evexpr

procedure vheader (keyword, op)

char	keyword[ARB]	# i: Keyword name
pointer	op		# o: Structure containing value of variable
#--
include "hcheck.com"

int	match, type, length, junk
pointer	sp, file, ldir, errmsg, root

string	int_special "group"
string	str_special "dir,ext,hdr,pix,root"
string	nokeyword   "vheader: keyword not found"
string	badkeyword  "Unrecognized name for special keyword (%s)"

bool	imgetb()
int	fnextn(), fnldir(), fnroot(), imgeti(), imgftype(), word_match()
real	imgetr()

begin

	call smark (sp)
	call salloc (file, SZ_FNAME, TY_CHAR)
	call salloc (ldir, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Get the type and length of the variable

	if (keyword[1] == '$') {
	    match = - word_match (keyword[2], int_special)
	    if (match == 0) 
		match = word_match (keyword[2], str_special)

	    if (match < 0) {
		type = TY_INT
		length = 0
	    } else if (match > 0) {
		type = TY_CHAR
		length = SZ_FNAME
	    } else {
		call sprintf (Memc[errmsg], SZ_LINE, badkeyword)
		call pargstr (keyword)
		call error (LOCAL, Memc[errmsg])
	    }

	} else {
	    iferr {
		type = imgftype (imc, keyword)
	    } then {
		call strcpy (keyword, keyc, SZ_KEYWORD)
		call error (MISSING, nokeyword)
	    }

	    if (type == TY_SHORT || type == TY_LONG)
		type = TY_INT

	    if (type == TY_CHAR) {
		length = SZ_FNAME
	    } else {
		length = 0
	    }
	}

	# Initialize data structure to hold return value

	call xev_initop (op, length, type)

	# Get special keywords
	# These are mostly variations on the image name

	if (keyword[1] == '$') {
	    if (match > 0) {
		call imgcluster (IM_NAME(imc), Memc[file], SZ_FNAME)
		root = file + fnldir (Memc[file], Memc[ldir], SZ_FNAME)
	    }

	    switch (match) {
	    case -1:
		# group number $group
		O_VALI(op) = max (1, IM_CLINDEX(imc))
	    case 0:
		# (not used)
		;
	    case 1:
		# directory name $dir
		call strcpy (Memc[ldir], O_VALC(op), length)
	    case 2:
		# extension $ext
		junk = fnextn (Memc[root], O_VALC(op), length)
	    case 3:
		# header file name $hdr
		call strcpy (Memc[root], O_VALC(op), length)
	    case 4:
		# pixel file name $pix
		call imdfile (Memc[root], O_VALC(op), length)
	    case 5:
		# root name $root
		junk = fnroot (Memc[root], O_VALC(op), length)
	    }

	# Get keyword from header

	} else {
	    switch (type) {
	    case TY_BOOL:
		O_VALB(op) = imgetb (imc, keyword)
	    case TY_CHAR:
		call imgstr (imc, keyword, O_VALC(op), length)
	    case TY_SHORT,TY_INT,TY_LONG:
		O_VALI(op) = imgeti (imc, keyword)
	    case TY_REAL:
		O_VALR(op) = imgetr (imc, keyword)
	    }
	}

	call sfree (sp)
end

