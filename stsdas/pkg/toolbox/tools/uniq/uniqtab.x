include	<ctype.h>
include	<tbset.h>

# T_UNIQTAB -- Rename the files named in the given table columns
#
# B.Simon	20-Sep-90	First Code
# B.Simon	19-Mar-91	Check for upper case names in tables
# B.Simon	31-Jul-92	Reworked logic for renaming files
# B.Simon	29-Mar-93	Strip whitespace from input filenames
# P.Greenfield  14-Jun-96	Use new table template routines for new syntax

procedure t_uniqtab ()

#--
pointer	input		# File name template
pointer	colnames	# Table columns containing file names
pointer	instr		# HST instrument associated with this table
pointer directory	# Directory containing files
bool	verbose		# Verbose message switch

bool	caps, image
int	ic, icode, ncol, nrow, icol, irow, maxroot, maxextn, junk
long	aeon
pointer	sp, tp, tp2, im, column, date, table, oldname, newname, tmpname
pointer	history, errmsg, newroot, col, oldextn, newextn

data	aeon  / 0 /

bool	clgetb(), is_upcase(), is_datafile()
int	tbnget(), gstrcpy(), access(), imaccess(), tbpsta(), fnextn()
int	strlen(), nowhite(), instcode(), word_count(), word_fetch()
long	clktime()
pointer	tbnopenp(), tbtopn(), immap()

string  mformat	  "%s -> %s\n"			         # Verbose message fmt
string	hformat	  "%s renamed to %s on %s"	    	 # History record fmt

string	badcolnam "Column %s not found in %s"
string	nofile    "Warning: could not find file (%s)\n"

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (colnames, SZ_FNAME, TY_CHAR)
	call salloc (instr, SZ_FNAME, TY_CHAR)
	call salloc (directory, SZ_FNAME, TY_CHAR)
	call salloc (column, SZ_COLNAME, TY_CHAR)

	call salloc (oldextn, SZ_FNAME, TY_CHAR)
	call salloc (date, SZ_FNAME, TY_CHAR)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (oldname, SZ_FNAME, TY_CHAR)
	call salloc (newname, SZ_FNAME, TY_CHAR)
	call salloc (tmpname, SZ_FNAME, TY_CHAR)
	call salloc (history, SZ_LINE, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Read task parameters

	input = tbnopenp ("input")
	call clgstr ("colnames", Memc[colnames], SZ_FNAME)
	call clgstr ("instr", Memc[instr], SZ_FNAME)
	call clgstr ("directory", Memc[directory], SZ_FNAME)
	verbose = clgetb ("verbose")

	# Convert instrument name to letter

	icode = instcode (Memc[instr])

	# Allocate array for column pointers

	ncol = word_count (Memc[colnames])
	call salloc (col, ncol, TY_STRUCT)

	# Create directory name

	newroot = gstrcpy (Memc[directory], Memc[newname], SZ_FNAME) + newname
	if (newroot > newname) {
	    newroot = newroot - 1
	    if (Memc[newroot] != '/' && Memc[newroot] != '$' ) {
		if (IS_ALNUM(Memc[newroot]) || Memc[newroot] == '.') {
		    newroot = newroot + 1
		    Memc[newroot] = '/'
		}
	    }
	    newroot = newroot + 1
	    Memc[newroot] = EOS
	}
	maxroot = SZ_FNAME + newname - newroot - 2

	# Loop over table names

	while (tbnget (input, Memc[table], SZ_FNAME) != EOF) {
	    tp = tbtopn (Memc[table], READ_WRITE, NULL)
	    nrow = tbpsta (tp, TBL_NROWS)

	    # Create array of column pointers

	    ic = 1
	    icol = 0
	    while (word_fetch (Memc[colnames], ic, 
			       Memc[column], SZ_FNAME) > 0) {
		call tbcfnd (tp, Memc[column], Memi[col+icol], 1)
		if (Memi[col+icol] == NULL) {
		    call sprintf (Memc[errmsg], SZ_LINE, badcolnam)
		    call pargstr (Memc[column])
		    call pargstr (Memc[table])
		    call error (1, Memc[errmsg])
		}
		icol = icol + 1
	    }

	    # Loop over all rows of the table

	    do irow = 1, nrow {
		# Loop over the columns which contain file names

		for (icol = 1; icol <= ncol; icol = icol + 1) {

		    # Create new file name root

		    if (icol == 1) {
			call makeid (Memc[newroot], maxroot)
			newextn = newname + strlen (Memc[newname])

			Memc[newextn] = icode
			Memc[newextn+1] = '.'
			Memc[newextn+2] = EOS

			newextn = newextn + 2
			maxextn = SZ_FNAME + newname - newextn
		    }

		    # Get the old file name, strip whitespace and
		    # add its extension to the new file name

		    call tbegtt (tp, Memi[col+icol-1], irow, 
				 Memc[tmpname], SZ_FNAME)
		    junk = nowhite (Memc[tmpname], Memc[oldname], SZ_FNAME)

		    caps = is_upcase (Memc[oldname])
		    if (caps)
			call strlwr (Memc[oldname])
		    
		    junk = fnextn (Memc[oldname], Memc[oldextn], SZ_FNAME)
		    call strcpy (Memc[oldextn], Memc[newextn], maxextn)
		    
		    if (access (Memc[oldname], 0, 0) == YES) {

			# Create the history record

			call cnvtime (clktime (aeon), Memc[date], SZ_FNAME)
			call sprintf (Memc[history], SZ_LINE, hformat)
			call pargstr (Memc[oldname])
			call pargstr (Memc[newname])
			call pargstr (Memc[date])

			# Test if old file is image

			image = imaccess (Memc[oldname], READ_ONLY) == YES &&
				imaccess (Memc[oldname], NEW_FILE) == YES

			if (access (Memc[newname], 0, 0) == YES  ) {
			    icol = 0

			} else if (image) {
			    iferr {
				call imrename (Memc[oldname], Memc[newname])
			    } then {
				icol = 0
			    } else {
				im = immap (Memc[newname], READ_WRITE, 0)
		    		call imputh (im, "HISTORY", Memc[history])
				call imunmap (im)
			    }

			} else if (! is_datafile (Memc[oldname])) {
			    iferr {
				call rename (Memc[oldname], Memc[newname])
			    } then {
				icol = 0
			    } else {
				tp2 = tbtopn (Memc[newname], READ_WRITE, NULL)
				call tbhadt (tp2, "HISTORY", Memc[history])
				call tbtclo (tp2)
			    }
			}

		    } else if (access (Memc[newname], 0, 0) == NO) {
			call printf (nofile)
			call pargstr (Memc[oldname])
			call flush (STDOUT)
			next
		    }

		    # Replace old name with new in table

		    if (icol > 0) {
			call strcpy (Memc[newroot], Memc[tmpname], SZ_FNAME)
			if (caps)
			    call strupr (Memc[tmpname])

			call tbeptt (tp, Memi[col+icol-1], irow, Memc[tmpname])

			# Print diagnostic message to show status

			if (verbose) {
			    call printf (mformat)
			    call pargstr (Memc[oldname])
			    call pargstr (Memc[newname])
			    call flush (STDOUT)
			}
		    }
		}
	    }
	    call tbtclo (tp)
	}

	call tbnclose (input)
	call sfree (sp)
end
