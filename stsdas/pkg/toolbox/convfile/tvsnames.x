include <tbset.h>

# This routine gets the names of the current input and output tables.
# EOF is returned if there are no more names in the input list.
#
# If the output is a directory (isdir = YES and list2 = NULL), then
# the output name is formed by concatenating the input name to outdir,
# the name of the output directory.  In this case, if the input table
# name includes an expression in brackets (relevant only for FITS files),
# it will be stripped off before concatenating, but it will remain on
# the input name.
#
# Phil Hodge, 24-Jun-1996  Subroutine created.
# Phil Hodge, 20-Apr-1999  Remove table type from calling sequence of tbparse;
#		call tvs_ttype (included in this file) to get table type.

int procedure tvs_names (list1, list2, isdir, outdir,
		table1, table2, maxch, table_type, brackets_present)

pointer list1		# i: fnt pointer for input list
pointer list2		# i: fnt pointer for output list, or NULL
int	isdir		# i: YES if output is just a directory name
char	outdir[ARB]	# i: output directory name, if isdir == YES
char	table1[maxch]	# o: name of input table
char	table2[maxch]	# o: name of output table, including directory
int	maxch		# i: size of input and output names
int	table_type	# o: FITS, text, or stsdas
int	brackets_present # o: YES if input includes an expression in brackets
#--
pointer sp
pointer fname		# file name without brackets
pointer brackets	# expression (if any) in brackets on input table name
pointer indir		# input directory name
int	itype, otype	# types of input and output tables
int	root_len, fnldir()
int	tbnget()
int	access(), tbtacc()
int	junk, hdu, tbparse(), tvs_ttype()
bool	streq()
errchk	tbparse

begin
	call smark (sp)
	call salloc (table1, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_LINE, TY_CHAR)
	call salloc (brackets, SZ_LINE, TY_CHAR)
	call salloc (table2, SZ_LINE, TY_CHAR)
	call salloc (indir, SZ_LINE, TY_CHAR)

	# Get both input and output table names before checking the
	# input name for syntax, to ensure that both lists stay in synch.
	if (tbnget (list1, table1, maxch) == EOF) {
	    call sfree (sp)
	    return (EOF)
	}
	if (isdir != YES)
	    if (tbnget (list2, table2, maxch) == EOF)
		call error (1, "lists not the same length")

	junk = tbparse (table1, Memc[fname], Memc[brackets], SZ_LINE, hdu)
	if (Memc[brackets] == EOS)
	    brackets_present = NO
	else
	    brackets_present = YES

	# Get the type of the input table based on filename extension.
	itype = tvs_ttype (Memc[fname])

	# Check for text file input.
	if (itype != TBL_TYPE_FITS) {
	    if (access (table1, READ_ONLY, TEXT_FILE) == YES) {
		# Make sure it's really a table, not just a text file.
		if (tbtacc (table1) != YES) {
		    call eprintf ("file %s:\n")
			call pargstr (table1)
		    call error (1, "file is not a table")
		}
		itype = TBL_TYPE_TEXT
	    }
	}
	table_type = itype

	if (isdir == YES) {

	    # We use Memc[fname], which is the name without any brackets,
	    # because brackets confuse fnldir, which we are using to get
	    # the length of any directory prefix.
	    root_len = fnldir (Memc[fname], Memc[indir], SZ_LINE)

	    # Copy the output directory name to table2, and concatenate
	    # the input file name (without directory prefix and without
	    # the bracket suffix).
	    call strcpy (outdir, table2, maxch)
	    call strcat (Memc[fname+root_len], table2, maxch)

	} else {

	    junk = tbparse (table2, Memc[fname], Memc[brackets], SZ_LINE, hdu)
	    otype = tvs_ttype (Memc[fname])

	    if (streq (table1, table2)) {
		call eprintf ("table %s:\n")
		    call pargstr (table1)
		call error (1, "can't copy table to itself")
	    }

	    if (itype == TBL_TYPE_FITS && otype != TBL_TYPE_FITS ||
		itype != TBL_TYPE_FITS && otype == TBL_TYPE_FITS) {
		call eprintf ("%s --> %s:\n")
		    call pargstr (table1)
		    call pargstr (table2)
		call error (1,
		"If one file is type FITS, the other must be as well")
	    }
	}

	if (table_type == TBL_TYPE_S_ROW || table_type == TBL_TYPE_S_COL) {
	    call tbtext (table1, table1, maxch)
	    call tbtext (table2, table2, maxch)
	}

	call sfree (sp)
	return (1)
end

int procedure tvs_ttype (fname)

char	fname[ARB]		# i: name of table file (brackets stripped off)
#--
int	ttype
int	len, strlen()
bool	streq()

begin
	if (streq (fname, "STDIN") || streq (fname, "STDOUT"))
	    return (TBL_TYPE_TEXT)

	len = strlen (fname)

	if (streq (fname[len-4], ".fits") ||
	    streq (fname[len-3], ".fit") ||
	    (fname[len-3] == '.' && fname[len] == 'f'))	# ".??f"
	    ttype = TBL_TYPE_FITS
	else
	    ttype = TBL_TYPE_S_ROW		# default

	return (ttype)
end
