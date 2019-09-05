include <error.h>
include <tbset.h>
include "tconvert.h"

# tconvert -- convert table format
# This task copies tables, converting between Sun and VAX format.
# FITS files (or individual tables within FITS files) and text tables
# are just copied.

# Phil Hodge, 21-Sep-1989	Task created.
# Phil Hodge, 24-Jul-1990	Delete call to get_root.
# Phil Hodge, 11-Oct-1990	Escape minus signs in input file names.
# Phil Hodge, 24-Jun-1996	Copy FITS files and text tables.

procedure tconvert()

pointer tablist1		# input table list
pointer tablist2		# output table list
char	option[SZ_FNAME]
bool	verbose			# print operations?
#--
pointer sp
pointer table1			# input table name
pointer table2			# output table name
pointer outdir			# output directory name
pointer list1, list2
int	direction		# direction of conversion
int	isdir			# YES if output is just a directory name
int	brackets_present	# YES if name includes expr in brackets
int	table_type		# type of input table (TBL_TYPE_ROW, etc)
bool	done
pointer tbnopen()
int	tbnlen(), tvs_names()
int	isdirectory()
bool	clgetb(), streq()
errchk	tbtcpy, fcopy

begin
	call smark (sp)
	call salloc (tablist1, SZ_LINE, TY_CHAR)
	call salloc (tablist2, SZ_LINE, TY_CHAR)
	call salloc (table1, SZ_LINE, TY_CHAR)
	call salloc (table2, SZ_LINE, TY_CHAR)
	call salloc (outdir, SZ_LINE, TY_CHAR)

	# Get input and output table template lists.

	call clgstr ("intable", Memc[tablist1], SZ_LINE)
	call clgstr ("outtable", Memc[tablist2], SZ_LINE)
	call clgstr ("direction", option, SZ_FNAME)
	verbose = clgetb ("verbose")

	if ((option[1] == EOS) || (option[1] == ' ') ||
		streq (option, "default"))
	    direction = TVS_DEFAULT
	else if (streq (option, "vs"))
	    direction = VAX_SUN
	else if (streq (option, "sv"))
	    direction = SUN_VAX
	else
	    call error (1, "direction should be 'sv', 'vs', or 'default'")

	list1 = tbnopen (Memc[tablist1])

	# Check if the output string is a directory.
	if (isdirectory (Memc[tablist2], Memc[outdir], SZ_LINE) > 0) {
	    isdir = YES
	    list2 = NULL
	} else {
	    isdir = NO
	    list2 = tbnopen (Memc[tablist2])
	    if (tbnlen (list1) != tbnlen (list2))
		call error (1,
			"Number of input and output tables are not the same.")
	}

	done = false
	while (!done) {

	    # Get the input and output table names.
	    iferr {
		if (tvs_names (list1, list2, isdir, Memc[outdir],
			Memc[table1], Memc[table2], SZ_LINE,
			table_type, brackets_present) == EOF) {
		    done = true
		}
	    } then {
		call erract (EA_WARN)
		next
	    }

	    if (done)
		break

	    # Escape minus signs with backslash.
	    call tvs_minus (Memc[table1], Memc[table1], SZ_LINE)
	    call tvs_minus (Memc[table2], Memc[table2], SZ_LINE)

	    if (verbose) {
		call eprintf ("%s -> %s\n")
		    call pargstr (Memc[table1])
		    call pargstr (Memc[table2])
	    }

	    # Copy the table.  For a FITS file, copy the entire file if
	    # no bracketed expression was given.  For a text file, copy it.
	    iferr {
		if (table_type == TBL_TYPE_FITS) {
		    if (brackets_present == YES) {
			call tbtcpy (Memc[table1], Memc[table2])
		    } else {
			call fcopy (Memc[table1], Memc[table2])
		    }
		} else if (table_type == TBL_TYPE_TEXT) {
		    call fcopy (Memc[table1], Memc[table2])
		} else {
		    call tvs_copy (Memc[table1], Memc[table2], direction)
		}
	    } then {
		call erract (EA_WARN)
		next
	    }
	}

	call tbnclose (list1)
	if (list2 != NULL)
	    call tbnclose (list2)

	call sfree (sp)
end
