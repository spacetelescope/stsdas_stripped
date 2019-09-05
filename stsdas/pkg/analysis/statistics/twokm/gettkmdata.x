include <chars.h>
include <tbset.h>
include "../surviv.h"

# GETTKMDATA -- Subroutine get_tkm_data adapted from the ASURV routine datain
# and datreg/xdata AND with apologies to the columns task in IRAF. 
# This routine reads data from the input file for the Schmitt method bivariate 
# problem.  The input file may be either an ascii text or a table file.

int procedure get_tkm_data (filename, section, ind, x, y, ntot)

# inputs --
char	filename[SZ_FNAME] # name of file to read from
char	section[SZ_LINE] # Section field of the filespec

# outputs --
int	ind[MAX_ASDAT]	# indicator of censoring
double	x[MAX_ASDAT]	# data for independent variable
double	y[MAX_ASDAT]	# data for dependent variable
int	ntot		# number of data values(up to MAX_ASDAT)

# locals --
char	title[SZ_LINE]	# descriptive title from the data file for this dataset
int	cvar		# column number of censoring indicator to read
int	ivar		# column number of independent variable to read
int	dvar		# column number of dependent variable to read
int	nvar		# largest of cvar, ivar, dvar
char	cencol[SZ_COLNAME] # Column name of censoring indicator (tables)
char	varcol[SZ_COLNAME] # Column name of independent variable (tables)
char	depcol[SZ_COLNAME] # Column name of dependent variable (tables)

		# Error message if number of points greater than MAX_ASDAT
string	file_too_big "File %s contains more than %d values--too big!\n"

# Variables for use of an ascii file
int	nchar		# number of characters converted
char	line[SZ_LINE]	# line of text
int	in		# file pointer
int	i		# loop index(to nvar)
int	open(), getline(), ctoi(), ctod(), ctowrd()

# Variables to get the column number (or name for a table)
int	junk, ip, op
pointer	sp, outstr

# Stuff for use of tables
bool	not_table
pointer	tp, tbtopn()
errchk	tbtopn
pointer	tpcen, tpvar, tpdep
bool	nullflag[MAX_ASDAT]
int	tbpsta()

# Error status indicator for INDEF values
bool	errstat

begin

	errstat = false
	title[1] = EOS

	# Find out if this is a table or ascii file
	not_table = false
	iferr (tp = tbtopn(filename, READ_ONLY, 0))
	    not_table = true

	if (not_table) {
	    # ASCII TEXT file
	    call smark(sp)
	    call salloc(outstr, SZ_LINE, TY_CHAR)

	    # Get the column numbers
	    op = 1
	    junk = ctoi(section, op, cvar)
	    junk = ctoi(section, op, ivar)
	    junk = ctoi(section, op, dvar)
	    nvar = max(cvar, ivar)
	    nvar = max(nvar, dvar)

	    # Open the input file
	    in = open(filename, READ_ONLY, TEXT_FILE)

	    # Read the first (and second) lines to get labels (if present)
	    nchar = getline(in, line)
	    if (line[1] == '#') {
	        # First line is label; get title and blank out the '#'
	        call strcpy(line, title, SZ_LINE)
	        title[1] = BLANK
	    } else {
	        # No title line; null out field
	        title[1] = EOS
	    }
	    # Rewind to the beginning-of-file to read the data
	    call seek(in, BOF)

	    ntot = 0
	    # Separate each line of the input file
	    while (getline(in, line) != EOF) {
	    # Skip commented out(#) lines or null lines
	        if ((line[1] != '#') && (line[1] != '\n')) {
		    ip = 1
		    ntot = ntot +1
		    if (ntot > MAX_ASDAT) {
			call eprintf(file_too_big)
			call pargstr(filename)
			call pargi(MAX_ASDAT)
			call close(in)
			call sfree(sp)
			return (ERR)
		    }
		    # Get censoring indicator and data value for each variable
		    for (i=1; i<=nvar; i=i+1) {
			if (i == cvar) 
		            nchar = ctoi(line, ip, ind[ntot])
			else if (i == ivar)
		            nchar = ctod(line, ip, x[ntot])
			else if (i == dvar)
		            nchar = ctod(line, ip, y[ntot])
			else
			    nchar = ctowrd(line, ip, Memc[outstr], SZ_LINE)
		    }
		    # Check for INDEF's
		    if (ind[ntot] == INDEFI || 
			x[ntot] == INDEFD || y[ntot] == INDEFD ) {
			call eprintf(" Indefinite value at point %d\n")
			call pargi(ntot)
			errstat = true
		    }
	        }
	    }

	    # Close the file
	    call close(in)

	    call sfree(sp)

	} else {
	    # TABLE file

	    # Get the title from the header
	    iferr (call tbhgtt(tp, "title", title, SZ_LINE))
		title[1] = EOS

	    # Get the names of the columns
	    op = 1
	    junk = ctowrd(section, op, cencol, SZ_COLNAME)
	    junk = ctowrd(section, op, varcol, SZ_COLNAME)
	    junk = ctowrd(section, op, depcol, SZ_COLNAME)

	    ntot = tbpsta(tp, TBL_NROWS)
	    if (ntot > MAX_ASDAT) {
		call eprintf(file_too_big)
		call pargstr(filename)
		call pargi(MAX_ASDAT)
		call tbtclo(tp)
		return (ERR)
	    }

	    # Get censoring column
	    call tbcfnd(tp, cencol, tpcen, 1)
	    if (tpcen == NULL) {
		call eprintf("Column %s not found(%s)\n")
		call pargstr(cencol)
		call pargstr(filename)
		call tbtclo(tp)
		return (ERR)
	    }
	    call tbcgti(tp, tpcen, ind, nullflag, 1, ntot)
	    do i = 1,ntot
		if (nullflag[i]) {
		    call eprintf(" Indefinite value in censor column at row %d\n")
		    call pargi(i)
		    errstat = true
		}

	    # Get independent variable column
	    call tbcfnd(tp, varcol, tpvar, 1)
	    if (tpvar == NULL) {
		call eprintf("Column %s not found(%s)\n")
		call pargstr(varcol)
		call pargstr(filename)
		call tbtclo(tp)
		return (ERR)
	    }
	    call tbcgtd(tp, tpvar, x, nullflag, 1, ntot)
	    do i = 1,ntot
		if (nullflag[i]) {
		    call eprintf(" Indefinite value in indvar column at row %d\n")
		    call pargi(i)
		    errstat = true
		}

	    # Get dependent variable column
	    call tbcfnd(tp, depcol, tpdep, 1)
	    if (tpdep == NULL) {
		call eprintf("Column %s not found(%s)\n")
		call pargstr(depcol)
		call pargstr(filename)
		call tbtclo(tp)
		return (ERR)
	    }
	    call tbcgtd(tp, tpdep, y, nullflag, 1, ntot)
	    do i = 1,ntot
		if (nullflag[i]) {
		    call eprintf(" Indefinite value in depvar column at row %d\n")
		    call pargi(i)
		    errstat = true
		}

	    # Close the table
	    call tbtclo(tp)

	}

	# Check if any INDEF's were supplied
	if (errstat)
	    call error(0, " Indefinite values (INDEF) are not allowed")

	# Print some summary information for this data
	call printf (" Title:  %s \n")
	call pargstr (title)
	call printf ("\n")
	call printf (" Data:  %s \n")
	call pargstr (filename)
	if (not_table) {
	    call printf (" Columns used: Censor Indicator:%5d\n")
	    call pargi (cvar)
	    call printf ("           Independent Variable:%5d\n")
	    call pargi (ivar)
	    call printf ("             Dependent Variable:%5d\n")
	    call pargi (dvar)
	} else {
	    call printf (" Columns used: Censor Indicator:  %s\n")
	    call pargstr (cencol)
	    call printf ("           Independent Variable:  %s\n")
	    call pargstr (varcol)
	    call printf ("             Dependent Variable:  %s\n")
	    call pargstr (depcol)
	}
	call printf ("\n")
	call printf (" Number of data points is %d\n")
	call pargi (ntot)
	call printf ("\n")

	return (OK)

end
