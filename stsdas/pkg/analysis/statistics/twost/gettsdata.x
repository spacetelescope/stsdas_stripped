include <chars.h>
include <tbset.h>
include "../surviv.h"

# GETTSDATA -- Subroutine get_ts_data adapted from the ASURV routine datain
# AND with apologies to the columns task in IRAF. 
# This routine reads data from the input file for the Two_sample univariate 
# problem.  The input file may be either an ascii text or a table file.

int procedure get_ts_data (filename, section, ista, ind, x, ntot, ifirst, isecon)

# inputs --
char	filename[SZ_FNAME] # name of file to read from
char	section[SZ_LINE] # Section field of the filespec

# outputs --
int	ista[MAX_ASDAT] # which group does this point belong to
int	ind[MAX_ASVAR,MAX_ASDAT] # indicator of censoring
double	x[MAX_ASVAR,MAX_ASDAT] # data for each of up to MAX_ASVAR variables
int	ntot		# number of data values(up to MAX_ASDAT)
int	ifirst		# first group to use
int	isecon		# second group to use
			# ifirst and isecon must correspond to values in ista

# locals --
char	title[SZ_LINE]	# descriptive title from the data file for this dataset
int	cvar		# column number of censoring indicator to read
int	ivar		# column number of variable to read
int	gvar		# column number of group indicator to read
int	nvar		# largest of cvar, ivar, gvar
char	cencol[SZ_COLNAME] # Column name of censoring indicator (tables)
char	varcol[SZ_COLNAME] # Column name of variable (tables)
char	groupcol[SZ_COLNAME] # Column name of group indicator (tables)

		# Error message if number of points greater than MAX_ASDAT
string	file_too_big "File %s contains more than %d values--too big!\n"
int	j		# loop index
pointer	scr		# pointer for working space

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
pointer	tpcen, tpvar, tpgrp
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
	    junk = ctoi(section, op, gvar)
	    # and the group indicators
	    junk = ctoi(section, op, ifirst)
	    junk = ctoi(section, op, isecon)
	    nvar = max(cvar, ivar)
	    nvar = max(nvar, gvar)

	    # Open the input file
	    in = open(filename, READ_ONLY, TEXT_FILE)

	    # Read the first (and second) lines to get labels (if present)
	    nchar = getline(in, line)
	    if (line[1] == '#') {
	        # First line is label; get title and blank out the '#'
	        call strcpy(line, title, SZ_LINE)
	        title[1] = BLANK
	    } 
	    else
	        title[1] = EOS

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
		            nchar = ctoi(line, ip, ind[1, ntot])
			else if (i == ivar)
		            nchar = ctod(line, ip, x[1, ntot])
			else if (i == gvar)
		            nchar = ctoi(line, ip, ista[ntot])
			else
			    nchar = ctowrd(line, ip, Memc[outstr], SZ_LINE)
		    }
		    # Check for INDEF's
		    if (ind[1,ntot] == INDEFI || 
			x[1,ntot] == INDEFD || ista[ntot] == INDEFI ) {
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
	    junk = ctowrd(section, op, groupcol, SZ_COLNAME)
	    # and the group indicators
	    junk = ctoi(section, op, ifirst)
	    junk = ctoi(section, op, isecon)

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
	    call calloc(scr, ntot, TY_INT)
	    call tbcgti(tp, tpcen, Memi[scr], nullflag, 1, ntot)
	    do j = 1, ntot {
		ind[1,j] = Memi[scr+j-1]
		if (nullflag[j]) {
		    call eprintf(" Indefinite value in censor column at row %d\n")
		    call pargi(j)
		    errstat = true
		}
	    }
	    call mfree(scr, TY_INT)

	    # Get variable
	    call tbcfnd(tp, varcol, tpvar, 1)
	    if (tpvar == NULL) {
		call eprintf("Column %s not found(%s)\n")
		call pargstr(varcol)
		call pargstr(filename)
		call tbtclo(tp)
		return (ERR)
	    }
	    call calloc(scr, ntot, TY_DOUBLE)
	    call tbcgtd(tp, tpvar, Memd[scr], nullflag, 1, ntot)
	    do j = 1, ntot {
		x[1,j] = Memd[scr+j-1]
		if (nullflag[j]) {
		    call eprintf(" Indefinite value in variable column at row %d\n")
		    call pargi(j)
		    errstat = true
		}
	    }
	    call mfree(scr, TY_DOUBLE)

	    # Get group indicator column
	    call tbcfnd(tp, groupcol, tpgrp, 1)
	    if (tpgrp == NULL) {
		call eprintf("Column %s not found(%s)\n")
		call pargstr(groupcol)
		call pargstr(filename)
		call tbtclo(tp)
		return (ERR)
	    }
	    call tbcgti(tp, tpgrp, ista, nullflag, 1, ntot)
	    do j = 1, ntot {
		if (nullflag[j]) {
		    call eprintf(" Indefinite value in group column at row %d\n")
		    call pargi(j)
		    errstat = true
		}
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
	    call printf ("                       Variable:%5d\n")
	    call pargi (ivar)
	    call printf ("                Group Indicator:%5d\n")
	    call pargi (gvar)
	} else {
	    call printf (" Columns used: Censor Indicator:  %s\n")
	    call pargstr (cencol)
	    call printf ("                       Variable:  %s\n")
	    call pargstr (varcol)
	    call printf ("                Group Indicator:  %s\n")
	    call pargstr (groupcol)
	}
	call printf ("                    Groups used:  %5d and %5d\n")
	call pargi (ifirst)
	call pargi (isecon)
	call printf ("\n")
	call printf (" Number of data points is %d\n")
	call pargi (ntot)
	call printf ("\n")

	return (OK)

end
