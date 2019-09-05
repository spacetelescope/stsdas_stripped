include <chars.h>
include <tbset.h>
include "../surviv.h"

# GETEMDATA -- Subroutine get_em_data adapted from the ASURV routine datain
# and datreg/xdata AND with apologies to the columns task in IRAF. 
# This routine reads data from the input file for the EM-method multivariate
# problem.  The input file may be either an ascii text or a table file.

int procedure get_em_data (filename, section, ind, x, y, y2, nvar, ntot)

# inputs --
char	filename[SZ_FNAME] # name of file to read from
char	section[SZ_LINE] # Section field of the filespec

# outputs --
int	ind[MAX_ASDAT]	# indicator of censoring
double	x[MAX_ASDAT,MAX_ASVAR]	# data for independent variables
			# N.B. The order of variables and data in the x array 
			# is the reverse of every other bi/multi-variate scheme
			# in the survival analysis package, so be carefull!
double	y[MAX_ASDAT]	# data for dependent variable
double	y2[MAX_ASDAT]	# data for dependent variable upper limit if 
			# dependent variable is confined
int	nvar		# number of independent variables
int	ntot		# number of data values(up to MAX_ASDAT)


# locals --
char	title[SZ_LINE]	# descriptive title from the data file for this dataset
int	cvar		# column number of censoring indicator to read
int	ivar[MAX_ASVAR+1] # column numbers of independent variables to read
int	dvar		# column number of dependent variable to read
bool	depvar_uplim	# Flag set if ind[j] == 5 (confined by two values)
			# used to force read for dep. var. upper limit
int	ncol		# largest column to be read in ascii text file
char	cencol[SZ_COLNAME] # Column name of censoring indicator (tables)
char	varcol[SZ_COLNAME,MAX_ASVAR] # Column names of independent variables (tables)
char	depcol[SZ_COLNAME] # Column name of dependent variable (tables)
char	dupcol[SZ_COLNAME] # Column name of dep. var. upper limits (tables)

		# Error message if number of points greater than MAX_ASDAT
string	file_too_big "File %s contains more than %d values--too big!\n"
int	j		# loop index

# Variables for use of an ascii file
int	nchar		# number of characters converted
char	line[SZ_LINE]	# line of text
int	in		# file pointer
int	i		# loop index(to nvar)
int	open(), getline(), ctoi(), ctod(), ctowrd()

# Variables to get the column number (or name for a table)
int	junk, ip, op
int	idepcol		# column number for dependent variable
pointer	sp, outstr

# Stuff for use of tables
bool	not_table
pointer	tp, tbtopn()
errchk	tbtopn
pointer	tpcen, tpvar[MAX_ASVAR], tpdep, tpdup
bool	nullflag[MAX_ASDAT]
int	tbpsta()
pointer	scr

double	one
data	one/1.0d0/

# Error status indicator for INDEF values
bool	errstat

begin

	errstat = false
	title[1] = EOS

	# Find out if this is a table or ascii file
	not_table = false
	iferr (tp = tbtopn(filename, READ_ONLY, 0))
	    not_table = true

	# The EM-method requires the first variable in x to be one
	call amovkd(one, x[1,1], MAX_ASDAT)

	if (not_table) {
	    # ASCII TEXT file
	    call smark(sp)
	    call salloc(outstr, SZ_LINE, TY_CHAR)

	    # Get the column numbers
	    op = 1
	    junk = ctoi(section, op, cvar)
	    for (i=1; i<=MAX_ASVAR+1 && section[op] !=EOS; i=i+1)
		junk = ctoi(section, op, ivar[i])
	    i = i - 1
	    dvar = ivar[i]
	    nvar = i - 1
	    ncol = max(cvar, dvar)
	    do i = 1,nvar
		ncol = max(ncol, ivar[i])
	    ncol = max(ncol, dvar+1)

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

	    # Initialize the dep. var. upper limit to zero
	    call aclrd(y2, MAX_ASDAT)

	    ntot = 0
	    # Separate each line of the input file
	    while (getline(in, line) != EOF) {
	    # Skip commented out(#) lines or null lines
	        if ((line[1] != '#') && (line[1] != '\n')) {
		    ntot = ntot +1
		    if (ntot > MAX_ASDAT) {
			call eprintf(file_too_big)
			call pargstr(filename)
			call pargi(MAX_ASDAT)
			call close(in)
			call sfree(sp)
			return (ERR)
		    }

		    ip = 1
		    depvar_uplim = false
		    # Get censoring indicator and data value for each variable
		    for (i=1; i<=ncol; i=i+1) {
			if (i == cvar) {
		            nchar = ctoi(line, ip, ind[ntot])
			    # If censoring indicates confinement in dep.var.,
			    # force decoding at end of line for the upper limit
			    # (or later in the line if cvar < dvar)
			    depvar_uplim = (ind[ntot] == 5)
			}
			else if (i == dvar)
		            nchar = ctod(line, ip, y[ntot])
			else if ((i == dvar+1) && depvar_uplim) {
			    nchar = ctod(line, ip, y2[ntot])
			    depvar_uplim = false
			} else {
			    nchar = ctowrd(line, ip, Memc[outstr], SZ_LINE)
			    do j = 1,nvar {
			        if (i == ivar[j]) {
				    op = 1
				    nchar = ctod(Memc[outstr], op, x[ntot,j+1])
				    break
				}
			    }
			}
		    }
		    # Check for INDEF's
		    if (ind[ntot] == INDEFI || y[ntot] == INDEFD ) {
			call eprintf(" Indefinite value at point %d\n")
			call pargi(ntot)
			errstat = true
		    }
		    else {
			do j = 1,nvar {
			    if (x[j,ntot] == INDEFD) {
				call eprintf(" Indefinite value at point %d\n")
				call pargi(ntot)
				errstat = true
			    }
			}
		    }
	        }
	    }
	    # If censoring says there is an upper limit in dependent 
	    # variable, reread the file to get them 
	    if (depvar_uplim) {
		# Rewind the file
		call seek(in,BOF)
		# Reread the entire file to extract upper limits to dep. var.
		j = 0
		while (getline(in, line) != EOF) {
		    if ((line[1] != '#') && (line[1] != '\n')) {
			j = j + 1
			if (ind[j] == 5) {
			    ip = 1
			    for (i=1; i<=dvar+1; i=i+1) {
				if (i == dvar+1)
				    nchar = ctod(line, ip, y2[j])
				else
				    nchar = ctowrd(line,ip, Memc[outstr],SZ_LINE)
			    }
			}
		        # Check for INDEF's
		        if (y2[ntot] == INDEFD) {
			    call eprintf(" Indefinite value at point %d\n")
			    call pargi(ntot)
			    errstat = true
		        }
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
	    for (i=1; i<=MAX_ASVAR+1 && section[op] != EOS; i=i+1)
		junk = ctowrd(section, op, varcol[1,i], SZ_COLNAME)
	    i = i - 1
	    call strcpy(varcol[1,i], depcol, SZ_COLNAME)
	    ncol = i - 1
	    nvar = ncol
	    # Set name of dependent variable upper limit column.
	    if (tbpsta (tp, TBL_WHTYPE) == TBL_TYPE_TEXT) {
		# Upper limit is next column after dependent variable.
		call strlwr (depcol)
		if (depcol[1] == 'c')
		    ip = 2			# c<number>
		else
		    ip = 1			# just a number
		junk = ctoi (depcol, ip, idepcol)
		call sprintf (dupcol, SZ_COLNAME, "c%d")
		    call pargi (idepcol+1)	# column number for upper limit
	    } else {
		# dupcol = depcol // "_up"
		call strcpy(depcol, dupcol, SZ_COLNAME)
		call strcat("_up", dupcol, SZ_COLNAME)
	    }

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
	    do j = 1,ntot {
		if (nullflag[j]) {
		    call eprintf(" Indefinite value in censor column at row %d\n")
		    call pargi(j)
		    errstat = true
		}
	    }

	    # Get independent variable column
	    call calloc(scr, ntot, TY_DOUBLE)
	    do i=1,ncol {
		call tbcfnd(tp, varcol[1,i], tpvar[i], 1)
		if (tpvar[i] == NULL) {
		    call eprintf("Column %s not found(%s)\n")
		    call pargstr(varcol[1,i])
		    call pargstr(filename)
		    call tbtclo(tp)
		    call mfree(scr, TY_DOUBLE)
		    return (ERR)
		}
		call tbcgtd(tp, tpvar[i], Memd[scr], nullflag, 1, ntot)
		do j=1,ntot {
		    x[j,i+1] = Memd[scr+j-1]
		    if (nullflag[j]) {
			call eprintf(" Indefinite value in indvar column %s at row %d\n")
			call pargstr(varcol[1,i])
			call pargi(j)
			errstat = true
		    }
		}
		call aclrd(Memd[scr], ntot)
	    }
	    call mfree(scr, TY_DOUBLE)

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
	    do j = 1,ntot {
		if (nullflag[j]) {
		    call eprintf(" Indefinite value in depvar column at row %d\n")
		    call pargi(j)
		    errstat = true
		}
	    }

	    # Get dependent variable upper limit column
	    call tbcfnd(tp, dupcol, tpdup, 1)
	    if (tpdup == NULL) {
		# Column not in table; be sure we don't need it!
		do j = 1,ntot {
		    if (ind[j] == 5) {
			call eprintf("Column %s not found(%s)\n")
			call pargstr(dupcol)
			call pargstr(filename)
			call tbtclo(tp)
			return (ERR)
		    }
		}
		# If we get here the column is not needed; zero it out
		call aclrd(y2, MAX_ASDAT)
	    }
	    call tbcgtd(tp, tpdep, y2, nullflag, 1, ntot)
	    do j = 1,ntot {
		if (nullflag[j]) {
		    call eprintf(" Indefinite value in uplim column at row %d\n")
		    call pargi(j)
		    errstat = true
		}
	    }

	    # Zero out elements of dependent variable upper limit 
	    # which will not be used as upper limits (ind[j] != 5)
	    do j = 1,ntot {
		if (ind[j] != 5)
		    y2[j] = 0.0
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
	    call printf ("        Independent Variable(s):%5d\n")
	    call pargi (ivar[1])
	    do i = 2,nvar {
		call printf ("%33t%5d\n")
		call pargi (ivar[i])
	    }
	    call printf ("             Dependent Variable:%5d\n")
	    call pargi (dvar)
	} else {
	    call printf (" Columns used: Censor Indicator:  %s\n")
	    call pargstr (cencol)
	    call printf ("        Independent Variable(s):  %s\n")
	    call pargstr (varcol[1,1])
	    do i = 2,nvar {
		call printf ("%33t  %s\n")
		call pargstr (varcol[1,i])
	    }
	    call printf ("             Dependent Variable:  %s\n")
	    call pargstr (depcol)
	}
	call printf ("\n")
	call printf (" Number of data points is %d\n")
	call pargi (ntot)
	call printf ("\n")

	return (OK)

end
