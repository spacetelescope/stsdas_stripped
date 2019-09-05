include <tbset.h>
include "tbtables.h"
include "tblerr.h"

# Read values for one column from a range of rows.
#
# Phil Hodge, 28-Dec-1987  Different data types combined into one file.
# Phil Hodge,  3-Feb-1992  Add option for text table type.
# Phil Hodge, 31-Mar-1993  Include short datatype.
# Phil Hodge,  4-Nov-1993  Include check on row number less than one.
# Phil Hodge, 17-May-1995  Change declaration of buffer in tbcgtt to 2-D array.
# Phil Hodge,  9-Apr-1995  Modify for FITS tables.
# Phil Hodge,  2-Mar-1998  Map selected row number to actual row number.
# Phil Hodge, 18-Jun-1998  Use tbfagi instead of tbfagb to get boolean.
# Phil Hodge, 28-Aug-2002  Use strsearch to check for INDEF in tbcgtt.
# Phil Hodge,  8-Aug-2011  Trac #750:  range of actual row numbers can be
#			greater than the range of rows in selected subset.

# tbcgtd -- getcol double
# Read values for one column from a range of rows.  This is for data type
# double precision.

procedure tbcgtd (tp, cp, buffer, nullflag, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
double	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	i, selrow		# loop indexes
errchk	tbegtd

begin
	i = 1
	do selrow = sel_firstrow, sel_lastrow {
	    call tbegtd (tp, cp, selrow, buffer[i])
	    nullflag[i] = (IS_INDEFD (buffer[i]))
	    i = i + 1
	}
end

# tbcgtr -- getcol real
# Read values for one column from a range of rows.  This is for data type real.

procedure tbcgtr (tp, cp, buffer, nullflag, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
real	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	i, selrow		# loop indexes
errchk	tbegtr

begin
	i = 1
	do selrow = sel_firstrow, sel_lastrow {
	    call tbegtr (tp, cp, selrow, buffer[i])
	    nullflag[i] = (IS_INDEFR (buffer[i]))
	    i = i + 1
	}
end

# tbcgti -- getcol integer
# Read values for one column from a range of rows.  This is for data type
# integer.

procedure tbcgti (tp, cp, buffer, nullflag, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
int	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	i, selrow		# loop indexes
errchk	tbegti

begin
	i = 1
	do selrow = sel_firstrow, sel_lastrow {
	    call tbegti (tp, cp, selrow, buffer[i])
	    nullflag[i] = (IS_INDEFI (buffer[i]))
	    i = i + 1
	}
end

# tbcgts -- getcol short
# Read values for one column from a range of rows.  This is for data type
# short integer.

procedure tbcgts (tp, cp, buffer, nullflag, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
short	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	i, selrow		# loop indexes
errchk	tbegts

begin
	i = 1
	do selrow = sel_firstrow, sel_lastrow {
	    call tbegts (tp, cp, selrow, buffer[i])
	    nullflag[i] = (IS_INDEFS (buffer[i]))
	    i = i + 1
	}
end

# tbcgtb -- getcol Boolean
# This is for data type Boolean.

procedure tbcgtb (tp, cp, buffer, nullflag, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
bool	buffer[ARB]		# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	ival
int	i, selrow		# loop indexes
errchk	tbegti

begin
	i = 1
	do selrow = sel_firstrow, sel_lastrow {
	    call tbegti (tp, cp, selrow, ival)
	    if (IS_INDEFI(ival)) {
		buffer[i] = false
		nullflag[i] = true
	    } else {
		buffer[i] = (ival == 1)
		nullflag[i] = false
	    }
	    i = i + 1
	}
end

# tbcgtt -- getcol text
# Read values for one column from a range of rows.  This is for character
# strings.

procedure tbcgtt (tp, cp, buffer, nullflag, lenstr, sel_firstrow, sel_lastrow)

pointer tp			# i: pointer to table descriptor
pointer cp			# i: pointer to descriptor of the column
char	buffer[lenstr,ARB]	# o: buffer for values
bool	nullflag[ARB]		# o: true if element is undefined in table
int	lenstr			# i: length of each element of buffer
int	sel_firstrow		# i: first row from which to get values
int	sel_lastrow		# i: last row from which to get values
#--
int	i, selrow		# loop indexes
int	strsearch()
errchk	tbegtr

begin
	i = 1
	do selrow = sel_firstrow, sel_lastrow {
	    call tbegtt (tp, cp, selrow, buffer[1,i], lenstr)
	    nullflag[i] = (buffer[1,i] == EOS ||
		    (strsearch (buffer[1,i], "INDEF") > 0))
	    i = i + 1
	}
end
