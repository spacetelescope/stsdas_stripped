include <tbset.h>
include <math.h>
include "precess.h"

# PTABLE.X functional interface for precessing columns of RA and DEC in a 
# table.  It is divided into two routines.  test_table() determines if all
# mandatory information has been defined.  ptabl_precess_table() does the 
# precession.  Note that it is the caller's responsibilty to call test_table()
# first.

#===========================================================================

# test_table -- determines if all the information necessary to precess the
# RA and DEC columns of a table has been defined.  If the newcopy flag is
# false then result columns are created in the table.  The names of these
# result columns are defined as the input column names prefixed by the string
# in the prefix argument.  NOTE that if the newcopy flag is FALSE the table
# must be READ_WRITE, otherwise this routine will not be able to create the 
# precessed RA-DEC columns 
#
# Andrew Cseko, Jr.  May-1990  Original
# Phil Hodge, 30-Jul-1990  Allow hours for ra, radians for ra or dec.
# Phil Hodge, 14-Jan-1993  In test_table, prepend "p_" to actual column name.

int procedure test_table (tp, i_colname, o_colname, prefix,
		newcopy, f_d_set, t_d_set, errmsg, maxch)

pointer tp			# i: table pointer
char	i_colname[SZ_COLNAME, 4] # i: data source column names
char	o_colname[SZ_COLNAME, 4] # o: result column names
char	prefix[ARB]		# i: prefix of result column names
bool	newcopy			# i: are results to be placed in a new table
bool	f_d_set, t_d_set	# i: if false, date(s) MUST!! come from table
char	errmsg[ARB]		# o: describes inability to precess table
int	maxch			# i: number of characters to errmsg
#--
int     c
pointer col[4]
pointer newcol
char    t_cname[SZ_COLNAME]
char    colunits[SZ_COLUNITS, 4]
char    colfmt[SZ_COLFMT, 4]
int     dummy
int     colnum
int     datatype[4]
int     lendata[4]
int     lenfmt[4]
char    tberr[SZ_LINE]

int     errget()

begin
	call tbcfnd (tp, i_colname, col, 4)

	if (NULL == col[RA]) {
	    call sprintf (errmsg, maxch, "'%s' column not found")
		call pargstr (i_colname[ 1, RA ])
	    return (ERR)
	}

	if (NULL == col[DEC]) {
	    call sprintf (errmsg, maxch, "'%s' column not found")
		call pargstr (i_colname[ 1, DEC ])
	    return (ERR)
	}

	if (!f_d_set)
	    if (NULL == col[FROMJD]) {
		call sprintf (errmsg, maxch, "'%s' column not found")
		    call pargstr (i_colname[ 1, FROMJD ])
		return (ERR)
	    }

	if (!t_d_set)
	    if (NULL == col[TOJD]) {
		call sprintf (errmsg, maxch, "'%s' column not found")
		    call pargstr (i_colname[ 1, TOJD ])
		return (ERR)
	    }

	#----------------------------------------------------------------------
	# handle creating new columns in the current table when newcopy is false

	if (!newcopy) {

	    do c = RA, DEC {
		call tbcinf (col[c], colnum, t_cname, colunits[1,c],
			colfmt[1,c], datatype[c], lendata[c], lenfmt[c])
		call strcpy (prefix,  o_colname[1,c], SZ_COLNAME)
		call strcat (t_cname, o_colname[1,c], SZ_COLNAME)
	    }

	    iferr {
		call tbcdef (tp, newcol, o_colname, colunits, colfmt, 
			datatype, lendata, 2)
	    } then {
		dummy = errget (tberr, SZ_LINE)

		call sprintf (errmsg, maxch,
		"unable to create output columns '%s' and '%s' because %s")
		    call pargstr (o_colname[1, RA])
		    call pargstr (o_colname[1, DEC])
		    call pargstr (tberr)

		return (ERR)
	    }
	}

	return (OK)
end

#=============================================================================

define DATE_SIZE   30

# ptabl_precess_table -- precess the RA and DEC columns of a table.
# Note this routine does not perform error checking.  It is the users 
# responsibilty to verify the correctness of the table with a call to 
# test_table()
# The units for the ra & dec columns will be gotten.  If they can be
# interpreted as hours, degrees or radians, that will be assumed to be
# correct; otherwise, the value of ra_units will determine what units
# will be assumed for ra & dec:  hours & degrees, degrees & degrees,
# or radians & radians.

procedure ptabl_precess_table (tp, i_colname, o_colname, newcopy, 
		f_d_set, t_d_set, f_day, t_day, ra_units)

pointer tp
char    i_colname[SZ_COLNAME, 4]
char    o_colname[SZ_COLNAME, 4]
bool    newcopy
bool    f_d_set, t_d_set
double  f_day, t_day
int	ra_units			# i: default units for RA
#--
pointer i_col_ptr[4]
pointer o_col_ptr[4]
char    date[DATE_SIZE]
double  ra, dec
double  zetaO, Z, theta
int	units[2]		# units that will be used for ra & dec
int     row, nrows
int	ip

int     tbpsta()
int     strlen()
int     interpret_date()

begin
	# get pointer to RA & DEC columns

	call tbcfnd (tp, i_colname, i_col_ptr, 4)
	if (newcopy) {
	    o_col_ptr[RA]  = i_col_ptr[RA]
	    o_col_ptr[DEC] = i_col_ptr[DEC]
	} else {
	    call tbcfnd (tp, o_colname, o_col_ptr, 2)
	}

	# Get the units for ra & dec from the units for those columns.
	call ad_units (i_col_ptr, ra_units, units)

	# loop over all rows calculating new values of RA & DEC

	nrows = tbpsta (tp, TBL_NROWS)

	do row = 1, nrows {

	    if ((row==1) || !f_d_set || !t_d_set) {
		if (!f_d_set) {
		    f_day = INDEF
		    call tbegtt (tp, i_col_ptr[FROMJD], row, date, DATE_SIZE)

		    call xt_stripwhite (date)
		    if (date[1] != EOS) {
			ip = 1
			if (strlen (date) != interpret_date (date, ip, f_day))
			    f_day = INDEF
		    }

		    if (IS_INDEFR (f_day)) {
			call eprintf (
		"\nerror > row %d of FROM EPOCH column uninterpretable\n");
			    call pargi (row)
		    }
		}

		if (!t_d_set) {
		    t_day = INDEF
		    call tbegtt (tp, i_col_ptr[TOJD], row, date, DATE_SIZE)

		    call xt_stripwhite (date)
		    if (date[1] != EOS) {
			ip = 1
			if (strlen (date) != interpret_date (date, ip, t_day))
				t_day = INDEF
		    }

		    if (IS_INDEFR (t_day)) {
	                 call eprintf (
		"\nerror > row %d of TO EPOCH column uninterpretable\n");
	                     call pargi (row)
		    }
		}

		if ((!IS_INDEFR (f_day)) && (!IS_INDEFR (t_day)))
		    call prec_angles (f_day, t_day, zetaO, Z, theta)
	    }

	    # get the ra and dec values from the table

	    call tbegtd (tp, i_col_ptr[RA],  row, ra)
	    call tbegtd (tp, i_col_ptr[DEC], row, dec)
 
	    # if any input value is INDEF then the result is INDEF

	    if ((!IS_INDEFR (ra)) && (!IS_INDEFR (dec)) &&
		(!IS_INDEFR (f_day)) && (!IS_INDEFR (t_day))) {

		# convert to radians (in-place)
		call ad_to_rad (ra, units[RA], ra)
		call ad_to_rad (dec, units[DEC], dec)

		call precess (ra, dec, zetaO, Z, theta)

		# convert back to degress (or hours or radians)
		call ad_from_rad (ra, units[RA], ra)
		call ad_from_rad (dec, units[DEC], dec)

		call tbeptd (tp, o_col_ptr[RA],  row, ra)
		call tbeptd (tp, o_col_ptr[DEC], row, dec)
	    }
	}
end
