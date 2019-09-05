include	<tbset.h>
include	"hstephem.h"

# ephem_out -- Initialize the output table for hstephem
#
# Description:
# ------------
#
# Version      Date           Author          Description
#     1       16-Jul-1992     J.-C. Hsu       design and coding
#     2       19-Sep-2005     Phil Hodge      change name of first column
#                                             from MJD to TIME
#-------------------------------------------------------------------------------
procedure ephem_out (tbname, tp, colidn)

					## inputs:
char	tbname[SZ_FNAME]		# output table name

					## outputs:
pointer	tp				# output table descripter
pointer	colidn[NCOL]			# column descripters

char	colnam[SZ_COLNAME, NCOL]
char	unit[SZ_COLUNITS, NCOL]
char	colfmt[SZ_COLFMT, NCOL] 	# column name(s), unit(s), and format(s)
int	dtype[NCOL]			# column data types
int	csize[NCOL]

int     i
int	exist

int	tbtacc()
pointer	tbtopn()
#------------------------------------------------------------------------------
begin

	# define the column names
	call strcpy ("TIME",	colnam[1, ID_MJD], SZ_COLNAME)
	call strcpy ("X",	colnam[1, ID_X], SZ_COLNAME)
	call strcpy ("Y",	colnam[1, ID_Y], SZ_COLNAME)
	call strcpy ("Z",	colnam[1, ID_Z], SZ_COLNAME)
	call strcpy ("VX",	colnam[1, ID_VX], SZ_COLNAME)
	call strcpy ("VY",	colnam[1, ID_VY], SZ_COLNAME)
	call strcpy ("VZ",	colnam[1, ID_VZ], SZ_COLNAME)

	# define the data type
	dtype[ID_MJD] = TY_DOUBLE
	dtype[ID_X] = TY_DOUBLE
	dtype[ID_Y] = TY_DOUBLE
	dtype[ID_Z] = TY_DOUBLE
	dtype[ID_VX] = TY_DOUBLE
	dtype[ID_VY] = TY_DOUBLE
	dtype[ID_VZ] = TY_DOUBLE

	do i = 1, NCOL
	    csize[i] = 1

	# define the units
	do i = 1, NCOL
	    unit[1, i] = EOS
	call strcpy ("day",	unit[1, ID_MJD], SZ_COLUNITS)
	call strcpy ("km",	unit[1, ID_X], SZ_COLUNITS)
	call strcpy ("km",	unit[1, ID_Y], SZ_COLUNITS)
	call strcpy ("km",	unit[1, ID_Z], SZ_COLUNITS)
	call strcpy ("km/s",	unit[1, ID_VX], SZ_COLUNITS)
	call strcpy ("km/s",	unit[1, ID_VY], SZ_COLUNITS)
	call strcpy ("km/s",	unit[1, ID_VZ], SZ_COLUNITS)

	# define the column print format
	do i = 1, NCOL
	    colfmt[1, i] = EOS
	call strcpy ("%16.10f",	colfmt[1, ID_MJD], SZ_COLFMT)
	call strcpy ("%9.3f",	colfmt[1, ID_X], SZ_COLFMT)
	call strcpy ("%9.3f",	colfmt[1, ID_Y], SZ_COLFMT)
	call strcpy ("%9.3f",	colfmt[1, ID_Z], SZ_COLFMT)
	call strcpy ("%7.3f",	colfmt[1, ID_VX], SZ_COLFMT)
	call strcpy ("%7.3f",	colfmt[1, ID_VY], SZ_COLFMT)
	call strcpy ("%7.3f",	colfmt[1, ID_VZ], SZ_COLFMT)

	#  check if the table exists
        exist = tbtacc (tbname)

	# if table already exists, send error message
        if (exist == YES) {
            call eprintf ("output table %s already exists\n")
	        call pargstr (tbname)
	    call error (1, "")
        } else {

	    # if table does not exist, initialize it, define all columns
	    # and open it
            tp = tbtopn (tbname, NEW_FILE, 0)
            if (tp == NULL) {
                call eprintf ("cannot create new table %s\n")
		    call pargstr (tbname)
		call error (1, "")
	    }
            iferr (call tbcdef (tp, colidn, colnam, unit, colfmt, dtype,
                        	csize, NCOL)) {
                call tbtclo (tp)
                call error (1, "cannot define columns")
            }
            call tbtcre (tp)
	}
end
