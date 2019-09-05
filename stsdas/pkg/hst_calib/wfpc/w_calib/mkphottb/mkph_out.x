include	<tbset.h>
include	"mkphottb.h"

# mkph_out -- Initialized the output table for mkphottb
#
# Description:
# ------------
#
# Version      Date        Author          Description
#     1       08-08-91     J.-C. Hsu       design and coding
#     2       11-18-91     J.-C. Hsu       add flatfield column
#-------------------------------------------------------------------------------
procedure mkph_out (tbname, tp, colidn)

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
	call strcpy ("PHOTMODE",	colnam[1, ID_PHOTMODE], SZ_COLNAME)

	call strcpy ("PHOTFLAM",	colnam[1, ID_PHOTFLAM], SZ_COLNAME)
	call strcpy ("PHOTZPT",		colnam[1, ID_PHOTZPT], SZ_COLNAME)
	call strcpy ("PHOTPLAM",	colnam[1, ID_PHOTPLAM], SZ_COLNAME)
	call strcpy ("PHOTBW",		colnam[1, ID_PHOTBW], SZ_COLNAME)

	call strcpy ("CAMERA",		colnam[1, ID_CAMERA], SZ_COLNAME)
	call strcpy ("DETECTOR",  	colnam[1, ID_DETECTOR], SZ_COLNAME)
	call strcpy ("FILTNAM1",	colnam[1, ID_FILTNAM1], SZ_COLNAME)
	call strcpy ("FILTNAM2",	colnam[1, ID_FILTNAM2], SZ_COLNAME)
	call strcpy ("FLATFIELD",	colnam[1, ID_FLAT], SZ_COLNAME)

	# define the data type
	dtype[ID_PHOTMODE] = -SZ_PHOTMODE
	dtype[ID_PHOTFLAM] = TY_REAL
	dtype[ID_PHOTZPT] = TY_REAL
	dtype[ID_PHOTPLAM] = TY_REAL
	dtype[ID_PHOTBW] = TY_REAL

	dtype[ID_CAMERA] = -SZ_CAMERA
	dtype[ID_DETECTOR] = TY_INT
	dtype[ID_FILTNAM1] = -SZ_FILTNAM1
	dtype[ID_FILTNAM2] = -SZ_FILTNAM2
	dtype[ID_FLAT] = -SZ_FLAT

	do i = 1, NCOL
	    csize[i] = 1

	# define the units
	do i = 1, NCOL
	    unit[1, i] = EOS
	call strcpy ("Mag",  	  unit[1, ID_PHOTZPT], SZ_COLUNITS)
	call strcpy ("Angstrom",  unit[1, ID_PHOTPLAM], SZ_COLUNITS)
	call strcpy ("Angstrom",  unit[1, ID_PHOTBW], SZ_COLUNITS)

	# define the column print format
	do i = 1, NCOL
	    colfmt[1, i] = EOS
	call strcpy ("%7.3f",  	colfmt[1, ID_PHOTZPT], SZ_COLFMT)
	call strcpy ("%10.3f",  colfmt[1, ID_PHOTPLAM], SZ_COLFMT)
	call strcpy ("%10.3f",  colfmt[1, ID_PHOTBW], SZ_COLFMT)

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
