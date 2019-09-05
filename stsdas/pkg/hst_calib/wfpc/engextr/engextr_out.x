# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	<tbset.h>
include	"engextr.h"

# engextr_out -- Initialized the output table for engextr
#
# Description:
# ------------
#
# Version      Date        Author          Description
#     1       05-28-91     J.-C. Hsu        design and coding
#-------------------------------------------------------------------------------
procedure engextr_out (tbname, infile, tp, colidn)

					## inputs:
char	tbname[SZ_FNAME]		# output table name
char	infile[SZ_FNAME]		# input file name

					## outputs:
pointer	tp				# output table descripter
pointer	colidn[NCOL]			# column descripters
int	csize[NCOL]

char	colnam[SZ_COLNAME, NCOL]
char	unit[SZ_COLUNITS, NCOL]
char	colfmt[SZ_COLFMT, NCOL] 	# column name(s), unit(s), and format(s)
int	dtype[NCOL]			# column data types

int     i
int	exist

int	tbtacc()
pointer	tbtopn()
#------------------------------------------------------------------------------
begin

	# define the column names
	call strcpy ("MNEMONIC",	colnam[1, ID_MNEMONIC], SZ_COLNAME)
	call strcpy ("KEYWORD",		colnam[1, ID_KEYWORD], SZ_COLNAME)

	call strcpy ("VALUE",		colnam[1, ID_CHVAL], SZ_COLNAME)
	call strcpy ("HEX",  		colnam[1, ID_HEXVAL], SZ_COLNAME)

	call strcpy ("COMMENT",		colnam[1, ID_COMMENT], SZ_COLNAME)

	# define the data type
	dtype[ID_MNEMONIC] = -SZ_MNEMONIC
	dtype[ID_KEYWORD] = -SZ_KEYWORD

	dtype[ID_CHVAL] = -SZ_CHVAL
	dtype[ID_HEXVAL] = TY_INT

	dtype[ID_COMMENT] = -SZ_COMMENT
	    
	do i = 1, NCOL
	    csize[i] = 1

	# define the units
	do i = 1, NCOL
	    unit[1, i] = EOS

	# define the column print format
	do i = 1, NCOL
	    colfmt[1, i] = EOS
	call strcpy ("%04x",  	colfmt[1, ID_HEXVAL], SZ_COLFMT)

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
