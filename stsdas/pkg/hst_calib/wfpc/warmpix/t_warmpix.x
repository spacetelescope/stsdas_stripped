include	<tbset.h>
include "warmpix.h"

define	NCOLS	5

#  t_warpix -- Warm/hot pixel fixing
#
#  Description:
#  ------------
#
#  Input table column names
#  ------------------------
#  "CHIP", "X", "Y", "EPOCH", "DARK"
#
#  Date		Author	    Version	Description
#  ----		------	    -------	-----------
#  08-28-1995	J.-C. Hsu		initial design and coding
#  09-22-1999	J.-C. Hsu   1.1		use specific column names, instead 
#					of c1, c2, etc.
#  04-28-2003	J.-C. Hsu   1.2		make it to work for FITS files
# ------------------------------------------------------------------------------
procedure t_warmpix ()

pointer	tpin 			# input image template pointer
pointer	tpmask 			# input mask template pointer
char    intable[SZ_FNAME]
char	text[SZ_LINE]
real    thresh[N_THRESH]
short	fix_dqval		# data quality value of fixed pixels
real	rej_val			# pixel value of rejected pixels
bool    verbose
pointer	chip, x, y, epoch, dark, nullflag
pointer	tp
char	colnam[SZ_COLNAME, NCOLS]
pointer	colptr[NCOLS]
int	nrows, i

pointer	tbtopn()
int	tbpsta()
#==============================================================================
begin
	# get CL parameters and related quantities
	call warm_in (tpin, tpmask, intable, thresh, fix_dqval, rej_val, 
			verbose)
 
	# announce start of the task
	call printf ("*** WARMPIX - Version %s ***\n")
	    call pargstr (VERSION)

	# read the hot pixel table
        tp = tbtopn (intable, READ_ONLY, 0)

	call strcpy ("CHIP", 	colnam[1, 1], SZ_COLNAME]
	call strcpy ("X", 	colnam[1, 2], SZ_COLNAME]
	call strcpy ("Y", 	colnam[1, 3], SZ_COLNAME]
	call strcpy ("EPOCH", 	colnam[1, 4], SZ_COLNAME]
	call strcpy ("DARK", 	colnam[1, 5], SZ_COLNAME]
        call tbcfnd (tp, colnam, colptr, NCOLS)
	do i = 1, NCOLS {
	    if (colptr[i] == NULL) {
		call sprintf (text, SZ_LINE, "column %s does not exist in the hot pixel table")
		    call pargstr (colnam[1, i])
            	call error (1, text)
	    }
        }

        nrows = tbpsta (tp, TBL_NROWS)
        call malloc (chip, nrows, TY_INT)
        call malloc (x, nrows, TY_INT)
        call malloc (y, nrows, TY_INT)
        call malloc (epoch, nrows, TY_DOUBLE)
        call malloc (dark, nrows, TY_REAL)
        call malloc (nullflag, nrows, TY_BOOL)

        call tbcgti (tp, colptr[1], Memi[chip], Memb[nullflag], 1, nrows)
        call tbcgti (tp, colptr[2], Memi[x], Memb[nullflag], 1, nrows)
        call tbcgti (tp, colptr[3], Memi[y], Memb[nullflag], 1, nrows)
        call tbcgtd (tp, colptr[4], Memd[epoch], Memb[nullflag], 1, nrows)
        call tbcgtr (tp, colptr[5], Memr[dark], Memb[nullflag], 1, nrows)

        call tbtclo (tp)

	# perform the calculation
	call warm_do (tpin, tpmask, thresh, fix_dqval, rej_val, 
			Memi[chip], Memi[x], Memi[y], Memd[epoch], Memr[dark], 
			nrows, verbose)

	# close file template
	call imtclose (tpin)
	call imtclose (tpmask)

	# free the memory
	call mfree (chip, TY_INT)
	call mfree (x, TY_INT)
	call mfree (y, TY_INT)
	call mfree (epoch, TY_DOUBLE)
	call mfree (dark, TY_REAL)
	call mfree (nullflag, TY_BOOL)

	# announce completion of task
	call printf ("Task WARMPIX completed with no error\n")
end
