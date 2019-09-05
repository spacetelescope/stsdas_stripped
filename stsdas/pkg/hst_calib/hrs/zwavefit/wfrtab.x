include	<error.h>
include	<tbset.h>
include	"wf.h"

# Column definitions.
define	COL_X1			1
define	COL_X2			2
define	COL_Y			3
define	COL_SIG			4
define	N_COLS			4

# Memory management.
define	Colname			Memc[colname+($1-1)*(SZ_COLNAME+1)]
define	Colptr			Memi[colptr+$1-1]
define	Nullflag		Memb[nullflag]
define	Sx			Memc[sx]

#---------------------------------------------------------------------------
.help wf_rtab 12Apr95 source
.ih
NAME
wf_rtab -- Read data points from table.
.endhelp
#---------------------------------------------------------------------------
procedure wf_rtab (wf, fname)

pointer	wf			# I:  Wavefit object.
char	fname[ARB]		# I:  Name of table to read.

# Declarations.
pointer	colname			# Column names.
pointer	colptr			# Column pointers.
pointer	nullflag		# NULL indicators.
pointer	sp			# Stack pointer.
int	strdic()		# Get string index.
pointer	sx			# Generic string.
pointer	t			# Table descriptor.
int	tbhgti()		# Get integer-valued header parameter.
int	tbpsta()		# Get table parameters.
pointer	tbtopn()		# Open a table.

errchk	malloc
errchk	salloc, sfree, smark
errchk	tbcfnd, tbcgtd, tbpsta, tbtclo, tbtopn

begin
	# Open the table.
	t = tbtopn (fname, READ_ONLY, NULL)
	
	call smark (sp)
	call salloc (colname, (SZ_COLNAME+1)*N_COLS, TY_CHAR)
	call salloc (colptr, N_COLS, TY_POINTER)
	call salloc (sx, SZ_LINE, TY_CHAR)

	# Get column names from user.
	call clgstr ("order_col", Colname(COL_X1), SZ_COLNAME)
	call clgstr ("line_col", Colname(COL_X2), SZ_COLNAME)
	call clgstr ("sample_col", Colname(COL_Y), SZ_COLNAME)
	call clgstr ("sig_col", Colname(COL_SIG), SZ_COLNAME)

	# Find the columns.  The sigma column is optional.
	call tbcfnd (t, Colname(1), Colptr(1), N_COLS)
	if (Colptr(COL_X1) == NULL) {
	    call sprintf (Sx, SZ_LINE, "no column '%s' giving X1 data")
	    call pargstr (Colname(COL_X1))
	    call error (1, Sx)
	}
	if (Colptr(COL_X2) == NULL) {
	    call sprintf (Sx, SZ_LINE, "no column '%s' giving X2 data")
	    call pargstr (Colname(COL_X2))
	    call error (1, Sx)
	}
	if (Colptr(COL_Y) == NULL) {
	    call sprintf (Sx, SZ_LINE, "no column '%s' giving Y data")
	    call pargstr (Colname(COL_Y))
	    call error (1, Sx)
	}

	# Read the data.
        WF_NDATA(wf) = tbpsta (t, TBL_NROWS)
	call malloc (nullflag, WF_NDATA(wf), TY_BOOL)

	call malloc (WF_X1_PTR(wf), WF_NDATA(wf), TY_DOUBLE)
	call malloc (WF_X2_PTR(wf), WF_NDATA(wf), TY_DOUBLE)
	call malloc (WF_Y_PTR(wf), WF_NDATA(wf), TY_DOUBLE)
	call malloc (WF_SIG_PTR(wf), WF_NDATA(wf), TY_DOUBLE)
	
	call tbcgtd (t, Colptr(COL_X1), WF_X1(wf,1), Nullflag, 1, WF_NDATA(wf))
	call tbcgtd (t, Colptr(COL_X2), WF_X2(wf,1), Nullflag, 1, WF_NDATA(wf))
	call tbcgtd (t, Colptr(COL_Y), WF_Y(wf,1), Nullflag, 1, WF_NDATA(wf))

	if (Colptr(COL_SIG) != NULL)
	    call tbcgtd (t, Colptr(COL_SIG), WF_SIG(wf,1), Nullflag, 1,
			 WF_NDATA(wf))
	else
	    call amovkd (1.d0, WF_SIG(wf,1), WF_NDATA(wf))

	# Get grating, aperture, and carrousel position.
        iferr (call tbhgtt (t, "grating", Sx, SZ_LINE)) {
            call erract (EA_WARN)
            call eprintf ("Cannot access 'grating' table header parameter.\n\tAperture conversion will fail.")
            WF_GRATING(wf) = 0
        } else {
            call strupr (Sx)
            WF_GRATING(wf) = strdic (Sx, Sx, SZ_LINE, GRATING_DICT)
            if (WF_GRATING(wf) == 0) {
                call eprintf ("Grating %s unknown.\n\tAperture conversion will fail.")
                call pargstr (Sx)
            }
        }
        
        iferr (call tbhgtt (t, "aperture", Sx, SZ_LINE)) {
            call erract (EA_WARN)
            call eprintf ("Cannot access 'aperture' table header parameter.\n\tAperture conversion will fail.")
            WF_APER(wf) = 0
        } else {
            call strupr (Sx)
            WF_APER(wf) = strdic (Sx, Sx, SZ_LINE, APER_DICT)
            if (WF_APER(wf) == 0) {
                call eprintf ("Aperture %s unknown.\n\tAperture conversion will fail.")
                call pargstr (Sx)
            }
        }
        
        iferr (WF_CARPOS(wf) = tbhgti (t, "carpos")) {
            call erract (EA_WARN)
            call error (1, "Need carrousel position to populate fit table")
        }

	# That's all folks.
	call mfree (nullflag, TY_BOOL)
	call sfree (sp)
	call tbtclo (t)
end
#---------------------------------------------------------------------------
# End of wf_rtab
#---------------------------------------------------------------------------
