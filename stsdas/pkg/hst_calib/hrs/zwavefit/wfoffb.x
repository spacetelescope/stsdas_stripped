include	<error.h>
include	<tbset.h>
include	"wf.h"

# Define column information
define	N_STR_COLS		4
define	N_COLS			(N_STR_COLS+WF_N_CS)
define	CS_START		(N_STR_COLS+1)

define	APERTURE		1
define	MINCPOS			2
define	MAXCPOS			3
define	GRATING			4

# Memory management.
define	Colname			Memc[colname+($1-1)*(SZ_COLNAME+1)]
define	Colptr			Memi[colptr+$1-1]
define	Nullflag		Memb[nullflag]
define	Sx			Memc[sx]
define	Sy			Memc[sy]

#---------------------------------------------------------------------------
.help wf_off_rtab 12Apr95 source
.ih
NAME
wf_off_rtab - Read SC1/2 to SSA conversion coefficients.
.endhelp
#---------------------------------------------------------------------------
procedure wf_off_rtab (wf, tname)

pointer	wf			# I:  Wavefit object.
char	tname[ARB]		# I:  Table to open

# Declarations.
pointer	colname			# Column names.
pointer	colptr			# Column pointers.
bool	found			# True if coefficients were found.
int	i			# Generic.
int	mincpos, maxcpos	# Bracketing carrousel positions.
pointer	nullflag		# NULL indicators.
int	row			# Current row in table.
pointer	sp			# Stack pointer.
int	strdic()		# String dictionary.
int	strlen()		# String length.
pointer	sx, sy			# Generic string.
pointer	t			# Table descriptor.
pointer	tbtopn()		# Open a table.
int	word_find()		# Find word in dictionary.

errchk	salloc, sfree, smark
errchk	tbtclo, tbtopn, tbrgtd, tbrgti, tbrgtt

begin
        # If column name is blank, nothing to do.
        if (strlen (tname) <= 0)
            call error (1, "wf_off_rtab: no table specified")

        # Open the table.
        t = tbtopn (tname, READ_ONLY, NULL)

        # Allocate memory for reading.
	call smark (sp)
	call salloc (colname, (SZ_COLNAME+1)*N_COLS, TY_CHAR)
	call salloc (colptr, N_COLS, TY_POINTER)
	call salloc (sx, SZ_LINE, TY_CHAR)
	call salloc (sy, SZ_LINE, TY_CHAR)
	call salloc (nullflag, N_COLS, TY_BOOL)

        # Retrieve the columns.
        call strcpy ("aperture", Colname(APERTURE), SZ_COLNAME)
        call strcpy ("min_carpos", Colname(MINCPOS), SZ_COLNAME)
        call strcpy ("max_carpos", Colname(MAXCPOS), SZ_COLNAME)
	call strcpy ("grating", Colname(GRATING), SZ_COLNAME)
        do i = 1, WF_N_CS {
            call sprintf (Colname(CS_START+i-1), SZ_COLNAME, "cs%d")
            call pargi (i-1)
        }
	call tbcfnd (t, Colname(1), Colptr(1), N_COLS)
        do i = 1, N_COLS
            if (Colptr(i) == NULL) {
                call sprintf (Sx, SZ_LINE, "wf_off_rtab: column %s missing")
                call pargstr (Colname(i))
                call error (1, Sx)
            }

        # Look for the coefficients.
        row = 0
	found = false
        repeat {
            row = row + 1

	    # Check grating.
	    iferr (call tbrgtt (t, Colptr(GRATING), Sx, Nullflag, SZ_LINE,
				1, row))
		break
	    call strupr (Sx)
	    if (WF_GRATING(wf) != strdic (Sx, Sx, SZ_LINE, GRATING_DICT))
		next
	    
            # See if aperture matches.
            iferr (call tbrgtt (t, Colptr(APERTURE), Sx, Nullflag, SZ_LINE,
				1, row))
		break
            call strupr (Sx)
            if (WF_APER(wf) != strdic (Sx, Sx, SZ_LINE, APER_DICT))
                next

	    # Check carrousel.
	    iferr {
		call tbrgti (t, Colptr(MINCPOS), mincpos, Nullflag, 1, row)
		call tbrgti (t, Colptr(MAXCPOS), maxcpos, Nullflag, 1, row)
	    } then
		break
	     
	    if (WF_CARPOS(wf) < mincpos || WF_CARPOS(wf) > maxcpos)
		next
	    
            # A match has been found.  Fill the coefficients.
            iferr (call tbrgtd (t, Colptr(CS_START), WF_CS(wf,1),
				Nullflag, WF_N_CS, row))
		break
	    found = TRUE
            break
        }
        call tbtclo (t)

	if (!found) {
	    call erract (EA_WARN)
	    call sprintf (Sx, SZ_LINE, "no row for grating %s, aperture %s, carrousel %d in offlib table")
	    i = word_find (WF_GRATING(wf), GRATING_DICT, Sy, SZ_LINE)
	    call pargstr (Sy)
	    i = word_find (WF_APER(wf), APER_DICT, Sy, SZ_LINE)
	    call pargstr (Sy)
	    call pargi (WF_CARPOS(wf))
	    call error (1, Sx)
	}

	# That's all folks.
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of wf_off_rtab
#---------------------------------------------------------------------------
