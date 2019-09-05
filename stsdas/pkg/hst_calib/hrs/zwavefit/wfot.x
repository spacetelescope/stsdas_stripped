include	<error.h>
include	<tbset.h>
include "wf.h"

# Memory management.
define	Colname		Memc[colname+(SZ_COLNAME+1)*($1-1)]
define	Colunits	Memc[colunits+(SZ_COLUNITS+1)*($1-1)]
define	Colfmt		Memc[colfmt+(SZ_COLFMT+1)*($1-1)]
define	Datatype	Memi[datatype+$1-1]
define	Lendata		Memi[lendata+$1-1]

#---------------------------------------------------------------------------
.help wfotac.x 23Mar95 source
.ih
NAME
.nf
.fi
.endhelp
#---------------------------------------------------------------------------
procedure wf_o_alloc (wf, table)

pointer	wf			# I:  Wavefit object.
char	table[ARB]		# I:  Table to open.

# Declarations.
int	tbtacc()		# Check for table existance.

errchk	wf_o_create, wf_o_open

begin
	# Create the column descriptor array.
	call malloc (WF_OT_CP_PTR(wf), WF_OT_NCOLS, TY_POINTER)

	# Create or append to the table.
	if (tbtacc (table) == YES)
	    call wf_o_open (wf, table)
	else
	    call wf_o_create (wf, table)
end
#---------------------------------------------------------------------------
# End of wf_o_alloc
#---------------------------------------------------------------------------
procedure wf_o_create (wf, table)

pointer	wf			# I:  WF object.
char	table[ARB]		# I:  Table to create.

# Declarations.
pointer	colfmt			# Formats.
pointer	colname			# column names.
pointer	colunits		# Units.
pointer	datatype		# Data type.
int	i,j			# Generic.
pointer	lendata			# Length of data.
pointer	sp			# Stack pointer
pointer	tbtopn()		# Open a table.

errchk	salloc, sfree, smark
errchk	tbcdef, tbtopn

begin
	call smark (sp)
	call salloc (colname, (SZ_COLNAME+1) * WF_OT_NCOLS, TY_CHAR)
	call salloc (colunits, (SZ_COLUNITS+1) * WF_OT_NCOLS, TY_CHAR)
	call salloc (colfmt, (SZ_COLFMT+1) * WF_OT_NCOLS, TY_CHAR)
	call salloc (datatype, WF_OT_NCOLS, TY_INT)
	call salloc (lendata, WF_OT_NCOLS, TY_INT)
	
	# Open table.
	WF_OT(wf) = tbtopn (table, NEW_FILE, NULL)

	# Create the column definitions.
        call strcpy ("carpos", Colname(WF_OT_CARPOS), SZ_COLNAME)
        call strcpy ("", Colunits(WF_OT_CARPOS), SZ_COLUNITS)
        Datatype(WF_OT_CARPOS) = TY_INT
        call strcpy ("", Colfmt(WF_OT_CARPOS), SZ_COLFMT)
        Lendata(WF_OT_CARPOS) = 1
        
        call strcpy ("grating", Colname(WF_OT_GRATING), SZ_COLNAME)
        call strcpy ("", Colunits(WF_OT_GRATING), SZ_COLUNITS)
        Datatype(WF_OT_GRATING) = -WF_OT_GRATING_SZ
        call strcpy ("", Colfmt(WF_OT_GRATING), SZ_COLFMT)
        Lendata(WF_OT_GRATING) = 1
        
	do i = 0, WF_NCOEF(wf)-1 {
            j = WF_OT_COEF_COL+i
	    call sprintf (Colname(j), SZ_COLNAME, "a%d")
	    call pargi (i)

	    call strcpy ("", Colunits(j), SZ_COLUNITS)
	    Datatype(j) = TY_DOUBLE
	    call strcpy ("", Colfmt(j), SZ_COLFMT)
	    Lendata(j) = 1
	}

	call tbcdef (WF_OT(wf), WF_OT_CP(wf,1), Colname(1), Colunits(1),
		     Colfmt(1), Datatype(1), Lendata(1), WF_OT_NCOLS)

	WF_OT_NROWS(wf) = 0

	# That's all folks.
	call tbtcre (WF_OT(wf))
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of wf_o_create
#---------------------------------------------------------------------------
procedure wf_o_open (wf, table)

pointer	wf			# I:  Wavefit object.
char	table[ARB]		# I:  Name of table to open.

# Declarations
pointer	colname			# Column names.
int	i			# Generic.
pointer	sp			# Stack pointer.
pointer	sx			# Generic string.
int	tbpsta()		# Get table parameter.
pointer	tbtopn()		# Open a table.

errchk	salloc, sfree, smark

begin
	call smark (sp)
	call salloc (colname, (SZ_COLNAME+1) * WF_OT_NCOLS, TY_CHAR)
	call salloc (sx, SZ_LINE, TY_CHAR)

	# Create the column definitions.
        call strcpy ("carpos", Colname(WF_OT_CARPOS), SZ_COLNAME)
	call strcpy ("grating", Colname(WF_OT_GRATING), SZ_COLNAME)
	do i = 0, WF_NCOEF(wf)-1 {
	    call sprintf (Colname(WF_OT_COEF_COL+i), SZ_COLNAME, "a%d")
	    call pargi (i)
	}

	# Open table.
	WF_OT(wf) = tbtopn (table, READ_WRITE, NULL)

	# Find the columns.  All columns must be there.
	call tbcfnd (WF_OT(wf), Colname(1), WF_OT_CP(wf,1), WF_OT_NCOLS)
	do i = 1, WF_OT_NCOLS
	    if (WF_OT_CP(wf,i) == NULL) {
		call sprintf (Sx, SZ_LINE, "column %s missing from table %s")
		call pargstr (Colname(i))
		call pargstr (table)
		call error (1, Sx)
	    }

	# Get number of rows in table.
	WF_OT_NROWS(wf) = tbpsta (WF_OT(wf), TBL_NROWS)

	# That's all folks
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of wf_o_open
#---------------------------------------------------------------------------
