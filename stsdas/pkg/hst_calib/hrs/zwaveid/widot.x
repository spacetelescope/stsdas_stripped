include	<error.h>
include	<tbset.h>
include "ot.h"

# Memory management.
define	Colname		Memc[colname+(SZ_COLNAME+1)*($1-1)]
define	Colunits	Memc[colunits+(SZ_COLUNITS+1)*($1-1)]
define	Colfmt		Memc[colfmt+(SZ_COLFMT+1)*($1-1)]
define	Datatype	Memi[datatype+$1-1]
define	Lendata		Memi[lendata+$1-1]

#---------------------------------------------------------------------------
.help widot.x 11Apr95 source
.ih
NAME
.nf
.fi
.endhelp
#---------------------------------------------------------------------------
pointer procedure wid_o_alloc (table)

char	table[ARB]		# I:  Table to open.

# Declarations.
pointer	ot			# OT object.
int	tbtacc()		# Check for table existance.

errchk	wid_o_new, wid_o_open

begin
	call malloc (ot, OT_SZ, TY_STRUCT)
	call malloc (OT_COL_PTR(ot), OT_NCOLS, TY_POINTER)
	call malloc (OT_APER_PTR(ot), SZ_LINE, TY_CHAR)
	call malloc (OT_GRAT_PTR(ot), SZ_LINE, TY_CHAR)

	# Open table if it exists, else create it.
	if (tbtacc (table) == YES)
	    call wid_o_open (ot, table)
	else
	    call wid_o_new (ot, table)
	
	return (ot)
end
#---------------------------------------------------------------------------
# End of wid_o_alloc
#---------------------------------------------------------------------------
procedure wid_o_free (ot)

pointer	ot			# IO: OT object, NULL on return.

errchk	mfree

begin
        # Write header parameters.
	call tbhadi (OT_T(ot), "carpos", OT_CARPOS(ot))
        call tbhadt (OT_T(ot), "grating", OT_GRAT(ot))
        call tbhadt (OT_T(ot), "aperture", OT_APER(ot))
        
	# Close out.
	call tbtclo (OT_T(ot))

	call mfree (OT_COL_PTR(ot), TY_POINTER)
	call mfree (OT_APER_PTR(ot), TY_CHAR)
	call mfree (OT_GRAT_PTR(ot), TY_CHAR)
	call mfree (ot, TY_STRUCT)
end
#---------------------------------------------------------------------------
# End of wid_o_free
#---------------------------------------------------------------------------
procedure wid_o_new (ot, table)

pointer	ot			# I:  OT object.
char	table[ARB]		# I:  Table to create.

# Declarations.
pointer	colfmt			# Formats.
pointer	colname			# column names.
pointer	colunits		# Units.
pointer	datatype		# Data type.
int	i			# Generic.
pointer	lendata			# Length of data.
pointer	sp			# Stack pointer
pointer	tbtopn()		# Open a table.

errchk	salloc, sfree, smark
errchk	tbcdef, tbtopn

begin
	call smark (sp)
	call salloc (colname, (SZ_COLNAME+1) * OT_NCOLS, TY_CHAR)
	call salloc (colunits, (SZ_COLUNITS+1) * OT_NCOLS, TY_CHAR)
	call salloc (colfmt, (SZ_COLFMT+1) * OT_NCOLS, TY_CHAR)
	call salloc (datatype, OT_NCOLS, TY_INT)
	call salloc (lendata, OT_NCOLS, TY_INT)
	
	# Open table.
	OT_T(ot) = tbtopn (table, NEW_FILE, NULL)

	# Create the column definitions.
	call strcpy ("line", Colname(C_LINE), SZ_COLNAME)
	call strcpy ("sporder", Colname(C_SPORDER), SZ_COLNAME)
	call strcpy ("pixel(pred)", Colname(C_PPOSP), SZ_COLNAME)
	call strcpy ("pixel(obs)", Colname(C_PPOSO), SZ_COLNAME)
	call strcpy ("wave(obs)", Colname(C_WAVEO), SZ_COLNAME)
	call strcpy ("sample(pred)", Colname(C_SPOSP), SZ_COLNAME)
	call strcpy ("sample(obs)", Colname(C_SPOSO), SZ_COLNAME)
	call strcpy ("diff(wave)", Colname(C_DIFFW), SZ_COLNAME)
	call strcpy ("diff(pixel)", Colname(C_DIFFP), SZ_COLNAME)
	call strcpy ("diff(sample)", Colname(C_DIFFS), SZ_COLNAME)
	call strcpy ("intensity", Colname(C_INTP), SZ_COLNAME)

	call strcpy ("angstroms", Colunits(C_LINE), SZ_COLUNITS)
	call strcpy ("spectral order", Colunits(C_SPORDER), SZ_COLUNITS)
	call strcpy ("pixel", Colunits(C_PPOSP), SZ_COLUNITS)
	call strcpy ("pixel", Colunits(C_PPOSO), SZ_COLUNITS)
	call strcpy ("angstroms", Colunits(C_WAVEO), SZ_COLUNITS)
	call strcpy ("sample", Colunits(C_SPOSP), SZ_COLUNITS)
	call strcpy ("sample", Colunits(C_SPOSO), SZ_COLUNITS)
	call strcpy ("angstroms", Colunits(C_DIFFW), SZ_COLUNITS)
	call strcpy ("pixel", Colunits(C_DIFFP), SZ_COLUNITS)
	call strcpy ("sample", Colunits(C_DIFFS), SZ_COLUNITS)
	call strcpy ("", Colunits(C_INTP), SZ_COLUNITS)

	Datatype(C_LINE) = TY_DOUBLE
	Datatype(C_SPORDER) = TY_INT
	Datatype(C_PPOSP) = TY_REAL
	Datatype(C_PPOSO) = TY_REAL
	Datatype(C_WAVEO) = TY_DOUBLE
	Datatype(C_SPOSP) = TY_REAL
	Datatype(C_SPOSO) = TY_REAL
	Datatype(C_DIFFW) = TY_DOUBLE
	Datatype(C_DIFFP) = TY_REAL
	Datatype(C_DIFFS) = TY_REAL
	Datatype(C_INTP) = TY_REAL
	
	do i = 1, OT_NCOLS {
	    call strcpy ("", Colfmt(i), SZ_COLFMT)
	    Lendata(i) = 1
	}

	call tbcdef (OT_T(ot), OT_COL(ot,1), Colname(1), Colunits(1),
		     Colfmt(1), Datatype(1), Lendata(1), OT_NCOLS)

	OT_CARPOS(ot) = INDEFI
	OT_APER(ot) = EOS
	OT_GRAT(ot) = EOS
	OT_NROWS(ot) = 0

	# That's all folks.
	call tbpset (OT_T(ot), TBL_MAXPAR, 5)
	call tbtcre (OT_T(ot))
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of wid_o_new
#---------------------------------------------------------------------------
procedure wid_o_open (ot, table)

pointer	ot			# I:  The OT object.
char	table[ARB]		# I:  Name of table to open.

# Declarations
pointer	colname			# Column names.
int	i			# Generic.
pointer	sp			# Stack pointer.
pointer	sx			# Generic string.
int	tbhgti()		# Get integer-valued header parameter.
int	tbpsta()		# Get table parameter.
pointer	tbtopn()		# Open a table.

errchk	salloc, sfree, smark

begin
	call smark (sp)
	call salloc (colname, (SZ_COLNAME+1) * OT_NCOLS, TY_CHAR)
	call salloc (sx, SZ_LINE, TY_CHAR)

	# Create the column definitions.
	call strcpy ("line", Colname(C_LINE), SZ_COLNAME)
	call strcpy ("sporder", Colname(C_SPORDER), SZ_COLNAME)
	call strcpy ("pixel(pred)", Colname(C_PPOSP), SZ_COLNAME)
	call strcpy ("pixel(obs)", Colname(C_PPOSO), SZ_COLNAME)
	call strcpy ("wave(obs)", Colname(C_WAVEO), SZ_COLNAME)
	call strcpy ("sample(pred)", Colname(C_SPOSP), SZ_COLNAME)
	call strcpy ("sample(obs)", Colname(C_SPOSO), SZ_COLNAME)
	call strcpy ("diff(wave)", Colname(C_DIFFW), SZ_COLNAME)
	call strcpy ("diff(pixel)", Colname(C_DIFFP), SZ_COLNAME)
	call strcpy ("diff(sample)", Colname(C_DIFFS), SZ_COLNAME)
	call strcpy ("intensity", Colname(C_INTP), SZ_COLNAME)

	# Open table.
	OT_T(ot) = tbtopn (table, READ_WRITE, NULL)

	# Find the columns.  All columns must be there.
	call tbcfnd (OT_T(ot), Colname(1), OT_COL(ot,1), OT_NCOLS)
	do i = 1, OT_NCOLS
	    if (OT_COL(ot,i) == NULL) {
		call sprintf (Sx, SZ_LINE, "column %s missing from table %s")
		call pargstr (Colname(i))
		call pargstr (table)
		call error (1, Sx)
	    }

	# Retreive the carrousel position.
	iferr (OT_CARPOS(ot) = tbhgti (OT_T(ot), "CARPOS")) {
	    call erract (EA_WARN)
	    call error (1, "cannot access header parameter 'carpos'")
	}

	# Retreive the aperture name.
	iferr (call tbhgtt (OT_T(ot), "APERTURE", OT_APER(ot), SZ_LINE)) {
	    call erract (EA_WARN)
	    call error (1, "cannot access header parameter 'aperture'")
	}

	# Retreive the grating name.
	iferr (call tbhgtt (OT_T(ot), "GRATING", OT_GRAT(ot), SZ_LINE)) {
	    call erract (EA_WARN)
	    call error (1, "cannot access header parameter 'grating'")
	}

	# Get number of rows in table.
	OT_NROWS(ot) = tbpsta (OT_T(ot), TBL_NROWS)

	# That's all folks
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of wid_o_open
#---------------------------------------------------------------------------
