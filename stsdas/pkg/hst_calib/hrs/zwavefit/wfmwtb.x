include	<tbset.h>
include	"wf.h"

# Column definitions.
define	NCOLS		5
define	SPORDER		1
define	WAVE		2
define	OSAM		3
define	CSAM		4
define	RES		5

# Memory management.
define	Col		Memi[col+$1-1]
define	Colname		Memc[colname+(SZ_COLNAME+1)*($1-1)]
define	Colunits	Memc[colunits+(SZ_COLUNITS+1)*($1-1)]
define	Colfmt		Memc[colfmt+(SZ_COLFMT+1)*($1-1)]
define	Datatype	Memi[datatype+$1-1]
define	Lendata		Memi[lendata+$1-1]
define	Sx		Memc[sx]

#---------------------------------------------------------------------------
.help wf_m_wtab 24Apr95 source
.ih
NAME
wf_m_wtab -- Write out fit model/residuals.
.endhelp
#---------------------------------------------------------------------------
procedure wf_m_wtab (fname, grat, aper, carpos, x1, x2, y, yc, res, ndata)

char	fname[ARB]		# I:  Table name.
int	grat			# I:  Grating id.
int	aper			# I:  Aperture id.
int	carpos			# I:  Carrousel position.
double	x1[ndata]		# I:  X1 data.
double	x2[ndata]		# I:  X2 data.
double	y[ndata]		# I:  Input Y data.
double	yc[ndata]		# I:  Calculated Y data.
double	res[ndata]		# I:  Residuals.
int	ndata			# I:  Number of data points.

# Declarations.
pointer	col			# Column descriptors
pointer	colfmt			# Formats.
pointer	colname			# column names.
pointer	colunits		# Units.
pointer	datatype		# Data type.
int	i			# Generic.
pointer	lendata			# Length of data.
pointer	sp			# Stack pointer
int	strlen()		# String length.
pointer	sx			# Generic string.
pointer	t			# Table descriptor.
pointer	tbtopn()		# Open a table.
int	word_find()		# Find word in dictionary.

errchk	tbcdef, tbcptd, tbpset, tbtcre, tbtopn

begin
	# If no name is given, return.
	if (strlen (fname) <= 0)
	    return

	call smark (sp)
	call salloc (col, NCOLS, TY_INT)
	call salloc (colname, (SZ_COLNAME+1) * NCOLS, TY_CHAR)
	call salloc (colunits, (SZ_COLUNITS+1) * NCOLS, TY_CHAR)
	call salloc (colfmt, (SZ_COLFMT+1) * NCOLS, TY_CHAR)
	call salloc (datatype, NCOLS, TY_INT)
	call salloc (lendata, NCOLS, TY_INT)
	call salloc (sx, SZ_LINE, TY_CHAR)
	
	# Open the table.
	t = tbtopn (fname, NEW_FILE, NULL)
	
	# Define the columns.
	call strcpy ("sporder", Colname(SPORDER), SZ_COLNAME)
	call strcpy ("wave", Colname(WAVE), SZ_COLNAME)
	call strcpy ("sample(obs)", Colname(OSAM), SZ_COLNAME)
	call strcpy ("sample(calc)", Colname(CSAM), SZ_COLNAME)
	call strcpy ("residual", Colname(RES), SZ_COLNAME)

	do i = 1, NCOLS {
	    call strcpy ("", Colunits(i), SZ_COLUNITS)
	    Datatype(i) = TY_DOUBLE
	    call strcpy ("", Colfmt(i), SZ_COLFMT)
	    Lendata(i) = 1
	}

	call tbcdef (t, Col(1), Colname(1), Colunits(1), Colfmt(1),
		     Datatype(1), Lendata(1), NCOLS)
	call tbpset (t, TBL_ALLROWS, ndata)
	call tbpset (t, TBL_MAXPAR, 3)
	call tbtcre (t)

	# Write the header parameters
	i = word_find (grat, GRATING_DICT, Sx, SZ_LINE)
	call tbhadt (t, "grating", Sx)
	i = word_find (aper, APER_DICT, Sx, SZ_LINE)
	call tbhadt (t, "aperture", Sx)
	call tbhadi (t, "carpos", carpos)

	# Write the data.
	call tbcptd (t, Col(SPORDER), x1, 1, ndata)
	call tbcptd (t, Col(WAVE), x2, 1, ndata)
	call tbcptd (t, Col(OSAM), y, 1, ndata)
	call tbcptd (t, Col(CSAM), yc, 1, ndata)
	call tbcptd (t, Col(RES), res, 1, ndata)

	# That's all folks.
	call tbtclo (t)
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of wf_m_wtab.
#---------------------------------------------------------------------------
