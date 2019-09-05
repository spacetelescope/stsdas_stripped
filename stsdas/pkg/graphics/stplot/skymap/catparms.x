procedure cat_parms (catalog, colnames, maxch, minrow, maxrow,
	faint, bright, ramin, decmin, ramax, decmax)

include <tbset.h>
include	"skymap.h"

char	catalog[ARB]				# Catalog table name
char	colnames[SZ_COLNAME,NUM_COLS]		# Column names
int	maxch
int	minrow, maxrow				# Range of table rows
real	faint, bright				# Magnitude limits
double	ramin, decmin, ramax, decmax		# Coordinate limits

real	rtemp
double	dtemp

int	clgeti()
real	clgetr()
double	clgetd()

begin
	call clgstr ("catalog",  catalog, maxch)
	call clgstr ("racol",    colnames[1,RA_COL],  maxch)
	call clgstr ("deccol",   colnames[1,DEC_COL], maxch)
	call clgstr ("magcol",   colnames[1,MAG_COL], maxch)
	call clgstr ("namecol",  colnames[1,NAM_COL], maxch)
	call clgstr ("classcol", colnames[1,CLS_COL], maxch)

	minrow = clgeti ("minrow")
	maxrow = clgeti ("maxrow")

	bright = clgetr ("brightlim")
	faint  = clgetr ("faintlim")

	if (!IS_INDEF(faint) && !IS_INDEF(bright) && faint < bright) {
	    # Switch faint and bright magnitude limits
	    rtemp  = faint
	    faint  = bright
	    bright = rtemp
	}

	ramin  = clgetd ("ramin")
	decmin = clgetd ("decmin")
	ramax  = clgetd ("ramax")
	decmax = clgetd ("decmax")

	if (IS_INDEFD(ramin))
	    ramin =    0.0d0
	if (IS_INDEFD(ramax))
	    ramax =   24.0d0
	if (IS_INDEFD(decmin))
	    decmin = -90.0d0
	if (IS_INDEFD(decmax))
	    decmax = +90.0d0

#	if (ramax < ramin) {
#	    # Switch R.A. limits
#	    dtemp = ramax
#	    ramax = ramin
#	    ramin = dtemp
#	}

	if (decmax < decmin) {
	    # Switch Dec. limits
	    dtemp  = decmax
	    decmax = decmin
	    decmin = dtemp
	}
end
