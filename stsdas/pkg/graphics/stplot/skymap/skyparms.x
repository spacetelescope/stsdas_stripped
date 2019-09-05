procedure sky_parms (ra, dec, scale, size, mirror, color,
    cmstyle, cmsize, connstyle)

include <math.h>
include	"skymap.h"

double	ra, dec				# Chart center
real	scale, size			# Chart scale
bool	mirror				# Flip chart?
int	color				# Color index
int	cmstyle				# Center marker style
real	cmsize				# Marker size
int	connstyle			# Connection style

pointer	sp, stydic, instyle, outstyle

real	clgetr()
double	clgetd()
bool	clgetb()
int	clgeti(), strdic()

begin
	ra = clgetd ("racen")
	if (!IS_INDEFD(ra))
	    ra = HRSTORAD(ra)

	dec = clgetd ("deccen")
	if (!IS_INDEFD(dec))
	    dec = DEGTORAD(dec)

	scale = clgetr ("scale")
	if (!IS_INDEFR(scale))
	    scale = SATORAD(scale)

	size = clgetr ("size")
	if (!IS_INDEFR(size))
	    size = DEGTORAD(size)

	mirror = clgetb ("mirror")

	color = clgeti ("color")

	call smark (sp)

	call salloc (stydic, SZ_LINE, TY_CHAR)
	call clgstr ("cenmark.p_min", Memc[stydic], SZ_LINE)
	call salloc (instyle, SZ_LINE, TY_CHAR)
	call clgstr ("cenmark", Memc[instyle], SZ_LINE)
	call salloc (outstyle, SZ_LINE, TY_CHAR)
	cmstyle = strdic (Memc[instyle], 
	    Memc[outstyle], SZ_LINE, Memc[stydic])

	cmsize = clgetr ("cmsize")

	call clgstr ("constyle.p_min", Memc[stydic], SZ_LINE)
	call clgstr ("constyle", Memc[instyle], SZ_LINE)
	connstyle = strdic (Memc[instyle], 
	    Memc[outstyle], SZ_LINE, Memc[stydic])

	call sfree (sp)
end
