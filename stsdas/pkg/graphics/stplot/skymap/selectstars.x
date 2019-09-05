int procedure select_stars (tblrow, raval, decval, magval, 
	class, names, namsiz, isnull, numrows, 
	faint, bright, ramin, decmin, ramax, decmax)

include <math.h>
include	"skymap.h"

int	tblrow[numrows]			# Catalog table row
double	raval[numrows]			# R.A. Values
double	decval[numrows]			# Dec. values
real	magval[numrows]			# Mag. values
int	class[numrows]			# Ojbect classes
char	names[namsiz,numrows]		# Object names
int	namsiz				# Names column width
bool	isnull[numrows,NUM_COLS]	# Null entry
int	numrows				# Number of input table rows
real	faint, bright			# Magnitude limits
double	ramin, decmin, ramax, decmax	# Coordinate limits

int	row
int	out
double	rminr, rmind, rmaxr, rmaxd	# Limits in radians

int	valid_pos()

begin
	out = 0

	# Make sure coordinate limits are in radians
	if (IS_INDEFD (ramin))
	    rminr = ramin
	else
	    rminr = HRSTORAD(ramin)

	if (IS_INDEFD (ramax))
	    rmaxr = ramax
	else
	    rmaxr = HRSTORAD(ramax)

	if (IS_INDEFD (decmin))
	    rmind = decmin
	else
	    rmind = DEGTORAD(decmin)

	if (IS_INDEFD (decmax))
	    rmaxd = decmax
	else
	    rmaxd = DEGTORAD(decmax)

	do row = 1, numrows {
	    # For each table row;  read columns
	    if (valid_pos (raval[row],  isnull[row,RA_COL], 
			   decval[row], isnull[row,DEC_COL], 
			   magval[row], isnull[row,MAG_COL], 
			   faint, bright, 
			   rminr, rmind, rmaxr, rmaxd) == YES) {
		out = out + 1
		tblrow[out] = row
		raval[out]  = raval[row]
		decval[out] = decval[row]
		magval[out] = magval[row]
		if (isnull[row,CLS_COL])
		    class[out] = 0
		else
		    class[out] = class[row]
		if (isnull[row,NAM_COL])
		    names[1,out] = EOS
		else
		    call strcpy (names[1,row], names[1,out], namsiz)
	    }
	}

	return (out)
end
