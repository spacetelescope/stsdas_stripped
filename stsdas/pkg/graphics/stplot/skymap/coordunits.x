procedure coord_units (coord, numrows, units)

include <math.h>
include <tbset.h>

double	coord[ARB]		# Coordinate values
int	numrows			# Number of values
char	units[SZ_COLUNITS]	# Catalog table column units string

double	const

int	strmatch()

begin
	# Assume degrees be default
	const = 1.0 / RADIAN

	if (strmatch (units, "{h}") > 0)
	    # Hours
	    const = 15.0 / RADIAN

	# Convert to radians
	call amulkd (coord, const, coord, numrows)
end
