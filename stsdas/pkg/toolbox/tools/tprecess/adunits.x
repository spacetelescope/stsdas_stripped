include <tbset.h>
include "precess.h"

define	RAD_TO_DEG	57.295779513082320877d0
define	RAD_TO_HOURS	3.8197186342054880584d0

# ad_units -- get units for ra & dec

procedure ad_units (cp, ra_units, units)

pointer cp[ARB]		# i: array of pointers to column descriptors
int	ra_units	# i: default units for RA
int	units[ARB]	# o: units for RA & DEC
#--
char	colunits[SZ_COLUNITS,2]	# units for ra & dec columns from table
int	strncmp()

begin
	# Get the units for ra & dec.
	call tbcigt (cp[RA], TBL_COL_UNITS, colunits[1,RA], SZ_COLNAME)
	call tbcigt (cp[DEC], TBL_COL_UNITS, colunits[1,DEC], SZ_COLNAME)

	call strlwr (colunits[1,RA])
	call strlwr (colunits[1,DEC])

	if ((strncmp (colunits[1,RA], "hour", 4) == 0) ||
	    (strncmp (colunits[1,RA], "hr", 2) == 0)) {
	    units[RA] = PREC_HOURS
	} else if (strncmp (colunits[1,RA], "deg", 3) == 0) {
	    units[RA] = PREC_DEGREES
	} else if (strncmp (colunits[1,RA], "rad", 3) == 0) {
	    units[RA] = PREC_RADIANS
	} else {
	    units[RA] = ra_units
	}

	if (strncmp (colunits[1,DEC], "deg", 3) == 0) {
	    units[DEC] = PREC_DEGREES
	} else if (strncmp (colunits[1,DEC], "rad", 3) == 0) {
	    units[DEC] = PREC_RADIANS
	} else {
	    if (ra_units == PREC_RADIANS)
		units[DEC] = PREC_RADIANS
	    else
		units[DEC] = PREC_DEGREES
	}
end

# ad_to_rad -- convert ra or dec to radians

procedure ad_to_rad (coord, units, coord_r)

double	coord		# i: ra or dec
int	units		# i: units for coord
double	coord_r		# o: coord converted to radians
#--

begin
	if (units == PREC_RADIANS)
	    coord_r = coord
	else if (units == PREC_DEGREES)
	    coord_r = coord / RAD_TO_DEG
	else if (units == PREC_HOURS)
	    coord_r = coord / RAD_TO_HOURS
end

# ad_from_rad -- convert ra or dec from radians to whatever

procedure ad_from_rad (coord_r, units, coord)

double	coord_r		# i: coord in radians
int	units		# i: units for coord
double	coord		# o: ra or dec converted from radians
#--

begin
	if (units == PREC_RADIANS)
	    coord = coord_r
	else if (units == PREC_DEGREES)
	    coord = coord_r * RAD_TO_DEG
	else if (units == PREC_HOURS)
	    coord = coord_r * RAD_TO_HOURS
end
