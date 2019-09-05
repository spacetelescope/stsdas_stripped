procedure rad_dms (arcrad, dms, units, maxch)

# Format declination in degrees, minutes, and seconds as two strings
# containing the numerical values and the units.

include <math.h>
include	"skymap.h"

double	arcrad
char	dms[ARB]
char	units[ARB]
int	maxch

int	d, m, s
char	cd[3], cm[3], cs[3]
int	sec

define	NDEC	0

begin
	units[1] = EOS
	dms[1]   = EOS

	# Integer seconds of arc
	sec = nint (RADTOSA(arcrad))

	# Sign of declination
	if (sec > 0)
	    # North
	    call strcpy ("+", dms, maxch)
	else if (sec < 0) {
	    # South
	    call strcpy ("-", dms, maxch)
	    sec = -sec
	} else {
	    # Zero declination
	    call strcpy ("0 ", dms, maxch)
	    call strcpy (" o", units, maxch)
	    return
	}

	# Separate fields
	s = mod (sec, 60)
	m = mod (sec / 60, 60)
	d = sec / 3600

	# Degrees
	if (d > 0) {
	    # Non-zero
	    call sprintf (cd, 3, "%02d ")
		call pargi (d)
	    call strcat (cd, dms, maxch)
	} else {
	    call strcat ("00 ", dms, maxch)
	}

	call strcpy ("   o", units, maxch)

	# Minutes of arc
	if (m > 0 || s > 0) {
	    call sprintf (cm, 3, "%02d\'")
		call pargi (m)
	    call strcat (cm, dms, maxch)
	    call strcat ("   ", units, maxch)
	}

	# Seconds of arc
	if (s > 0) {
	    call sprintf (cs, 3, "%02d\"")
		call pargi (s)
	    call strcat (cs, dms, maxch)
	    call strcat ("   ", units, maxch)
	}
end
