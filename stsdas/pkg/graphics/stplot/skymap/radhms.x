procedure rad_hms (rarad, hms, units, maxch)

# Format right ascension in hours, minutes, and seconds as two strings
# containing the numerical values and the units.

include <math.h>
include	"skymap.h"

double	rarad
char	hms[ARB]
char	units[ARB]
int	maxch

int	sec
int	h, m
real	s
char	ch[5], cm[5], cs[5]

begin
	units[1] = EOS
	hms[1]   = EOS

	if (rarad == 0.0) {
	    # 0 hours RA
	    call strcpy ("0 ", hms,   maxch)
	    call strcpy (" h", units, maxch)
	    return
	}

	# Seconds of time
	sec = nint (RADTOST(rarad))

	# Range:  0 to 24 hours
	if (sec < 0)
	    sec = sec + STPERDAY
	else if (sec > STPERDAY)
	    sec = mod (sec, STPERDAY)

	# Separater fields
	s = mod (sec, 60)
	m = mod (sec / 60, 60)
	h = sec / 3600

	# Format fields
	if (h > 0) {
	    # Non-zero hours
	    call sprintf (ch, 5, " %02d ")
		call pargi (h)
	    call strcat (ch, hms, maxch)
	} else {
	    call strcat (" 00 ", hms, maxch)
	}

	call strcat ("   h", units, maxch)


	if (m > 0 || s > 0) {
	    # Minutes
	    call sprintf (cm, 5, "%02d ")
		call pargi (m)
	    call strcat (cm, hms, maxch)
	    call strcat ("  m", units, maxch)
	}

	if (s > 0) {
	    # Seconds
	    call sprintf (cs, 5, "%02d ")
		call pargr (s)
	    call strcat (cs, hms, maxch)
	    call strcat ("  s", units, maxch)
	}
end
