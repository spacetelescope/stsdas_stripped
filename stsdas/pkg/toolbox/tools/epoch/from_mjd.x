include	"epoch.h"

# TO_YMD -- Convert MJD to numerical year, month, day
#
# Description:
# ------------
# Takes care of both Gregorian and Julian calendars.
#----------------------------------------------------------------------------
procedure to_ymd (mjd, year, month, day, doy, framjd, style)

double	mjd			# input: modified Julian date
int	year			# output: Year
int	month			# output: Month (1-12)
int	day			# output: Day of month
int	doy			# output: day of the year
double	framjd			# output: fraction of the day
int	style			# Gregorian or Julian calendar

int	bom[13]			# Beginning of month
int	bomleap[13]		# Beginning of month for leap year
data	bom/1,32,60,91,121,152,182,213,244,274,305,335,366/
data	bomleap/1,32,61,92,122,153,183,214,245,275,306,336,367/
int	diff, shift, d1, d2, d3, q1, q2, q3, q4, inmjd
bool	leap
#-----------------------------------------------------------------------------
begin

	#separate the integer and fraction parts of MJD
	inmjd = int(mjd)
	if (mjd < 0.d0)
	    inmjd = int(mjd*(1.d0-1.d-15)) - 1
	framjd = mjd - double(inmjd)

	switch (style) {

	# for Gregorian calendar (i.e. new style)
	case NEWSTYLE:
	    diff = inmjd - MJD2001
	    shift = 0

	    # make the difference > 0, so the mod function will work properly
	    if (diff < 0){
		shift = abs((diff-1) / QCENTURY) + 1
		diff = diff + shift * QCENTURY
	    }

	    # find how many 4-centuries, centuries, 4-years, and years are there
	    q1 = diff / QCENTURY
	    d1 = mod (diff, QCENTURY)
	    q2 = d1 / CENTURY
	    
	    # take care of the last day of the 400th year
	    if (q2 == 4) q2 = q2 - 1
	    d2 = d1 - q2 * CENTURY
	    q3 = d2 / QANNUM
	    d3 = d2 - q3 * QANNUM
	    q4 = d3 / ANNUM

	    # take care of the last day of the leap year
	    if (q4 == 4) q4 = q4 - 1
	    doy = d3 - q4 * ANNUM + 1

	    year = 2001 + 400*(q1-shift) + 100*q2 + 4*q3 + q4 

	    leap = (mod (year, 4) == 0 &&
	    		(mod (year, 100) != 0 || mod (year, 400) == 0))
	
	# for Julian calendar (i.e. old style)
	case OLDSTYLE:
	    diff = inmjd - MJD1201
	    shift = 0

	    # make the difference > 0, so the mod function will work properly
	    if (diff < 0){
		shift = abs((diff-1) / QANNUM) + 1
		diff = diff + shift * QANNUM
	    }

	    # find how many 4-years, and years are there
	    q3 = diff / QANNUM
	    d3 = diff - q3 * QANNUM
	    q4 = d3 / ANNUM

	    # take care of the last day of the leap year
	    if (q4 == 4) q4 = q4 - 1
	    doy = d3 - q4 * ANNUM + 1

	    year = 1201 + 4*(q3-shift) + q4

	    leap = (mod (year, 4) == 0)

	default:
	    call error (1, "unknown calendar")
	}

	# calculate the month and day
	month = 1
	if (leap) {
	    while (doy >= bomleap(month+1)) 
		month = month + 1
	    day = doy - bomleap(month) + 1
	} else {
	    while (doy >= bom(month+1)) 
		month = month + 1
	    day = doy - bom(month) + 1
	}
end
