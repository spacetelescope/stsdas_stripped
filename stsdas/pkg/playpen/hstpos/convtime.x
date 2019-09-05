include	<ctype.h>
include	<ctotok.h>
include <printf.h>
include <mach.h>
include "convtime.h"

# CONVTIME -- convert a string spacecraft clock time (DD-MMM-YYYY HH:MM:SS.SS)
#	      to a string Modified Julian Date. This routine is a reworked
#	      version of the month_parse.x which is used in the epoch task.
#
# S. Hulbert 	Aug91	Original

procedure convtime (instr, strmjd, maxchar)

char	instr[ARB]	#I: spacecraft clock time (dd-mmm-yyy hh:mm:ss.ss)
char	strmjd[ARB]	#I: mjd as a string        
int	maxchar		#I: maximum characters in strings

int	style			# input: Julian or Gregorian calendar
double	mjd			# output: modified Julian date

int	month, ip, i
int	yy, dd, jp, nchar
char	tmpstr[SZ_LINE]
char	monstr[SZ_MONTH+1]
char	dummy[SZ_LINE]
double	fraday, doy, hour

int	strdic(), next_num(), ctotok(),	ctod(), dtoc()
double	yd_to_mjd()

begin

	# look for first character of the month in the string
	ip = 0
	do i = 1, maxchar {
	    if (IS_ALPHA(instr[i])) {
		ip = i
		break
	    }
	}

	# now check dictionary for valid month
	if (ip != 0) {
	    call strcpy (instr[ip], monstr, SZ_MONTH)
	    call strlwr (monstr)
	    month = strdic (monstr, dummy, SZ_MONTH, MONTH)

	    # if we have a good month      
	    if (month != 0) {

		ip = 1
		hour = 0.d0

        	if (next_num (instr, ip, dd) == 0)
            		call error (1, "Ambiguous time string 1")

        	if (next_num (instr, ip, yy) == 0)
            		call error (1, "Ambiguous time string 2")

		while (!IS_DIGIT(instr[ip]) && instr[ip] != EOS)
	    	    ip = ip + 1
		    if (ctotok (instr, ip, tmpstr, SZ_LINE) == TOK_NUMBER) {
	   		jp = 1
	    		nchar = ctod (tmpstr, jp, hour)
		    }    

		# calculate fraction of the day
		fraday = double(hour)/24.d0 

		style = NEWSTYLE
		call day_of_year (yy, month, dd, fraday, doy, style)
		mjd = yd_to_mjd (yy, doy, style)
		nchar = dtoc(mjd, strmjd, maxchar, NDIGITS_DP, FMT_GENERAL, 
				SZ_FNAME)
	    } else {
		call error (1, "unable to convert packet time")
	    }
	} else {
	    call error (1, "unable to convert packet time")
	}

end

# DAY_OF_YEAR -- The day number for the given year is returned.
#
# Description:
# ------------
# Convert input year, month (may be any integer), day (may have fraction) to 
# year and day of the year (jan 1 = 1, Feb 1 = 32 etc.)
# Can accomodate both Julian and Gregorian calendar
#----------------------------------------------------------------------------
procedure day_of_year (year, month, day, fraday, doy, style)

int	year			# input/output: Year
int	month			# input/output: Month (1-12)
int	day			# Day of month
double	fraday			# fraction of the Day 
double	doy			# output: day of the year
int	style			# Gregorian or Julian calendar

int	bom[13]			# Beginning of month
data	bom/1,32,60,91,121,152,182,213,244,274,305,335,366/
int	mm, shift, dummy
bool	leap
#-----------------------------------------------------------------------------
begin
	# make sure the month is between 1 and 12
	mm = month
	if (mm < 1) {
	    shift = (mm - 12) / 12
	    month = mm - shift * 12
	    year = year - shift
	} else if (mm > 12) {
	    shift = (mm - 1) / 12
	    year = year + shift
	    month = mod (mm-1, 12) + 1
	}

	doy = double(bom[month] + day - 1) + fraday

	switch (style) {

	# for Gregorian calendar (i.e. new style)
	case NEWSTYLE:
	    leap = (mod (year, 4) == 0 &&
	    		(mod (year, 100) != 0 || mod (year, 400) == 0))
	
	# for Julian calendar (i.e. old style)
	case OLDSTYLE:
	    leap = (mod (year, 4) == 0) 

	default:
	    call error (1, "unknown calendar style")
	}
	if (month > 2 && leap)
	    doy = doy + 1.d0

	# issue caution message if unusual month/day appears
	if (mm > 12 || mm < 1) {
	    call printf ("CAUTION: unusual input month %d\n")
		call pargi (mm)
	}
	dummy = bom[month+1]
	if (month == 2 && leap)
	    dummy = dummy + 1
	if (day > dummy - bom[month] || day < 1) {
	    call printf ("CAUTION: unusual input day %d\n")
		call pargi (day)
	}
	if (fraday > 1.d0 || fraday < 0.d0)
	    call printf ("CAUTION: unusual input hour\n")
end

# YD_TO_MJD -- convert year and day of the year to modified Julian date

double procedure yd_to_mjd (year, doy, style)

int	year			# Year
double	doy			# Day of month
int	style			# Gregorian or Julian calendar

int	shift, year0, dy, fourcent, cent, fouryear, yy, r1, r2
double	mjd
#-------------------------------------------------------------------------------
begin
 	# make sure the year is within limit
	if (abs(year) > MAX_YEAR)
	    call error (1, "year out of range")

	switch (style) {

	# for Gregorian calendar (i.e. new style)
	case NEWSTYLE:
	    year0 = 2001
	    shift = 0

	    # only deal with positive year difference
	    dy = year - year0
	    if (dy < 0) 
	        shift = (dy - 399) / 400
	
	    dy = dy - shift*400
	    mjd = MJD2001 + shift * QCENTURY
	    
	    # find how many 4-centuries, centuries, 4-years, and years are there
	    fourcent = dy / 400
	    r1 = mod(dy, 400)
	    cent = r1 / 100
	    r2 = mod(r1, 100)
	    fouryear = r2 / 4
	    yy = mod(r2, 4)

	    # subtract one because MJD2001 is Jan 1 0h
	    mjd = mjd + fourcent * QCENTURY + cent * CENTURY + 
			fouryear * QANNUM + yy * ANNUM + doy - 1	

	# for Julian calendar (i.e. old style)
	case OLDSTYLE:
	    year0 = 1201
	    shift = 0

	    # only deal with positive year difference
	    dy = year - year0
	    if (dy < 0) 
	        shift = (dy - 3) / 4
	
	    dy = dy - shift*4
	    mjd = MJD1201 + shift * QANNUM
	    fouryear = dy / 4
	    yy = mod(dy, 4)

	    # subtract one because MJD1201 is Jan 1 0h
	    mjd = mjd + fouryear * QANNUM + yy * ANNUM + doy - 1	

	default:
	    call error (1, "unknown calendar style")
	}
	return (mjd)
end

#  NEXT_NUM --  Get the next group of digits and convert them to an integer
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  01-Aug-1990  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

int procedure next_num (str, ip, ival)

char	str[SZ_LINE]		# input: input character string
int	ip			# input: index of the string to be searched
int	ival			# output: numerical value of the digit group

int	ip_start, k, lp, nchar
char	tmp[SZ_LINE]

int	ctoi()
#==============================================================================
begin

	ival = 0
	while (!(IS_DIGIT(str[ip])) && str[ip] != EOS)
	    ip = ip + 1
	ip_start = ip
	while (IS_DIGIT(str[ip]))
	    ip = ip + 1

	k = ip - ip_start
	lp = 1
	if (k != 0) {
	    call strcpy (str[ip_start], tmp, k)
	    nchar = ctoi (tmp, lp, ival)
	}
	return (k)
end
