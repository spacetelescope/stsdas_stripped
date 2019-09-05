include	"epoch.h"
include	<ctype.h>

#  from_eng -- decode the engineering time format string
#
#  Description:
#  ------------
#  The engineering string has the following format:
#  YMDhhmm, where Y is the year (range from 1 to Z, where 1 = 1981, A = 1990),
#  M is the month (range from 1 to C, where 1 = January, A = October), D is
#  the day (range from 1 to V, where 1 = 1, V = 31), hh is the hour (range
#  from 00 to 24), mm is the minute (range from 00 to 60).
#
#  Date		Author			Description
#  ----		------			-----------
#  24-Sep-1990  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

procedure from_eng (instr, mjd)

char	instr[SZ_LINE]		# input: time string
double	mjd			# output: modified Julian date

int	j
int	yy, mm, day
int	hh, minute
double	fraday, doy

int	strncmp()
double	yd_to_mjd()
#==============================================================================
begin

	# if the first characters are "eng", strip them and the following blanks
	j = 1
	if (strncmp(instr, "eng", 3) == 0) {
	    j = 4
	    while (instr[j] != EOS && IS_WHITE(instr[j])) 
		j = j + 1
	}

	# get the year character
	if (instr[j] >= '1' && instr[j] <= '9')
	    yy = 1981 + (instr[j]-'1')
	else if (instr[j] >= 'a' && instr[j] <= 'z')
	    yy = 1990 + (instr[j]-'a')
	else 
	    call error (1, "illegal year")

	# get the month character
	if (instr[j+1] >= '1' && instr[j+1] <= '9')
	    mm = 1 + (instr[j+1]-'1')
	else if (instr[j+1] >= 'a' && instr[j+1] <= 'c')
	    mm = 10 + (instr[j+1]-'a')
	else 
	    call error (1, "illegal month")

	# get the day character
	if (instr[j+2] >= '1' && instr[j+2] <= '9')
	    day = 1 + (instr[j+2]-'1')
	else if (instr[j+2] >= 'a' && instr[j+2] <= 'v')
	    day = 10 + (instr[j+2]-'a')
	else 
	    call error (1, "illegal day")

	# get the hour characters
	if (instr[j+3] >= '0' && instr[j+3] <= '2' &&
		instr[j+4] >= '0' && instr[j+4] <= '9') {
	    hh = (instr[j+3]-'0')*10 + (instr[j+4]-'0')

	    # get the minute characters
	    if (instr[j+5] >= '0' && instr[j+5] <= '6' &&
			instr[j+6] >= '0' && instr[j+6] <= '9')
	        minute = (instr[j+5]-'0')*10 + (instr[j+6]-'0')
	    else if (instr[j+5] == EOS || IS_WHITE(instr[j+5]))
		minute = 0
	    else
		call error (1, "illegal minute")
	} else if (instr[j+3] == EOS || IS_WHITE(instr[j+3]))
	    hh = 0 
	else 
	    call error (1, "illegal hour")

	fraday = (double(hh) + double(minute)/60.d0)/24.d0
	call day_of_year (yy, mm, day, fraday, doy, NEWSTYLE)
	mjd = yd_to_mjd (yy, doy, NEWSTYLE)
end

#  to_eng -- encode the engineering time format string
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  24-Sep-1990  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

procedure to_eng (year, month, day, fraday, outstr)

int	year			# input: year
int	month			# input: month
int	day			# input: day
double	fraday			# input: fraction of a day
char	outstr[SZ_LINE]		# ouput: time string

int	hh, minute
double	hour
#==============================================================================
begin
	# year character
	if (year >= 1981 && year <= 1989)
	    outstr[1] = '1' + (year-1981)
	else if (year >= 1990 && year <= 2015)
	    outstr[1] = 'A' + (year-1990)
	else
	    outstr[1] = '*'

	# month character
	if (month >= 1 && month <= 9)
	    outstr[2] = '1' + (month-1)
	else if (month >= 10 && month <= 12)
	    outstr[2] = 'A' + (month-10)
	else
	    outstr[2] = '*'

	# day character
	if (day >= 1 && day <= 9)
	    outstr[3] = '1' + (day-1)
	else if (day >= 10 && day <= 31)
	    outstr[3] = 'A' + (day-10)
	else
	    outstr[3] = '*'

	hour = fraday*24.d0
	hh = int (hour)
	minute = int ((hour-double(hh))*60.d0+1.d-8)

	# hour/minute characters
	outstr[4] = '0' + hh/10
	outstr[5] = '0' + mod(hh, 10)
	outstr[6] = '0' + minute/10
	outstr[7] = '0' + mod(minute, 10)
	outstr[8] = EOS
end
