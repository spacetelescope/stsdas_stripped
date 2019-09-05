include "epoch.h"
include	<ctype.h>
include	<ctotok.h>

#  MONTH_PARSE -- Parse the input string which contains a month name
#
#  Description:
#  ------------
#  Parse the input time string which contains an English month name (may be
#  abbreviated, e. g. Jan, Feb, etc.)
#
#  The input string is supposed to have the following pattern:
#  (a delimiter is one or more characters which is not a digit, this
#  includes space, tab, period, hyphen, slash, etc...)
#  
#  numerical day, a delimiter (may be null), month name, a delimiter (may be 
#  null), numerical year
#  Or:
#  month name, a delimiter (may be null), numerical day, a delimiter (may be 
#  null), numerical year
#  Or:
#  numerical year, a delimiter (may be null), month name, a delimiter (may be 
#  null), numerical day
#
#  The above may be followed by an optional HH:MM:SS.(decimal second).  The Hour
#  is in 24-hour system.  For example 13:24:23.346576879
#
#  Date		Author			Description
#  ----		------			-----------
#  08-Aug-1990  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

procedure month_parse (instr, month, dmystyle, style, flag1900, bcflag, mjd)

char	instr[SZ_LINE]		# input: time string
char	dmystyle[SZ_LINE]	# input: day-month-year style
int	month			# input: numerical month
int	style			# input: Julian or Gregorian calendar
bool	flag1900		# input: two digit year +1900 ?
bool	bcflag			# input: year is BC?
double	mjd			# output: modified Julian date

int	ip, yy, dd, jp, nchar, n1, n2
char	tmpstr[SZ_LINE]
double	fraday, doy, hour

int	next_num()
int	strsearch()
int	ctotok()
int	ctod()
double	yd_to_mjd()
#==============================================================================
begin

	ip = 1
	hour = INDEF

	if (next_num (instr, ip, n1) == 0)
	    call error (1, "(month_parse 10): Ambiguous input time string")

	if (next_num (instr, ip, n2) == 0)
	    call error (1, "(month_parse 20): Ambiguous input time string")

	while (!IS_DIGIT(instr[ip]) && instr[ip] != EOS)
	    ip = ip + 1
	if (ctotok (instr, ip, tmpstr, SZ_LINE) == TOK_NUMBER) {
	    jp = 1
	    nchar = ctod (tmpstr, jp, hour)
	}    

	# dmy style dictate the numbers, if month name appears first, it must be
	# mdy
	if (strsearch(dmystyle, "mdy") != 0 || 
		strsearch(dmystyle, "dmy") != 0 || IS_ALPHA(instr[1])) {
	    dd = n1
	    yy = n2
	} else if (strsearch(dmystyle, "ymd") != 0) {
	    dd = n2
	    yy = n1
	} else
	    call error (1, "(month_parse 30): input string does not make sense")

	# if the year is BC
	if (bcflag)
	    yy = 1 - yy

	# add 1900 to the year if applicable
	else if (flag1900 && yy < 100)
	    yy = yy + 1900

	# calculate fraction of the day
	call fracd (hour, instr, fraday)

	call day_of_year (yy, month, dd, fraday, doy, style)
	mjd = yd_to_mjd (yy, doy, style)
end
