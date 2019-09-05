include "epoch.h"
include	<ctype.h>
include	<ctotok.h>

#  THREE_NUM -- Parses the input string of numerical month, day, and year
#
#  Description:
#  ------------
#  Parses the input time string which contains at least three numbers
#
#  The input string is supposed to have the following pattern:
#  (a delimiter is one or more characters which is not a digit, this
#  includes space, tab, period, hyphen, slash, etc...)
#  Either:
#  numerical day, a delimiter, numerical month, a delimiter, numerical year
#  Or:
#  numerical month, a delimiter, numerical day, a delimiter, numerical year
#  Or:
#  numerical year, a delimiter, numerical month, a delimiter, numerical day
#
#  The above may be followed by an optional HH:MM:SS.(decimal second).  The Hour
#  is in 24-hour system.  For example 13:24:23.346576879
#
#  Date		Author			Description
#  ----		------			-----------
#  08-Aug-1990  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

procedure three_num (instr, dmystyle, style, flag1900, bcflag, mjd)

char	instr[SZ_LINE]		# input: time string
char	dmystyle[SZ_LINE]	# input: day month year style string
int	style			# input: Julian or Gregorian calendar
bool	flag1900		# input: add 1900 to two digit year?
bool	bcflag			# input: year is BC?
double	mjd			# output: modified Julian date

int	ip, yy, dd, jp, nchar, n1, n2, n3
int	month			# numerical month
char	tmpstr[SZ_LINE]
double	fraday, doy, hour

int	next_num()
int	ctotok()
int	ctod()
int	strsearch()
double	yd_to_mjd()
#==============================================================================
begin

	ip = 1
	hour = INDEF

	if (next_num (instr, ip, n1) == 0)
	    call error (1, "(three_num 10): Ambiguous input time string")

	if (next_num (instr, ip, n2) == 0)
	    call error (1, "(three_num 20): Ambiguous input time string")

	if (next_num (instr, ip, n3) == 0)
	    call error (1, "(three_num 30): Ambiguous input time string")

	# decide which is which
	if (strsearch(dmystyle, "mdy") != 0) {
	    month = n1
	    dd = n2
	    yy = n3
	} else if (strsearch(dmystyle, "dmy") != 0) {
	    month = n2
	    dd = n1
	    yy = n3
	} else if (strsearch(dmystyle, "ymd") != 0) {
	    month = n2
	    dd = n3
	    yy = n1
	} else
	    call error (1, "(three_num 40): input string does not make sense")

	while (!IS_DIGIT(instr[ip]) && instr[ip] != EOS)
	    ip = ip + 1
	if (ctotok (instr, ip, tmpstr, SZ_LINE) == TOK_NUMBER) {
	    jp = 1
	    nchar = ctod (tmpstr, jp, hour)
	}    

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
