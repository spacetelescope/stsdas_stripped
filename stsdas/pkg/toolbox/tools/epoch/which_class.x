include	"epoch.h"
include	<ctype.h>

#  WHICH_CLASS -- Determine which category of time format the input string is
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  01-Aug-1990  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

procedure which_class (instr, length, informat, dmystyle, style, flag1900,
			bcflag, zone, mjd)

char	instr[SZ_LINE]		# input: time string
int	length			# input: length of the input string
int	informat		# input: input format spec
char	dmystyle[SZ_LINE]	# input: format spec for three-number input
int	style			# input: Julian or Gregorian calendar
bool	flag1900		# input: add 1900 to two digit year
double	zone			# input: time zone
bool	bcflag			# input: year is BC?
double	mjd			# output: modified Julian date

int	month, code, ip, i
double	xmjd
char	monstr[SZ_MONTH+1]
char	dummy[SZ_LINE]

int	strdic()
int	strncmp()
long	clktime()
#==============================================================================
begin

	# Category: an English word
	# WORDS are defined in epoch.h
	code = strdic (instr, dummy, length, WORDS) 
	if (code == LAUNCH) {
	    mjd = TLAUNCH
	    return
	} else if (code == DEPLOY) {
	    mjd = TDEPLOY
	    return
	} else if (code != 0) {

	    # override the calendar type
	    style = NEWSTYLE
	    xmjd = double(MJD1980) + double(clktime(0))/SECPERDAY
	    mjd = xmjd + zone/24.D0
	    if (code == TODAY)

		# mjd must be positive in this statement
		mjd = int(xmjd)
	    return
	}
		
	# Category: A qualifier is specified (in the parameter file)
	if (informat == ENG || strncmp (instr, "eng", 3) == 0) {
	    call from_eng (instr, mjd)
	    return
	} else if (informat != BLANKSPACE) {
	    call custom_parse (instr, length, informat, mjd)
	    return
	}

	# Category: A Month name appears in the input string
	# serach the input string for month notation (e.g. jan, feb,...etc.)
	ip = 0
	do i = 1, length {
	    if (IS_ALPHA(instr[i])) {
		ip = i
		break
	    }
	}
	if (ip != 0) {
	    call strcpy (instr[ip], monstr, SZ_MONTH)
	    month = strdic (monstr, dummy, SZ_MONTH, MONTH)
	    if (month != 0)
	        call month_parse (instr, month, dmystyle, style, flag1900, 
					bcflag, mjd)

	    # Category: A qualifier is specified (in the input string)
	    else if (IS_ALPHA(instr[1]))
	        call custom_parse (instr, length, informat, mjd)
	    else

		# Category: Input string has three numbers for day, month, year
	    	call three_num (instr, dmystyle, style, flag1900, bcflag, mjd)

	# Category: Input string has three numbers for day, month, year
	} else
	    call three_num (instr, dmystyle, style, flag1900, bcflag, mjd)
end
