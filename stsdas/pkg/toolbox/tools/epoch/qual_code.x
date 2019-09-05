include "epoch.h"
include	<ctype.h>

#  QUAL_CODE -- Parse the input string to determine the qualifier
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  01-Aug-1990  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

procedure qual_code (instr, code)

char	instr[SZ_LINE]		# input: time string
int	code			# output: format qualifier code

int	strncmp()
int	strlen()
#==============================================================================
begin

	if (strlen(instr) == 0) {
	    code = BLANKSPACE
	    return
	}
	if (strncmp(instr, "mjd", 3) == 0 && !IS_ALPHA(instr[4]))
	    code = MJD
	else if (strncmp(instr, "jd", 2) == 0 && !IS_ALPHA(instr[3]))
	    code = JD
	else if (strncmp(instr, "cdbs", 4) == 0 && !IS_ALPHA(instr[5]))
	    code = CDBS 
	else if (strncmp(instr, "geisut", 6) == 0 && !IS_ALPHA(instr[7]))
	    code = GEISUT
	else if (strncmp(instr, "dmf", 3) == 0 && !IS_ALPHA(instr[4]))
	    code = DMF
	else if (strncmp(instr, "epchtime", 8) == 0 && !IS_ALPHA(instr[9]))
	    code = EPCHTIME

	# same as epchtime
	else if (strncmp(instr, "timeffec", 8) == 0 && !IS_ALPHA(instr[9]))
	    code = EPCHTIME
	else if (strncmp(instr, "obsstrtt", 8) == 0 && !IS_ALPHA(instr[9]))
	    code = OBSSTRTT
	else if (strncmp(instr, "eng", 3) == 0 )
	    code = ENG
	else if (strncmp(instr, "sms", 3) == 0 && !IS_ALPHA(instr[4]))
	    code = SMS
	else if (strncmp(instr, "j", 1) == 0 && !IS_ALPHA(instr[2]))
	    code = JULIAN
	else if (strncmp(instr, "b", 1) == 0 && !IS_ALPHA(instr[2]))
	    code = BESSEL
	else {
	    call eprintf (
		"(qual_code 10): Unknown qualifier or vague input string %s\n")
		call pargstr (instr)
	    call error (1, "")
	}
end
