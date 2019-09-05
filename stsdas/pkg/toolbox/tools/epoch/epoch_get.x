include	"epoch.h"
include	<ctype.h>

#  EPOCH_GET -- Read CL parameters for the task EPOCH.
#
#  Description:
#  ------------
#  Read CL parameters and do necessary checking and conversions.
#  
#  Input CL parameters:
#  -----------------
#
#  "instring"		Input time string
#  "qualifier"		Input string format qualifier
#  "dmy_style"		day month year format style
#  "timezone"		Time zone relative to GMT
#  "add1900"		Add 1900 to the two digit year?
#  "calendar"		Julian or Gregorian calendar for the input time 
#  "printout"		print out selections
#
#  Date		Author			Description
#  ----		------			-----------
#  01-Aug-1990  J.-C. Hsu		design and code
#------------------------------------------------------------------------------

procedure epoch_get (instr, length, qualifier, dmystyle, zone, style, flag1900,
			bcflag, printout)

char	instr[SZ_LINE]		# output: input time string
int	length			# output: length of the input string
int	qualifier		# output: input format qualifier code
char	dmystyle[SZ_LINE]	# output: day month year format style
double	zone			# output: time zone
int	style			# output: Julian or Gregorian calendar
bool	flag1900		# output: add 1900 to two digit year
bool	bcflag			# output: year is BC?
char	printout[SZ_LINE]	# output: output selections

char	zonestr[SZ_LINE]	# input time zone string
char	rawstr[SZ_LINE]
char	stylestr[SZ_LINE]
char	qualstr[SZ_LINE]
char	tmpstr[SZ_LINE]
int	i, j, k, ip

int	strlen()
int	strsearch()
int	ctod()
bool	clgetb()
#==============================================================================
begin

	# read the input time string, convert to lower case, and determine 
	# its length
	call clgstr ("instring", rawstr, SZ_LINE)
	call strlwr (rawstr)

	# get rid of any trailing blanks (and tabs)
	j = strlen (rawstr)
	while (IS_WHITE(rawstr[j]) && j > 0)
	    j = j - 1
	rawstr[j+1] = EOS

	# get rid of any leading blanks (and tabs)
	i = 1
	while (IS_WHITE(rawstr[i]))
	    i = i + 1

	length = j - i + 1
	if (length <= 0)
	    call error (1, "(epoch_get 10): null input string")
	call strcpy (rawstr[i], instr, length)

	# read the time zone string
	call clgstr ("timezone", zonestr, SZ_LINE)
	call strlwr (zonestr)

	ip = 1
	    
	# if the time zone is not a number, interpret the string
	if (ctod(zonestr, ip, zone) == 0) {
	    if (strsearch (zonestr, "est") != 0)
		zone = EST	
	    else if (strsearch (zonestr, "edt") != 0)
		zone = EDT
	    else if (strsearch (zonestr, "cst") != 0)
		zone = CST
	    else if (strsearch (zonestr, "cdt") != 0)
		zone = CDT
	    else if (strsearch (zonestr, "mst") != 0)
		zone = MST
	    else if (strsearch (zonestr, "mdt") != 0)
		zone = MDT
	    else if (strsearch (zonestr, "pst") != 0)
		zone = PST
	    else if (strsearch (zonestr, "pdt") != 0)
		zone = PDT
	    else if (strsearch (zonestr, "gmt") != 0)
		zone = GMT
	    else
		call error (1, "(epoch_get 20): unrecognizable time zone")
	}

	# input calendar style
	call clgstr ("calendar", stylestr, SZ_LINE)
	if (strsearch (stylestr, "ns") != 0)
	    style = NEWSTYLE
	else if (strsearch (stylestr, "os") != 0)
	    style = OLDSTYLE
	else
	    call error (1, "(epoch_get 30): illegal calendar style")

	# read the input format qualifier string
	call clgstr ("qualifier", rawstr, SZ_LINE)
	call strlwr (rawstr)

	# get rid of any leading blanks (and tabs)
	i = 1
	while (IS_WHITE(rawstr[i]))
	    i = i + 1

	call strcpy (rawstr[i], qualstr, strlen(rawstr)-i+1)
	call qual_code (qualstr, qualifier)

	# input the day-month-year style
	call clgstr ("dmy_style", dmystyle, SZ_LINE)
	call strlwr (dmystyle)

	# read the add 1900 flag
	flag1900 = clgetb ("add1900")

	# search for BC pattern
	bcflag = (strsearch (instr, "bc") != 0 || 
		  strsearch (instr, "b.c") != 0 ||
		  strsearch (instr, "b. c") != 0)

	# read printout parameter, and prepare it such that separaters are 
	# commas, and the string has a comma at the end
	call clgstr ("printout", tmpstr, SZ_LINE)
	call strlwr (tmpstr)
	
	k = 1
	do j = 1, strlen(tmpstr) {
#	    if (!IS_WHITE(tmpstr[j])) {
	    	k = k + 1
		if (IS_ALPHA(tmpstr[j]))
	    	    printout[k] = tmpstr[j]
		else
		    printout[k] = ','
#	    }
	}
	printout[1] = ','
	printout[k+1] = ','
	printout[k+2] = EOS
end
