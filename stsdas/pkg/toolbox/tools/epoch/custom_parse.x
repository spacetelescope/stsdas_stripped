include "epoch.h"
include	<ctype.h>
include	<ctotok.h>

#  CUSTOM_PARSE -- Parse the input string with a qualifier
#
#  Description:
#  ------------
#
#  Date		Author			Description
#  ----		------			-----------
#  01-Aug-1990  J.-C. Hsu		Design and coding
#------------------------------------------------------------------------------

procedure custom_parse (instr, length, informat, mjd)

char	instr[SZ_LINE]		# input: time string
int	length			# input: length of the input string
int	informat		# input: input format spec
double	mjd			# output: modified Julian date

int	i, ip, jp, nchar, code, nletter
int	ival1, ival2
int	yy, mm, day
int	n1, n2, n3
double	fraday, doy, hour
double	dval
char	tmpstr[SZ_LINE]
char	tokstr[SZ_LINE]
char	outstr[SZ_LINE]

int	ctotok()
int	ctod()
int	ctoi()
int	next_num()
double	yd_to_mjd()
#==============================================================================
begin

	# if the first character is an alphabet, determine the qualifier
	if (IS_ALPHA(instr[1])) {
	    call qual_code (instr, code)

	    # if the qualifier is also specified by the CL parameter, 
	    # check consistency
	    if (informat != BLANKSPACE) {
	    	if (code != informat)
		    call error (1, "(custom_parse 10): inconsistent qualifiers")
	    }
	} else {
	    if (informat != BLANKSPACE)
		code = informat
	    else
		call error (1, "(custom_parse 20): vague input string")
	}

	# strip the leading letters, if there is any
	nletter = 0
	do i = 1, length {
	    if (IS_ALPHA(instr[i]))
		nletter = nletter + 1
	    else 
		break
	}

	# copy the rest of the string to another string
	call strcpy (instr[nletter+1], tmpstr, length-nletter)

	# parse the stripped string
	ip = 1

	# the following format should have one and only one number following the
	# qualifier
	if (code == MJD || code == JD || code == DMF || code == JULIAN || 
		code == BESSEL || code == EPCHTIME || code == OBSSTRTT) {
	    if (ctotok (tmpstr, ip, outstr, length-nletter) != TOK_NUMBER)
		call error (1, 
		    "(custom_parse 30): input time string does not make sense")
	    
	    jp = 1
	    nchar = ctod (outstr, jp, dval)
	    switch (code) {
	    case MJD:
		mjd = dval
	    case JD:
		mjd = dval - JDMJD
	    case DMF:
		mjd = dval/SECPERDAY + double(MJD1980)
	    case EPCHTIME:
		mjd = dval/SECPERDAY + double(MJD850101)
	    case OBSSTRTT:
		mjd = dval/SECPERDAY + double(MJD850100)

	    # Julian epoch, see Astronomical Almanac 1989, P. B4
	    # Juilan epoch = J[2000.0 + (JD-24545.0)/365.25]
	    case JULIAN:
		mjd = (dval-2000d0) * 365.25d0 + 51544.5d0	    

	    # Besselian epoch, see Astronomical Almanac 1989, P. B4
	    # Besselian epoch = B[1900.0 + (JD-2415020.31352)/365.242198781]
	    case BESSEL:
		mjd = (dval-1900d0) * 365.242198781d0 + 15019.81352d0
	    default:
		call error (1, "(custom_parse 40): unknown qualifier")
	    }

	# the UTCO in GEIS file, it needs two arguments, they are the two
	# integer representation of the VMS 64-bit time format
	# the first argument is UTCO1 (byte 5-8 of UTCO) and the second is 
	# UTCO2 (byte 1-4 of UTCO)  Either of the two arguments can be negative
	# so the minus sign is significant, i.e. can not be used as a separator.
	} else if (code == GEISUT){
	    if (ctotok (tmpstr, ip, outstr, length-nletter) != TOK_NUMBER)
		call error (1, 
		    "(custom_parse 50): input time string does not make sense")
	    jp = 1
	    nchar = ctoi (outstr, jp, ival1)
	    if (ctotok (tmpstr, ip, outstr, length-nletter) != TOK_NUMBER)
		call error (1, 
		    "(custom_parse 60): input time string does not make sense")
	    jp = 1
	    nchar = ctoi (outstr, jp, ival2)

	    # VMS 64-bit time format
	    if (ival1 < 0) ival2 = ival2 + 1
	    mjd = (double(ival2)*2.d0**32 + double(ival1)) / 8.64d11
	
	# CDBS format yyyymmdd:hhmmss.ss characters after the : (including :) 
	# are optional.  Always in Gregorian calendar.
	} else if (code == CDBS) {
	    if (next_num (instr, ip, n1) == 0)
	 	call error (1, 
		    "(custom_parse 70): input string does not make sense")
	    if (next_num (instr, ip, n2) != 0) {
		nchar = next_num (instr, ip, n3)
		hour = ((((double(mod(n2, 100)) + double(n3)/100.d0) / #sec
			60.d0) + double(mod(n2, 10000)/100)) /		# min
			60.d0) + double(n2/10000)			# hour
	    } else 
		hour = 0.d0
	    yy = n1 / 10000
	    mm = mod (n1, 10000) / 100
	    day = mod(n1, 100)
	    fraday = hour/24.d0
	    call day_of_year (yy, mm, day, fraday, doy, NEWSTYLE)
	    mjd = yd_to_mjd (yy, doy, NEWSTYLE)

	# SMS case, yyyy.ddd:hh:mm:ss, where ddd is the number of days since 
	# Jan. 0, i.e. Jan2 is 002.  Always in Gregorian calendar.
	} else if (code == SMS) {
	    hour = 0.d0
	    if (next_num (instr, ip, yy) == 0)
	 	call error (1, 
		    "(custom_parse 80): input string does not make sense")
	    if (next_num (instr, ip, n2) == 0)
	 	call error (1, 
		    "(custom_parse 90): input string does not make sense")
	    while (!IS_DIGIT(instr[ip]) && instr[ip] != EOS)
	    	ip = ip + 1
	    if (ctotok (instr, ip, tokstr, SZ_LINE) == TOK_NUMBER) {
	    	jp = 1
	    	nchar = ctod (tokstr, jp, hour)
	    }
	    doy = double(n2) + hour/24.d0
	    mjd = yd_to_mjd (yy, doy, NEWSTYLE)

	} else
	    call error (1, "(custom_parse 100): illegal qualifier")

	# make sure argument is reasonable
	if (abs(mjd) > MAX_MJD)
	    call error (1, "argument is out of range")
end
