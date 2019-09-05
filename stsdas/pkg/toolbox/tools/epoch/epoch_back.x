include	"epoch.h"

# EPOCH_BACK -- put the result of EPOCH to output parameters and STDOUT
#
# Description:
# ------------
#----------------------------------------------------------------------------
procedure epoch_back (mjd, printout)

double	mjd			# input: modified Julian date
char	printout[SZ_LINE]	# input: output selections

int	year			# Year
int	yy
int	month			# Month (1-12)
int	day			# Day of month
int	doy			# day of the year
double	framjd			# fraction of the day

int	d
double	dd, dmf
bool	allflag
char	ymdhms[SZ_LINE]
char	old_ymdhms[SZ_LINE]
char	sms[SZ_LINE]
char	engstr[SZ_LINE]
char	dname[SZ_DAY]
char	mname[SZ_MONTH]
char	bcstr[SZ_DAY]

int	strsearch()
#-----------------------------------------------------------------------------
begin

	# output Gregorian calendar
	call to_ymd (mjd, year, month, day, doy, framjd, NEWSTYLE)
	call name_month (month, mname)

	# find the day of the week
	dd = mod (mjd+3.d0, 7.d0)
	if (dd < 0.d0) dd = dd + 7.d0
	d = int (dd)
	switch (d) {
	case 0:
	    call strcpy ("SUN", dname, SZ_DAY)
	case 1:
	    call strcpy ("MON", dname, SZ_DAY)
	case 2:
	    call strcpy ("TUE", dname, SZ_DAY)
	case 3:
	    call strcpy ("WED", dname, SZ_DAY)
	case 4:
	    call strcpy ("THU", dname, SZ_DAY)
	case 5:
	    call strcpy ("FRI", dname, SZ_DAY)
	case 6:
	    call strcpy ("SAT", dname, SZ_DAY)
	default:
	    call error (1, "illegal day of the week")
	}

	# seconds from 1980 Jan 1 0h
	dmf = (mjd - double(MJD1980)) * SECPERDAY

	# put numerical parameters
	call clputd ("mjd", mjd)
	call clputd ("jd", mjd+2400000.5d0)
	call clputd ("dmf", dmf)

	# put numerical year (int), month (int), day of the month (double)
	call clputi ("year", year)
	call clputi ("month", month)
	call clputd ("day_of_month", day+framjd)

	# put civil calendar date string parameters
	yy = year
	call strcpy ("  ", bcstr, SZ_DAY)
	if (year < 1) {
	    yy = 1 - year
	    call strcpy ("BC", bcstr, SZ_DAY)
	}
	call sprintf (ymdhms, SZ_LINE, "%2d %s %d %014.5h %s")
	    call pargi (day)
	    call pargstr (mname)
	    call pargi (yy)
	    call pargd (framjd*24.d0)
	    call pargstr (bcstr)
	call clpstr ("date", ymdhms)

	# write SMS output string
	call sprintf (sms, SZ_LINE, "%d.%03d %0.5h")
	    call pargi (year)
	    call pargi (doy)
	    call pargd (framjd*24.d0)
	call clpstr ("sms", sms)
	
	# write engineering output string
	call to_eng (year, month, day, framjd, engstr)
	call clpstr ("eng", engstr)
	
	# output Julian calendar
	call to_ymd (mjd, year, month, day, doy, framjd, OLDSTYLE)
	call name_month (month, mname)

	yy = year
	call strcpy ("  ", bcstr, SZ_DAY)
	if (year < 1) {
	    yy = 1 - year
	    call strcpy ("BC", bcstr, SZ_DAY)
	}
	call sprintf (old_ymdhms, SZ_LINE, "%2d %s %d %014.5h %s")
	    call pargi (day)
	    call pargstr (mname)
	    call pargi (yy)
	    call pargd (framjd*24.d0)
	    call pargstr (bcstr)
	call clpstr ("date_os", old_ymdhms)

	# print on the terminal
	allflag = (strsearch(printout, "all,") !=0)

	if (strsearch(printout, "date,") !=0 || allflag) {
	    call printf ("%s         %s\n")
		call pargstr (ymdhms)
	    	call pargstr (dname)
	}

	if (strsearch(printout, "os,") !=0 || allflag) {
	    call printf ("%s   (Julian calendar)\n")
	    	call pargstr (old_ymdhms)
	}

	if (strsearch(printout, "mjd,") !=0 || allflag) {

	    # use f instead of g format to get the desired number of digits 
	    # after the decimal point
	    call printf ("MJD %17.10f\n")
	    	call pargd (mjd)
	}

	if (strsearch(printout, ",jd,") !=0 || allflag) {
	    call printf ("JD %17.10f\n")
	    	call pargd (mjd+2400000.5d0)
	}

	if (strsearch(printout, "sms,") !=0 || allflag) {
	    call printf ("SMS %s\n")
	    	call pargstr (sms)
	}

	if (strsearch(printout, "dmf,") !=0 || allflag) {
	    call printf ("DMF %17.5f\n")
	    	call pargd (dmf)
	}

	if (strsearch(printout, "eng,") !=0 || allflag) {
	    call printf ("ENG %s\n")
	    	call pargstr (engstr)
	}
end
