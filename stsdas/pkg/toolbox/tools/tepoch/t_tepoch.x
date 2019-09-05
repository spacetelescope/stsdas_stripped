include	<tbset.h>
include <ctype.h>
include	"epoch.h"

define	SZ_COL_LEN	30	# length of the output string column

#  T_TEPOCH -- Convert epochs in one column to another format and put in 
#  another column
#
#  Description:
#  ------------
#
#  Input table parameters
#  ----------------------
#
#  Date		Author			Description
#  ----		------			-----------
#  16-Mar-1998	J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure t_tepoch ()

char    input[SZ_FNAME]
char    incol[SZ_FNAME]
char    outcol[SZ_FNAME]
char    infmt[SZ_FNAME]
char    outfmt[SZ_FNAME]
char    outtype[SZ_FNAME]

pointer	tp			# table pointer
pointer	cptr_in, cptr_out
int	nrows			# number of rows in the table

char    instr[SZ_FNAME]         # input time string
int     length                  # length of the input string
int     qualifier               # input format qualifier code
char	dmystyle[SZ_FNAME]	# day month year format style
int	style			# Julian or Gregorian calendar
bool	flag1900		# add 1900 to two digit year
bool	bcflag			# year is BC?
char	rawstr[SZ_FNAME]
char	stylestr[SZ_FNAME]
char	outstr[SZ_COL_LEN]
double	dval
double	mjd
int	i, j, k

pointer tbtopn()
int	tbpsta()
int	strlen()
int	strsearch()
bool	streq()
bool	clgetb()
#==============================================================================
begin

        # read CL parameters
        call clgstr ("input", input, SZ_FNAME)
        call clgstr ("incol", incol, SZ_FNAME)
        call clgstr ("outcol", outcol, SZ_FNAME)
        call clgstr ("qualifier", rawstr, SZ_FNAME)
        call clgstr ("outfmt", outfmt, SZ_FNAME)
        call clgstr ("outtype", outtype, SZ_FNAME)

	call strupr (outfmt)

	# read other CL parameters
	# input calendar style
	call clgstr ("calendar", stylestr, SZ_FNAME)
	if (strsearch (stylestr, "ns") != 0)
	    style = NEWSTYLE
	else if (strsearch (stylestr, "os") != 0)
	    style = OLDSTYLE
	else
	    call error (1, "illegal calendar style")

	# "clean up" the input format string
	call strlwr (rawstr)

	# get rid of any leading blanks (and tabs)
	i = 1
	while (IS_WHITE(rawstr[i]))
	    i = i + 1

	call strcpy (rawstr[i], infmt, strlen(rawstr)-i+1)
	call qual_code (infmt, qualifier)

	# input the day-month-year style
	call clgstr ("dmy_style", dmystyle, SZ_FNAME)
	call strlwr (dmystyle)

	# read the add 1900 flag
	flag1900 = clgetb ("add1900")

	# open the table
        tp = tbtopn (input, READ_WRITE, NULL)

	# find the input column
	call tbcfnd1 (tp, incol, cptr_in)

	# define the output column
	call tep_outcol (tp, outcol, outfmt, outtype,    cptr_out)

	# how many rows are in the table?
        nrows = tbpsta (tp, TBL_NROWS)

	# process each row
	do k = 1, nrows {
	    call tbegtt (tp, cptr_in, k, rawstr, SZ_FNAME)
	
	    # "clean up" the input string
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
	        call error (1, "null input string")

	    call strcpy (rawstr[i], instr, length)

	    # search for BC pattern in the input string
	    bcflag = (strsearch (instr, "bc") != 0 || 
		      strsearch (instr, "b.c") != 0 ||
		      strsearch (instr, "b. c") != 0)

	    call which_class (instr, length, qualifier, dmystyle, style,
				flag1900, bcflag, 0., mjd)
	    call tep_out (mjd, outfmt, outtype,   dval, outstr)
	    if ((streq(outfmt,"JD") || streq(outfmt,"MJD") || 
	         streq(outfmt,"DMF")) && (outtype[1] == 'd'))
		call tbeptd (tp, cptr_out, k, dval)
	    else
		call tbeptt (tp, cptr_out, k, outstr)
	}

	call tbtclo (tp)
end


#  tep_outcol -- Define the output column for the task tepoch
#------------------------------------------------------------------------------
procedure tep_outcol (tp, outcol, outfmt, outtype,    cptr_out)

# inputs:
pointer	tp			# table pointer
char    outcol[ARB]		# output column name
char    outfmt[ARB]		# output (time) format/unit
char    outtype[ARB]		# output column data type

# output:
pointer	cptr_out

# local:
char	ofmt[SZ_FNAME]
int	otype, ex_type

int	tbcigi()
bool	streq()
#==============================================================================
begin
	# find the output column
	call tbcfnd1 (tp, outcol, cptr_out)

	# determine the output column format and data type
	ofmt[1] = EOS
	if ((streq(outfmt,"JD") || streq(outfmt,"MJD") || 
	     streq(outfmt,"DMF")) && (outtype[1] == 'd')) {
	    otype = TY_DOUBLE
	    if (streq(outfmt,"JD")) call strcpy ("%17.10f", ofmt, SZ_FNAME)
	    if (streq(outfmt,"MJD")) call strcpy ("%17.10f", ofmt, SZ_FNAME)
	    if (streq(outfmt,"DMF")) call strcpy ("%17.5f", ofmt, SZ_FNAME)
	} else {
	    otype = -SZ_COL_LEN
	}

	# existing output column
	if (cptr_out != NULL) {
	    ex_type = tbcigi (cptr_out, TBL_COL_DATATYPE)
	    if (otype != TY_DOUBLE) {
		if (ex_type >= otype) 
		    call error (1, "Existing column does not have enough space for the output.")
	    } else {
		if (ex_type != otype)
		    call error (1, "Existing column is not double precision.")
	    }
	} else 
	    call tbcdef (tp, cptr_out, outcol, outfmt, ofmt, otype, 1, 1)
end


# tep_out -- write output for tepoch
#
#----------------------------------------------------------------------------
procedure tep_out (mjd, outfmt, outtype,   dval, outstr)

# inputs:
double	mjd			# modified Julian date
char	outfmt[ARB]		# output format 
char	outtype[ARB]		# output data type
double	dval
char	outstr[ARB]

# locals:
int	year			# Year
int	yy
int	month			# Month (1-12)
int	day			# Day of month
int	doy			# day of the year
double	framjd			# fraction of the day
char	prefx[SZ_FNAME]

int	d
double	dd
char	engstr[SZ_FNAME]
char	dname[SZ_DAY]
char	mname[SZ_MONTH]
char	bcstr[SZ_DAY]

bool	streq()
bool	strne()
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

	# compute civil calendar year
	yy = year
	call strcpy ("  ", bcstr, SZ_DAY)
	if (year < 1) {
	    yy = 1 - year
	    call strcpy ("BC", bcstr, SZ_DAY)
	}

	if (outtype[1] == 'p' && strne(outfmt, "DATE")) {
	    call strcpy (outfmt, prefx, SZ_FNAME)
	    call strcat (" ", prefx, SZ_FNAME)
	} else 
	    prefx[1] = EOS

	if (streq (outfmt, "DMF")) {

	    # seconds from 1980 Jan 1 0h
	    dval = (mjd - double(MJD1980)) * SECPERDAY
	    call sprintf (outstr, SZ_COL_LEN, "%s%17.5f")
	        call pargstr (prefx)
	        call pargd (dval)
        } else if (streq (outfmt, "JD")) {
	    dval = mjd+2400000.5d0
	    call sprintf (outstr, SZ_COL_LEN, "%s%17.10f")
	        call pargstr (prefx)
	        call pargd (dval)
        } else if (streq (outfmt, "MJD")) {
	    dval = mjd
	    call sprintf (outstr, SZ_COL_LEN, "%s%17.10f")
	        call pargstr (prefx)
	        call pargd (dval)
        } else if (streq (outfmt, "DATE")) {
	    call sprintf (outstr, SZ_COL_LEN, "%2d %s %d %014.5h %s")
	        call pargi (day)
	        call pargstr (mname)
	        call pargi (yy)
	        call pargd (framjd*24.d0)
	        call pargstr (bcstr)

	# write SMS output string
        } else if (streq (outfmt, "SMS")) {
	    call sprintf (outstr, SZ_COL_LEN, "%s%d.%03d %0.5h")
	        call pargstr (prefx)
	        call pargi (year)
	        call pargi (doy)
	        call pargd (framjd*24.d0)
	
	# write engineering output string
        } else if (streq (outfmt, "ENG")) {
	    call to_eng (year, month, day, framjd, engstr)
	    call sprintf (outstr, SZ_COL_LEN, "%s%s")
	        call pargstr (prefx)
	        call pargstr (engstr)
	}
end
