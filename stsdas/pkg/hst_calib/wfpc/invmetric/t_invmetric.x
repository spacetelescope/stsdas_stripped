include <tbset.h>
include <ctotok.h>
include	"invmetric.h"

#  t_invmetric -- Convert RA/Dec to X/Y for WF/PC or WFPC2 
#
#  Description:
#  ------------
#  
#  Input CL parameters:
#  -----------------
#  "infile"		Input science data file template name
#  "ra"			right ascension
#  "dec"		declination
#  "ra_unit"		input RA in hours?
#
#  Output CL parameters:
#  -----------------
#  "x"			X pixel coordinate
#  "y"			Y pixel coordinate
#  "chip"		which chip is the output XY at
#
#  Date		Author		Version		Description
#  ----		------		-------		-----------
#  23-Jun-1995  J.-C. Hsu	2.1		design and coding
#  18-Apr-2003  J.-C. Hsu	2.2		Make it work on FITS files
#------------------------------------------------------------------------------
procedure t_invmetric()

char	infile[SZ_FNAME]
char	x_str[SZ_LINE], y_str[SZ_LINE]
char	ra_unit[SZ_LINE]
	
char	reffile[SZ_FNAME]
pointer	ipin, ipref
bool	refflag
int	fixchip
int	instru
int	iccd, order
int	len
char	strval[SZ_LINE]
char	text[SZ_LINE]
char	x_substr1[SZ_LINE], x_substr2[SZ_LINE]
char	y_substr1[SZ_LINE], y_substr2[SZ_LINE]
char	ra_str[SZ_LINE], dec_str[SZ_LINE]
int	x_ip, y_ip
int	i, ii, k, kp1
int	x_token1, x_token2
int	y_token1, y_token2
char	x_tabname[SZ_FNAME], y_tabname[SZ_FNAME]	
char	x_colname[SZ_COLNAME], y_colname[SZ_COLNAME]	
pointer	x_tp, y_tp
int	nrows, x_nrows, y_nrows		# number of rows in the table(s)
int	row
bool	x_null, y_null
pointer	colptr[10]
real	ax[(MAX_ORDER+1)**2, 4]		# geometric distortion coefficients
real	ay[(MAX_ORDER+1)**2, 4]
real	xo[4]		# boundary coefficients
real	yo[4]
real	trans[SZ_TRANS, 4]

int	imgeti()
int	ctotok()
int	ctoi()
pointer	tbtopn()
int	tbpsta()
pointer	gf_map()
bool	streq()
int	strlen()
int	nowhite()
#==============================================================================
begin

	# read input CL parameters
	call clgstr("infile", infile, SZ_FNAME)
	call clgstr("ra", x_str, SZ_LINE)
	call clgstr("dec", y_str, SZ_LINE)
	call clgstr("ra_unit", ra_unit, SZ_LINE)
	
	# open the input image
	ipin = gf_map(infile, READ_ONLY, 0)		

	# open the reference group (2)
	k = nowhite(infile, reffile, SZ_FNAME)
	len = strlen(reffile)
	refflag = false
	if (reffile[len] == ']') {
	    if (reffile[len-2] != '[')
		call error(1, "subsection not allowed")

	    # decide if a group number been specified
	    kp1 = len-1
	    k = ctoi(reffile, kp1, fixchip)
	    if (reffile[len-1] == '2')
		refflag = true
	    else
	        reffile[len-1] = '2'
	} else {
	    fixchip = 0
	    call strcat("[2]", reffile, SZ_FNAME)
	}
	if (refflag)
	    ipref = ipin
	else
	    ipref = gf_map(reffile, READ_ONLY, 0)

	# which instrument and CCD was used?
	iferr (call imgstr(ipin, "INSTRUME", strval, SZ_LINE)) {
	    call sprintf(text, SZ_LINE, 
		    "input image %s does not have the keyword INSTRUME")
		call pargstr(infile)
	    call error(1, text)
	}
	iccd = imgeti(ipin, "DETECTOR")
	if (streq(strval, "WFPC2")) instru = WFPC2
	else if (streq(strval, "WFPC")) instru = WFPC
	else call error(1, "illegal instrument")

	# read the geometric distortion (Legendre polynomial) coefficients 
	# for all chips
	if (instru == WFPC) {
	    do i = 1, 4 {
		ii = i
		if (iccd > 4) ii = i + 4
		call get_coeff(ax[1,i], ay[1,i], ii, order)
	    }
	} else if (instru == WFPC2) {
	    do i = 1, 4 {
		call get_coeff2(ax[1,i], ay[1,i], i, order)
	    }
	}

	# read the inter-chip transformation coefficients
	if (instru == WFPC) {
	    do i = 1, 4 {
		ii = i
		if (iccd > 4) ii = i + 4
		call get_trans(ii, trans[1, i])
	    }
	} else if (instru == WFPC2) {
	    do i = 1, 4 {
		call get_trans2(i, trans[1, i])
	    }
	}

	# read the boundary info for all chips
	if (instru == WFPC) {
	    if (iccd <= 4) {
		xo[1] = 19.
		yo[1] = 29.
		xo[2] = 13.
		yo[2] = 25.
		xo[3] = 21.
		yo[3] = 26.
		xo[4] = 18.
		yo[4] = 28.
	    } else {
		xo[1] = 32.
		yo[1] = 30.
		xo[2] = 20.
		yo[2] = 26.
		xo[3] = 24.
		yo[3] = 29.
		xo[4] = 16.
		yo[4] = 33.
	    }
	} else if (instru == WFPC2) {
	    xo[1] = 44
	    yo[1] = 53
	    xo[2] = 43
	    yo[2] = 24
	    xo[3] = 29
	    yo[3] = 46
	    xo[4] = 43
	    yo[4] = 41
	}

	# parse the input X string to decide which of the following mode is 
	# intended: (1) one pair of X and Y (one numerical argument), and 
	# (2) a table (two alphanumerical arguments)
	x_ip = 1
	y_ip = 1
	x_token1 = ctotok(x_str, x_ip, x_substr1, SZ_LINE)

	# print header
	call printf("# INVMETRIC version: %s\n")
	    call pargstr(VERSION)
	call printf("# input file: %s\n")
	    call pargstr(infile)
	call printf("#     RA          Dec           X       Y     chip\n")

	# single pair of RA and Dec as input:
	#---------------------------------
	if (x_token1 == TOK_NUMBER) {
	    y_token1 = ctotok(y_str, y_ip, y_substr1, SZ_LINE)
	    if (y_token1 != TOK_NUMBER)
		call error(1, "RA and Dec input do not match")
	    call invmetric_do(x_str, y_str, ax, ay, xo, yo, trans, 
				ipref, iccd, order, ra_unit, instru, fixchip)

	# table input for RA and Dec:
	#-------------------------
	} else {
	    x_token2 = ctotok(x_str, x_ip, x_substr2, SZ_LINE)
	    if (x_token2 == TOK_EOS)
		call error(1, "illegal input RA string")
	    call strcpy(x_substr1, x_tabname, SZ_LINE)
	    call strcpy(x_substr2, x_colname, SZ_LINE)

	    y_token1 = ctotok(y_str, y_ip, y_substr1, SZ_LINE)
	    if (y_token1 == TOK_EOS)
		call error(1, "empty input Dec string")
	    y_token2 = ctotok(y_str, y_ip, y_substr2, SZ_LINE)
	    if (y_token2 == TOK_EOS) {
		call strcpy(y_substr1, y_colname, SZ_LINE)
		y_tabname[1] = EOS
	    } else {
		call strcpy(y_substr1, y_tabname, SZ_LINE)
		call strcpy(y_substr2, y_colname, SZ_LINE)
		if (streq(y_tabname, x_tabname))
		    y_tabname[1] = EOS
	    }
	    x_tp = tbtopn(x_tabname, READ_ONLY, 0)
	    x_nrows = tbpsta(x_tp, TBL_NROWS)
	    call tbcfnd(x_tp, x_colname, colptr[1], 1)
	    if (colptr[1] == NULL)
		call error(1, "RA column does not exist")
	    if (y_tabname[1] == EOS)
	   	y_tp = x_tp
	    else 
	    	y_tp = tbtopn(y_tabname, READ_ONLY, 0)
	    y_nrows = tbpsta(y_tp, TBL_NROWS)
	    call tbcfnd(y_tp, y_colname, colptr[2], 1)
	    if (colptr[2] == NULL)
		call error(1, "Dec column does not exist")

	    nrows = min(x_nrows, y_nrows)
	    if (nrows <= 0) 
		call error(1, "empty input table(s)")
		
	    do row = 1, nrows {

	        # read the data
	        call tbrgtt(x_tp, colptr[1], ra_str, x_null, SZ_LINE, 1, row)
	        call tbrgtt(y_tp, colptr[2], dec_str, y_null, SZ_LINE, 1, row)
		if (x_null || y_null) {
		    call printf("# invalid data at row %d, skip\n")
			call pargi(row)
		    next
		}
	        call invmetric_do(ra_str, dec_str, ax, ay, xo, yo, trans, 
				   ipref, iccd, order, ra_unit, instru, fixchip)
	    }
	    call tbtclo(x_tp)
	    if (y_tabname[1] != EOS)
	        call tbtclo(y_tp)
	}

	call gf_unmap(ipin)
	if (!refflag)
	    call gf_unmap(ipref)
end
