include <tbset.h>
include <ctotok.h>
include	"metric.h"

#  t_metric -- correct pixel coordiante to be free from geometric distortion
#
#  Description:
#  ------------
#  
#  Input CL parameters:
#  -----------------
#  "infile"		Input science data file template name
#  "x"			X pixel coordinate
#  "y"			Y pixel coordinate
#  "hms"		print hour-minute-sec as output format?
#  "centroid"		do centroid?
#  "boxsize"		centroid box size
#
#  Output CL parameters:
#  -----------------
#  "ra"			right ascension
#  "dec"		declination
#
#  Date		Author		Version		Description
#  ----		------		-------		-----------
#  16-Oct-1992  J.-C. Hsu	1.0		design and coding
#  16-Dec-1992  J.-C. Hsu	1.1		Add boxsize as input parameter
#  16-Feb-1993  J.-C. Hsu	1.2		Add 0.5 pixel subtraction and
#		global rotation of 0.2878(PC) and 0.05334(WF) degrees
#  06-Jul-1994  J.-C. Hsu	1.3		Include WFPC2
#  28-Apr-2003  J.-C. Hsu	2.2		Work for FITS file
#  06-Jun-2007  W.J.  Hack  2.3     Fixed for PyRAF 
#------------------------------------------------------------------------------

procedure t_metric ()

char	infile[SZ_FNAME]
char	x_str[SZ_LINE], y_str[SZ_LINE]
bool	hms
#bool	geo_corr
bool	centroid
int	boxsize
	
char	reffile[SZ_FNAME]
pointer	ipin, ipref
bool	refflag
int	instru
int	iccd, order
int	key
real	x0, y0
int	len
int	wcs
char	strval[SZ_LINE]
char	text[SZ_LINE]
char	x_substr1[SZ_LINE], x_substr2[SZ_LINE]
char	y_substr1[SZ_LINE], y_substr2[SZ_LINE]
int	x_ip, y_ip
int	k, kp1, kp2
int	x_token1, x_token2
int	y_token1, y_token2
char	x_tabname[SZ_FNAME], y_tabname[SZ_FNAME]	
char	x_colname[SZ_COLNAME], y_colname[SZ_COLNAME]	
pointer	x_tp, y_tp
int	nrows, x_nrows, y_nrows		# number of rows in the table(s)
int	row
bool	x_null, y_null
pointer	colptr[10]
real	ax[(MAX_ORDER+1)**2]
real	ay[(MAX_ORDER+1)**2]
real	trans[SZ_TRANS]
char	rastr[SZ_LINE], decstr[SZ_LINE]

bool	clgetb()
int	clgeti()
int	imgeti()
int	ctotok()
int	ctor()
pointer	tbtopn()
int	tbpsta()
pointer	gf_map()
bool	streq()
int	strlen()
int	nowhite()
#==============================================================================
begin

	# read input CL parameters
	call clgstr ("infile", infile, SZ_FNAME)
	call clgstr ("x", x_str, SZ_LINE)
	call clgstr ("y", y_str, SZ_LINE)
	hms = clgetb ("hms")
#	geo_corr = clgetb ("geo_corr")
	centroid = clgetb ("centroid")
	boxsize = clgeti ("boxsize")
	
	# open the input image
	ipin = gf_map (infile, READ_ONLY, 0)		

	# open the reference group (2)
	k = nowhite (infile, reffile, SZ_FNAME)
	len = strlen (reffile)
	refflag = false
	if (reffile[len] == ']') {
	    if (reffile[len-2] != '[')
		call error (1, "subsection not allowed")
	    if (reffile[len-1] == '2')
		refflag = true
	    else
	        reffile[len-1] = '2'
	} else
	    call strcat ("[2]", reffile, SZ_FNAME)
	if (refflag)
	    ipref = ipin
	else
	    ipref = gf_map (reffile, READ_ONLY, 0)

	# which instrument and CCD was used?
	iferr (call imgstr (ipin, "INSTRUME", strval, SZ_LINE)) {
	    call sprintf (text, SZ_LINE, 
		    "input image %s does not have the keyword INSTRUME")
		call pargstr (infile)
	    call error (1, text)
	}
	iccd = imgeti (ipin, "DETECTOR")
	if (streq (strval, "WFPC2")) instru = WFPC2
	else if (streq (strval, "WFPC")) instru = WFPC
	else call error (1, "illegal instrument")

	# read the geometric distortion (Legendre polynomial) coefficients 
	# for the specified chip
	if (instru == WFPC) call get_coeff (ax, ay, iccd, order)
	if (instru == WFPC2) call get_coeff2 (ax, ay, iccd, order)

	# read the inter-chip transformation coefficients
	if (instru == WFPC) call get_trans (iccd, trans)
	if (instru == WFPC2) call get_trans2 (iccd, trans)

	# parse the input X string to decide which of the following mode is 
	# intended: (1) cursor by the user (NULL argument), (2) one pair of 
	# X and Y (one numerical argument), and (3) a tbale (two alphanumerical
	# arguments)
	x_ip = 1
	y_ip = 1
	x_token1 = ctotok(x_str, x_ip, x_substr1, SZ_LINE)

	# print header
	call printf ("# METRIC version: %s\n")
	    call pargstr (VERSION)
	call printf ("# input file: %s\n")
	    call pargstr (infile)
	call printf ("# centroid box size = %d pixels\n")
	    call pargi (boxsize)
	call printf ("#x(raw)  y(raw) x(cent) y(cent) x(geom) y(geom) ")
	call printf ("x(meta) y(meta)    RA          Dec")
	call printf ("\n")

	# interactive cursor mode:
	#-------------------------
	if (x_token1 == TOK_EOS) {
	    key = 'a'
	    while (key != 'q') {	
	        call clgcur ("cursor", x0, y0, wcs, key, strval, SZ_LINE)
		if (key == 'q') break
		centroid = (key == 'c')
	        call metric_do (x0, y0, ax, ay, trans, ipin, ipref, iccd, 
				order, hms, centroid, boxsize, instru,rastr,decstr)
	    }

	# single pair of X and Y as input:
	#---------------------------------
	} else if (x_token1 == TOK_NUMBER) {
	    kp1 = 1
	    kp2 = 1
	    y_token1 = ctotok (y_str, y_ip, y_substr1, SZ_LINE)
	    if (y_token1 != TOK_NUMBER)
		call error (1, "X and Y input do not match")
	    k = ctor (x_substr1, kp1, x0)
	    k = ctor (y_substr1, kp2, y0)
	    call metric_do (x0, y0, ax, ay, trans, ipin, ipref, iccd, 
				order, hms, centroid, boxsize, instru,rastr,decstr)

	# table input for X and Y:
	#-------------------------
	} else {
	    x_token2 = ctotok (x_str, x_ip, x_substr2, SZ_LINE)
	    if (x_token2 == TOK_EOS)
		call error (1, "illegal input X string")
	    call strcpy (x_substr1, x_tabname, SZ_LINE)
	    call strcpy (x_substr2, x_colname, SZ_LINE)

	    y_token1 = ctotok (y_str, y_ip, y_substr1, SZ_LINE)
	    if (y_token1 == TOK_EOS)
		call error (1, "empty input Y string")
	    y_token2 = ctotok (y_str, y_ip, y_substr2, SZ_LINE)
	    if (y_token2 == TOK_EOS) {
		call strcpy (y_substr1, y_colname, SZ_LINE)
		y_tabname[1] = EOS
	    } else {
		call strcpy (y_substr1, y_tabname, SZ_LINE)
		call strcpy (y_substr2, y_colname, SZ_LINE)
		if (streq (y_tabname, x_tabname))
		    y_tabname[1] = EOS
	    }
	    x_tp = tbtopn (x_tabname, READ_ONLY, 0)
	    x_nrows = tbpsta (x_tp, TBL_NROWS)
	    call tbcfnd (x_tp, x_colname, colptr[1], 1)
	    if (colptr[1] == NULL)
		call error (1, "X column does not exist")
	    if (y_tabname[1] == EOS)
	   	y_tp = x_tp
	    else 
	    	y_tp = tbtopn (y_tabname, READ_ONLY, 0)
	    y_nrows = tbpsta (y_tp, TBL_NROWS)
	    call tbcfnd (y_tp, y_colname, colptr[2], 1)
	    if (colptr[2] == NULL)
		call error (1, "Y column does not exist")

	    nrows = min (x_nrows, y_nrows)
	    if (nrows <= 0) 
		call error (1, "empty input table(s)")
		
	    do row = 1, nrows {

	        # read the data
	        call tbrgtr (x_tp, colptr[1], x0, x_null, 1, row)
	        call tbrgtr (y_tp, colptr[2], y0, y_null, 1, row)
		    if (x_null || y_null) {
		        call printf ("# invalid data at row %d, skip\n")
			    call pargi (row)
		        next
		    }

	        call metric_do (x0, y0, ax, ay, trans, ipin, ipref, iccd, 
				order, hms, centroid, boxsize, instru,rastr,decstr)
	    }
	    call tbtclo (x_tp)
	    if (y_tabname[1] != EOS)
	        call tbtclo (y_tp)
	}
	call clpstr ("ra", rastr)
	call clpstr ("dec", decstr)

	call gf_unmap (ipin)
	if (!refflag)
	    call gf_unmap (ipref)
end
