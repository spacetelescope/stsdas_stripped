# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"
define	NEXPTIME	128
define	NTEMP		8

#  w_cmd -- dissect wfpc commands listed in ICD-08 Appendix V, Fig. V-17
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  11-Jul-1991  J.-C. Hsu		coding
#------------------------------------------------------------------------------
procedure w_cmd (mode, msb, lsb, tp, colidn, nrows, loc)

						## input
int	mode
int	msb[ARB], lsb[ARB]
pointer	tp
pointer	colidn[NCOL]
int	loc[NCMD]
						## inputs/outputs
int	nrows
						## local
char	chval[SZ_CHVAL]
char	iformat[SZ_LINE], rformat[SZ_LINE]
int	temp[NTEMP]
real	exptime[NEXPTIME]
int	hexval
int	steps, m

int	bitson()
						## data
data	temp/-82, -87, -92, -97, -102, -107, -111, -115/
include	"exptime.h"
#==============================================================================
begin
	call sprintf (iformat, SZ_LINE, "%%%dd")
	    call pargi (SZ_CHVAL)
	call sprintf (rformat, SZ_LINE, "%%%d.2f")
	    call pargi (SZ_CHVAL)

	# Execute command
	if (mode == EXEC) {
	    m = loc[EXEC]

	    hexval = bitson (msb[m], 7, 6)
	    if (hexval == 0)
		chval[1] = EOS
	    else
		call strcpy ("Invalid",	chval, SZ_CHVAL)
	    call eng_write ("", "INVALID1", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
		
	    call cmdbit (msb[m], 4, "AUTOERAS", "No Auto Erase", "Auto Erase", 
				nrows, tp, colidn)
	    call cmdbit (msb[m], 3, "KDOTS", 	"No K Spot", "K Spot", 
				nrows, tp, colidn)
	    call cmdbit (msb[m], 2, "CALLAMP", 	"No Calibration", "Calibration",
				nrows, tp, colidn)
	    call cmdbit (msb[m], 1, "SHUTTER", 	"No Exposure", "Exposure", 
				nrows, tp, colidn)
	    call cmdbit (msb[m], 0, "ERASE", 	"No Erase", "Erase", 
				nrows, tp, colidn)
		
	    hexval = bitson (lsb[m], 7, 0)
	    if (hexval > 127)
		chval[1] = EOS
	    else {
	        call sprintf (chval, SZ_CHVAL, rformat)
		    call pargr (exptime[hexval+1])
	    }
	    call eng_write ("", "EXPTIME", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
		
	} else if (mode == INFREQ) {
	    m = loc[INFREQ]

	    hexval = bitson (msb[m], 7, 6)
	    if (hexval == 0)
		chval[1] = EOS
	    else
		call strcpy ("Invalid",	chval, SZ_CHVAL)
	    call eng_write ("", "INVALID2", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
		
	    call cmdbit (msb[m], 3, "APRTDOIT", "UV Fld Mirror NoOp", 
				"UV Flood Mirror Op", nrows, tp, colidn)
	    call cmdbit (msb[m], 2, "APRTCLOS", "UV Mirror Clear", 
				"UV Mirror Flood Position", nrows, tp, colidn)
	    call cmdbit (msb[m], 1, "FOCSDOIT", "Focus No Op", 
				"Focus Op", nrows, tp, colidn)
	    call cmdbit (msb[m], 0, "FOCSOUT", 	"Focus Forward", 
				"Focus Rearward", nrows, tp, colidn)
		
	    hexval = bitson (lsb[m], 2, 0)
	    call sprintf (chval, SZ_CHVAL, iformat)
		call pargi (temp[hexval+1])
	    call eng_write ("", "TEMPRQST", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
		
	} else if (mode == PREP) {
	    m = loc[PREP]

	    call cmdbit (msb[m], 7, "INVALID3", "Invalid", "",
				nrows, tp, colidn)
	    call cmdbit (msb[m], 6, "TWOBYTWO", "Standard Mode", 
				"2 x 2 Mode", nrows, tp, colidn)
	    call cmdbit (msb[m], 5, "PYRAMDOT", "Pyramid No Op", 
				"Pyramid Op", nrows, tp, colidn)
	    call cmdbit (msb[m], 4, "PCMODE", 	"WFC", 
				"PC", nrows, tp, colidn)
	    call cmdbit (msb[m], 3, "READCCD1", "No CCD 1/5", 
				"CCD 1/5", nrows, tp, colidn)
	    call cmdbit (msb[m], 2, "READCCD2", "No CCD 2/6", 
				"CCD 2/6", nrows, tp, colidn)
	    call cmdbit (msb[m], 1, "READCCD3", "No CCD 3/7", 
				"CCD 3/7", nrows, tp, colidn)
	    call cmdbit (msb[m], 0, "READCCD4", "No CCD 4/8", 
				"CCD 4/8", nrows, tp, colidn)

	    call cmdbit (lsb[m], 7, "FILTCLRZ", "No Clear Filters", 
				"Clear Filters", nrows, tp, colidn)

	    hexval = bitson (lsb[m], 6, 3)
	    call sprintf (chval, SZ_CHVAL, iformat)
		call pargi (hexval+1)
	    call eng_write ("", "SLCTWHEL", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1

	    hexval = bitson (lsb[m], 2, 0)
	    steps = hexval
	    if (hexval >= 4) steps = 4
	    call sprintf (chval, SZ_CHVAL, iformat)
		call pargi (steps)
	    call eng_write ("", "SLCTFILT", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
	} 
end

#  u_cmd -- dissect wfpc2 commands listed in ICD-08 Appendix VI, Fig. VI-18a
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  08-Dec-1993  J.-C. Hsu		adapted from w_cmd.x
#------------------------------------------------------------------------------
procedure u_cmd (mode, msb, lsb, tp, colidn, nrows, loc)

						## input
int	mode
int	msb[ARB], lsb[ARB]
pointer	tp
pointer	colidn[NCOL]
int	loc[NCMD]
						## inputs/outputs
int	nrows
						## local
char	chval[SZ_CHVAL]
char	iformat[SZ_LINE], rformat[SZ_LINE]
int	temp[NTEMP]
real	exptime[NEXPTIME]
int	hexval
int	steps, m

int	bitson()
						## data
data	temp/-22, -32, -43, -52, -72, -78, -84, -90/
include	"exptime.h"
#==============================================================================
begin
	call sprintf (iformat, SZ_LINE, "%%%dd")
	    call pargi (SZ_CHVAL)
	call sprintf (rformat, SZ_LINE, "%%%d.2f")
	    call pargi (SZ_CHVAL)

	# Execute command
	if (mode == EXEC) {
	    m = loc[EXEC]

	    hexval = bitson (msb[m], 7, 6)
	    if (hexval == 0)
		chval[1] = EOS
	    else
		call strcpy ("Invalid",	chval, SZ_CHVAL)
	    call eng_write ("", "INVALID1", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
		
	    call cmdbit (msb[m], 4, "AUTOERAS", "No Auto Erase", "Auto Erase", 
				nrows, tp, colidn)
	    call cmdbit (msb[m], 3, "KDOTS", 	"No K Spot", "K Spot", 
				nrows, tp, colidn)
	    call cmdbit (msb[m], 2, "CALLAMP", 	"No Calibration", "Calibration",
				nrows, tp, colidn)
	    call cmdbit (msb[m], 1, "SHUTTER", 	"No Exposure", "Exposure", 
				nrows, tp, colidn)
	    call cmdbit (msb[m], 0, "ERASE", 	"No Erase", "Erase", 
				nrows, tp, colidn)
		
	    hexval = bitson (lsb[m], 7, 0)
	    if (hexval > 127)
		chval[1] = EOS
	    else {
	        call sprintf (chval, SZ_CHVAL, rformat)
		    call pargr (exptime[hexval+1])
	    }
	    call eng_write ("", "EXPTIME", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
		
	} else if (mode == INFREQ) {
	    m = loc[INFREQ]

	    hexval = bitson (msb[m], 7, 6)
	    if (hexval == 0)
		chval[1] = EOS
	    else
		call strcpy ("Invalid",	chval, SZ_CHVAL)
	    call eng_write ("", "INVALID2", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
		
	    call cmdbit (msb[m], 3, "CSFMDOIT", "CalSys Mirror NoOp", 
				"CalSys Mirror Op", nrows, tp, colidn)
	    call cmdbit (msb[m], 2, "CSFMCLOS", "Cal Sys Mirror OUT", 
				"Cal Sys Mirror IN", nrows, tp, colidn)
	    call cmdbit (msb[m], 1, "POMDOIT", "POM No Op", 
				"POM Op", nrows, tp, colidn)
	    call cmdbit (msb[m], 0, "POMSTEP", 	"POM plus step", 
				"POM minus step", nrows, tp, colidn)
		
	    hexval = bitson (lsb[m], 2, 0)
	    call sprintf (chval, SZ_CHVAL, iformat)
		call pargi (temp[hexval+1])
	    call eng_write ("", "TEMPRQST", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
		
	} else if (mode == PREP) {
	    m = loc[PREP]

	    call cmdbit (msb[m], 7, "INVALID3", "Invalid", "",
				nrows, tp, colidn)
	    call cmdbit (msb[m], 6, "TWOBYTWO", "Standard Mode", 
				"2 x 2 Mode", nrows, tp, colidn)
	    call cmdbit (msb[m], 4, "ATODGAIN", "7 e/DN", 
				"15 e/DN", nrows, tp, colidn)
	    call cmdbit (msb[m], 3, "READCCD1", "No CCD 1", 
				"CCD 1", nrows, tp, colidn)
	    call cmdbit (msb[m], 2, "READCCD2", "No CCD 2", 
				"CCD 2", nrows, tp, colidn)
	    call cmdbit (msb[m], 1, "READCCD3", "No CCD 3", 
				"CCD 3", nrows, tp, colidn)
	    call cmdbit (msb[m], 0, "READCCD4", "No CCD 4", 
				"CCD 4", nrows, tp, colidn)

	    call cmdbit (lsb[m], 7, "FILTCLRZ", "No Clear Filters", 
				"Clear Filters", nrows, tp, colidn)

	    hexval = bitson (lsb[m], 6, 3)
	    call sprintf (chval, SZ_CHVAL, iformat)
		call pargi (hexval+1)
	    call eng_write ("", "SLCTWHEL", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1

	    hexval = bitson (lsb[m], 2, 0)
	    steps = hexval
	    if (hexval >= 4) steps = 4
	    call sprintf (chval, SZ_CHVAL, iformat)
		call pargi (steps)
	    call eng_write ("", "SLCTFILT", chval, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
	} 
end

# cmdbit - write messages corresponding to a command bit to the output table

procedure cmdbit (indata, bitloc, keyword, chval0, chval1, nrows, tp, colidn)

						## input
int	indata
int	bitloc
char	keyword[SZ_KEYWORD]
char	chval0[SZ_CHVAL], chval1[SZ_CHVAL]
pointer	tp
pointer	colidn[NCOL]
						## inputs/outputs
int	nrows
						## local
int	hexval

int	bitson()
#------------------------------------------------------------------------------
begin
	hexval = bitson (indata, bitloc, bitloc)
	if (hexval == 0)
	    call eng_write ("", keyword, chval0, hexval, nrows+1, tp, colidn)
	else
	    call eng_write ("", keyword, chval1, hexval, nrows+1, tp, colidn)
	nrows = nrows + 1
end
