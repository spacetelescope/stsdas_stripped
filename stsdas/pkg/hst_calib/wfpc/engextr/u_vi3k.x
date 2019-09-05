# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"
define	SHUTINT	10./255.
define	SHUTSP	160./255.

#  u_vi3k -- extract wfpc2 engineering data listed in Table VI-3k
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  08-Dec-1993  J.-C. Hsu		adapted from w_v3g.x
#------------------------------------------------------------------------------
procedure u_vi3k (indata, msb, lsb, inmask, tp, colidn, errval, nrows, 
			loc)

						## input
int	indata[ARB], msb[ARB], lsb[ARB]
short	inmask[ARB]
pointer	tp
pointer	colidn[NCOL]
int	errval
int	loc[NCMD]
						## inputs/outputs
int	nrows
						## local
char	mnemonic[SZ_MNEMONIC]
char	keyword[SZ_KEYWORD]
char	blank[1]
char	chval[SZ_CHVAL]
char	str1[SZ_CHVAL], str2[SZ_CHVAL]
char	iformat[SZ_LINE], rformat[SZ_LINE]
int	fwheel, fsteps, hexval, hexval1, hexval2
int	i, j, j1, j2, mk
int	k, m, dummy[100]
int	indx1[2], indx2[2]
real	shutter
bool	flag

int	bitson(), andi()
#==============================================================================
begin
	blank[1] = EOS
	call sprintf (iformat, SZ_LINE, "%%%dd")
	    call pargi (SZ_CHVAL)
	call sprintf (rformat, SZ_LINE, "%%%d.2f")
	    call pargi (SZ_CHVAL)

	# filter clear and position command [mnemonic 49, 50]
	flag = (inmask[48] == OKVAL)
	if (flag) {
	    hexval1 = msb[48]
	    hexval2 = lsb[48]
	} else {
	    hexval1 = errval
	    hexval2 = errval
	}
	call eng_write ("U01K049A", blank, blank, hexval1, nrows+1, tp, colidn)
	call eng_write ("U01K050A", blank, blank, hexval2, nrows+2, tp, colidn)
	nrows = nrows + 2

	# filter wheel counts [mnemonic 90, 92]
	call strcpy ("U01U090A",	mnemonic, SZ_MNEMONIC)
	indx1[1] = 88
	indx2[1] = 90
	indx1[2] = 90
	indx2[2] = 92

	do i = 1, 2 {
	    mnemonic[7] = '0'+ (i-1)*2

	    flag = (inmask[indx1[i]] == OKVAL && inmask[indx2[i]] == OKVAL)
	    if (flag) {
	        hexval = msb[indx2[i]] + lsb[indx1[i]] * 0100x
	        fwheel = andi(lsb[indx1[i]], 0F0x) / 010x
	        fsteps = msb[indx2[i]] + andi(lsb[indx1[i]], 0Fx) * 0100x
	    } else {
	        hexval = errval
	        fwheel = errval
	        fsteps = errval
	    }

	    # write to the output table
	    call eng_write (mnemonic, blank, blank, hexval, nrows+1, tp, colidn)
	    call strcpy ("FWHEEL1",	keyword, SZ_KEYWORD)
	    keyword[7] = '0'+i
	    call eng_write (blank, keyword, blank, fwheel, nrows+2, tp, colidn)
	    call strcpy ("FSTEPS1",	keyword, SZ_KEYWORD)
	    keyword[7] = '0'+i
	    call eng_write (blank, keyword, blank, fsteps, nrows+3, tp, colidn)
	    if (flag) {
		call sprintf (str1, SZ_CHVAL, iformat)
		    call pargi (fwheel+1)
		call sprintf (str2, SZ_CHVAL, iformat)
		    call pargi (fsteps)
	        call tbrptt (tp, colidn[ID_CHVAL], str1, SZ_CHVAL, 1, nrows+2)
	        call tbrptt (tp, colidn[ID_CHVAL], str2, SZ_CHVAL, 1, nrows+3)
	    }
	    nrows = nrows + 3
	}

	# check sum, execute port 1, infrequent port 2, prepare/readout port 2
	# RIU A and B commands [mnemonic 94-103]
	if (inmask[92] == OKVAL)
	    hexval = lsb[92]
	else
	    hexval = errval
	call eng_write ("U01U094A", "CHKSUM", blank, hexval, nrows+1, tp, 
			colidn)
	nrows = nrows + 1

	# Comment out code related to word 95 because it is overwritten in
	# the .x0h file, see the update file for details.  8/17/94 JC Hsu.
	#if (inmask[loc[EXEC]] == OKVAL)
	#    hexval = indata[loc[EXEC]]
	#else
	#    hexval = errval
	#call eng_write ("U01K095A", "EXECPORT", blank, hexval, nrows+1, tp, 
	#		colidn)
	#nrows = nrows + 1
	#call u_cmd (EXEC, msb, lsb, tp, colidn, nrows, loc)

	if (inmask[loc[INFREQ]] == OKVAL)
	    hexval = indata[loc[INFREQ]]
	else
	    hexval = errval
	call eng_write ("U01K097A", "INFQPORT", blank, hexval, nrows+1, tp, 
			colidn)
	nrows = nrows + 1
	call u_cmd (INFREQ, msb, lsb, tp, colidn, nrows, loc)

	if (inmask[loc[PREP]] == OKVAL)
	    hexval = indata[loc[PREP]]
	else
	    hexval = errval
	call eng_write ("U01K099A", "READOUTP", blank, hexval, nrows+1, tp, 
			colidn)
	nrows = nrows + 1
	call u_cmd (PREP, msb, lsb, tp, colidn, nrows, loc)

	if (inmask[loc[RIUA]] == OKVAL)
	    hexval = indata[loc[RIUA]]
	else
	    hexval = errval
	call eng_write ("U10K101A", "LASTRIUA", blank, hexval, nrows+1, tp, 
			colidn)
	nrows = nrows + 1

	if (inmask[loc[RIUB]] == OKVAL)
	    hexval = indata[loc[RIUB]]
	else
	    hexval = errval
	call eng_write ("U10K103A", "LASTRIUB", blank, hexval, nrows+1, tp, 
			colidn)
	nrows = nrows + 1

	# Failsafe warning
	if (indata[loc[RIUA]] == 0C180x || indata[loc[RIUB]] == 0C180x)
	    call strcpy ("Release Pyramid",	chval, SZ_MNEMONIC)
	else if (indata[loc[RIUA]] == 0F240x || indata[loc[RIUB]] == 0F240x)
	    call strcpy ("Spread Shutter",	chval, SZ_MNEMONIC)
	else if (indata[loc[RIUA]] == 0D723x || indata[loc[RIUB]] == 0D723x)
	    call strcpy ("Bank A clr sensors",	chval, SZ_MNEMONIC)
	else if (indata[loc[RIUA]] == 0E578x || indata[loc[RIUB]] == 0E578x)
	    call strcpy ("Bank B clr sensors",	chval, SZ_MNEMONIC)
	else if (indata[loc[RIUA]] == 084CEx || indata[loc[RIUB]] == 084CEx)
	    call strcpy ("Shutter A-Open",	chval, SZ_MNEMONIC)
	else if (indata[loc[RIUA]] == 094CEx || indata[loc[RIUB]] == 094CEx)
	    call strcpy ("Shutter A-Close",	chval, SZ_MNEMONIC)
	else if (indata[loc[RIUA]] == 0A4CEx || indata[loc[RIUB]] == 0A4CEx)
	    call strcpy ("Shutter B-Open",	chval, SZ_MNEMONIC)
	else if (indata[loc[RIUA]] == 0B4CEx || indata[loc[RIUB]] == 0B4CEx)
	    call strcpy ("Shutter B-Close",	chval, SZ_MNEMONIC)
	else
	    chval[1] = EOS
	call eng_write (blank, "FAILSAFE", chval, 0, nrows+1, tp, colidn)
	nrows = nrows + 1
	    
	# RIU commands received [mnemonic 105, 106]
	call strcpy ("U10J106A",	mnemonic, SZ_MNEMONIC)
	call strcpy ("CMDRCVDB",	keyword, SZ_KEYWORD)
	do k = 2, 1, -1 {
	    mnemonic[7] = '7' - k
	    keyword[8] = 'A' + (k-1)

	    if (inmask[104] == OKVAL) {
	    	j = 4 * (k-1)
	    	hexval = bitson (msb[104], j+3, j)
	    	if (hexval == 0) {
		    call strcpy ("No",	chval, SZ_CHVAL)
	    	} else {
		    call strcpy ("Yes",	chval, SZ_CHVAL)
	    	}
	    } else {
		hexval = errval
		chval[1] = EOS
	    }

	    call eng_write (mnemonic, keyword, chval, hexval, nrows+1, 
				tp, colidn)
	    nrows = nrows + 1
	}

	# 6.144 MHz Working [mnemonic 109]
	if (inmask[104] == OKVAL) {
	    hexval = bitson(lsb[104], 6, 6)
	    if (hexval == 0) {
		call strcpy ("No",	chval, SZ_CHVAL)
	    } else {
		call strcpy ("Yes",	chval, SZ_CHVAL)
	    }
	} else {
	    hexval = errval
	    chval[1] = EOS
	}
	call eng_write ("U01J109B", "WORK6144", chval, hexval, nrows+1, 
				tp, colidn)
	nrows = nrows + 1

	# Science ADC Data to mp, SDF Selected, Line Start Received,
	# RIU Eng. Data Received [mnemonic 110-114]
	call strcpy ("U01X110B",	mnemonic, SZ_MNEMONIC)

	do k = 0, 4 {
	    mnemonic[7] = '0'+ k
	    if (k >= 3) {
	    	mnemonic[2] = '1'
	    	mnemonic[3] = '0'
	    } else {
	    	mnemonic[2] = '0'
	    	mnemonic[3] = '1'
	    }

	    if (inmask[104] == OKVAL) {
	    	hexval = bitson(lsb[104], 4-k, 4-k)
	    	if (hexval == 0) {
		    call strcpy ("No",	chval, SZ_CHVAL)
		    if (k == 1)
		    	call strcpy ("A",	chval, SZ_CHVAL)
	    	} else {
		    call strcpy ("Yes",	chval, SZ_CHVAL)
		    if (k == 1)
		    	call strcpy ("B",	chval, SZ_CHVAL)
	    	}
	    } else {
		hexval = errval
		chval[1] = EOS
	    }
	    if (k == 0) 	call strcpy ("SCI2ADC",	 keyword, SZ_KEYWORD)
	    else if (k == 1) 	call strcpy ("SDFSLCT",	 keyword, SZ_KEYWORD)
	    else if (k == 2) 	call strcpy ("LNSTRCVD", keyword, SZ_KEYWORD)
	    else if (k == 3) 	call strcpy ("ENGRCVDB", keyword, SZ_KEYWORD)
	    else if (k == 4) 	call strcpy ("ENGRCVDA", keyword, SZ_KEYWORD)

	    call eng_write (mnemonic, keyword, chval, hexval, nrows+1, tp, 
				colidn)
	    nrows = nrows + 1
	}

	# RIU command gate ID [mnemonic 119, 120]
	do k = 2, 1, -1 {
	    if (k == 1) {
	        call strcpy ("U10J120A",	mnemonic, SZ_MNEMONIC)
	        call strcpy ("RIUCMIDA",	keyword, SZ_KEYWORD)
	    } else {
	        call strcpy ("U10J119A",	mnemonic, SZ_MNEMONIC)
	        call strcpy ("RIUCMIDB",	keyword, SZ_KEYWORD)
	    }

	    if (inmask[106] == OKVAL) {
	    	j = 2 * (k-1)
	    	hexval = bitson (msb[106], j+1, j)
	    	call strcpy ("Port 0",	chval, SZ_CHVAL)
	    	chval[6] = '0' + hexval + 1
	    } else {
		hexval = errval
		chval[1] = EOS
	    }

	    call eng_write (mnemonic, keyword, chval, hexval, nrows+1, 
				tp, colidn)
	    nrows = nrows + 1
	}

	# Engr. ADC [mnemonic 129]
	# (byte swapped here)
	if (inmask[108] == OKVAL)
	    hexval = lsb[108] * 100x + msb[108]
	else
	    hexval = errval
	call eng_write ("U01J129A", "ENGRADC", blank, hexval, nrows+1, tp, 
			colidn)
	nrows = nrows + 1

	# fixed bits [mnemonic 132]
	if (inmask[110] == OKVAL) {
	    hexval = bitson (msb[110], 7, 6)
	    if (hexval == 2) 
	    	call strcpy ("Normal",		chval, SZ_CHVAL)
	    else 
	    	call strcpy ("Undefined",	chval, SZ_CHVAL)
	} else {
	    hexval = errval
	    chval[1] = EOS
	}
	call eng_write ("U05X132B", "FIXBITS", chval, hexval, nrows+1, 
				tp, colidn)
	nrows = nrows + 1

	# UV flood mirror failsafe status [mnemonic 133]
	if (inmask[110] == OKVAL) {
	    hexval = bitson (msb[110], 5, 4)
	    if (hexval == 0) 
	    	call strcpy ("Disabled",	chval, SZ_CHVAL)
	    else if (hexval == 3)
	    	call strcpy ("Armed",	chval, SZ_CHVAL)
	    else 
	    	call strcpy ("Armed, Poss. Fail",	chval, SZ_CHVAL)
	} else {
	    hexval = errval
	    chval[1] = EOS
	}
	call eng_write ("U11X133B", "CALMFST", chval, hexval, nrows+1, 
				tp, colidn)
	nrows = nrows + 1

	# UV flood mirror failsafe status [mnemonic 134]
	if (inmask[110] == OKVAL) {
	    hexval = bitson (msb[110], 3, 2)
	    if (hexval == 0 || hexval == 3) 
	    	call strcpy ("Error",	chval, SZ_CHVAL)
	    else if (hexval == 1)
	    	call strcpy ("Open - Cal Mode",		chval, SZ_CHVAL)
	    else if (hexval == 2)
	    	call strcpy ("Closed - Obs Mode",	chval, SZ_CHVAL)
	} else {
	    hexval = errval
	    chval[1] = EOS
	}
	call eng_write ("U11X134B", "CALMIRS", chval, hexval, nrows+1, 
				tp, colidn)
	nrows = nrows + 1

	# blade sensors [mnemonic 135, 136]
	call strcpy ("U06X136B",	mnemonic, SZ_MNEMONIC)
	call strcpy ("SHUTOPNA",	keyword, SZ_KEYWORD)
	do k = 2, 1, -1 {
	    if (inmask[110] == OKVAL) {
	    	hexval = bitson(msb[110], k-1, k-1)
	    	if (hexval == 0) {
		    call strcpy ("Closed",	chval, SZ_CHVAL)
	    	} else {
		    call strcpy ("Open",	chval, SZ_CHVAL)
	    	}
	    } else {
	        hexval = errval
	        chval[1] = EOS
	    }
	    mnemonic[7] = '7' - k
	    keyword[8] = 'A' + (k-1)
	    call eng_write (mnemonic, keyword, chval, hexval, nrows+1, 
				tp, colidn)
	    nrows = nrows + 1
	}

	# filter clear sensors [mnemonic 137-164]
	call strcpy ("U03X137B",	mnemonic, SZ_MNEMONIC)
	call strcpy ("FLTCLA01",	keyword, SZ_KEYWORD)
	dummy[1] = lsb[110]
	dummy[2] = msb[112]
	dummy[3] = lsb[112]
	dummy[4] = msb[114]

	do k = 1, 4 {
	    j1 = 110 + (k/2) * 2
	    do i = 1, 6 {
	    	m = (i + 36 + (k-1)*6) + 4 * (k/3)
	    	mnemonic[6] = '0'+ (m/10)
	    	mnemonic[7] = '0'+ mod(m, 10)
	    	mk = i + mod(k+1, 2) * 6
		keyword[6] = 'A'+ (k/3)
		keyword[7] = '0'+ (mk/10)
		keyword[8] = '0'+ mod(mk, 10)

		if (inmask[j1] == OKVAL) {
	    	    hexval = bitson(dummy[k], i-1, i-1)
	    	    if (hexval == 0) {
		        call strcpy ("Not Clear",	chval, SZ_CHVAL)
	    	    } else {
		        call strcpy ("Clear",	chval, SZ_CHVAL)
	    	    }
		} else {
		    hexval = errval
		    chval[1] = EOS
		}
		call eng_write (mnemonic, keyword, chval, hexval, nrows+1, tp, 
				colidn)
	    	nrows = nrows + 1
	    }
	}

	# Science ADC link [mnemonic 169]
	if (inmask[114] == OKVAL)
	    hexval = lsb[114]
	else
	    hexval = errval
	call eng_write ("U01J169A", "SCIADCLN", blank, hexval, nrows+1, tp, 
			colidn)
	nrows = nrows + 1

	# shutter interpolations and speeds [mnemonic 170-181]
	call strcpy ("U06W170A",	mnemonic, SZ_MNEMONIC)

	do k = 1, 2 {
	    dummy[1] = msb[116+6*(k-1)]
	    dummy[2] = lsb[116+6*(k-1)]
	    dummy[3] = msb[118+6*(k-1)]
	    dummy[4] = lsb[118+6*(k-1)]
	    dummy[5] = msb[120+6*(k-1)]
	    dummy[6] = lsb[120+6*(k-1)]

	    call strcpy ("SHUTAINT",	keyword, SZ_KEYWORD)
	    keyword[5] = 'A' + k - 1

	    do i = 1, 6 {
	    	m = (i + 69 + (k-1)*6)
	    	j1 = m / 10
	    	j2 = mod(m, 10)
	    	mnemonic[6] = '0'+j1
	    	mnemonic[7] = '0'+j2
		shutter = SHUTINT
		if (i > 1) {
		    keyword[6] = '0' + i - 1
		    keyword[7] = EOS
		    shutter = SHUTSP
		}
		if (inmask[((i-1+(k-1)*6)/2) * 2 + 116] == OKVAL) {
		    hexval = dummy[i]
	    	    shutter = hexval * shutter
		    call sprintf (str1, SZ_CHVAL, rformat)
		    	call pargr (shutter)
		} else {
		    hexval = errval
		    str1[1] = EOS
		}
		call eng_write (mnemonic, keyword, str1, hexval, nrows+1, 
					tp, colidn)
	    	nrows = nrows + 1
	    }
	}

	# shutter speed flags [mnemonic 186]
	if (inmask[130] == OKVAL)
	    hexval = bitson (lsb[130], 2, 0)
	else
	    hexval = errval
	call eng_write ("U06J186A", blank, blank, hexval, nrows+1, tp, colidn)
	nrows = nrows + 1

	call strcpy ("SHUTSPD1",	keyword, SZ_KEYWORD)
	do i = 1, 3 {
	    keyword[8] = '0' + i
	    hexval = bitson (lsb[130], 3-i, 3-i)
	    call eng_write (blank, keyword, blank, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
	}
end
