# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"

#  w_v3i -- extract wfpc engineering data listed in Table V-3i
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  28-Jun-1991  J.-C. Hsu		coding
#------------------------------------------------------------------------------
procedure w_v3i (indata, msb, lsb, inmask, tp, colidn, errval, nrows)

						## input
int	indata[ARB], msb[ARB], lsb[ARB]
short	inmask[ARB]
pointer	tp
pointer	colidn[NCOL]
int	errval
						## inputs/outputs
int	nrows
						## local
char	mnemonic[SZ_MNEMONIC]
#char	keyword[SZ_KEYWORD]
char	chval[SZ_CHVAL]
int	hexval1, hexval2
int	i, j, j1, j2
int	k, m
#==============================================================================
begin

	# Error messages [mnemonic 210, 223]
	call strcpy ("W07U210A",	mnemonic, SZ_MNEMONIC)
	do k = 1, 14 {
	    m = 10 + k - 1
	    j1 = m / 10
	    j2 = mod(m, 10)
	    mnemonic[6] = '0'+j1
	    mnemonic[7] = '0'+j2
	    i = 164 + (k-1) * 4
	    j = 166 + (k-1) * 4

	    # check data quality mask
	    if (inmask[i] == OKVAL && inmask[j] == OKVAL) {
		hexval1 = indata[i] 
  		hexval2 = indata[j]
	    	if (msb[i] == 0)
		    call strcpy ("No Error",		chval, SZ_CHVAL)
	    	else if (msb[i] == 1)
		    call strcpy ("Timing Error",	chval, SZ_CHVAL)
	    	else if (msb[i] == 2)
		    call strcpy ("Spare RAM",		chval, SZ_CHVAL)
	    	else if (msb[i] == 3)
		    call strcpy ("Primary RAM",		chval, SZ_CHVAL)
	    	else if (msb[i] == 4)
		    call strcpy ("Command Count",	chval, SZ_CHVAL)
	    	else if (msb[i] == 5)
		    call strcpy ("Command Change", 	chval, SZ_CHVAL)
	    	else if (msb[i] == 6)
		    call strcpy ("Shutter Encoder", 	chval, SZ_CHVAL)
	    } else {
	        hexval1 = errval
	        hexval2 = errval
	        chval[1] = EOS
	    }

	    call eng_write (mnemonic, "", chval, hexval1, nrows+1, tp, colidn)
	    call eng_write ("", "", "", hexval2, nrows+2, tp, colidn)
	    nrows = nrows + 2
	}
end
