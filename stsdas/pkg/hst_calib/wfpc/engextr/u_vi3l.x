# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"

#  u_vi3l -- extract wfpc2 engineering data listed in Table VI-3l
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  08-Dec-1993  J.-C. Hsu		adapted from w_v3h.x
#------------------------------------------------------------------------------
procedure u_vi3l (indata, msb, lsb, inmask, tp, colidn, errval, nrows)

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
char	keyword[SZ_KEYWORD]
char	chval[SZ_CHVAL]
int	hexval
int	i, j1, j2, j3
int	k, m
#==============================================================================
begin

	# microprocessor registers [mnemonic 190, 205]
	call strcpy ("U07U190A",	mnemonic, SZ_MNEMONIC)
	call strcpy ("CLS3RG00",	keyword, SZ_KEYWORD)
	do k = 1, 16 {
	    m = 190 + k - 1
	    j1 = m / 100
	    j2 = mod(m, 100) / 10
	    j3 = mod(m, 10)
	    mnemonic[5] = '0'+j1
	    mnemonic[6] = '0'+j2
	    mnemonic[7] = '0'+j3
	    keyword[7] = '0'+(k-1)/10
	    keyword[8] = '0'+mod((k-1),10)

	    i = 132 + (k-1) * 2

	    # check data quality mask
	    if (inmask[i] == OKVAL) {
		hexval = indata[i]
		chval[1] = EOS

		# constant fields
		if (k == 14 && indata[i] == 176Dx)
		    call strcpy ("OK",	chval, SZ_CHVAL)
		else if (k == 16 && indata[i] == 0FF00x)
		    call strcpy ("OK",	chval, SZ_CHVAL)
		else if (k == 2 && indata[i] == 1202x)
		    call strcpy ("OK",	chval, SZ_CHVAL)
		else if (k == 3 && indata[i] == 40FBx)
		    call strcpy ("OK",	chval, SZ_CHVAL)
	    	else if (k == 7) {
		    if (hexval == 06B2x)
		        call strcpy ("Failsafe",	chval, SZ_CHVAL)
		    else if (hexval == 0743x)
		        call strcpy ("Move Shutter",	chval, SZ_CHVAL)
		    else if (hexval == 07A3x)
		        call strcpy ("Infrequent",	chval, SZ_CHVAL)
		    else if (hexval == 084Cx)
		        call strcpy ("Prepare",		chval, SZ_CHVAL)
		    else if (hexval == 0B7Cx)
		        call strcpy ("Expose",		chval, SZ_CHVAL)
		    else if (hexval == 0E76x)
		        call strcpy ("Readout",		chval, SZ_CHVAL)
	    	    else 
		        call strcpy ("Illegal value", 	chval, SZ_CHVAL)
		}
	    } else {
	        hexval = errval
		chval[1] = EOS
	    }

	    call eng_write (mnemonic, keyword, chval, hexval, nrows+1, 
				tp, colidn)
	    nrows = nrows + 1
	}
end
