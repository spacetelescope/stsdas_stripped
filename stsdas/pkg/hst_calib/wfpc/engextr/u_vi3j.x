# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"

#  u_vi3j -- extract wfpc2 engineering data listed in Table VI-3j
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  08-Dec-1993  J.-C. Hsu		adapted from w_v3f.x
#------------------------------------------------------------------------------
procedure u_vi3j (indata, inmask, tp, colidn, errval, nrows)

						## input
int	indata[ARB]
short	inmask[ARB]
pointer	tp
pointer	colidn[NCOL]
int	errval
						## inputs/outputs
int	nrows
						## local
char	mnemonic[SZ_MNEMONIC]
#char	keyword[SZ_KEYWORD]
char	str1[SZ_CHVAL]
char	rformat[SZ_LINE]
real	temp
int	hexval, tempcode, i, cj
bool	status

int	eng16to12()
#==============================================================================
begin
	call sprintf (rformat, SZ_LINE, "%%%d.2f")
	    call pargi (SZ_CHVAL)

	# temperatures [mnemonic 1-39]
	# first row
	call strcpy ("U01T001A",	mnemonic, SZ_MNEMONIC)
	if (inmask[1] == OKVAL) {
	    hexval = indata[1]
	    tempcode = eng16to12 (hexval, status)
	    if (status) {
	    	call templin (tempcode, 1, temp) 
	    	call sprintf (str1, SZ_CHVAL, rformat)
		    call pargr (temp)
	    } else
		str1[1] = EOS
	} else {
	    hexval = errval
	    str1[1] = EOS
	}

	# write to the output table
	call eng_write (mnemonic, "", str1, hexval, nrows+1, tp, colidn)
	nrows = nrows + 1

	# the rest of rows
	do i = 2, 38, 2 {
	    if (i==10 || i==12 || i==14 || i==22 || i==28 || i==30) next
	    if (inmask[i] == OKVAL) {
		hexval = indata[i]
	    	tempcode = eng16to12 (hexval, status)
		if (status) {
		    if (i <= 30) {
	    	    	call templin (tempcode, 1, temp) 
		    } else {
		    	cj = (i - 30) / 2 + 1
	    	    	call templin (tempcode, cj, temp) 
		    }
	    	    call sprintf (str1, SZ_CHVAL, rformat)
		    	call pargr (temp)
		} else 
		    str1[1] = EOS
	    } else {
	    	hexval = errval
		str1[1] = EOS
	    }

	    mnemonic[6] = '0'+ (i+1)/10
	    mnemonic[7] = '0'+ mod((i+1),10)
	    if (i <= 14)
	    	mnemonic[3] = '1'
	    else if (i >= 16 && i <= 22)
	    	mnemonic[3] = '4'
	    else if (i >= 24 && i <= 26)
	    	mnemonic[3] = '2'
	    else if (i >= 28)
	    	mnemonic[3] = '4'
	    
	    # write to the output table
	    call eng_write (mnemonic, "", str1, hexval, nrows+1, tp, colidn)
	    nrows = nrows + 1
	}
end
