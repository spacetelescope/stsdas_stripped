# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"

#  w_v3f -- extract wfpc engineering data listed in Table V-3f
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  29-May-1991  J.-C. Hsu		coding
#------------------------------------------------------------------------------
procedure w_v3f (indata, inmask, tp, colidn, errval, nrows)

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

	# temperatures [mnemonic 1-47]
	# first row
	call strcpy ("W01T001A",	mnemonic, SZ_MNEMONIC)
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
	do i = 2, 46, 2 {
	    if (inmask[i] == OKVAL) {
		hexval = indata[i]
	    	tempcode = eng16to12 (hexval, status)
		if (status) {
		    if (i <= 30) {
	    	    	call templin (tempcode, 1, temp) 
		    } else {
		    	cj = (i - 30) / 2
	    	    	call temppoly (tempcode, cj, temp) 
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
