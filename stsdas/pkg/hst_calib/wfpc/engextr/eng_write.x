# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"

#  eng_write -- write a row of engineering data to the output table
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  26-Jun-1991  J.-C. Hsu		coding
#------------------------------------------------------------------------------
procedure eng_write (mnemonic, keyword, chval, hexval, nrows, tp, colidn)

						## input
char	mnemonic[SZ_MNEMONIC]
char	keyword[SZ_KEYWORD]
char	chval[SZ_CHVAL]
int	hexval
int	nrows
pointer	tp
pointer	colidn[NCOL]
#==============================================================================
begin
	if (mnemonic[1] != EOS)
	    call tbrptt (tp, colidn[ID_MNEMONIC], mnemonic, SZ_MNEMONIC, 1, 
				nrows)
	if (keyword[1] != EOS)
	    call tbrptt (tp, colidn[ID_KEYWORD], keyword, SZ_KEYWORD, 1, nrows)
	if (chval[1] != EOS)
	    call tbrptt (tp, colidn[ID_CHVAL], chval, SZ_CHVAL, 1, nrows)

	call tbrpti (tp, colidn[ID_HEXVAL], hexval, 1, nrows)
end
