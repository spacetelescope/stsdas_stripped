# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include	"engextr.h"

#  engextr_grp  -- compare engineering data between groups and compute video
#		 offsets for each group
#
#  Description:
#  ------------
#  
#  Input CL parameters:
#  -----------------
#
#  Date		Author			Description
#  ----		------			-----------
#  24-Sep-1991  J.-C. Hsu		coding
#------------------------------------------------------------------------------
procedure engextr_gr(ifile, ipin, npix, tp, colidn, nrows)

						## input
char	ifile[SZ_FNAME]
pointer	ipin
int	npix
pointer	tp
pointer	colidn[NCOL]
int	nrows
						## local
pointer	sp
pointer	arrdata
pointer	data, msb, lsb, msb1, lsb1
char	rformat[SZ_LINE], keyword[SZ_KEYWORD], chval[SZ_CHVAL]
int	dummy, i, j, np[2]
int	group, ngr, nerr
real	xmin, xmax
real	sum[2]

pointer	imgs2s()
int	gf_gcount()
int	andi()
#==============================================================================
begin

	# define output format
	call sprintf(rformat, SZ_LINE, "%%%d.2f")
	    call pargi(SZ_CHVAL)

	# allocate memory 
	call smark(sp)
	call salloc(data, npix, TY_INT)
	call salloc(msb, npix, TY_INT)
	call salloc(lsb, npix, TY_INT)
	call salloc(msb1, npix, TY_INT)
	call salloc(lsb1, npix, TY_INT)

	ngr = gf_gcount(ipin)

	do group = 1, 4 {

	    do j = 1, 2 {
	        np[j] = 0
	        sum[j] = 0.
	    }
	    if (group <= ngr) {
		call gf_opengr(ipin, group, xmin, xmax, 0)

	    	# read data from input file
	    	arrdata = imgs2s(ipin, ENGCOL, ENGCOL, 1, npix)

	    	# preserve the positive values of input data, and separate the 
	    	# two bytes into MSB and LSB
	    	do i = 1, npix {
	    	    dummy = Mems[arrdata+i-1]
	            Memi[data-1+i] = andi(dummy, 0FFFFx)
		    Memi[lsb-1+i] = andi(dummy, 0FFx)
		    Memi[msb-1+i] = andi(dummy,0FF00x) / 100x
	        }	
	        if (group == 1) {
		    do i = 1, npix {
		        Memi[lsb1-1+i] = Memi[lsb-1+i]
		        Memi[msb1-1+i] = Memi[msb-1+i]
		    }
	        } else {
		    nerr = 0
		    if (Memi[lsb] != Memi[lsb1] || Memi[msb] != Memi[msb1])
			nerr = nerr + 1
		    do i = 2, npix, 2 {
		        if (Memi[lsb-1+i] != Memi[lsb1-1+i] ||
		    	        Memi[msb-1+i] != Memi[msb1-1+i])
			    nerr = nerr + 1
		    }
		    if (nerr != 0) {
		        call printf(
			    "Group %d has %d points different from Group 1\n")
			    call pargi(group)
			    call pargi(nerr)
		    }
	        }

	        # do the video bias averaging
	        do i = 3, 399, 2 {
	   	    sum[1] = sum[1] + real(Memi[lsb-1+i]) + real(Memi[msb-1+i])
		    np[1] = np[1] + 2 
		}
	        sum[1] = sum[1] / real(np[1])
		if (npix == 800) {
	            do i = 401, 799, 2 {
	   	    	sum[2] = sum[2] + real(Memi[lsb-1+i]) + 
					real(Memi[msb-1+i])
		        np[2] = np[2] + 2 
		    }
	            sum[2] = sum[2] / real(np[2])
		}
	    }
	    call strcpy("BIAS1T",    keyword, SZ_KEYWORD)
	    keyword[5] = '0' + group
            call sprintf(chval, SZ_CHVAL, rformat)
	        call pargr(sum[1])
	    call eng_write("", keyword, chval, 0, nrows+1, tp, colidn)
	    keyword[6] = 'B'
            call sprintf(chval, SZ_CHVAL, rformat)
	        call pargr(sum[2])
	    call eng_write("", keyword, chval, 0, nrows+2, tp, colidn)
	    nrows = nrows + 2
	}
	call sfree(sp)
end
