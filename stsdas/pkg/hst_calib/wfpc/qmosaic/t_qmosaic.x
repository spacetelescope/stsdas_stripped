include <imhdr.h>
include	"qmosaic.h"

#  t_qmosaic -- Do a quick mosaic of the four WFPC or WFPC2 frames 
#
#  Description:
#  ------------
#  
#  Date		Author		Description
#  ----		------		-----------
#  31-Mar-1994  J.-C. Hsu	Ver 2.0: redesign to simplify the user 
#				interface and to include WFPC2
#  24-Apr-2003  J.-C. Hsu	Ver 2.1: Make working for FITS extension files
#------------------------------------------------------------------------------

procedure t_qmosaic()

pointer	tpin 		# file template pointers
int	nfin 		# number of input files
pointer	ipin, ipout
char	interp[SZ_LINE]
int	ngrp
char	fin[SZ_FNAME], fout[SZ_FNAME]
real	xmin, xmax
int	j, k, grp
int	iccd
real	orient
pointer	op
char    text[SZ_LINE], tstring[SZ_LINE]
	
pointer	gf_map()
pointer	imps2r()
int	imgeti()
real	imgetr()
long	clktime()
#==============================================================================
begin

	# read the input parameters
	call qmosaic_in (tpin, fout, nfin, interp)

	# loop all input files
	do k = 1, nfin {
	 
	    # check out input file's attributes
	    call qmosaic_check (tpin, ipin, fin, ngrp)
	
	    # create the output file using the first input image as the template
	    if (k == 1) {
	    	ipout = gf_map (fout, NEW_COPY, ipin)
	    	IM_LEN(ipout, 1) = SZ_OUT
	    	IM_LEN(ipout, 2) = SZ_OUT
	    	op = imps2r(ipout, 1, SZ_OUT, 1, SZ_OUT)

	    	# reset the output buffer
	    	do j = 1, SZ_OUT*SZ_OUT {
	            Memr[op+j-1] = 0.
	    	}
	    }

	    # loop all groups
	    do grp = 1, ngrp {

	        # open the group and read the data
	        if (ngrp != 1) call gf_opengr (ipin, grp, xmin, xmax, 0)

	        # put the frames into the proper quardrants
		call qmosaic_do (ipin, fin, grp, fout, Memr[op], interp)
	    }

	    # read the orientation info from last group of last image
	    if (k == nfin) {
		iccd = imgeti (ipin, "DETECTOR")
		orient = imgetr (ipin, "ORIENTAT")
	    }

	    # close the input file
	    call gf_unmap (ipin)
	}

	# write orientation to the output image
	orient = orient - 90. * real(mod(iccd, 4))
	call gf_iputr(ipout, "ORIENTAT", orient)

	# write null wcs keywords to the output file
	call gf_iputr(ipout, "CRPIX1", 0.)
	call gf_iputr(ipout, "CRPIX2", 0.)
	call gf_iputd(ipout, "CRVAL1", 0.)
	call gf_iputd(ipout, "CRVAL2", 0.)
	call gf_iputr(ipout, "CD1_1", 0.)
	call gf_iputr(ipout, "CD1_2", 0.)
	call gf_iputr(ipout, "CD2_1", 0.)
	call gf_iputr(ipout, "CD2_2", 0.)

	# record the time this task is run
 	call cnvtime (clktime(0), tstring, SZ_LINE)
        call sprintf (text, SZ_LINE, 
		"Ran task QMOSAIC, version %s, at %s; interp = %s")
            call pargstr (VERSION)
            call pargstr (tstring)
            call pargstr (interp)
	call gf_iputh(ipout, "HISTORY", text)

	call gf_unmap (ipout)
end
