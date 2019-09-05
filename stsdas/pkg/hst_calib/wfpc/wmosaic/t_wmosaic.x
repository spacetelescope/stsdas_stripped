include <imhdr.h>
include	"wmosaic.h"

#  t_wmosaic -- put the geometrically corrected four frames of a WFPC image 
#		together
#
#  Description:
#  ------------
#  
#  Date		Author		Description
#  ----		------		-----------
#  26-Feb-1993  J.-C. Hsu	Version 1.0: design and coding
#  25-Jun-1993  J.-C. Hsu	Version 1.1: use better border equations
#  06-Jul-1994  J.-C. Hsu	Version 1.2: Include WFPC2
#  23-Jun-1995  J.-C. Hsu	Version 2.1: Add paddings to the seam
#  10-Mar-2003  J.-C. Hsu	Version 3.0: Works for both GEIS and FITS input
#------------------------------------------------------------------------------

procedure t_wmosaic()

pointer	tpin, tpout
			# file template pointers
int	nfin
			# number of input, flat field, and output files
pointer	ipin, ipout
int	ngrp
char	fout[SZ_FNAME]
real	xmin, xmax
int	j , k, grp
pointer	op, norm
real	crpix1, crpix2, cd1_1, cd1_2, cd2_1, cd2_2
double	crval1, crval2
char    text[SZ_LINE], tstring[SZ_LINE]
	
pointer	gf_map()
pointer	imps2r()
real	imgetr()
double	imgetd()
long	clktime()
#==============================================================================
begin

	# read the input parameters
	call mosaic_in (tpin, tpout, nfin)

	# print out the version number
	call printf ("WMOSAIC version: %s\n")
	    call pargstr (VERSION)

	# allocate memory
	call malloc (norm, SZ_OUT*SZ_OUT, TY_REAL)

	# loop all input files
	do k = 1, nfin {
	 
	    # check out input file's attributes
	    call mosaic_check (tpin, tpout, ipin, fout, ngrp)
	
	    # create the output file using the 4th group as the template
	    call gf_opengr (ipin, 4, xmin, xmax, 0)
	    ipout = gf_map (fout, NEW_COPY, ipin)
	    IM_LEN(ipout, 1) = SZ_OUT
	    IM_LEN(ipout, 2) = SZ_OUT
	    op = imps2r(ipout, 1, SZ_OUT, 1, SZ_OUT)

	    # reset the output buffer
	    do j = 1, SZ_OUT*SZ_OUT {
	        Memr[op+j-1] = 0.
	        Memr[norm+j-1] = 0.
	    }

	    # loop all groups
	    do grp = 1, ngrp {

	        # open the group and read the data
	        call gf_opengr (ipin, grp, xmin, xmax, 0)

	        # perform the distortion correction
		call mosaic_do (ipin, Memr[op], Memr[norm])

		# get the reference CRPIX's
		if (grp == REFGROUP) {
		    crpix1 = imgetr (ipin, "CRPIX1")
		    crpix2 = imgetr (ipin, "CRPIX2")
		    crval1 = imgetd (ipin, "CRVAL1")
		    crval2 = imgetd (ipin, "CRVAL2")
		    cd1_1 = imgetr (ipin, "CD1_1")
		    cd1_2 = imgetr (ipin, "CD1_2")
		    cd2_1 = imgetr (ipin, "CD2_1")
		    cd2_2 = imgetr (ipin, "CD2_2")
		}    
	    }

	    # normalize to compensate for the flux redistribution due to 
	    # geometric correction
	    do j = 1, SZ_OUT*SZ_OUT {
		if (Memr[norm+j-1] == 0.) next
		Memr[op+j-1] = Memr[op+j-1] / Memr[norm+j-1]
	    }

	    # calculate wcs for the output file
	    call gf_iputr(ipout, "CRPIX1", real(SZ_OUT)-(crpix1+790.))
	    call gf_iputr(ipout, "CRPIX2", real(SZ_OUT)-(crpix2+790.))
	    call gf_iputd(ipout, "CRVAL1", crval1)
	    call gf_iputd(ipout, "CRVAL2", crval2)
	    call gf_iputr(ipout, "CD1_1", -cd1_1)
	    call gf_iputr(ipout, "CD1_2", -cd1_2)
	    call gf_iputr(ipout, "CD2_1", -cd2_1)
	    call gf_iputr(ipout, "CD2_2", -cd2_2)

	    # record the time this task is run
 	    call cnvtime (clktime(0), tstring, SZ_LINE)
            call sprintf (text, SZ_LINE, "Ran task WMOSAIC, version %s, at %s")
            	call pargstr (VERSION)
            	call pargstr (tstring)
	    call gf_iputh(ipout, "HISTORY", text)

	    # close the input/output file
	    call gf_unmap (ipin)
	    call gf_unmap (ipout)
	}

	call mfree (norm, TY_REAL)
end
