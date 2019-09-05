include	<mach.h>
include	<math.h>
include "uchscale.h"

#  t_uchscale -- Change plate scale of a WFPC/WFPC2 image 
#
#  Description:
#  ------------
#  
#  Date		Author		Description
#  ----		------		-----------
#  23-Sep-1994  J.-C. Hsu	Version 1.0: design and coding
#  29-Apr-2003  J.-C. Hsu	Version 1.1: make to work for FITS files
#------------------------------------------------------------------------------

procedure t_uchscale()

pointer	tpin
			# file template pointers
int	nfin
			# number of input, flat field, and output files
pointer	ipin
char	fin[SZ_LINE]
char    cluster[SZ_PATHNAME]
char    sect[SZ_FNAME]
char    ksect[SZ_FNAME]
int     ngrp, igrp, cl_index, cl_size
int	acmode
real	xmin, xmax
int	k, grp
int	nchar
int	nscale, scale_cam[MAX_GROUP]
int	instru
real	cd1_1, cd1_2, cd2_1, cd2_2
char    strscale[SZ_LINE], tstring[SZ_LINE], strval[SZ_LINE], text[SZ_LINE]
real	pscale
real	pa
real	newscale[MAX_GROUP]
	
int	strtor()

pointer	gf_map()
int	imtgetim()
real	imgetr()
int	gf_gcount()
long	clktime()
bool	streq()
#==============================================================================
begin

	# read the input parameters
	call uchscale_in(tpin, nfin, strscale)
	call strlwr(strscale)
	acmode = READ_WRITE

        nscale = strtor(strscale, newscale)
        if (nscale > MAX_GROUP)
            call error(1, "too many input scales")

	if (nscale == 0)
	    acmode = READ_ONLY
	else {

	    # figure out plate scale's "unit"
	    do k = 1, nscale {
	        if (newscale[k] > PCSCALE/(1.+TOL) && 
			newscale[k] < PCSCALE*(1.+TOL))
	            scale_cam[k] = PC
	        else if (newscale[k] > WFSCALE/(1.+TOL) && 
			newscale[k] < WFSCALE*(1.+TOL))
	            scale_cam[k] = WF
	        else if (newscale[k] > PCSCALE_AS/(1.+TOL) && 
			newscale[k] < PCSCALE_AS*(1.+TOL)) {
		    newscale[k] = newscale[k]/3600.
	            scale_cam[k] = PC
	        } else if (newscale[k] > WFSCALE_AS/(1.+TOL) && 
			newscale[k] < WFSCALE_AS*(1.+TOL)) {
		    newscale[k] = newscale[k]/3600.
	            scale_cam[k] = WF
	        } else
		    call error(1, "unreasonable input new scales")
	    }

	    # use the last newscale to fill in, if the user supplies fewer 
	    # newscale's
	    if (nscale < MAX_GROUP) {
	        do k = nscale+1, MAX_GROUP {
		    newscale[k] = newscale[nscale]
		    scale_cam[k] = scale_cam[nscale]
	        }
	    }
	}

	# loop all input files
	do k = 1, nfin {
	 
	    # read the next file name in the template list
	    nchar = imtgetim(tpin, fin, SZ_FNAME) 

	    # open the input image
            call imparse(fin, cluster, SZ_PATHNAME, ksect, SZ_FNAME,
                            sect, SZ_FNAME, cl_index, cl_size)

	    # open with the cluster name, not the input file name, to insure
	    # group parameters get updated when a particular group is specified
	    iferr (ipin = gf_map(cluster, acmode, 0)) {
	        call printf("input data file %s does not exist\n")
	    	    call pargstr(fin)
		call flush(STDOUT)
	        next
	    }

	    # determine how many input groups 
            ngrp = 1
            if (cl_index <= 0)
                ngrp = gf_gcount(ipin)
	
	    # must be WFPC or WFPC2
            iferr (call imgstr(ipin, "INSTRUME", strval, SZ_LINE)) {
	        call printf("no INSTRUME keyword in the header of file %s\n")
	    	    call pargstr(fin)
		call flush(STDOUT)
		call gf_unmap(ipin)
	        next
	    }
            if (streq(strval, "WFPC2")) {
		instru = WFPC2
	    } else if (streq(strval, "WFPC")) {
                call imgstr(ipin, "CAMERA", strval, SZ_LINE)
                if (streq(strval, "WF")) {
		    instru = WF
                } else if (streq(strval, "PC")) {
		    instru = PC
		}
	    } else {
	        call printf("Illegal instrument %s in the header of file %s\n")
	    	    call pargstr(strval)
	    	    call pargstr(fin)
		call flush(STDOUT)
		call gf_unmap(ipin)
	        next
	    }

	    # record the time this task is run
	    if (strscale[1] != EOS) {
 	    	call cnvtime(clktime(0), tstring, SZ_LINE)
            	call sprintf(text, SZ_LINE, 
				"Ran task UCHSCALE, version %s, at %s")
            	    call pargstr(VERSION)
            	    call pargstr(tstring)
	        call gf_iputh(ipin, "HISTORY", text)
	    }

	    # loop all groups
	    do grp = 1, ngrp {

		# point to the desired group (esp. when group no. is specified)
            	igrp = cl_index
            	if (cl_index <= 0) igrp = grp

	        # open the group and read the data
	        call gf_opengr(ipin, igrp, xmin, xmax, 0)

		# read CD matrix
		cd1_1 = imgetr(ipin, "CD1_1")
		cd1_2 = imgetr(ipin, "CD1_2")
		cd2_1 = imgetr(ipin, "CD2_1")
		cd2_2 = imgetr(ipin, "CD2_2")
		pscale = sqrt(abs(cd1_1*cd2_2 - cd1_2*cd2_1))

		# print the current plate scales if not updating, otherwise
		# print the new plate scales
		if (nscale == 0) {
		    call printf(
	"%s[%1d]: plate scale is %0.4e deg/pixel (%0.5f\"/pixel)\n")
			call pargstr(cluster)
		    	call pargi(igrp)
		    	call pargr(pscale)
		    	call pargr(pscale*3600.)

		} else {
		    if (pscale < EPSILON) { 
			pa = imgetr(ipin, "ORIENTAT") / RADIAN
			cd1_1 = -newscale[grp] * cos(pa)
			cd1_2 =  newscale[grp] * sin(pa)
			cd2_1 =  newscale[grp] * sin(pa)
			cd2_2 =  newscale[grp] * cos(pa)
		    } else {
		        cd1_1 = cd1_1 * newscale[grp] / pscale
		        cd1_2 = cd1_2 * newscale[grp] / pscale
		        cd2_1 = cd2_1 * newscale[grp] / pscale
		        cd2_2 = cd2_2 * newscale[grp] / pscale
		    }
            	    call sprintf(text, SZ_LINE, "%s[%1d]: change plate scale from %0.4e deg/pixel (%0.5f\"/pixel) to %0.4e deg/pixel (%0.5f\"/pixel)")
			call pargstr(cluster)
		        call pargi(igrp)
		        call pargr(pscale)
		        call pargr(pscale*3600.)
		        call pargr(newscale[grp])
		        call pargr(newscale[grp]*3600.)

	            # write the new CD matrix back
	            call gf_iputr(ipin, "CD1_1", cd1_1)
	            call gf_iputr(ipin, "CD1_2", cd1_2)
	            call gf_iputr(ipin, "CD2_1", cd2_1)
	            call gf_iputr(ipin, "CD2_2", cd2_2)
		    call printf("%s\n")
			call pargstr(text)
	            call gf_iputh(ipin, "HISTORY", text)
		}
	    }

	    # close the input file
	    call gf_unmap(ipin)
	}

        # close file template
        call imtclose(tpin)
end
