include	<mach.h>
include	<math.h>
include "uchcoord.h"

#  t_uchcoord -- Modify WFPC2 group parameters caused by the PDB WFPC2 
#		 aperture parameter changes in order to make the
#		 absolute position correct.
#
#  Description:
#  ------------
#  
#  Date		Author		Description
#  ----		------		-----------
#  16-Nov-1995  J.-C. Hsu	design and coding
#  06-Feb-1996  J.-C. Hsu	Version 1.0: add the rotation (v3 to Y) change
#  05-Jul-1996  J.-C. Hsu	Version 1.1: change the two epoch boundaries
#  30-Jun-2000  J.-C. Hsu	Version 1.2: Add SIAF 13.0, 14.0 and 25.0. Also 
#					     apply coefficients based on 
#					     detector number, not group number
#  20-Mar-2001  J.-C. Hsu	Version 2.0: make change to accomodate the new
#					     keyword PROCDATE in OTFR
#  25-Apr-2003  J.-C. Hsu	Version 2.1: make it to work for FITS files
#  10-Jul-2003  J.-C. Hsu       Version 2.2: use PROCTIME instead of PROCDATE 
#                                            to make OTFR to work properly
#-------------------------------------------------------------------------------
										
procedure t_uchcoord()

pointer	tpin 		# file template pointers
int	nfin 		# number of input files
pointer	ipin
char	fin[SZ_LINE]
char    cluster[SZ_PATHNAME]
char    sect[SZ_FNAME]
char    ksect[SZ_FNAME]
int     ngrp, igrp, cl_index, cl_size
int	acmode
real	xmin, xmax
int	j, k, grp
int	nchar
int	instru
real    cd1_1, cd1_2, cd2_1, cd2_2
real	crpix1, crpix2
bool	update
double	ra, dec, ra_new, dec_new
double	exptm
real    v2[MAX_GROUP, MAX_EPOCH]
real    v3[MAX_GROUP, MAX_EPOCH]
real    scale[MAX_GROUP, MAX_EPOCH]
real    beta_y[MAX_GROUP, MAX_EPOCH]
real    xref[MAX_GROUP, MAX_EPOCH]
real    yref[MAX_GROUP, MAX_EPOCH]
real	del_v2
real	del_v3
char    tstring[SZ_LINE], strval[SZ_LINE], text[SZ_LINE]
real    old_pscale, pscale
real	pa_v3, pa, old_orient, orient, v3_to_y
double	epoch[MAX_EPOCH+1]
char	epochs[LEN_EPOCH, MAX_EPOCH]
bool	real_change[MAX_EPOCH+1]
int	det, ep, ep0, epoch_to_use, current_epoch, original_epoch, rc_after
double	pdbepoch, procdate
bool	pdb, otfr
	
pointer	gf_map()
int	imtgetim()
real	imgetr()
double	imgetd()
int	imgeti()
int	gf_gcount()
long	clktime()
bool	streq()
#==============================================================================
begin

	# read the aperture parameters
	call uchcoord_pdb (v2, v3, scale, beta_y, xref, yref, 
				epoch, epochs, real_change, ep0)

	# read the input parameters
	call uchcoord_in (tpin, nfin, update)
	if (update) acmode = READ_WRITE
	else acmode = READ_ONLY

	# loop all input files
	do k = 1, nfin {
	 
	    # read the next file name in the template list
	    nchar = imtgetim (tpin, fin, SZ_FNAME) 

	    # open the input image
            call imparse (fin, cluster, SZ_PATHNAME, ksect, SZ_FNAME,
                            sect, SZ_FNAME, cl_index, cl_size)

	    # open with the cluster name, not the input file name, to insure
	    # group parameters get updated when a particular group is specified
	    iferr (ipin = gf_map (cluster, acmode, 0)) {
	        call printf ("input data file %s does not exist\n")
	    	    call pargstr (fin)
		call flush (STDOUT)
	        next
	    }

	    # determine how many input groups 
            ngrp = 1
            if (cl_index <= 0)
                ngrp = gf_gcount(ipin)
	
	    # must be WFPC2
            iferr (call imgstr (ipin, "INSTRUME", strval, SZ_LINE)) {
	        call printf ("no INSTRUME keyword in the header of file %s\n")
	    	    call pargstr (fin)
		call flush (STDOUT)
		call gf_unmap (ipin)
	        next
	    }
            if (streq (strval, "WFPC2")) {
		instru = WFPC2
	    } else {
	        call printf ("Illegal instrument %s in the header of file %s\n")
	    	    call pargstr (strval)
	    	    call pargstr (fin)
		call flush (STDOUT)
		call gf_unmap (ipin)
	        next
	    }

	    # record the time this task is run
 	    	call cnvtime (clktime(0), tstring, SZ_LINE)
            	call sprintf (text, SZ_LINE, 
				"Ran task UCHCOORD, version %s, at %s")
            	    call pargstr (VERSION)
            	    call pargstr (tstring)
	        call gf_iputh(ipin, "HISTORY", text)
	    
	    # read the V3 position angle and exposure start time
	    pa_v3 = imgetr (ipin, "PA_V3")
	    exptm = imgetd (ipin, "EXPSTART")

	    iferr(pdbepoch = imgetd (ipin, "PDBEPOCH")) pdb = false
	    else pdb = true
	
	    iferr(procdate = imgetd (ipin, "PROCTIME")) otfr = false
	    else otfr = true
	
	    # New in version 2.0:
	    # Step (1): 
	    #	Campute rc_after, the first "real-change" PDB-epoch after 
	    #	expstart (note, the latest PDB-epoch, which is set to 
	    #	infinity, is also set to be a "real-change" epoch to make 
	    #	this step work)
	    #
	    # Step (2):
	    #	Compute epoch_to_use, the PDB-epoch immediately before 
	    #	rc_after (may be either "real-change" or "better-knowledge")
	    #
	    # Step (3): 
	    #	Determine which PDB-epoch was originally applied to this file 
	    #	(original_epoch):
	    #
	    # 	(a) if procdate does not exist:
	    # 	  original_epoch = the PDB-epoch immediately before expstart
	    #
	    #   (b) if procdate exists:
	    #	  if epoch_to_use is before procdate:
	    #	    original_epoch = epoch_to_use
	    #	  else:
	    #	    original_epoch = the latest PDB-epoch which is *before*
	    #	    		procdate *and* before or equal to epoch_to_use
	    #	
	    # Step (4): 
	    #	Find out if the file has been processed by uchcoord before, 
	    #	i.e. find out the PDB-epoch being applied most recently to 
	    #	this file (current_epoch):
	    #
	    #	if pdbepoch exists:
	    #		current_epoch = pdbepoch
	    #	else:
	    #		current_epoch = original_epoch
	    #
	    # Step (5): 
	    #	The PDB-epoch *should* be applied to this file now is
	    #	(epoch_to_use):
	    #
	    #	Update the WCS info in the header(s) if 
	    #	current_epoch < epoch_to_use
	    #
	    # Step (6): 
	    #	Update/add the keyword pdbepoch to epoch_to_use.

	    # determine which epoch does the observation fall in (need to 
	    # edit UCHCOORD_PDB.X when a new installation becomes available)
	    ep = 0
	    do j = 1, ep0 {
	    	if (exptm >= epoch[j] && exptm < epoch[j+1]) {
		    ep = j
		    break
		}
	    }
	    if (!otfr) original_epoch = ep

	    # Step (1), (2)
	    # compute the "real-change" epoch after ep, and the epoch to be used
	    do j = ep+1, ep0+1 {
		if (real_change[j]) {
		    rc_after = j
		    break
		}
	    }
	    epoch_to_use = j-1
	
	    # Step (3)
	    if (otfr) {
		if (epoch[epoch_to_use] < procdate) {
		    original_epoch = epoch_to_use
		} else {
		    do j = epoch_to_use, ep, -1 {
			if (epoch[j] < procdate) {
			    original_epoch = j
			    break
			}
		    }
		}
	    }

	    # Step (4)
	    # if this image was updated by this task before, use PDBEPOCH 
	    # as current epoch
	    current_epoch = original_epoch
	    if (pdb) {
	        do j = 1, ep0 {
	    	    if (abs(pdbepoch - epoch[j]) <= EPSILON) {
		        current_epoch = j
		        break
		    }
	        }
	    }
	    if (!update) {
		call printf ("The epoch of the PDB parameters used in %s is %s\n")
		    call pargstr (cluster)
		    call pargstr (epochs[1,current_epoch])
	    } else {

		# if the image is using current parameters, no need to proceed
		if (current_epoch == epoch_to_use) {
		    if (pdb) call printf ("This image has already been updated with current parameters, no changes will be made.\n")
		    else call printf ("The parameters used in this image are current, no changes will be made.\n")
		    next
		}
	    }

	    # loop all groups
	    do grp = 1, ngrp {

		# point to the desired group (esp. when group no. is specified)
            	igrp = cl_index
            	if (cl_index <= 0) igrp = grp

	        # open the group and read the data
	        call gf_opengr (ipin, igrp, xmin, xmax, 0)

		# read the detector number
                det = imgeti (ipin, "DETECTOR")

		# read CD matrix and CRPIX
                cd1_1 = imgetr (ipin, "CD1_1")
                cd1_2 = imgetr (ipin, "CD1_2")
                cd2_1 = imgetr (ipin, "CD2_1")
                cd2_2 = imgetr (ipin, "CD2_2")

                crpix1 = imgetr (ipin, "CRPIX1")
                crpix2 = imgetr (ipin, "CRPIX2")

		# calculate the orientation (NCP to Y axis, CCW) in degrees
		old_orient = atan2 (cd2_1, cd2_2) * RADIAN

		v3_to_y = mod ((old_orient - pa_v3 + 720.), 360.)
                old_pscale = sqrt (abs(cd1_1*cd2_2 - cd1_2*cd2_1))

		# if no update, simply print out group parameter info
		if (!update) {
		    call printf ("%s[%d]: plate scale is %0.4e deg/pixel (%0.5f\"/pixel)\n")
			call pargstr (cluster)
			call pargi (igrp)
			call pargr (old_pscale)
			call pargr (old_pscale*3600.)
		    #call printf ("%17w plate scale corresponding to the observation date MJD%0.1f is %0.5f\"/pixel\n")
			#call pargd (exptm)
			#call pargr (scale[det, current_epoch])
		    #call printf ("%17w angle from V3 to Y-axis is %0.2f deg\n")
			#call pargr (v3_to_y)
		} else {

	        # Step (5)
		# add the orientation correction:
		orient = old_orient + beta_y[det, epoch_to_use] - beta_y[det, current_epoch]
		orient = orient / RADIAN
		pscale = scale[det, epoch_to_use] / 3600.
                cd1_1 = -pscale * cos(orient)
                cd1_2 =  pscale * sin(orient)
                cd2_1 =  pscale * sin(orient)
                cd2_2 =  pscale * cos(orient)

		ra = imgetd (ipin, "CRVAL1")
		dec = imgetd (ipin, "CRVAL2")

		# calculate the new RA and Dec
		del_v2 = v2[det, epoch_to_use] - v2[det, current_epoch]
		del_v3 = v3[det, epoch_to_use] - v3[det, current_epoch]
		pa = pa_v3 / RADIAN
		ra_new = ra + (del_v2*cos(pa)+del_v3*sin(pa)) /
			      (3600.*cos(dec/RADIAN))
		dec_new = dec + (-del_v2*sin(pa)+del_v3*cos(pa)) / 3600.

	        # write the new CRVAL, CRPIX, and CD matrix
	        call gf_iputd(ipin, "CRVAL1", ra_new)
	        call gf_iputd(ipin, "CRVAL2", dec_new)

	        call gf_iputr(ipin, "CRPIX1", xref[det, epoch_to_use])
	        call gf_iputr(ipin, "CRPIX2", yref[det, epoch_to_use])

	        call gf_iputr(ipin, "ORIENTAT", orient*RADIAN)

	        call gf_iputr(ipin, "CD1_1", cd1_1)
	        call gf_iputr(ipin, "CD1_2", cd1_2)
	        call gf_iputr(ipin, "CD2_1", cd2_1)
	        call gf_iputr(ipin, "CD2_2", cd2_2)

		# print the group parameters changes
            	call sprintf (text, SZ_LINE, 
				"%s[%1d]: Change plate scale from %0.5f\"/pixel to %0.5f\"/pixel")
		    call pargstr (cluster)
		    call pargi (igrp)
		    call pargr (old_pscale*3600.)
		    call pargr (pscale*3600.)
		call printf ("%s\n")
		    call pargstr (text)
	        call gf_iputh(ipin, "HISTORY", text)

            	call sprintf (text, SZ_LINE, 
				"%s[%1d]: Change orientation from %0.3f to %0.3f")
		    call pargstr (cluster)
		    call pargi (igrp)
		    call pargr (old_orient)
		    call pargr (orient*RADIAN)
		call printf ("%s\n")
		    call pargstr (text)
	        call gf_iputh(ipin, "HISTORY", text)

            	call sprintf (text, SZ_LINE, 
				"%s[%1d]: Change CRVAL1 from %0.5f to %0.5f")
		    call pargstr (cluster)
		    call pargi (igrp)
		    call pargd (ra)
		    call pargd (ra_new)
		call printf ("%s\n")
		    call pargstr (text)
	        call gf_iputh(ipin, "HISTORY", text)

            	call sprintf (text, SZ_LINE, 
				"%s[%1d]: Change CRVAL2 from %0.5f to %0.5f")
		    call pargstr (cluster)
		    call pargi (igrp)
		    call pargd (dec)
		    call pargd (dec_new)
		call printf ("%s\n")
		    call pargstr (text)
	        call gf_iputh(ipin, "HISTORY", text)
		}
	    }

	    # Step (6)
	    # Add or update the keyword PDBEPOCH
	    if (pdb) call gf_iputd(ipin, "PDBEPOCH", epoch[epoch_to_use])
	    else call gf_iaddd(ipin, "PDBEPOCH", epoch[epoch_to_use])

	    # close the input file
	    call gf_unmap (ipin)
	}

        # close file template
        call imtclose (tpin)
end
