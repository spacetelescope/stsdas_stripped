include <mach.h>
include <imhdr.h>
include	"gsky.h"

#  gsky_do -- Compute sky.
#
#
#  
#  Date		Author		Description
#  ----		------		-----------
#  12-Jun-1996  I. Busko	Naive adaptation from xcrrej task.
#  25-Mar-1997  I. Busko	Add SUBSKY keyword.
#  09-Feb-1998  I. Busko        Update header when subtracting sky.
#  25-Feb-1999  JC Hsu          Modify gsky_sky call to add parameter expname
#  19-May-1999  JC Hsu          Modify gsky_sky call to add parameter bunit
#------------------------------------------------------------------------------
procedure gsky_do (tpin, tpmask, par)

# inputs:
pointer	tpin 			# input image template pointer
pointer tpmask 			# mask template pointer
pointer	par 			# par structure pointer

# locals:
pointer	ipin[MAX_FILES]
pointer ipmask[MAX_FILES]
real	skyval[MAX_FILES]

int	dim_x, dim_y
char	fdata[SZ_FNAME, MAX_FILES]
char	tstring[SZ_LINE], text[SZ_LINE]
int	nfiles			# number of existing files
int	nmasks			# number of mask files
int	ngrp
int	k, n, grp
short	dqpat
pointer	v
real	xmin, xmax
	
int	imtlen()
long	clktime()
#==============================================================================
begin
	dqpat = short (BADBITS(par))

	nfiles = imtlen(tpin)
	nmasks = imtlen(tpmask)

	# if images and masks lists differ in size, abort
	if ((nfiles != nmasks) && (nmasks > 0))
	    call error (1, "Images and masks list differ in size.")

	# open input files, check parameters
	call gsky_check (tpin, tpmask, par, fdata, ipin, ipmask, ngrp, 
                         dim_x, dim_y)

	# allocate array space
	call salloc (v, IM_MAXDIM, TY_LONG)

	# loop all groups
	do grp = 1, ngrp {

	    # if more than one group, open the next group
	    if (grp > 1) {

	    	do n = 1, nfiles
		    call gf_opengr (ipin[n], grp, xmin, xmax, 0)
		if (nmasks > 0) {
	    	    do n = 1, nmasks
		        call gf_opengr (ipmask[n], grp, xmin, xmax, 0)
		}
	    }

	    # calculate the sky levels
	    call gsky_sky (ipin, ipmask, nfiles, nmasks, dim_x, dim_y, 
                          	LOWER(par), UPPER(par), dqpat, BUNIT(par),
			 	EXPNAME(par), skyval)
	    if (VERBOSE(par)) {
		do k = 1, nfiles {
		    call printf ("%s   %d      %0.3f\n")
			call pargstr (fdata[1,k])
	                call pargi (grp)
			call pargr (skyval[k])
	            call flush (STDOUT)
		}
	    }

	    # write last image's sky into CL parameter
	    call clputr ("skyvalue", skyval[nfiles])

	    # subtract sky value and update header keyword
	    if (SUBSKY(par)) {
		do n = 1, nfiles
		    call subsky (ipin[n], skyval[n])
	        if (SKYNAME(par) != EOS) {
		    do n = 1, nfiles
		        call imaddr (ipin[n], SKYNAME(par), skyval[n])
	        }
	    }
	}

	# write history of ending time to output file
	if (SUBSKY(par)) {
	    call imaddb (ipin[1], SUBSKY_KEYW, true)
	    call cnvtime (clktime(0), tstring, SZ_LINE)
	    call sprintf (text, SZ_LINE, 
                         "Sky-subtracted by GSKY ver. %s at %s")
	        call pargstr (VERSION)
	        call pargstr (tstring)
	    call imputh (ipin[1], "HISTORY", text)
	    if (SKYNAME(par) != EOS) {
	        call cnvtime (clktime(0), tstring, SZ_LINE)
	        call sprintf (text, SZ_LINE, 
                             "GSKY ver. %s wrote %s keyword at %s")
	            call pargstr (VERSION)
	            call pargstr (SKYNAME(par))
	            call pargstr (tstring)
	        call imputh (ipin[1], "HISTORY", text)
	    }
	}

	# close input files
	do n = 1, nfiles
	    call imunmap (ipin[n])
	if (nmasks > 0) {
	    do n = 1, nmasks
	        call imunmap (ipmask[n])
	}
end
