include	<imhdr.h>
include	<imset.h>
include <ctype.h>
include	<time.h>
include "wpdef.h"

#--------------------------------------------------------------------11 Jun 96--
.help scale.x Jun96 wfpc$combine
.ih
NAME
  scalef - Compute scale factors for the images
.ih
DESCRIPTION
This procedure does CLIO to determine the type of scaling desired.  
The output header parameters for exposure time and NCOMBINE are set.  
The scaling and weighting factors are logged.  The logging is done here 
because some of the information is only available here.  This routine 
is taken from the `images.imcombine' package, V2.9 of IRAF. 
.endhelp
#-------------------------------------------------------------------------------
# FSCALE --	Compute scale factors for the images.  Performs CLIO to 
#		determine the type of scaling desired.  
#
#  Revision history:
#	Nov 90 by RAShaw	Development version
#	Jun 96 by RAShaw	Change function name from "scale" to avoid 
#				name conflict with F90 intrinsic function. 

bool procedure scalef(in, out, nimages, log, low, high)

include "wpcom.h"

# Calling arguments:
pointer		in[nimages]		# Input images
pointer		out			# Output image
int		nimages			# Number of images to combine
int		log			# Log file descriptor
real		low, high		# Rejection limits

# Local variables:
bool		doscale			# Perform input image scaling?
int		i, nout			#
real		mean			# 
real		exposure		# 
bool		expscale 		# 
pointer		exptime			# Exposure time for input images
pointer		expname 		# FITS keyword for exposure time
pointer		fname 			# 
bool		modescale 		# Scale image [sections] by the mode?
bool		modeoffset 		# 
pointer		modes 			# 
pointer		ncombine		# Number of images previously combined
pointer		sp			# Pointer to stack memory
char		str[SZ_FNAME]		# Name of selected option for log
pointer		time 			# 
bool		weight			# Are images to be weighted?
pointer		x1, x2, xs		# 

# Functions used:
real		asumr()			# 
real		asumi()			# 
bool		clgetb()		# Get Boolean parameter from CL
long		clktime()		# 
real		imc_moder()		# 
short		imc_modes()		# 
int		imgeti()		# Get integer value from image header
real		imgetr()		# Get real value from image header

begin
	errchk	imc_moder, imc_modes, imgeti, imgetr
	call smark (sp)
	call salloc (ncombine, nimages, TY_INT)
	call salloc (exptime, nimages, TY_REAL)
	call salloc (modes, nimages, TY_REAL)
	call salloc (expname, SZ_FNAME, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (x1, IM_MAXDIM, TY_INT)
	call salloc (x2, IM_MAXDIM, TY_INT)
	call salloc (xs, IM_MAXDIM, TY_INT)

	# Determine type of scaling desired and exposure time keyword.
	expscale   = clgetb ("exposure")
	modescale  = clgetb ("scale")
	modeoffset = clgetb ("offset")
	weight     = clgetb ("weight")
	call clgstr ("modesec", Memc[fname], SZ_FNAME)
	call clgstr ("expname", Memc[expname], SZ_FNAME)
	call clgstr ("option", str, SZ_FNAME)

	# Get the number of images previously combined and the exposure times.
	# The default combine number is 1 and the default exposure is 0.

	do i = 1, nimages {
	    iferr (Memi[ncombine+i-1] = imgeti (in[i], "ncombine"))
		Memi[ncombine+i-1] = 1
	    if (Memc[expname] != EOS) {
	        iferr (Memr[exptime+i-1] = imgetr (in[i], Memc[expname]))
		    Memr[exptime+i-1] = 0.
	    } else
		Memr[exptime+i-1] = INDEF
	}

	# Set the default scaling factors.
	call amovkr (INDEF, Memr[modes], nimages)
	call amovkr (1., SCALES, nimages)
	call amovkr (0., ZEROS, nimages)
	call amovkr (1., WTS, nimages)

	# Set scaling factors.  Mode scaling overrides exposure scaline and
	# offset scaling.
	if (modescale) {
	    call amovki (1, Memi[x1], IM_NDIM(in[1]))
	    call amovi (IM_LEN(in[1],1), Memi[x2], IM_NDIM(in[1]))
	    call amovki (1, Memi[xs], IM_NDIM(in[1]))
	    call subsectn (Memc[fname], Memi[x1], Memi[x2], Memi[xs],
		IM_NDIM(in[1]))
	    do i = 1, nimages {
		switch (IM_PIXTYPE(in[i])) {
		case TY_SHORT:
		    Memr[modes+i-1] = imc_modes (in[i], Memi[x1], Memi[x2],
			Memi[xs])
		default:
		    Memr[modes+i-1] = imc_moder (in[i], Memi[x1], Memi[x2],
			Memi[xs])
		}
		SCALES[i] = Memr[modes+i-1]
		if (SCALES[i] <= 0.)
		    call error (0, "Mode must be positive for scaling")
		if (weight)
	            WTS[i] = sqrt (Memi[ncombine+i-1] * SCALES[i])
	    }
	} else {
	    if (expscale)
	        do i = 1, nimages {
		    SCALES[i] =  max (0.001, Memr[exptime+i-1])
		    if (weight)
	                WTS[i] = sqrt (Memi[ncombine+i-1] * SCALES[i])
		}
	    if (modeoffset) {
	        call amovki (1, Memi[x1], IM_NDIM(in[1]))
	        call amovi (IM_LEN(in[1],1), Memi[x2], IM_NDIM(in[1]))
	        call amovki (1, Memi[xs], IM_NDIM(in[1]))
	        call subsectn (Memc[fname], Memi[x1], Memi[x2], Memi[xs],
		    IM_NDIM(in[1]))
	        do i = 1, nimages {
		    switch (IM_PIXTYPE(in[i])) {
		    case TY_SHORT:
		        Memr[modes+i-1] = imc_modes (in[i], Memi[x1], Memi[x2],
			    Memi[xs])
		    default:
		        Memr[modes+i-1] = imc_moder (in[i], Memi[x1], Memi[x2],
			    Memi[xs])
		    }
		    ZEROS[i] = Memr[modes+i-1] / SCALES[i]
		    if (weight) {
			if (ZEROS[i] <= 0.)
			    call error (0,
				"Mode must be positive for weighting")
		        WTS[i] = sqrt (Memi[ncombine+i-1]*SCALES[i]/ZEROS[i])
		    }
	        }
	    }
	}

	# Change to relative scaling factors.
	mean = asumr (ZEROS, nimages) / nimages
	call asubkr (ZEROS, mean, ZEROS, nimages)
	mean = asumr (SCALES, nimages) / nimages
	call adivkr (SCALES, mean, SCALES, nimages)
	call amulkr (ZEROS, mean, ZEROS, nimages)
	mean = asumr (WTS, nimages)
	call adivkr (WTS, mean, WTS, nimages)

	# Because of finite arithmetic it is possible for the offsets 
	# to be nonzero even when they are all equal.  Just for the sake 
	# of a nice log set the offsets in this case.
	for (i=2; (i<=nimages) && (ZEROS[i] == ZEROS[1]); i=i+1)
	    ;
	if (i > nimages)
	    call aclrr (ZEROS, nimages)

	# If all scale factors, offsets, and weights are equal then don't 
	# actually scale.
	for (i=2; (i<=nimages) && (SCALES[i] == SCALES[1]); i=i+1)
	    if ((ZEROS[i] != ZEROS[1]) || (WTS[i] != WTS[1]))
		break
	if (i > nimages)
	    doscale = false
	else
	    doscale = true

	# Set the output header parameters.
	nout = asumi (Memi[ncombine], nimages)
	call gf_iaddi(out, "ncombine", nout)
	if (Memc[expname] != EOS) {
	    exposure = 0.
	    do i = 1, nimages
	        exposure = exposure + WTS[i] * Memr[exptime+i-1] / SCALES[i]
	    call gf_iaddr(out, Memc[expname], exposure)
	} else
	    exposure = INDEF

	# Append to the log if not null.
	call cnvdate (clktime(0), Memc[time], SZ_DATE)
	if (log != NULL) {
	    if ((low > 0.) || (high > 0.)) {
	        call fprintf (log,
		    "%s combine: %s, lowreject=%g, highreject=%g\n")
		    call pargstr (Memc[time])
		    call pargstr (str)
		    call pargr (low)
		    call pargr (high)
	    } else {
	        call fprintf (log, "%s combine: %s\n")
		    call pargstr (Memc[time])
		    call pargstr (str)
	    }
	    if (doscale) {
	        call fprintf (log, "  %20s %6s %6s %6s %6s %6s %6s\n")
	            call pargstr ("Images")
	            call pargstr ("N")
	            call pargstr ("Exp")
	            call pargstr ("Mode")
	            call pargstr ("Scale")
	            call pargstr ("Offset")
	            call pargstr ("Weight")
	        do i = 1, nimages {
		    call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_FNAME)
	            call fprintf (log, "  %20s %6d %6.1f %6g %6.3f %6g %6.3f\n")
		        call pargstr (Memc[fname])
		        call pargi (Memi[ncombine+i-1])
		        call pargr (Memr[exptime+i-1])
		        call pargr (Memr[modes+i-1])
		        call pargr (1./SCALES[i])
		        call pargr (-ZEROS[i])
		        call pargr (WTS[i])
	        }
	        call fprintf (log, "  -------------------- ------ ------\n")
	        call imstats (out, IM_IMAGENAME, Memc[fname], SZ_FNAME)
	        call fprintf (log, "  %20s %6d %6.1f\n")
		    call pargstr (Memc[fname])
	            call pargi (nout)
	            call pargr (exposure)
	    } else {
	        call fprintf (log, "  %20s %6s\n")
	            call pargstr ("Images")
	            call pargstr ("N")
	        do i = 1, nimages {
		    call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_FNAME)
	            call fprintf (log, "  %20s %6d\n")
		        call pargstr (Memc[fname])
		        call pargi (Memi[ncombine+i-1])
	        }
	        call fprintf (log, "  -------------------- ------\n")
	        call imstats (out, IM_IMAGENAME, Memc[fname], SZ_FNAME)
	        call fprintf (log, "  %20s %6d\n")
		    call pargstr (Memc[fname])
	            call pargi (nout)
	    }
	    call flush (log)
	}

	call sfree (sp)
	return (doscale)
end
