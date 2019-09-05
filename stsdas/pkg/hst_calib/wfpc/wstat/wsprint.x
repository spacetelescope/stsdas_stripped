include "wstat.h"

################################################################################
#										
# PHEADER -- Print the banner fields.
#
#	Initial code:	10/91 by RAShaw						
#	Add DQF name in the header :	11/95 by JC Hsu

procedure pheader (image, dqimage, dqfflag)

include "wstat.com"

#  Calling arguments:
char	image[SZ_FNAME]		# image name
char	dqimage[SZ_FNAME]	# DQ image name
bool	dqfflag

begin
	call printf ("\n# Image statistics for '%s' ")
	    call pargstr (image)
	if (dqfflag) {
	    call printf ("with mask '%s' ")
	        call pargstr (dqimage)
	} else
	    call printf ("with no mask ")
	call printf ("\n# GROUP")

	if (npix)
	    call printf ("    NPIX")
	if (minmax)
	    call printf ("        MIN        MAX")
	if (mean)
	    call printf ("       MEAN")
	if (median)
	    call printf ("     MIDPT")
	if (mode)
	    call printf ("      MODE")
	if (stddev)
	    call printf ("    STDDEV")
	if (skew) 
	    call printf ("      SKEW")
	if (kurt) 
	    call printf ("  KURTOSIS")

	call printf ("\n")
	call flush (STDOUT)
end


################################################################################
#										#
# WPRINT --	Print the fields for each group.				#
#										
#	10/91 RAShaw:     Initial code
#	10/20/95  JC Hsu: Fix a bug for no good point case

procedure wprint (wst, group)

include "wstat.com"

#  Calling arguments:
pointer	wst			# pointer to the statistics structure
int	group			# image group

begin
	call printf ("[%5d]")
	    call pargi (group)
	if (npix) {
	    call printf (" %7d")
		call pargi (WS_NPIX(wst))
	}

	# take care of the zero point case - JC Hsu 10/20/95
	if (WS_NPIX(wst) <= 0) {
	    WS_MIN(wst) = INDEF
	    WS_MAX(wst) = INDEF
	    WS_MEAN(wst) = INDEF
	    WS_MEDIAN(wst) = INDEF
	    WS_MODE(wst) = INDEF
	    WS_STDDEV(wst) = INDEF
	    WS_SKEW(wst) = INDEF
	    WS_KURT(wst) = INDEF
	}
	if (minmax) {
	    call printf (" %10.6g %10.6g")
		call pargr (WS_MIN(wst))
		call pargr (WS_MAX(wst))
	}
	if (mean) {
	    call printf (" %10.6g")
		call pargr (WS_MEAN(wst))
	}
	if (median) {
	    call printf (" %9.5g")
		call pargr (WS_MEDIAN(wst))
	}
	if (mode) {
	    call printf (" %9.5g")
		call pargr (WS_MODE(wst))
	}
	if (stddev) {
	    call printf (" %9.5g")
		call pargr (WS_STDDEV(wst))
	}
	if (skew) {
	    call printf (" %9.5g")
		call pargr (WS_SKEW(wst))
	}
	if (kurt) {
	    call printf (" %9.5g")
		call pargr (WS_KURT(wst))
	}

	call printf ("\n")
	call flush (STDOUT)
end
