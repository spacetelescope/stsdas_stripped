include	"upp.h"

procedure det_uaper (im, shh, ngrps, chip, apergrp, fname, fits)

pointer shh
pointer	im
int	ngrps
int	chip
int 	apergrp
char	fname[ARB]
char	fits[ARB]


char 	aperdat[SZ_UAPER, MAX_APER]
int	aperchip[MAX_APER]
int	apernum
int	init
char	uaper[SZ_UAPER]
int	found, detector
int	k
real 	dmin, dmax
bool	apereq

int	imgeti()
bool	streq()

data	init/0/
data	apernum/23/

begin
	# read in aperture values for WFPC2	
	if (init == 0) {
		call strcpy("PC1", aperdat[1,1], SZ_UAPER)
		aperchip[1] = 1
		call strcpy("WF2", aperdat[1,2], SZ_UAPER)
		aperchip[2] = 2
		call strcpy("WF3", aperdat[1,3], SZ_UAPER)
		aperchip[3] = 3
		call strcpy("WF4", aperdat[1,4], SZ_UAPER)
		aperchip[4] = 4
		call strcpy("WFALL", aperdat[1,5], SZ_UAPER)
		aperchip[5] = 3
		call strcpy("PC1-FIX", aperdat[1,6], SZ_UAPER)
		aperchip[6] = 1
		call strcpy("WF2-FIX", aperdat[1,7], SZ_UAPER)
		aperchip[7] = 2
		call strcpy("WF3-FIX", aperdat[1,8], SZ_UAPER)
		aperchip[8] = 3
		call strcpy("WF4-FIX", aperdat[1,9], SZ_UAPER)
		aperchip[9] = 4
		call strcpy("WFALL-FIX", aperdat[1,10], SZ_UAPER)
		aperchip[10] = 3
		call strcpy("FQUVN33", aperdat[1,11], SZ_UAPER)
		aperchip[11] = 2
		call strcpy("POLQN33", aperdat[1,12], SZ_UAPER)
		aperchip[12] = 2
		call strcpy("POLQN18", aperdat[1,13], SZ_UAPER)
		aperchip[13] = 2
		call strcpy("POLQP15P", aperdat[1,14], SZ_UAPER)
		aperchip[14] = 1
		call strcpy("POLQP15W", aperdat[1,15], SZ_UAPER)
		aperchip[15] = 2
		call strcpy("FQCH4W2", aperdat[1,16], SZ_UAPER)
		aperchip[16] = 2
		call strcpy("FQCH4W3", aperdat[1,17], SZ_UAPER)
		aperchip[17] = 3
		call strcpy("FQCH4W4", aperdat[1,18], SZ_UAPER)
		aperchip[18] = 4
		call strcpy("FQCH4N33", aperdat[1,19], SZ_UAPER)
		aperchip[19] = 2
		call strcpy("FQCH4N15", aperdat[1,20], SZ_UAPER)
		aperchip[20] = 1
		call strcpy("FQCH4P15", aperdat[1,21], SZ_UAPER)
		aperchip[21] = 1
		call strcpy("F160BN15", aperdat[1,22], SZ_UAPER)
		aperchip[22] = 3
		call strcpy("F160AN15", aperdat[1,23], SZ_UAPER)
		aperchip[23] = 3

		init = 1
	}
	
	# Get aperture name (aper_1) from .shh file
	call imgstr (shh, "APER_1", uaper, SZ_UAPER)
	found = 0

	# set up default values, just to make sure something gets assigned
	apergrp = 1
	chip = 1	
	
	# Now, look for aperture name in aperture array (aperdat) and 
	# output associated chip number (aperchip)
	do k=1, apernum {
		 apereq = streq (uaper, aperdat[1,k])
		if (apereq) {
			chip = aperchip[k]
			found = 1
			break
		}
	}
	
	# Now that we have looped through all the apertures, make sure
	# we found the aperture, and if not, set to 1.
	if (found == 0) {
	# did not match aperture name
		call printf ("det_uaper: Unknown Aperture Name - Using Group 1")
		chip = 0
	}

	# reset found indicator for next check
	found = 0

	# Determine which group in the image corresponds to this detector
	 do k=1, ngrps {
		if (streq(fits,"geis")) {
                	call gf_opengr(im, k, dmin, dmax, 0)
			detector = imgeti (im, "DETECTOR")
		} else {
			call fit_detector(fname, k, detector)
		}

		if (chip == detector) { 
			apergrp = k
			found = 1
		}
	}
	
	# If we did not match a chip with a detector, set to default case
	#	of 1
	if (found == 0) 
		apergrp = 1

end
