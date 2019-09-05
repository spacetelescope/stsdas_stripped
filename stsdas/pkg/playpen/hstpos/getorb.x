include <imhdr.h>
include	"orbdat.h"

# GETORB -- Read the orbital parameters from the shp
#
#
# S. Hulbert, Oct 91    Original

procedure getorb (im, orb)

pointer im			#I: image pointer for absolute sensitivity
pointer	orb

double 	imgetd()

begin

	# allocate memory for the structure
	call malloc (orb, LEN_ORB, TY_DOUBLE)

	# get orbital parameters
	iferr {    
	    EPCHTIME(orb) = imgetd (im, "EPCHTIME")
	    SDMEANAN(orb) = imgetd (im, "SDMEANAN")
	    CIRVELOC(orb) = imgetd (im, "CIRVELOC")
	    COSINCLI(orb) = imgetd (im, "COSINCLI")
	    ECCENTRY(orb) = imgetd (im, "ECCENTRY")
	    ECCENTX2(orb) = imgetd (im, "ECCENTX2")
	    ECBDX4D3(orb) = imgetd (im, "ECBDX4D3")
	    ECBDX3(orb) = imgetd (im, "ECBDX3")
	    ESQDX5D2(orb) = imgetd (im, "ESQDX5D2")
	    FDMEANAN(orb) = imgetd (im, "FDMEANAN")
	    RCASCNRV(orb) = imgetd (im, "RCASCNRV")
	    ARGPERIG(orb) = imgetd (im, "ARGPERIG")
	    MEANANOM(orb) = imgetd (im, "MEANANOM")
	    RCARGPER(orb) = imgetd (im, "RCARGPER")
	    RASCASCN(orb) = imgetd (im, "RASCASCN")
	    SINEINCL(orb) = imgetd (im, "SINEINCL")
	    SEMILREC(orb) = imgetd (im, "SEMILREC")
	} then {
	    call error (1, "unable to read orbital parameters")
	}

	    iferr {
	        PSANGLV3(orb) = imgetd (im, "PSANGLV3")
	        RTASCNV1(orb) = imgetd (im, "RTASCNV1")
                DECLNV1(orb) = imgetd (im, "DECLNV1")
	    } then {
		iferr {
	            PSANGLV3(orb) = imgetd (im, "PA_V3")
	            RTASCNV1(orb) = imgetd (im, "RA_V1")
                    DECLNV1(orb) = imgetd (im, "DEC_V1")
		} then {
	            call error (1, "unable to read orbital parameters")
		}
	    }

end
