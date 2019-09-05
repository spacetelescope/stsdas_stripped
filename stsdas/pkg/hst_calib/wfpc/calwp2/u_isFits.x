include <imhdr.h>
include <imio.h>
include <fio.h>
include <error.h>
include "u_incl.h"
include "u_data.h"

procedure u_isFits (nam)

# Input arguments
pointer nam                             # Pointer to NAMe structure

#  Local variables:
pointer im           			# input image descriptor
char	fitsName[SZ_FNAME]		# temporary FITS file name variable
char	geisName[SZ_FNAME]		# temporary GEIS file name variable
char    text[SZ_LINE]     		# text of error message

#  Functions used:
int     access()
pointer gf_map()

errchk  gf_map

begin

	# Fetch input rootname from CL & store in global 
	call clgstr ("inname", IN_ROOT(nam), SZ_FNAME)

	# Construct input image name for FITS file by using _d0f.fits extention.
        call strcpy (IN_ROOT(nam), fitsName, SZ_FNAME)  
        call strcat ("_d0f.fits", fitsName, SZ_FNAME)  

	# Construct input image name for GEIS file by using .d0h extention.
        call strcpy (IN_ROOT(nam), geisName, SZ_FNAME)  
        call strcat (".d0h", geisName, SZ_FNAME)  
	
	# Check if the files exist.
        if (access(fitsName, 0, 0) == YES) {
	    call strcpy("T", ISFITS(nam), SZ_FNAME)
	   
            # Check if it is standard fits and not waiver FITS format
            iferr (im = gf_map(fitsName, READ_ONLY, 0)) {
                call sprintf (text, SZ_LINE, "Error opening input file: %s")
                    call pargstr (fitsName)
                call u_error (text)
                call gf_unmap (im)
            }
        } else {
	    if (access(geisName, 0, 0) == YES) {
	        call strcpy("F", ISFITS(nam), SZ_FNAME)
            } else {
	        call strcpy("F", ISFITS(nam), SZ_FNAME)
	        call printf("File not found or type FITS/GEIS not compatible!\n")
	    }
	}
end
