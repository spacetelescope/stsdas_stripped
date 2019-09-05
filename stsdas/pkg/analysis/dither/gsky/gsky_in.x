include	"gsky.h"

#  gsky_in -- Read CL parameters for the gsky task.
# 
#  Input CL parameters:
#  -----------------
#
#  "input"	Input images
#  "masks"	Input masks
#  "lower"	lower limit of allowed pixel values
#  "upper"	upper limit of allowed pixel values
#  "expname"	keyword name of the exposure time (only for countrate image use)
#  "skyname"	name of the keyword to be updated with the sky value
#  "verbose"	print out verbose messages?
#
#  Date		Author			Description
#  ----		------			-----------
#  12-Jun-1996  I. Busko		Naive adaptation from xcrrej
#  25-Feb-1999  JC Hsu			Add expname
#  19-May-1999  JC Hsu			Add bunit
#
#------------------------------------------------------------------------------
procedure gsky_in (tpin, tpmask, par)

pointer	tpin 			# input image template pointer
pointer tpmask 			# mask template pointer
pointer	par			# par structure pointer

real	clgetr()
bool	clgetb()
int	imtlen()
pointer	imtopenp()
#==============================================================================
begin

	# open image and mask templates and find out 
	# how many files are in there 
	tpin = imtopenp ("input")
	tpmask = imtopenp ("masks")

        if (imtlen(tpin) > MAX_FILES) 
	    call error (1, "Too many input images")

	# check that mask template must be null or match the image template 
	if (imtlen(tpmask) > 0) {
            if (imtlen(tpmask) != imtlen(tpin))
                call error (1, "Number of images and masks do not match.")
	} 

	# read other parameters
        LOWER(par) = clgetr ("lower")
        UPPER(par) = clgetr ("upper")
	SUBSKY(par) = clgetb ("subsky")
	call clgstr ("bunit", BUNIT(par), SZ_FNAME)
	call clgstr ("expname", EXPNAME(par), SZ_FNAME)
	call clgstr ("skyname", SKYNAME(par), SZ_FNAME)
	VERBOSE(par) = clgetb ("verbose")

	if (VERBOSE(par) && SUBSKY(par)) {
	    call printf ("# Sky will be subtracted from images. \n")
	    call flush (STDOUT)
	}

	# read data quality flags
	call dqval (BADBITS(par))
end
