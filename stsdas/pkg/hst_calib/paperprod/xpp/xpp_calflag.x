# This procedure will read in calibration file info from image history
# 	records.  It will then perform desired checks on the PEDIGREE
#	and completion of calibration steps for error conditions.
#	Each detected error condition will add 1 to the value of 'calerr',
#	which is then returned to the calling routine, and populates 
#	the 'cflag' array with error messages for each error.
#	This array can then be printed out on the summary page in the 
# 	'Calibration Data Quality Summary' section.

include "xpp.h"

int procedure xpp_calflag(img, ref, ped, cflags, nref)

pointer	img
char	ref[SZ_PED, ARB]
char	ped[SZ_PED, ARB]
char 	cflags[SZ_LINE, ARB]
int 	nref


int	calerr
int	calcheck, c1

int	i
char	pedval[SZ_LINE, MAX_PED]
int	pednum

int	pedcheck()

begin
	# initialize values
	calerr = 0
	i = 0	
	c1 = 0

	# read in calibration file names and pedigree info from history 
	call pp_pedigree (img, ref, ped, nref)

	if (nref == 0) 
		return(-1)

	# Start checking for calibration data quality errors
	#	
	# GEOHFILE: valid PEDIGREE or not
	call strcpy ("", pedval[1,1], SZ_LINE)
	call strcpy ("DUMMY", pedval[1,2], SZ_LINE)
	call strcpy ("GROUND", pedval[1,3], SZ_LINE)
	pednum = 3
	calcheck = pedcheck (img, "GEOHFILE", pedval, pednum, ref, ped, nref)
	if (calcheck > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Geometric Correction file is not up to date", cflags[1,calerr], SZ_LINE)
	}


	# UNIHFILE: check its PEDIGREE
	# use 'pedval' and 'pednum' as defined for GEOHFILE
	pednum=3
	calcheck = pedcheck (img, "UNIHFILE", pedval, pednum, ref, ped, nref)
	if (calcheck > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Flat-field file not up to date", cflags[1,calerr], SZ_LINE)
	}


	# done... return value of 'calerr'
	# this value also corresponds to number of error messages
	# created in 'cflags'

	return (calerr)
end


