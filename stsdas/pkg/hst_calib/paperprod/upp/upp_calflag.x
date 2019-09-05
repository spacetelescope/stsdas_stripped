# This procedure will read in calibration file info from image history
# 	records.  It will then perform desired checks on the PEDIGREE
#	and completion of calibration steps for error conditions.
#	Each detected error condition will add 1 to the value of 'calerr',
#	which is then returned to the calling routine, and populates 
#	the 'cflag' array with error messages for each error.
#	This array can then be printed out on the summary page in the 
# 	'Calibration Data Quality Summary' section.

include "upp.h"

int procedure upp_calflag(img, ref, ped, cflags, nref)

pointer	img
char	ref[SZ_PED, ARB]
char	ped[SZ_PED, ARB]
char 	cflags[SZ_LINE, ARB]
int 	nref


int	calerr
int	calcheck, c1, c2, c3, c4

int	i
real	exptime
char	pedval[SZ_LINE, MAX_PED]
int	pednum

int	corrcheck()
int	pedcheck()
real	imgetr()

begin
	# initialize values
	calerr = 0
	i = 0	
	c1 = corrcheck (img, "IMAGETYP", "BIAS")
	c2 = corrcheck (img, "IMAGETYP", "DARK")
	c3 = corrcheck (img, "IMAGETYP", "FLAT")
	c4 = corrcheck (img, "IMAGETYP", "ECAL")
	#c5 = corrcheck (img, "IMAGETYP", "ECAL")
	exptime = imgetr(img, "EXPTIME")
	
#	call eprintf("c1 = %d, c2 = %d, c3 = %d, c4 = %d\n")
#		call pargi(c1)
#		call pargi(c2)
#		call pargi(c3)
#		call pargi(c4)


	# read in calibration file names and pedigree info from history 
	call pp_pedigree (img, ref, ped, nref)

	# Start checking for calibration data quality errors
	#
	# BLEVCORR: set to COMPLETE or not
	#
	calcheck = corrcheck(img, "BLEVCORR", "COMPLETE")
	# if BLEVCORR was not set to COMPLETE, increment 'calerr' 
	# and copy error string into 'cflags'
	if (calcheck > 0 && c1 > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Bias Level Correction not Completed", cflags[1,calerr], SZ_LINE)
	}
	
	# MASKCORR: set to COMPLETE or not
	calcheck = corrcheck (img, "MASKCORR", "COMPLETE")
#		call eprintf("calcheck(MASKCORR) = %d\n")
#			call pargi(calcheck)

	if (calcheck > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Mask correction not Completed", cflags[1,calerr], SZ_LINE)
	}
		
	# MASKFILE: valid PEDIGREE or not
	call strcpy ("", pedval[1,1], SZ_LINE)
	call strcpy ("DUMMY", pedval[1,2], SZ_LINE)
	pednum = 2
	calcheck = pedcheck (img, "MASKFILE", pedval, pednum, ref, ped, nref)
	if (calcheck > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Mask file is not up to date", cflags[1,calerr], SZ_LINE)
	}

	# ATODCORR: set to COMPLETE or not
	calcheck = corrcheck (img, "ATODCORR", "COMPLETE")
	if (calcheck > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("A-to-D Correction not completed", cflags[1,calerr], SZ_LINE)
	}

	# BIASCORR: set to COMPLETE or not
	# and check that the image was not BIAS
	calcheck = corrcheck (img, "BIASCORR", "COMPLETE")
	if (calcheck > 0 && c1 > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Bias correction not completed", cflags[1,calerr], SZ_LINE)
	}

	# BIASFILE: check its PEDIGREE
	# use 'pedval' and 'pednum' as defined for MASKFILE
	
	calcheck = pedcheck (img, "BIASFILE", pedval, pednum, ref, ped, nref)
	if (calcheck > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Bias file is not up to date", cflags[1,calerr], SZ_LINE)
	}

	# SHADCORR: set to COMPLETE or not
	calcheck = corrcheck (img, "SHADCORR", "COMPLETE")

	if (calcheck > 0 && exptime < 10.0 && c1 > 0 && c2 > 0 ) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Shaded Shutter correction not completed", cflags[1,calerr], SZ_LINE)
	}
	
	# SHADFILE: confirm it has a PEDIGREE, if one was used
	pednum = 1
	calcheck = pedcheck (img,"SHADFILE", pedval, pednum, ref, ped, nref)

	if ( c1 > 0 && c2 > 0 && calcheck > 0 ) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Shaded shutter correction file not up to date", cflags[1,calerr], SZ_LINE)
	}

	# FLATCORR: set to COMPLETE or not
	# if image is NOT a BIAS, DARK, INTFLAT, or VISFLAT, we have an error
	calcheck = corrcheck (img, "FLATCORR", "COMPLETE")
	if (calcheck > 0 && c1 > 0 && c2 > 0 && c3 > 0 && c4 > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Flat field correction not completed", cflags[1,calerr], SZ_LINE)
	}
		
	# FLATFILE: PEDIGREE can not be BLANK, DUMMY, or GROUND
	call strcpy ("", pedval[1,1], SZ_LINE)
	call strcpy ("DUMMY", pedval[1,2], SZ_LINE)
	call strcpy ("GROUND", pedval[1,3], SZ_LINE)
	pednum = 3
	calcheck = pedcheck (img, "FLATFILE", pedval, pednum, ref, ped, nref)
	if (calcheck > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Flat-field file not up to date", cflags[1,calerr], SZ_LINE)
	}

	# DARKCORR: set to COMPLETE or not
	# if TARGNAME is not BIAS or DARK, we have an error
	calcheck = corrcheck (img, "DARKCORR", "COMPLETE")
	if (calcheck > 0 && c1 > 0 && c2 > 0 && exptime >= 60.0 ) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Dark correction not completed", cflags[1,calerr], SZ_LINE)
	}
	
	# DARKFILE: PEDIGREE can not be BLANK or DUMMY
	pednum = 2
	calcheck = pedcheck (img, "DARKFILE", pedval, pednum, ref, ped, nref)
	if (calcheck > 0) {
		calerr = calerr + 1
		calcheck = 0
		call strcpy ("Dark file not up to date", cflags[1,calerr], SZ_LINE)
	}

	# done... return value of 'calerr'
	# this value also corresponds to number of error messages
	# created in 'cflags'

	return (calerr)
end


# This procedure will check a calibration keyword value to see if
# it matches the given successful value.
int procedure corrcheck (img, name, value)

pointer	img
char	name[SZ_LINE]
char	value[SZ_LINE]

char	keyval[SZ_LINE]

int	err

#bool	streq()
int	strsearch()

begin

	#initialize error value to 0 (default value, indicates success)
	err = 0
	
	# get the value of the keyword from the image
	iferr (call imgstr(img, name, keyval, SZ_LINE)) keyval[1] = EOS
	
	# find which value of 'ref' corresponds to the given keyword
            if (strsearch (keyval, value) == 0) {
#		call eprintf("keyval = %s, value = %s\n")
#			call pargstr(keyval)
#			call pargstr(value)

        # found the proper keyword, so lets check to see if it was set
	# to the desired value
		err = 1
        }

	return (err)
end


# This procedure will check a PEDIGREE value against a given list
# of bad values.  If there is a match, return 'err = 1' as an error 
# condition, otherwise 'err = 0' indicating no problems with calibration
# quality.
int procedure pedcheck (img, name, values, num, ref, ped, nref)

pointer	img
char	name[ARB]
char	values[SZ_LINE, ARB]
int	num, nref
char	ref[SZ_PED, ARB]
char	ped[SZ_PED, ARB]

int	i, j
int	err
char	refname[SZ_LINE]

bool	streq()
int	strsearch()

begin
	err = 0
	# get the actual filename associated with the given keyword
	iferr (call imgstr(img, name, refname, SZ_LINE)) refname[1] = EOS
        do i = 1, nref {

            if (streq (refname, ref[1,i])) {
                do j = 1, num {

			if (strsearch(ped[1,i], values[1,j]) > 1) { 
				err = 1 
				break
			} else { err = 0 }
		}
                break
            }
        }

#	call eprintf ("PEDCHECK: name = %s, err = %d\n")
#		call pargstr(name)
#		call pargi(err)
	return (err)
end

