include <imhdr.h>
include	<tbset.h>
include <imio.h>
include <mach.h>
include <error.h>
include "wrdata.h"

#################################################################################
# WR_GPHOTMODE -- Construct value for PHOTMODE keyword from the following 	#
#		  header keywords:						#
#			CAMERA, DETECTOR, MODE, <bunit>, FILTNAM1, FILTNAM2.  	#
#		  The PHOTMODE keyword is constructed by concatenating the 	#
#		  string values of the keywords, except that the integer 	#
#		  value of DETECTOR is first converted to its character 	#
#		  representation, BUNIT is hardwired in the code to "DN", 	#
#		  and MODE is truncated to one character.  			#
#
#	10/90  RAShaw		Initial implementation 	
#	10/90  J.W. MacKenty	Revised for inclusion in calwfp  
#	 8/91  RAShaw		Changed default for FILTNAMn from EOS to "OPEN"
#	10/92  JCHsu 		Add CAL to photmode string if flatfield 
#				calibration is applied

procedure wr_gphotmode (gphotmode)

include "wrincl.h"

# Calling arguments:
char	gphotmode[SZ_LINE]	# Character value of PHOTMODE keyword (returned)

# Local variables:
char	bunit[SZ_LINE]		# Brightness unit: DN or PH (photons)
char	camera[SZ_LINE]		# Camera in use: wide-field or planetary
char	mode[SZ_LINE]		# mode (on chip binning used)
char	cal[SZ_LINE]		# flatfield calibration applied?
char	work[SZ_LINE]		# Workspace for constructing PHOTMODE

# Functions used:
bool	streq()			# Test for string equality
int	strlen()		# Return the length of a character string

errchk	strcpy, strlen, wr_kwerr, wr_error

begin
	#  Set Camera state
	if ( WFC )
	    call strcpy ( "WF", camera, SZ_LINE )
	else
	    call strcpy ( "PC", camera, SZ_LINE )

	#  Determine the chip binning mode, then save the first character of 
	#  that keyword.  
	if ( DOFULL )
	    call strcpy ( "F", mode, SZ_LINE )
	else
	    call strcpy ( "A", mode, SZ_LINE )

#  Set the brightness unit to Data Numbers.  Note that this is currently 
#  hardwired, but that another potentially valid value would be "PH" for photons.
	call strcpy ("DN", bunit, SZ_LINE)

#  The name strings for each filter are presently in the global keywords
#  FILTER1 and FILTER2; set them = "OPEN" if they are empty (i.e. = EOS)
	if (streq (FILTER1, EOS))
	    call strcpy ("OPEN", FILTER1, SZ_LINE)
	if (streq (FILTER2, EOS))
	    call strcpy ("OPEN", FILTER2, SZ_LINE)

#  if flatfield calibration is applied, add "CAL" at the end of photmode 
	if ( DOFLAT )
	    call strcpy ( ",CAL", cal, SZ_LINE )
	else
	    cal[1] = EOS

#  Concatenate strings, separated by commas.  
	call sprintf (work, SZ_LINE, "%2s,%1d,%1s,%2s,%s,%s%s")
	    call pargstr (camera)
	    call pargi (CHIPNUM)
	    call pargstr (mode)
	    call pargstr (bunit)
	    call pargstr (FILTER1)
	    call pargstr (FILTER2)
	    call pargstr (cal)

#  Copy the result to "gphotmode" and quit.  Note that "gphotmode" must be no 
#  longer than 48 characters because it is the value of a GROUP parameter.  

	if ( strlen(work) > 48)
	    call wr_error ("PHOTMODE string longer than 48 chars")
	else
	    call strcpy (work, gphotmode, SZ_LINE)
end

#################################################################################
# WR_PHOTOM --	  Search photometry reference file (PHOTTAB) for a match with 	#
#		  the keyword PHOTMODE.  If found, copy from that table row 	#
#		  the values for the group parameter keywords PHOTFLAM, 	#
#		  PHOTZPT, PHOTPLAM, PHOTBW.  					#
#										#
#		  The pointer to the image containing the keywords is required 	#
#		  as input.  This image is modified.  The c0flag parameter 	#
#		  limits the printing of the missing calibration message to 	#
#		  the c0h file case only.  The grpnum parameter is included 	#
#		  in missing calibration error messages since it is possible 	#
#		  to have phot calibration on only a subset of the groups.  	#
#
# Author: John W. MacKenty 31 May 1991

procedure wr_photom ( im, c0flag, grpnum )

include "wrincl.h"

pointer im			# pointer to image being updated
bool	c0flag			# only print phot missing message for c0h
int	grpnum			# current group number
pointer	cp_photmode		# photmode column pointer
pointer cp_flam			# photflam: inverse sensitivity
pointer	cp_zpt			# photzpt: zero point
pointer	cp_plam			# photplam: pivot wavelength
pointer	cp_bw			# photbw: bandwidth of filter

int	trows			# number of rows in phot table
int	row			# loop index for search
bool	pflag			# match found flag
char	photmode[SZ_LINE]	# Character value of PHOTMODE keyword
char	pmode[SZ_LINE]		# photmode from PHOTTAB
real	photflam		# values for photometry keywords
real	photzpt			#   zero-point wavelength
real	photplam		#   pivot wavelength
real	photbw			#   
char	text[SZ_LINE]		# buffer for warning message string

# Functions used:
bool    streq()                 # string equality test
int	tbpsta()                # Return number of rows in tables

	errchk	wr_kwerr, wr_error, tbcfnd, tbpsta, tbegtt, tbegtr,
	    imgstr, streq, imputr, wr_strpwht, sprintf, pargi

begin
	#  Get photmode value from image keyword
	iferr ( call imgstr ( im, "PHOTMODE", photmode, SZ_LINE ) )
	    call wr_kwerr ( "PHOTMODE" )

	#  Map photmode column in table
	call tbcfnd ( PH_TAB, "photmode", cp_photmode, 1 )
	if ( cp_photmode == NULL )
	    call wr_error ( "photmode column not found in PHOTTAB" )

	#  Find number of rows in table
	iferr ( trows = tbpsta ( PH_TAB, TBL_NROWS ) )
	    call wr_error ( "could not determine size of PHOTTAB" )

#  Search photmode column for match
	pflag = false
	do row = 1, trows {
	    iferr ( call tbegtt ( PH_TAB, cp_photmode, row, pmode, SZ_LINE ) )
		call wr_error ( "Row read from PHOTTAB failed" )

#  Strip trailing whitespace from table entry
            call wr_strpwht (pmode)
            if ( streq ( photmode, pmode ) ) {   

#  Match found case: map columns
		iferr ( call tbcfnd ( PH_TAB, "photflam", cp_flam, 1 ) )
		    call wr_error ( "photflam column not found in PHOTTAB" )
		if ( cp_flam == NULL )
		    call wr_error ( "photflam column pointer map failed" )

		iferr ( call tbcfnd ( PH_TAB, "photzpt", cp_zpt, 1 ) )
		    call wr_error ( "photzpt column not found in PHOTTAB" )
		if ( cp_zpt == NULL )
		    call wr_error ( "photzpt column pointer map failed" )

		iferr ( call tbcfnd ( PH_TAB, "photplam", cp_plam, 1 ) )
		    call wr_error ( "photplam column not found in PHOTTAB" )
		if ( cp_plam == NULL )
		    call wr_error ( "photplam column pointer map failed" )

		iferr ( call tbcfnd ( PH_TAB, "photbw", cp_bw, 1 ) )
		    call wr_error ( "photbw column not found in PHOTTAB" )
		if ( cp_bw == NULL )
		    call wr_error ( "photbw column pointer map failed" )

		# get photometry keyword values
		iferr ( call tbegtr ( PH_TAB, cp_flam, row, photflam ) )
		    call wr_error ( "photflam value not found in PHOTTAB" )
		iferr ( call tbegtr ( PH_TAB, cp_zpt, row, photzpt ) )
		    call wr_error ( "photzpt value not found in PHOTTAB" )
		iferr ( call tbegtr ( PH_TAB, cp_plam, row, photplam ) )
		    call wr_error ( "photplam value not found in PHOTTAB" )
		iferr ( call tbegtr ( PH_TAB, cp_bw, row, photbw ) )
		    call wr_error ( "photbw value not found in PHOTTAB" )

		# copy value to image group parameter keywords
		iferr ( call imputr ( im, "PHOTFLAM", photflam ) )
		    call wr_error ( "Error in writing PHOTFLAM" )
		iferr ( call imputr ( im, "PHOTZPT", photzpt ) )
		    call wr_error ( "Error in writing PHOTZPT" )
		iferr ( call imputr ( im, "PHOTPLAM", photplam ) )
		    call wr_error ( "Error in writing PHOTPLAM" )
		iferr ( call imputr ( im, "PHOTBW", photbw ) )
		    call wr_error ( "Error in writing PHOTBW" )
		pflag = true			# indicate match found
		break				# stop search
	    }
	}

#  Print warning message if no calibration found and is_c0h file
	if ( !pflag && c0flag ) {
	    call sprintf ( text, SZ_LINE,
		"Photometric calibration not available for group %d" )
	    call pargi ( grpnum )
	    call wr_message ( text )
	}
end

