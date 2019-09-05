#			File:	u_phot.x
include <imhdr.h>
include	<tbset.h>
include <imio.h>
include <mach.h>
include <error.h>
include "u_incl.h"
include "u_data.h"

################################################################################
#										
# U_PHOTMODE --	Construct value for PHOTMODE keyword from the following 	
#		header keywords:						
#		<WFPC2>, DETECTOR, ATODGAIN, FILTNAM1, FILTNAM2, <CAL> 		
#										
#		The PHOTMODE keyword is constructed by concatenating the 	
#		string values of the keywords, except that the integer value 	
#		of DETECTOR is first converted to its character representation. 
#										
#  Last Modified:								
#	 20 Aug 92 by RAShaw	Initial implementation				
#	 10 Sep 93 by CYZhang	Match the new GRAPHTAB and COMPTAB keywords	
#	 19 Oct 93 by CYZhang	Attach LRFWAVE to Linear Ramp Filters		
#	 15 Feb 94 by CYZhang	Remove DN from PHOTMODE string			
#	 21 Nov 2000  JC Hsu	properly format LRF filters in photmode

#        14 May 2002  JT Miller increase nwave from 1000 to 10000

procedure u_photmode (cam, kw)

# Calling arguments:
pointer	cam			# pointer to CAMera data structure
pointer	kw			# pointer to KeyWord data structure

# Local variables:
#char	bunit[SZ_PHOT]		# Brightness unit: DN or PH (photons)
char	gain[SZ_PHOT]		# A-to-D Gain setting
char	text[SZ_PHOT]		# Workspace for constructing PHOTMODE
char	filter1[SZ_PHOT]	# filter 1 string
char	filter2[SZ_PHOT]	# filter 2 string
char	tx_end[1]		# EOS

# Functions used:
bool	streq()			# Test for string equality
int	strncmp()		# Test for string comparison
int	strlen()		# Return the length of a character string
bool	fp_equalr()		# Test equality for two real numbers

errchk	strcpy, strlen, u_kwerr, u_error

begin
	call strcpy (EOS, text, SZ_PHOT)

	#  Determine the chip binning mode, then save the first character 
	#  of that keyword.  
	#  There is no need for photometric keywords for "FULL" or "AREA" and
	#  for "OPEN" filter specification anymore in the
	#  latest hstgraph_930903a.tab and hstcomp_930903a.tab
	#  Chris Burrows recommended to leave these things out -- CYZ 9/9/93

	#  Set the brightness unit to Data Numbers.  Note that this is currently
	#  hardwired to "DN", but that another potentially valid value would 
	#  be "PH" for photons. 

	# If ATODGAIN is set up right (not empty), DN must be used. CYZ 10/9/93
	# When DN is used without ATODGAIN set up, ATODGAIN=A2D7
	# Bill Sparks said DN caused lots of confusion, it should be removed
	# from the PHOTMODE string, even though it is still a legitimate
	# keyword in the cdbs database  --CYZ 15/2/94
#	call strcpy ("DN", bunit, SZ_PHOT)

	# Set "gain" string according to the ATODGAIN value 
	# The latest version has A2D15 and A2D7 rather than A2DHI and 
	# A2DLO -- CYZ 9/9/93
	if ( fp_equalr(A2DGAIN(cam), HI_A2D) )
	    call strcpy ("A2D15", gain, SZ_PHOT)
	else if ( fp_equalr(A2DGAIN(cam), LO_A2D) )
	    call strcpy ("A2D7", gain, SZ_PHOT)
	else if ( !fp_equalr(A2DGAIN(cam), HI_A2D) && 
	    !fp_equalr(A2DGAIN(cam), LO_A2D) )
		call strcpy ("A2D7", gain, SZ_PHOT)	

	# The name strings for each filter are presently in the global keywords 
	# FILTER1 and FILTER2; set them = "OPEN" if they are empty (i.e. = EOS)
	# Instead set them = "" if they are empty (see the latest tables) -- CYZ
	# 9/9/93
	if (streq (FILTER1(cam), text))
	    call strcpy ("", FILTER1(cam), SZ_PHOT)
	if (streq (FILTER2(cam), text))
	    call strcpy ("", FILTER2(cam), SZ_PHOT)
	
	# Deal with Linear Ramp Filters: 
	# if either filter name is FRnnnX, the corresponding item in 
	# photmode should be LRF#nnnn.n, where nnnn.n is from the keyword 
	# LRFWAVE.  JC Hsu 11/21/2000.
	if (strncmp (FILTER1(cam), "FR", 2) == 0) {
	    call sprintf (filter1, SZ_PHOT, "LRF#%0.1f")
	        call pargr (LRFWAVE(cam))
	} else 
	    call strcpy (FILTER1(cam), filter1, SZ_PHOT)

	if (strncmp (FILTER2(cam), "FR", 2) == 0) {
	    call sprintf (filter2, SZ_PHOT, "LRF#%0.1f")
	        call pargr (LRFWAVE(cam))
	} else 
	    call strcpy (FILTER2(cam), filter2, SZ_PHOT)
	    
	#  Add EOS to the end of the PHOTMODE string
	tx_end[1] = EOS

	#  Concatenate strings, separated by commas.  
	call sprintf (text, SZ_LINE, "WFPC2,%1d,%s,%s,%s,%s%s")
	    call pargi   (DETECTOR(cam))
	    call pargstr (gain)

	    # Remove "bunit" (DN) from the PHOTMODE string -- CYZ 15/2/94
#	    call pargstr (bunit)
	    call pargstr (filter1)
	    call pargstr (filter2)
	    call pargstr ("CAL")
	    call pargstr (tx_end)

	# Copy the result to PHOTMODE(cam) and quit.  
	if (strlen (text) > SZ_PHOT)
	    call u_error ("PHOTMODE string longer than 48 chars")

	call strcpy (text, PHOTMODE(kw), SZ_PHOT)
end

################################################################################
#										
# U_PHOTMODE2 --	Construct value for PHOTMODE keyword from the following 	
#		header keywords:						
#		<WFPC2>, DETECTOR, ATODGAIN, FILTNAM1, FILTNAM2, CONT#, <CAL> 		
#										
#		The PHOTMODE2 keyword is constructed by concatenating the 	
#		string values of the keywords, except that the integer value 	
#		of DETECTOR is first converted to its character representation. 
#       This keyword includes the time-dependent contamination term.
#										
#  Last Modified:								
#	 20 Aug 92 by RAShaw	Initial implementation				
#	 10 Sep 93 by CYZhang	Match the new GRAPHTAB and COMPTAB keywords	
#	 19 Oct 93 by CYZhang	Attach LRFWAVE to Linear Ramp Filters		
#	 15 Feb 94 by CYZhang	Remove DN from PHOTMODE string			
#	 21 Nov 2000  JC Hsu	properly format LRF filters in photmode

#        14 May 2002  JT Miller increase nwave from 1000 to 10000
#    13 Nov 2007  WJ Hack   Add contamination term

procedure u_photmode2 (cam, kw)

# Calling arguments:
pointer	cam			# pointer to CAMera data structure
pointer	kw			# pointer to KeyWord data structure

# Local variables:
#char	bunit[SZ_PHOT]		# Brightness unit: DN or PH (photons)
char	gain[SZ_PHOT]		# A-to-D Gain setting
char	text[SZ_PHOT]		# Workspace for constructing PHOTMODE
char	filter1[SZ_PHOT]	# filter 1 string
char	filter2[SZ_PHOT]	# filter 2 string
char    expdate[SZ_PHOT]    # expstart mjd string
char	tx_end[1]		# EOS

# Functions used:
bool	streq()			# Test for string equality
int	strncmp()		# Test for string comparison
int	strlen()		# Return the length of a character string
bool	fp_equalr()		# Test equality for two real numbers

errchk	strcpy, strlen, u_kwerr, u_error

begin
	call strcpy (EOS, text, SZ_PHOT)

	#  Determine the chip binning mode, then save the first character 
	#  of that keyword.  
	#  There is no need for photometric keywords for "FULL" or "AREA" and
	#  for "OPEN" filter specification anymore in the
	#  latest hstgraph_930903a.tab and hstcomp_930903a.tab
	#  Chris Burrows recommended to leave these things out -- CYZ 9/9/93

	#  Set the brightness unit to Data Numbers.  Note that this is currently
	#  hardwired to "DN", but that another potentially valid value would 
	#  be "PH" for photons. 

	# If ATODGAIN is set up right (not empty), DN must be used. CYZ 10/9/93
	# When DN is used without ATODGAIN set up, ATODGAIN=A2D7
	# Bill Sparks said DN caused lots of confusion, it should be removed
	# from the PHOTMODE string, even though it is still a legitimate
	# keyword in the cdbs database  --CYZ 15/2/94
#	call strcpy ("DN", bunit, SZ_PHOT)

	# Set "gain" string according to the ATODGAIN value 
	# The latest version has A2D15 and A2D7 rather than A2DHI and 
	# A2DLO -- CYZ 9/9/93
	if ( fp_equalr(A2DGAIN(cam), HI_A2D) )
	    call strcpy ("A2D15", gain, SZ_PHOT)
	else if ( fp_equalr(A2DGAIN(cam), LO_A2D) )
	    call strcpy ("A2D7", gain, SZ_PHOT)
	else if ( !fp_equalr(A2DGAIN(cam), HI_A2D) && 
	    !fp_equalr(A2DGAIN(cam), LO_A2D) )
		call strcpy ("A2D7", gain, SZ_PHOT)	

	# The name strings for each filter are presently in the global keywords 
	# FILTER1 and FILTER2; set them = "OPEN" if they are empty (i.e. = EOS)
	# Instead set them = "" if they are empty (see the latest tables) -- CYZ
	# 9/9/93
	if (streq (FILTER1(cam), text))
	    call strcpy ("", FILTER1(cam), SZ_PHOT)
	if (streq (FILTER2(cam), text))
	    call strcpy ("", FILTER2(cam), SZ_PHOT)
	
	# Deal with Linear Ramp Filters: 
	# if either filter name is FRnnnX, the corresponding item in 
	# photmode2 should be LRF#nnnn.n, where nnnn.n is from the keyword 
	# LRFWAVE.  JC Hsu 11/21/2000.
	if (strncmp (FILTER1(cam), "FR", 2) == 0) {
	    call sprintf (filter1, SZ_PHOT, "LRF#%0.1f")
	        call pargr (LRFWAVE(cam))
	} else 
	    call strcpy (FILTER1(cam), filter1, SZ_PHOT)

	if (strncmp (FILTER2(cam), "FR", 2) == 0) {
	    call sprintf (filter2, SZ_PHOT, "LRF#%0.1f")
	        call pargr (LRFWAVE(cam))
	} else 
	    call strcpy (FILTER2(cam), filter2, SZ_PHOT)

    # Deal with Contamination term:
    # photmode2 should be cont#nnnnn, where nnnnn is from the keyword
    #
    call sprintf (expdate, SZ_PHOT, "cont#%0.3f")
        call pargr(EXPDATE(cam))
	    
	#  Add EOS to the end of the PHOTMODE string
	tx_end[1] = EOS

	#  Concatenate strings, separated by commas.  
	call sprintf (text, SZ_LINE, "WFPC2,%1d,%s,%s,%s,%s,%s%s")
	    call pargi   (DETECTOR(cam))
	    call pargstr (gain)
	    call pargstr (filter1)
	    call pargstr (filter2)
        call pargstr (expdate)
	    call pargstr ("CAL")
	    call pargstr (tx_end)

	# Copy the result to PHOTMODE2(cam) and quit.  
	if (strlen (text) > SZ_PHOT)
	    call u_error ("PHOTMODE2 string longer than 48 chars")

	call strcpy (text, PHOTMODE2(kw), SZ_PHOT)
end

################################################################################
#										
#  U_PHOTMATCH -- Search GRAPHTAB and COMPTAB files for a match with  	 	
#		  the keyword PHOTMODE.  If found, compute the grand 		
#		  throughput table and the values for the group parameter 	
#		  keywords: PHOTFLAM, PHOTZPT, PHOTPLAM, PHOTBW.  		
#										
#		  The grpnum parameter is included in missing calibration 	
#		  error messages since it is possible to have phot 		
#		  calibration on only a subset of the groups.  			
#										
#  Revision History:								
#	23 Jul 1992 by RAShaw	Initial implementation				
#	10 Sep 1993 by CYZhang	Not used any more				

procedure u_photmatch (tp, kw, grpnum)

#  Calling arguments:
pointer	tp			# pointer to table descriptor
pointer kw			# pointer to KeyWord structure
int	grpnum			# current group number

#  Local variables:
pointer	cp_photmode		# photmode column pointer
pointer cp_flam			# photflam: inverse sensitivity
pointer	cp_zpt			# photzpt: magnitude zero point
pointer	cp_plam			# photplam: pivot wavelength
pointer	cp_bw			# photbw: bandwidth of filter
bool	pflag			# match found flag
char	pmode[SZ_PHOT]		# PHOTMODE(cam) from PHOTTAB
int	row			# loop index for search
char	text[SZ_LINE]		# buffer for warning message string
int	trows			# number of rows in phot table

# Functions used:
bool    streq()                 # string equality test
int	tbpsta()                # Return number of rows in tables

errchk	u_kwerr, u_error, tbcfnd, tbpsta, tbegtt, tbegtr,
	    imgstr, streq, gf_putr, u_strpwht, u_warn, sprintf, pargi

begin

	# Map PHOTMODE column in table
	call tbcfnd (tp, "PHOTMODE", cp_photmode, 1)
	if (cp_photmode == NULL)
	    call u_error ("PHOTMODE column not found in PHOTTAB")

	# Find number of rows in table
	iferr ( trows = tbpsta (tp, TBL_NROWS) )
	    call u_error ("Could not determine size of PHOTTAB")

	# Search PHOTMODE column for match
	pflag = false
	do row = 1, trows {
	    iferr ( call tbegtt (tp, cp_photmode, row, pmode, SZ_PHOT) )
		call u_error ("Row read from PHOTTAB failed")

	    # Strip trailing whitespace from table entry
            call u_strpwht (pmode)
            if ( streq (PHOTMODE(kw), pmode) ) {   

		#  Match found case: map columns
		iferr ( call tbcfnd (tp, "PHOTFLAM", cp_flam, 1) )
		    call u_error ("PHOTFLAM column not found in PHOTTAB")
		if (cp_flam == NULL)
		    call u_error ("PHOTFLAM column pointer map failed" )

		iferr ( call tbcfnd (tp, "PHOTZPT", cp_zpt, 1) )
		    call u_error ("PHOTZPT column not found in PHOTTAB")
		if (cp_zpt == NULL)
		    call u_error ("PHOTZPT column pointer map failed" )

		iferr ( call tbcfnd (tp, "PHOTPLAM", cp_plam, 1) )
		    call u_error ("PHOTPLAM column not found in PHOTTAB")
		if (cp_plam == NULL)
		    call u_error ("PHOTPLAM column pointer map failed")

		iferr ( call tbcfnd (tp, "PHOTBW", cp_bw, 1) )
		    call u_error ("PHOTBW column not found in PHOTTAB")
		if (cp_bw == NULL)
		    call u_error ("PHOTBW column pointer map failed")

		#  Fetch photometry keyword values
		iferr (call tbegtr (tp, cp_flam, row, PHOTFLAM(kw)))
		    call u_error ("PHOTFLAM value not found in PHOTTAB")
		iferr (call tbegtr (tp, cp_zpt, row, PHOTZPT(kw)))
		    call u_error ("PHOTZPT value not found in PHOTTAB")
		iferr (call tbegtr (tp, cp_plam, row, PHOTPLAM(kw)))
		    call u_error ("PHOTPLAM value not found in PHOTTAB")
		iferr (call tbegtr (tp, cp_bw, row, PHOTBW(kw)))
		    call u_error ("PHOTBW value not found in PHOTTAB")

		#  Indicate match found & stop search
		pflag = true
		break
	    }
	}

	#  Print warning message if no calibration found and is_c0h file
	if (!pflag) {
	    call sprintf (text, SZ_LINE,
			"Photometric calibration not available for group %d")
		call pargi (grpnum)
		call u_warn (text)
	}

end

#  u_photcalc -- calculate synthetic photmetry for WFPC2
#
#  This subroutine first creates an array of wavelengths. Then it
#  computes the instrument passband and error on the input wavelength
#  set. Finally it computes four scalar photometric parameters from the 
#  instrument passband: the inverse sensitivity, PHOT(1), the zero
#  point parameter, PHOT(2), the pivot wavelength, PHOT(3), and the 
#  rms bandwith, PHOT(4)
#
#  History:
#  --------
#  Version     Date          Author        Description
#     1      04-08-93        JC Hsu	   adapt from gtphot.f
#     2      08-09-93	     B. Simon	   Adapted to updated synphot lib
#     3	     01-25-94	     C.Y. Zhang    Get phot_path to HISTORY
#     4	     17 Nov 2000     JC Hsu  	   Clean up comments and declare all 
#					   c3t columns at the beginning
#------------------------------------------------------------------------------

procedure u_photcalc (kw, cam, grpnum, thr_tp, grftbl, cmptbl,
		      phot_path, maxpath)

pointer	kw		# io: keyword structure
pointer cam
int	grpnum		# i:  group number
pointer	thr_tp		# i:  throughput table pointer
char	grftbl[ARB]	# i:  graph table name
char	cmptbl[ARB]	# i:  component table name
char	phot_path[ARB]	# o:  history info for phot
int	maxpath		# i:  max size of phot_path

# local
bool	logspace
char	colname[SZ_COLNAME], keyword[SZ_KEYWORD]
int	nwave, j
pointer	sp, wave, filt, filterr, colptr[5]
real	phot[4]

errchk	getbandx, phopar

begin
	# Write diagnostic message

	call printf ("photmode = %s\n")
	    call pargstr (PHOTMODE(kw))
	call flush (STDOUT)

	# Define wavelength set length and type
 
	# increase nwave from 1000 to 10000 to improve accuracy of narrow band
	# filters (5/14/2002 JC Hsu / Jay T Miller)	
	nwave = 10000
	logspace = true

	# Compute throughput and photmetric quantities
	call smark (sp)
	call salloc (wave, nwave, TY_REAL)
	call salloc (filt, nwave, TY_REAL)
	call salloc (filterr, nwave, TY_REAL)
    

	call getbandx (PHOTMODE(kw), grftbl, cmptbl, logspace, nwave,
		     Memr[wave], Memr[filt], Memr[filterr])

	call phopar (nwave, Memr[wave], Memr[filt], phot)

	# Write photmetric quantites to structure
	PHOTFLAM(kw) = phot[1]
	PHOTZPT(kw) = phot[2]
	PHOTPLAM(kw) = phot[3]
	PHOTBW(kw) = phot[4]

	# Write throughput to table
	if (grpnum == 1) {

	    # define all the columns first
	    call tbcdef (thr_tp, colptr[1], "WAVELENGTH", " ", " ", 
				TY_REAL, 1, 1)
	    do j = 1,4 {
		call sprintf (colname, SZ_COLNAME, "THROUGHPUT_%d")
		    call pargi (j)
		call tbcdef (thr_tp, colptr[j+1], colname, " ", " ", 
			TY_REAL, 1, 1)
	    }

	    # write wavelengths
	    call tbcptr (thr_tp, colptr, Memr[wave], 1, nwave)
	}

	# write throughputs
	call tbcptr (thr_tp, colptr[grpnum+1], Memr[filt], 1, nwave)

	call sprintf (keyword, SZ_KEYWORD, "PHMODE_%d")
	call pargi (grpnum)

	call tbhadt (thr_tp, keyword, PHOTMODE(kw))

	# Get history info for phot
	call listpath (PHOTMODE(kw), grftbl, cmptbl, phot_path, maxpath)

	call sfree (sp)

end

#  u_contcalc -- calculate synthetic photmetry zero-point corrections
#                 for WFPC2 based on time-dependent contamination throughputs
#
#  This subroutine first creates an array of wavelengths. Then it
#  computes the instrument passband and error on the input wavelength
#  set. Finally it computes four scalar photometric parameters from the 
#  instrument passband: the inverse sensitivity, PHOT(1), the zero
#  point parameter, PHOT(2), the pivot wavelength, PHOT(3), and the 
#  rms bandwith, PHOT(4)
#  It then computes a delta-mag based on the PHOT(1) [PHOTFLAM] with and
#  without the contamination term and reports that to the image header.
#
#
#  History:
#  --------
#  Version     Date          Author        Description
#     1      04-08-93        JC Hsu	   adapt from gtphot.f
#     2      08-09-93	     B. Simon	   Adapted to updated synphot lib
#     3	     01-25-94	     C.Y. Zhang    Get phot_path to HISTORY
#     4	     17 Nov 2000     JC Hsu  	   Clean up comments and declare all 
#					   c3t columns at the beginning
#     5      13 Nov 2007     WJ Hack    Copied from u_photcalc and modified
#------------------------------------------------------------------------------

procedure u_contcalc (kw, cam, grpnum, thr_tp, grftbl, cmptbl,
		      phot_path, maxpath)

pointer	kw		# io: keyword structure
pointer cam
int	grpnum		# i:  group number
pointer	thr_tp		# i:  throughput table pointer
char	grftbl[ARB]	# i:  graph table name
char	cmptbl[ARB]	# i:  component table name
char	phot_path[ARB]	# o:  history info for phot
int	maxpath		# i:  max size of phot_path

# local
bool	logspace
char	colname[SZ_COLNAME], keyword[SZ_KEYWORD]
int	nwave, j
pointer	sp, wave, filt, filterr, colptr[5]
real	phot[4]
real    dmag

errchk	getbandx, phopar

begin
	# Write diagnostic message

	call printf ("photmode(cont) = %s\n")
	    call pargstr (PHOTMODE2(kw))
	call flush (STDOUT)

	# Define wavelength set length and type
 
	# increase nwave from 1000 to 10000 to improve accuracy of narrow band
	# filters (5/14/2002 JC Hsu / Jay T Miller)	
	nwave = 10000
	logspace = true

	# Compute throughput and photmetric quantities
	call smark (sp)
	call salloc (wave, nwave, TY_REAL)
	call salloc (filt, nwave, TY_REAL)
	call salloc (filterr, nwave, TY_REAL)
    

	call getbandx (PHOTMODE2(kw), grftbl, cmptbl, logspace, nwave,
		     Memr[wave], Memr[filt], Memr[filterr])

	call phopar (nwave, Memr[wave], Memr[filt], phot)


	# Write photmetric quantites to structure
	#PHOTFLAM(kw) = phot[1]
	#PHOTZPT(kw) = phot[2]
	#PHOTPLAM(kw) = phot[3]
	#PHOTBW(kw) = phot[4]
    # Conversion of counts to mags
    #mags = -2.5 * log10(COUNTS * photflam) + photzpt
    # Compute delta magnitude compared to contamination-free PHOTFLAM
    if (phot[1] > 0.0 && PHOTFLAM(kw) > 0.0){
    dmag = -2.5 * (log10(phot[1]) - log10(PHOTFLAM(kw)))
    } else {
    dmag = 0.0
    }
    DELTAMAG(kw) = dmag
    
	# Get history info for phot
	call listpath (PHOTMODE2(kw), grftbl, cmptbl, phot_path, maxpath)

	call sfree (sp)

end
