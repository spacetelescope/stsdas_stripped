#			File:	u_init.x

include <imhdr.h>
include <imio.h>
include <error.h>
include <tbset.h>
include	<mach.h>
include "u_data.h"
include "u_incl.h"			# structure variables


################################################################################
#										
#  U_SETUP -- 	Initialize global parameters, open input image, get "action"	
#		keywords, reference and output file names, and validate  	
#		WF/PC-2 format.  						
#										
#  History:								
#	24 Jul 1992  RAShaw	Initial implementation				
#	 6 Aug 1992  JCHsu	Bug fixes 					
#	21 Nov 2000  JCHsu	populate photmode properly for LRF filters

procedure u_setup (act, cam, nam, ptr)

include "u_context.h"			# global common

#  Calling arguments:
pointer	act				# pointer to ACTion keyword structure
pointer	cam				# pointer to CAMera structure
pointer	nam				# pointer to FILEnames structure
pointer	ptr				# image descriptor structure

#  Local variables:
char	text[SZ_LINE]			# char buffer for message
#char	noinheritName[SZ_FNAME]		#
int	element				# element number in group image

char 	fitsName1[SZ_FNAME], fitsName3[SZ_FNAME]
pointer gf_map()
pointer im



#  Functions used:
pointer	u_input()			# map input image

errchk	u_input, imgstr, u_kwerr, u_message, u_get_act, u_validate, strcpy

begin

	#  Construct input image & DQF names for file name structure
	if (ISFITS(nam) == 'T') {
	    call strcpy (IN_ROOT(nam), IN_IMG(nam), SZ_FNAME)  
       	    call strcat ("_d0f.fits", IN_IMG(nam), SZ_FNAME)
#	    call strcpy (IN_IMG(nam),  noinheritName, SZ_FNAME)
#	    call strcat ("[noinherit]",  noinheritName, SZ_FNAME)

#	    call strcpy (IN_ROOT(nam), IN_DQF(nam), SZ_FNAME)
#	    call strcat ("_q0f.fits", IN_DQF(nam), SZ_FNAME)
	   
#	    call strcpy (IN_ROOT(nam), BLVL_IMG(nam), SZ_FNAME)
#	    call strcat ("_x0f.fits", BLVL_IMG(nam), SZ_FNAME)
	   
#	    call strcpy (IN_ROOT(nam), BLVL_DQF(nam), SZ_FNAME)
#	    call strcat ("_q1f.fits", BLVL_DQF(nam), SZ_FNAME)
	   
#	    iferr ( NGROUP(cam) = gf_gcount (imp))
#	    call u_kwerr ( "NEXTEND" )
	    
	}
	else {
	    call strcpy (IN_ROOT(nam), IN_IMG(nam), SZ_FNAME)  
	    call strcat (".d0h", IN_IMG(nam), SZ_FNAME)  

	    call strcpy (IN_ROOT(nam), IN_DQF(nam), SZ_FNAME)
	    call strcat (".q0h", IN_DQF(nam), SZ_FNAME)
        }


	call strcpy (IN_DQF(nam), fitsName1, SZ_FNAME)
#	call strcpy (BLVL_IMG(nam), fitsName2, SZ_FNAME)
	call strcpy (BLVL_DQF(nam), fitsName3,SZ_FNAME)
	   
#	call strcpy (IN_DQF(nam), fitsName1, SZ_FNAME)
	call strcpy (IN_ROOT(nam), IN_DQF(nam),  SZ_FNAME)
	call strcat ("_q0f.fits", IN_DQF(nam), SZ_FNAME)

	# Check if the FITS file is there.
	iferr (im = gf_map (IN_DQF(nam), READ_ONLY, 0)) {
#	then  
#	    im = gf_map (IN_DQF(nam), READ_ONLY, 0)
#	    if (im == NULL)
	   	call sprintf (text, SZ_LINE, "Error opening input!! %s" )
	    	    call pargstr (IN_DQF(nam))

	    #If FITS file is missing search for GEIS counterpart
	    #call strcpy (fitsName1, IN_DQF(nam), SZ_FNAME)
	    call strcpy (IN_ROOT(nam), IN_DQF(nam),  SZ_FNAME)
	    call strcat (".q0h", IN_DQF(nam), SZ_FNAME)
	    iferr (im = gf_map (IN_DQF(nam), READ_ONLY, 0)) {
		call sprintf (text, SZ_LINE, "Error opening input## %s" )
	    	    call pargstr (IN_DQF(nam))
	    	call u_error (text)
	    }
	}
	
	# Open and check the input image and its DQF for element 1
	element = 1
        IN_IMG_P(ptr) = u_input (element, IN_IMG(nam))
	#IN_IMG_P(ptr) = u_input (element, noinheritName)

	# Note: ROOT is accessed by the ERROR handling routines.  
	# Errors reported before ROOT is populated will not include the 
	# ROOTNAME in the message.  
	iferr ( call imgstr (IN_IMG_P(ptr), "ROOTNAME", ROOT, SZ_LINE) )
	    call u_kwerr ("ROOTNAME")

	call sprintf (text, SZ_LINE, 
	    "Starting CALWP2: Input = %s   Output = %s")
		call pargstr (ROOT)
		call pargstr (OUT_ROOT(nam))
		call u_message (text)
	
	IN_DQF_P(ptr) = u_input (element, IN_DQF(nam))

	# Fetch the action keywords and the necessary reference file names 
	# from header
	call u_get_act (act, nam, IN_IMG_P(ptr))

	# Validate camera header informantion and fetch other relevant 
	# header info.
	call u_validate (act, cam, IN_IMG_P(ptr), nam)	

	#  Construct output image and DQF names
	if (ISFITS(nam) == 'T') {
	    call strcpy (OUT_ROOT(nam), OUT_IMG(nam), SZ_FNAME)
            call strcat ("_c0f.fits", OUT_IMG(nam), SZ_FNAME)
	   
            call strcpy (OUT_ROOT(nam), OUT_DQF(nam), SZ_FNAME)
            call strcat ("_c1f.fits", OUT_DQF(nam), SZ_FNAME)
	} else {
	    call strcpy (OUT_ROOT(nam), OUT_IMG(nam), SZ_FNAME)
            call strcat (".c0h", OUT_IMG(nam), SZ_FNAME)

            call strcpy (OUT_ROOT(nam), OUT_DQF(nam), SZ_FNAME)
            call strcat (".c1h", OUT_DQF(nam), SZ_FNAME)
	}

	#  Construct output group params text filename
	call strcpy (OUT_ROOT(nam), OUT_GRP(nam), SZ_FNAME)
	call strcat (".cgr", OUT_GRP(nam), SZ_FNAME)
end

################################################################################
#										
#  U_GET_ACT --	Get the action keywords	and the necessary reference file 	
#		names.  							
#										
#  Last revised:								
#	11 Aug 92 by RAShaw	Initial implementation				
#	22 Aug 93 by CYZhang  	Add ATODCORR, DOHISTOS, and SHADCORR            
# 	 2 Sep 93 by CYZhang	Copied thruput table part from JCHsu            
#	 8 Feb 94 by CYZhang	Change THRU_TBL root from IN_ROOT to OUT_ROOT	
#    3 Apr 07 by WJHack     Added WF4TCORR

procedure u_get_act (act, nam, imp)

#  Calling argument:
pointer	act				# Pointer to ACTion keyword structure
pointer	nam				# pointer to file NAMe structure
pointer	imp				# input image descriptor

#  Local variables:
char	text[SZ_FNAME]			# Temp. value of string header keyword

#  Functions used:
bool	streq ()			# Test two strings for equality

errchk imgstr, u_kwerr, streq, strcpy, strcat, strupr

begin

	#  Apply static mask?
	iferr ( call imgstr (imp, "MASKCORR", text, SZ_LINE) )
	    call u_kwerr ("MASKCORR")
	call strupr (text)
	if ( streq (text, "PERFORM") ) {
	    DOMASK(act) = true
	    iferr (call imgstr (imp, "MASKFILE", MASK(nam), SZ_FNAME))
		call u_kwerr ("MASKFILE")
	} else if ( streq (text, "OMIT") || streq (text, "COMPLETE") ) 
	    DOMASK(act) = false
	else 
	    call u_kwerr ("MASKCORR")

	#  Apply AtoD convertion correction?  -- CYZhang, 8/28/93
	iferr ( call imgstr (imp, "ATODCORR", text, SZ_LINE) )
	    call u_kwerr ("ATODCORR")
	call strupr (text)
	if ( streq (text, "PERFORM") ) {
	    DOATOD(act) = true
	    iferr (call imgstr (imp, "ATODFILE", ATOD_IMG(nam), SZ_FNAME))
		call u_kwerr ("ATODFILE")

	} else if ( streq (text, "OMIT") || streq (text, "COMPLETE") ) 
	    DOATOD(act) = false
	else 
	    call u_kwerr ("ATODCORR")

	#  Apply WF4T correction?  -- WJHack, 4/3/07
    #
    # It should turn this step off if the keyword does not exist
    # so that it will not 'harm' data retrieved prior to the pipeline
    # update which adds the keywords for this step.
    #
    DOWF4T(act) = true
	iferr ( call imgstr (imp, "WF4TCORR", text, SZ_LINE) ) {
	    call u_warn ("WF4TCORR")
        DOWF4T(act) = false
        }
	call strupr (text)
    if ( DOWF4T(act) ){
	    if ( streq (text, "PERFORM") ) {
	        DOWF4T(act) = true
            iferr (call imgstr (imp, "WF4TFILE", WF4T_IMG(nam), SZ_FNAME))
  		        call u_kwerr ("WF4TFILE")
            if (streq(WF4T_IMG(nam), "N/A") || streq(WF4T_IMG(nam), "")) {
                DOWF4T(act) = false
        		call u_warn ("No WF4TFILE specified. Skipping WF4TCORR.")
            }
	    } else if ( streq (text, "OMIT") || streq (text, "COMPLETE") ) 
	        DOWF4T(act) = false
	    else 
	        call u_kwerr ("WF4TCORR")
    }
	#  Do histograms?  -- CYZhang, 9/1/93
	iferr ( call imgstr (imp, "DOHISTOS", text, SZ_LINE) )
	    call u_kwerr ("DOHISTOS")
	call strupr (text)
	if ( streq (text, "PERFORM") ) {
	    DOHIST(act) = true
	    # Construct histogram image name for file name structure

	    # if input is FITS, output c2h as FITS image 
	    if (ISFITS(nam) == 'T') {
	    	call strcpy (OUT_ROOT(nam), HIST_IMG(nam), SZ_FNAME)  
	    	call strcat ("_c2f.fits", HIST_IMG(nam), SZ_FNAME)
	    } else {
	    
	    	# Change the IN_ROOT to OUT_ROOT -- 7/2/94 CYZ	    
	    	call strcpy (OUT_ROOT(nam), HIST_IMG(nam), SZ_FNAME)  
	    	call strcat (".c2h", HIST_IMG(nam), SZ_FNAME)  
	    }
	} else if ( streq (text, "OMIT") || streq (text, "COMPLETE") ) 
	    DOHIST(act) = false
	else 
	    call u_kwerr ("DOHISTOS")

	# Apply shutter shading correction?  -- CYZhang, 8/28/93
	iferr ( call imgstr (imp, "SHADCORR", text, SZ_LINE) )
	    call u_kwerr ("SHADCORR")
	call strupr (text)
	if ( streq (text, "PERFORM") ) {
	    DOSHAD(act) = true
	    iferr ( call imgstr (imp, "SHADFILE", SHAD_IMG(nam), SZ_FNAME) )
		call u_kwerr ("SHADFILE")
	} else if ( streq (text, "OMIT") || streq (text, "COMPLETE") )
	    DOSHAD(act) = false
	else
	    call u_kwerr ("SHADCORR")

	# Remove global bias level?
	iferr ( call imgstr ( imp, "BLEVCORR", text, SZ_LINE ) )
	    call u_kwerr ("BLEVCORR")
	call strupr (text)

	if ( streq (text, "PERFORM") ) {
	    DOBLEV(act) = true
            iferr (call imgstr (imp, "BLEVFILE", BLVL_IMG(nam), SZ_FNAME))
		call u_kwerr ( "BLEVFILE" )
            iferr (call imgstr (imp, "BLEVDFIL", BLVL_DQF(nam), SZ_FNAME))
		call u_kwerr ( "BLEVDFIL" )

	} else if ( streq (text, "OMIT") || streq (text, "COMPLETE") ) 
	    DOBLEV(act) = false
	else 
	    call u_kwerr ("BLEVCORR")

	#  Subtract bias image?
	iferr ( call imgstr (imp, "BIASCORR", text, SZ_LINE) )
	    call u_kwerr ("BIASCORR")
	call strupr (text)

	if ( streq (text, "PERFORM") ) {
	    DOBIAS(act) = true
	    iferr ( call imgstr (imp, "BIASFILE", BIAS_IMG(nam), SZ_FNAME))
		call u_kwerr ("BIASFILE")
	    iferr (call imgstr (imp, "BIASDFIL", BIAS_DQF(nam), SZ_FNAME))
		call u_kwerr ("BIASDFIL")
	} else if ( streq (text, "OMIT") || streq (text, "COMPLETE") ) 
	    DOBIAS(act) = false
	else 
	    call u_kwerr ("BIASCORR")

	# Apply dark correction?
	iferr ( call imgstr (imp, "DARKCORR", text, SZ_LINE) )
	    call u_kwerr ("DARKCORR")
	call strupr (text)

	if ( streq (text, "PERFORM") ) {
	    DODARK(act) = true
            iferr (call imgstr (imp, "DARKFILE", DARK_IMG(nam), SZ_FNAME))
		call u_kwerr ("DARKFILE")
            iferr (call imgstr (imp, "DARKDFIL", DARK_DQF(nam), SZ_FNAME))
		call u_kwerr ("DARKDFIL")

	} else if ( streq (text, "OMIT") || streq (text, "COMPLETE") ) 
	    DODARK(act) = false
	else 
	    call u_kwerr("DARKCORR")

	# Apply flat field correction?
	iferr ( call imgstr (imp, "FLATCORR", text, SZ_LINE) )
	    call u_kwerr ("FLATCORR")
	call strupr (text)

	if ( streq (text,"PERFORM") ) {
	    DOFLAT(act) = true
            iferr (call imgstr (imp, "FLATFILE", FLAT_IMG(nam), SZ_FNAME))
		call u_kwerr ( "FLATFILE" )
            iferr (call imgstr (imp, "FLATDFIL", FLAT_DQF(nam), SZ_FNAME))
		call u_kwerr ( "FLATDFIL" )

	} else if ( streq (text, "OMIT") || streq (text, "COMPLETE") ) 
	    DOFLAT(act) = false
	else 
	    call u_kwerr ("FLATCORR")

	# Construct PHOTMODE string?
	iferr ( call imgstr (imp, "DOPHOTOM", text, SZ_LINE) ) 
	    call u_kwerr ("DOPHOTOM")
	call strupr (text)

	if ( streq (text,"PERFORM") ) {
	    DOPHOT(act) = true

	    #  Construct throughput table name for file name
	    #  structure -- Provided by JCHsu
	    #  call strcpy (IN_ROOT(nam), THRU_TBL(nam), SZ_FNAME)	    

	    # if input is FITS, output c3t as FITS table 
	    if (ISFITS(nam) == 'T') {
	    	call strcpy (OUT_ROOT(nam), THRU_TBL(nam), SZ_FNAME)  
	    	call strcat ("_c3t.fits", THRU_TBL(nam), SZ_FNAME)
	    } else {
	    
	    	# Change the IN_ROOT to OUT_ROOT -- 7/2/94 CYZ	    
	    	call strcpy (OUT_ROOT(nam), THRU_TBL(nam), SZ_FNAME)  
	    	call strcat (".c3t", THRU_TBL(nam), SZ_FNAME)  
	    }
	    
	    iferr (call imgstr (imp, "GRAPHTAB", GRAPH_TBL(nam), SZ_FNAME))
		call u_kwerr ( "GRAPHTAB" )
            iferr (call imgstr (imp, "COMPTAB", COMP_TBL(nam), SZ_FNAME))
		call u_kwerr ( "COMPTAB" )

#           iferr (call imgstr (imp, "PHOTTAB", THRU_TBL(nam), SZ_FNAME))
#		call u_kwerr ( "PHOTTAB" )

	} else if ( streq (text, "OMIT") || streq (text, "COMPLETE") )
	    DOPHOT(act) = false
	else 
	    call u_kwerr ("DOPHOTOM")

	# Specify output data type
	iferr ( call imgstr (imp, "OUTDTYPE", text, SZ_LINE) )
	    call u_kwerr ("OUTDTYPE")
	call strupr (text)

	if ( streq (text, "REAL") ) 
	    OUTDTYPE(act) = TY_REAL
	else if ( streq (text, "LONG") ) 
	    OUTDTYPE(act) = TY_LONG
	else if ( streq (text, "SHORT") ) 
	    OUTDTYPE(act) = TY_SHORT
	else 
	    call u_kwerr ("OUTDTYPE")

end

################################################################################
#										
# U_VALIDATE --	Get other necessary information from the input image and 	
#		check input image for valid WF/PC format.  			
#										
#  Last revised:								
#	13 Aug 92 by RAShaw	Initial implementation				
#	22 Aug 93 by CYZhang	Get expsure time for SHADCORR operation		
#				Get UBAY3 temperature for AtoD correction
#   13 Nov 07    WJ Hack   Added EXPSTART keyword to CAM structure

procedure   u_validate (act, cam, imp, nam)

#  Calling arguments:
pointer	act				# Pointer to ACTion keyword structure
pointer	cam				# Pointer to CAMera structure
pointer	imp				# input image descriptor
pointer nam				# pointer to FILEnames structure

#  Local variables:
char	text[SZ_LINE]			# text of error message

#  Functions called:
bool	imgetb()			# get keyword value:	TY_BOOL
int	imgeti()			# 			TY_INT
real	imgetr()			# 			TY_REAL
bool	streq()				# string comparison
int	strncmp()
int     gf_gcount()

errchk	imgstr, u_kwerr, u_error, u_warn, imgeti, imgetb, imgetr,
	    streq

begin

	#  Ensure that input image is from WFPC2
	iferr ( call imgstr (imp, "INSTRUME", text, SZ_LINE) )
	    call u_kwerr ("INSTRUME")
	call strupr (text)

	if ( !streq (text, "WFPC2") )
	    call u_error ("Keyword INSTRUME not equal to WFPC2")

	#  Ensure that input image is 2-D
	if (IM_NDIM (imp) != 2)
	    call u_error ("Input image not 2-D")

	NPTS(cam) = IM_LEN (imp, 1)
	NPTS(cam) = IM_LEN (imp, 2)
	if (NPTS(cam) > NX_FULL || NPTS(cam) > NY_FULL)
	    call u_error ("Input image size exceeds max expected for WFPC-2")

	# Must be group image with <= 4 groups
        if (ISFITS(nam) == 'T') {
	    iferr ( NGROUP(cam) = gf_gcount (imp))
	        call u_kwerr ( "NEXTEND" )
        } else {
	    iferr ( NGROUP(cam) = imgeti (imp, "GCOUNT") )
	        call u_kwerr ( "GCOUNT" )
            if ( !imgetb (imp, "GROUPS") || NGROUP(cam) > 4 )
	        call u_error ("Input image has no groups or more than 4")
        }
	
	#  Get exposure time  -- CYZhang (22 Aug 1993)
	if ( DOSHAD(act) ) {
	    iferr ( EXPTIME(cam) = imgetr (imp, "EXPTIME") )
		call u_kwerr ("EXPTIME")
	    if (EXPTIME(cam) > 10.0)
		call u_warn ("SHADCORR set to PERFORM but EXPTIME > 10 sec!")
	}
	
	#  Get total time clocks were stopped (i.e. since last erase)
	if ( DODARK(act) ) {
	    iferr ( DARKTIME(cam) = imgetr (imp, "DARKTIME") )
		call u_kwerr ("DARKTIME")
	}

	#  Get saturated pixel DN (all pixels >= this value are flagged)
	iferr ( SATLIM(cam) = imgeti (imp, "SATURATE") )
	    call u_kwerr ("SATURATE")

	#  Get length of the AtoD table lines -- CYZhang  28/8/93
	ATODLENGTH(cam) = SATLIM(cam) + 1

	#  Determine if on chip binning (AREA mode) was used
        iferr ( call imgstr (imp, "MODE", text, SZ_LINE) )
	    call u_kwerr ("MODE")
	call strupr (text)

        if ( streq (text, "FULL") ) 
	    IS_FULL(cam) = true
	else if ( streq (text, "AREA") ) 
	    IS_FULL(cam) = false
	else 
	    call u_kwerr ("MODE")

	#  Determine if the clock is set to ON or OFF -- CYZhang, 9/29/93
        iferr ( call imgstr (imp, "SERIALS", text, SZ_LINE) )
	    call u_kwerr ("SERIALS")
	call strupr (text)

        if ( streq (text, "ON") ) 
	    IS_CLKON(cam) = true
	else if ( streq (text, "OFF") ) 
	    IS_CLKON(cam) = false
	else 
	    call u_kwerr ("SERIALS")

	# Fetch A-to-D gain setting and check for valid value
	iferr ( A2DGAIN(cam) = imgetr (imp, "ATODGAIN") )
#	    call u_kwerr ("ATODGAIN")
	    call u_warn ("ATODGAIN keyword missing from header")

	if (abs (A2DGAIN(cam) - HI_A2D) > EPSILONR && 
	    abs (A2DGAIN(cam) - LO_A2D) > EPSILONR) 
	    call u_warn ("ATODGAIN value is invalid")

	#  Get UBAY3TMP temperature -- Added by CYZhang, 8/28/93
	if ( DOATOD(act) ) {
	    iferr ( UBAY3TMP(cam) = imgetr (imp, "UBAY3TMP") )
		call u_kwerr ("UBAY3TMP")
	    UBAY3TMP(cam) = UBAY3TMP(cam) + 273.15  # in Kelvins
	}
	# Get MJD of start of exposure
    iferr (EXPDATE(cam) = imgetr(imp,"EXPSTART") )
    call u_warn ("EXPSTART keyword missing from header")
    
	# Get name strings for each filter
        iferr ( call imgstr (imp, "FILTNAM1", FILTER1(cam), SZ_LINE) )
	    call u_kwerr ("FILTNAM1")

        iferr ( call imgstr (imp, "FILTNAM2", FILTER2(cam), SZ_LINE) )
	    call u_kwerr ("FILTNAM2")

	if (strncmp (FILTER1(cam), "FR", 2) == 0 || 
	    strncmp (FILTER2(cam), "FR", 2) == 0) {

	    iferr ( LRFWAVE(cam) = imgetr (imp, "LRFWAVE") )
		call u_warn ("LRFWAVE keyword missing from header")
	}

	#  Get shutter blade name -- Added by CYZhang, 9/29/93
        iferr ( call imgstr (imp, "SHUTTER", SHTBLD(cam), SZ_LINE) )
	    call u_kwerr ("SHUTTER")
	
	#  Get output scaling parameters
	if ( OUTDTYPE(act) == TY_LONG || OUTDTYPE(act) == TY_SHORT ) {
	    iferr ( USCALE(cam) = imgetr (imp, "USCALE") )
		call u_kwerr ( "USCALE" )
	    iferr ( UZERO(cam) = imgetr (imp, "UZERO") )
		call u_kwerr ("UZERO")
	}

	#  Get output data RSDP fill value
	iferr ( RSDPFILL(cam) = imgeti (imp, "RSDPFILL") )
	    call u_kwerr ("RSDPFILL")
end
