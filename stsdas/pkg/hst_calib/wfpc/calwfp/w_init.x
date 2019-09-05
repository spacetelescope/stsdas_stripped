include <imhdr.h>
include <imio.h>
include <error.h>
include "wrdata.h"

#################################################################################
# WR_SETUP -- 	Initialize global parameters, open input image, get "action"	#
#		keywords, reference and output file names, and other data 	#
#		from the image header.  					#

procedure wr_setup ( )

include "wrincl.h"

char	text[SZ_LINE]			# char buffer for message
int	element				# element number in group image
pointer	wr_input

errchk	strcpy, strcat, wr_input, imgstr, wr_kwerr, sprintf, pargstr,
		wr_message, wr_gaction, wr_gnames, wr_gother, wr_gout

begin
	# Set files not open
        II_OPEN   = false ; ID_OPEN  = false	# Input image and DQF
	MD_OPEN   = false			# Static mask DQF
	A_OPEN    = false			# AtoD conversion file
        BL_OPEN   = false ; BLD_OPEN = false	# Bias level file and DQF
        BI_OPEN   = false ; BD_OPEN  = false	# Bias image and DQF
        DI_OPEN   = false ; DD_OPEN  = false	# Dark image and DQF
        PI_OPEN   = false ; PD_OPEN  = false	# Preflash image and DQF
	RI_OPEN   = false ; RD_OPEN  = false	# Superpurge image and DQF
        FI_OPEN   = false ; FD_OPEN  = false	# Flat Field image and DQF
	PH_OPEN   = false			# Photometry table
        OI_OPEN   = false ; OD_OPEN  = false	# Output image and DQF
        H_OPEN    = false			# Output Histogram file
        SD_OPEN   = false			# Output Saturated Pixel DQF
	G_OPEN    = false			# Output group param text file

# assemble input image name in global common
	call strcpy ( IN_ROOT, II_NAME, SZ_FNAME)  
	call strcat (".d0h", II_NAME, SZ_FNAME)  

# assemble input image DQF name in global common
	call strcpy ( IN_ROOT, ID_NAME, SZ_FNAME)
	call strcat (".q0h", ID_NAME, SZ_FNAME)

# open and check the input image and its DQF for element 1

	element = 1				# first element
	II_IM = wr_input ( element, II_NAME, II_OPEN )

	iferr ( call imgstr ( II_IM, "ROOTNAME", ROOT, SZ_LINE ) )
	    call wr_kwerr ( "ROOTNAME" )

	call sprintf ( text, SZ_LINE,
	    "Starting CALWFP: Input = %s   Output = %s" )
	call pargstr ( IN_ROOT )
	call pargstr ( OUT_ROOT )
	call wr_message ( text )

	ID_IM = wr_input ( element, ID_NAME, ID_OPEN )

	call wr_gaction ( )			# get the action keywords
	call wr_gnames ( )			# get the reference file names
	call wr_gother ( )			# get other relevant data
	call wr_gout ( )			# get output file names
end

#################################################################################
# WR_GACTION --	Get the action keywords						#

procedure   wr_gaction ( )

include "wrincl.h"

#  Local variables:
char	text[SZ_LINE]			# Temp. value of string header keyword

#  Functions used:
int	imaccf()			# Test if header keyword exists
bool	streq()				# Test two strings for equality

errchk imgstr, wr_kwerr, streq, strupr

begin
	iferr ( call imgstr ( II_IM, "MASKCORR", text, SZ_LINE ) )
	    call wr_kwerr ( "MASKCORR" )
	call strupr ( text )
	if ( streq(text,"YES") ) 
	    DOMASK = true
	else if ( streq(text,"NO") || streq(text,"DONE") ) 
	    DOMASK = false
	else 
	    call wr_kwerr( "MASKCORR" )

	iferr ( call imgstr ( II_IM, "ATODCORR", text, SZ_LINE ) )
		call wr_kwerr ( "ATODCORR" )
	call strupr ( text )
	if ( streq(text,"YES") ) 
	    DOATOD = true
	else if ( streq(text,"NO") || streq(text,"DONE") ) 
	    DOATOD = false
	else 
	    call wr_kwerr( "ATODCORR" )

	iferr ( call imgstr ( II_IM, "BLEVCORR", text, SZ_LINE ) )
	    call wr_kwerr ( "BLEVCORR" )
	call strupr ( text )

#  1/92:  Will handle old image headers w/o BIASEVEN/ODD keywords by printing 
#	  values on STDOUT and continuing.  NB: the odd/even pattern is still 
#	  correctly removed, and the DEZERO keyword is correct, but the 
#	  BIASEVEN/ODD values are simply not recorded in these image headers.  
	if ( streq(text,"YES") ) { 
	    DOBLEV = true
	} else if ( streq(text,"NO") || streq(text,"DONE") ) 
	    DOBLEV = false
	else 
	    call wr_kwerr( "BLEVCORR" )
#
# 10/6/92 set the BOEFLAG outside the previous IF such that it will still work
# for files without the BIASEVEN/ODD keywords when BLEVCORR is not set (JC Hsu)
#
	if ( imaccf ( II_IM, "BIASEVEN") == NO ) {
	    BOEFLAG = true
	    call wr_message (
	      "BIASEVEN, BIASODD keywords missing: reporting values to STDOUT")
	} else
	    BOEFLAG = false

	iferr ( call imgstr ( II_IM, "BIASCORR", text, SZ_LINE ) )
	    call wr_kwerr ( "BIASCORR" )
	call strupr ( text )
	if ( streq(text,"YES") ) 
	    DOBIAS = true
	else if ( streq(text,"NO") || streq(text,"DONE") ) 
	    DOBIAS = false
	else 
	    call wr_kwerr( "BIASCORR" )

	iferr ( call imgstr ( II_IM, "PREFCORR", text, SZ_LINE ) )
	    call wr_kwerr ( "PREFCORR" )
	call strupr ( text )
	if ( streq(text,"YES") ) 
	    DOPREF = true
	else if ( streq(text,"NO") || streq(text,"DONE") ) 
	    DOPREF = false
	else 
	    call wr_kwerr( "PREFCORR" )

	iferr ( call imgstr ( II_IM, "PURGCORR", text, SZ_LINE ) )
	    call wr_kwerr ( "PURGCORR" )
	call strupr ( text )
	if ( streq(text,"YES") ) DOPURG = true
	else if ( streq(text,"NO") || streq(text,"DONE") ) DOPURG = false
	else call wr_kwerr( "PURGCORR" )

	iferr ( call imgstr ( II_IM, "DARKCORR", text, SZ_LINE ) )
	    call wr_kwerr ( "DARKCORR" )
	call strupr ( text )
	if ( streq(text,"YES") ) 
	    DODARK = true
	else if ( streq(text,"NO") || streq(text,"DONE") ) 
	    DODARK = false
	else 
	    call wr_kwerr( "DARKCORR" )

	iferr ( call imgstr ( II_IM, "FLATCORR", text, SZ_LINE ) )
		call wr_kwerr ( "FLATCORR" )
	call strupr ( text )
	if ( streq(text,"YES") ) 
	    DOFLAT = true
	else if ( streq(text,"NO") || streq(text,"DONE") ) 
	    DOFLAT = false
	else 
	    call wr_kwerr( "FLATCORR" )

# 2 Jun 1991: will handle old data without DOPHOTOM keyword by printing a 
#	warning message and continuing w/o photometry
	iferr ( call imgstr ( II_IM, "DOPHOTOM", text, SZ_LINE ) ) {
	    call wr_message ("DOPHOTOM keyword missing: continues w/o phot")
	    DOPHOT = false
	}
	else {
	    call strupr ( text )
	    if ( streq(text,"YES") ) 
		DOPHOT = true
	    else if ( streq(text,"NO") || streq(text,"DONE") )
		DOPHOT = false
	    else 
		call wr_kwerr( "DOPHOTOM" )
	}
	iferr ( call imgstr ( II_IM, "DOSATMAP", text, SZ_LINE ) )
	    call wr_kwerr ( "DOSATMAP" )
	call strupr ( text )
	if ( streq(text,"YES") ) 
	    DOSAT = true
	else if ( streq(text,"NO") || streq(text,"DONE") ) 
	    DOSAT = false
	else 
	    call wr_kwerr( "DOSATMAP" )

	iferr ( call imgstr ( II_IM, "DOHISTOS", text, SZ_LINE ) )
	    call wr_kwerr ( "DOHISTOS" )
	call strupr ( text )
	if ( streq(text,"YES") ) 
	    DOHIST = true
	else if ( streq(text,"NO") || streq(text,"DONE") ) 
	    DOHIST = false
	else 
	    call wr_kwerr( "DOHISTOS" )

# 14 Jun 1991: will handle old data without DOGRPFIL keyword defaults 
# 	silently to YES
# 22 Oct 1991: commented out.  DOGRPFIL keyword not added to header after all
#
#	iferr ( call imgstr ( II_IM, "DOGRPFIL", text, SZ_LINE ) ) {
	    DOGRP = true
#	}
#	else {
#		call strupr ( text )
#		if ( streq(text,"YES") ) DOGRP = true
#		else if ( streq(text,"NO") || streq(text,"DONE") )
#			DOGRP = false
#		else call wr_kwerr( "DOGRPFIL" )
#	}

	iferr ( call imgstr ( II_IM, "OUTDTYPE", text, SZ_LINE ) )
	    call wr_kwerr ( "OUTDTYPE" )
	call strupr ( text )
	if ( streq ( text, "REAL" ) ) 
	    OUTDTYPE = TY_REAL
	else if ( streq ( text, "LONG" ) ) 
	    OUTDTYPE = TY_LONG
	else if ( streq ( text, "SHORT" ) ) 
	    OUTDTYPE = TY_SHORT
	else 
	    call wr_kwerr( "OUTDTYPE" )
end

#################################################################################
# WR_GNAMES --	Get the necessary reference file names				#

procedure   wr_gnames ( )

include "wrincl.h"

errchk imgstr, wr_kwerr

begin
	# Load DQF and calibration file names
	if ( DOMASK ) {
	    iferr ( call imgstr ( II_IM, "MASKFILE", MD_NAME, SZ_FNAME ) )
		call wr_kwerr ( "MASKFILE" )
	}
	if ( DOATOD ) {
	    iferr ( call imgstr ( II_IM, "ATODFILE", A_NAME, SZ_FNAME ) )
		call wr_kwerr ( "ATODFILE" )
	}
        if ( DOBLEV ) {
            iferr ( call imgstr ( II_IM, "BLEVFILE", BL_NAME, SZ_FNAME ) )
		call wr_kwerr ( "BLEVFILE" )
            iferr ( call imgstr ( II_IM, "BLEVDFIL", BLD_NAME, SZ_FNAME ) )
		call wr_kwerr ( "BLEVDFIL" )
	}
        if ( DOBIAS ) {
	    iferr ( call imgstr ( II_IM, "BIASFILE", BI_NAME, SZ_FNAME ) )
		call wr_kwerr ( "BIASFILE" )
	    iferr ( call imgstr ( II_IM, "BIASDFIL", BD_NAME, SZ_FNAME ) )
		call wr_kwerr ( "BIASDFIL" )
	}
        if ( DOPREF ) {
            iferr ( call imgstr ( II_IM, "PREFFILE", PI_NAME, SZ_FNAME ) )
		call wr_kwerr ( "PREFFILE" )
            iferr ( call imgstr ( II_IM, "PREFDFIL", PD_NAME, SZ_FNAME ) )
		call wr_kwerr ( "PREFDFIL" )
	}
        if ( DOPURG ) {
            iferr ( call imgstr ( II_IM, "PURGFILE", RI_NAME, SZ_FNAME ) )
		call wr_kwerr ( "PURGFILE" )
            iferr ( call imgstr ( II_IM, "PURGDFIL", RD_NAME, SZ_FNAME ) )
		call wr_kwerr ( "PURGDFIL" )
	}
        if ( DODARK ) {
            iferr ( call imgstr ( II_IM, "DARKFILE", DI_NAME, SZ_FNAME ) )
		call wr_kwerr ( "DARKFILE" )
            iferr ( call imgstr ( II_IM, "DARKDFIL", DD_NAME, SZ_FNAME ) )
		call wr_kwerr ( "DARKDFIL" )
	}
        if ( DOFLAT ) {
            iferr ( call imgstr ( II_IM, "FLATFILE", FI_NAME, SZ_FNAME ) )
		call wr_kwerr ( "FLATFILE" )
            iferr ( call imgstr ( II_IM, "FLATDFIL", FD_NAME, SZ_FNAME ) )
		call wr_kwerr ( "FLATDFIL" )
	}
        if ( DOPHOT ) {
            iferr ( call imgstr ( II_IM, "PHOTTAB", PH_NAME, SZ_FNAME ) )
		call wr_kwerr ( "PHOTTAB" )
	}
end


#################################################################################
# WR_GOTHER --	Get other necessary information from the input image and 	#
#		check input image for valid WF/PC format.  			#

procedure   wr_gother ( )

include "wrincl.h"

char	text[SZ_LINE]
real	imgetr()
int	imgeti ()
bool	imgetb ()
bool	streq()

errchk imgstr, wr_kwerr, streq, wr_error, 
	    imgeti, imgetb, imgetr, strupr

begin
	# make certain that input image is from WF/PC
	iferr ( call imgstr ( II_IM, "INSTRUME", text, SZ_LINE ) )
	    call wr_kwerr ( "INSTRUME" )
	call strupr ( text )
	if ( !streq( text, "WFPC" ) )
	    call wr_error("Keyword INSTRUME not equal to WFPC")

	if ( IM_NDIM( II_IM )!=2 )			# Must be 2D
	    call wr_error ("Input image not 2D")

	KNX = IM_LEN( II_IM, 1 )			# Input image size
	KNY = IM_LEN( II_IM, 2 )
	if ( KNX > FULL_NX || KNY > FULL_NY )
	    call wr_error("Input image exceeds max dim expected for WF/PC")

	iferr ( NGROUP = imgeti ( II_IM, "GCOUNT" ) )	# Must be group image
	    call wr_kwerr ( "GCOUNT" )		# with <= 4 groups

	if ( !imgetb( II_IM, "GROUPS" ) || NGROUP > 4 )
	    call wr_error ( "Input image has no groups or more than 4" )

# get shutter open time
	iferr ( ETIME = imgetr ( II_IM, "EXPTIME" ) )
	    call wr_kwerr( "EXPTIME" )

# get total time clocks were stopped (i.e. since last erase)
	if ( DODARK || DOPURG ) {
	    iferr ( DTIME = imgetr ( II_IM, "DARKTIME" ) )
		call wr_kwerr ( "DARKTIME" )
	}

# get exposure time for Preflash
	if ( DOPREF ) {
	    iferr ( PTIME = imgetr ( II_IM, "PREFTIME" ) )
		call wr_kwerr ( "PREFTIME" )
	}

# get time from last purge to readout
# JWM 11/27/89 Fix to protect against potential divide by zero
	if ( DOPURG ) {
	    iferr ( RTIME = imgetr ( II_IM, "PURGTIME" ) )
		call wr_kwerr ( "PURGTIME" )
	    if ( RTIME > 0.01 & RTIME > (DTIME + 1.0) ) {	# times OK
		PURGESCALE = 30.0 * 
			( ( 600.0/(RTIME-DTIME) ) ** 0.33333 ) *
			( 1.0 - ( (RTIME-DTIME)/RTIME ) ** 0.33333 )
	    } else
		call wr_error("PURGTIME too small or less than DARKTIME")
	}
# get saturated pixel DN (all pixels >= this value are flagged)
	iferr ( SATLIM = imgeti(II_IM, "SATURATE") )
	    call wr_kwerr ( "SATURATE" )
	ATODLENGTH = SATLIM + 1

# determine if WF/PC was in Wide Field Camera or Planetary Camera mode
        iferr ( call imgstr ( II_IM, "CAMERA", text, SZ_LINE ) )
	    call wr_kwerr ( "CAMERA" )
	call strupr ( text )
        if ( streq(text,"WF") ) 
	    WFC = true
	else if ( streq(text,"PC") ) 
	    WFC = false
	else 
	    call wr_kwerr( "CAMERA" )

# determine if on chip binning (AREA mode) was used
        iferr ( call imgstr ( II_IM, "MODE", text, SZ_LINE ) )
		call wr_kwerr ( "MODE" )
	call strupr ( text )
        if ( streq(text,"FULL") ) 
	    DOFULL = true
	else if ( streq(text,"AREA") ) 
	    DOFULL = false
	else 
	    call wr_kwerr( "MODE" )

# get name strings for each filter
        iferr ( call imgstr ( II_IM, "FILTNAM1", FILTER1, SZ_LINE ) )
	    call wr_kwerr ( "FILTNAM1" )

        iferr ( call imgstr ( II_IM, "FILTNAM2", FILTER2, SZ_LINE ) )
	    call wr_kwerr ( "FILTNAM2" )

	if ( DOATOD ) {		# in Celcius in header, Kelvins in pipeline
	    iferr ( BAY3TEMP = imgetr ( II_IM, "WBA3PCTM" ) )
		call wr_kwerr ( "WBA3PCTM" )
	    BAY3TEMP = BAY3TEMP + 273.15		# convert to Kelvins
	}
# get output scaling parameters
	if ( OUTDTYPE == TY_LONG || OUTDTYPE == TY_SHORT ) {
	    iferr ( WSCALE = imgetr ( II_IM, "WSCALE" ) )
		call wr_kwerr ( "WSCALE" )
	    iferr ( WZERO = imgetr ( II_IM, "WZERO" ) )
		call wr_kwerr ( "WZERO " )
	}
#	# get input data fill values
#	iferr ( PFILL = imgetb ( II_IM, "PODPSFF" ) )		# PODPS fill
#	    call wr_kwerr ( "PODPSFF" )
#	if ( PFILL ) {
#	    iferr ( PODPSFILL = imgeti ( II_IM, "PODPSFP" ) )
#		call wr_kwerr ( "PODPSFP" )
#	}
#	iferr ( SFILL = imgetb ( II_IM, "STDCFFF" ) )		# ST-DDCF fill
#	    call wr_kwerr ( "STDCFFF" )
#	if ( SFILL ) {
#	    iferr ( STDCFFILL = imgeti ( II_IM, "STDCFFP" ) )
#		call wr_kwerr ( "STDCFFP" )
#	}

# get output data fill value
	iferr ( RSDPFILL = imgeti ( II_IM, "RSDPFILL" ) )	# RSDP fill
	    call wr_kwerr ( "RSDPFILL" )

# set length of output histograms
	HISTLENGTH = 4096
end

#################################################################################
# WR_GOUT --	Get output file names.  					#

procedure wr_gout ( )

include "wrincl.h"

errchk strcpy, strcat

begin
        call strcpy ( OUT_ROOT, OI_OUT, SZ_FNAME )	# output image name
        call strcat ( ".c0h", OI_OUT, SZ_FNAME )

        call strcpy ( OUT_ROOT, OD_OUT, SZ_FNAME )	# output DQF name
        call strcat ( ".c1h", OD_OUT, SZ_FNAME )

	if ( DOHIST ) {					# output histograms
	    call strcpy ( OUT_ROOT, H_OUT, SZ_FNAME )
	    call strcat ( ".c2h", H_OUT, SZ_FNAME )
	}
        if ( DOSAT ) {					# output saturated
	    call strcpy ( OUT_ROOT, SD_OUT, SZ_FNAME ) # pixels map (DQF)
	    call strcat ( ".c3h", SD_OUT, SZ_FNAME )
	}
	if ( DOGRP ) {					# output group params
	    call strcpy ( OUT_ROOT, G_OUT, SZ_FNAME ) # text file
	    call strcat ( ".cgr", G_OUT, SZ_FNAME )
	}
end

