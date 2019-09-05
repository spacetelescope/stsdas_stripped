#			File:	u_util.x

include <imhdr.h>
include <error.h>
include "u_data.h"
include "u_incl.h"

################################################################################
#										
#  U_ADDGRP --	Add group suffix to file name.  Note: does not include group	
#		total in extension; only used for opening files.  		
#										
#  History: 								
#	11 Aug 1992 by RAShaw	Initial implementation				

procedure u_addgrp (rootname, fullname, element)

char	rootname[SZ_FNAME]	# rootname
char	fullname[SZ_FNAME]	# complete name
int	element			# element number

errchk	strcpy, strcat, u_error

begin
	call strcpy ( rootname, fullname, SZ_FNAME )

	#  Select group extension
	switch (element) {
	    case 1: 
		call strcat("[1]", fullname, SZ_FNAME )
	    case 2:
		call strcat("[2]", fullname, SZ_FNAME )
	    case 3:
		call strcat("[3]", fullname, SZ_FNAME )
	    case 4:
		call strcat("[4]", fullname, SZ_FNAME )
	    default: 
		call u_error ("Unexpected group element number")
	}
end

################################################################################
#										
# U_UPDTKW1 -- Update Keywords in the output Image.  This puts 'COMPLETE' 	
#		in the appropriate output image processing flags and sets the 	
#		dezero, biaseven, biasodd, and photometry keywords.  		
#										
#  History: 								
#	9 Oct 1993 by CYZhang	Initial implementation				
#				Move all keywords of image statistics to	
#				U_UPDTKW2; Use gf_iaddr and gf_iastr to replace 
#				calls to gf_iputr and gf_ipstr 			
#      16 Nov 1993 by CYZhang 	Populate PHOTTAB with throughput table file name
#				when running in PODPS environment	
#      11 Apr 2007 by WJHack    Added WF4TCORR support	

procedure u_updtkw1 (im, act, kw, cam, nam, gpix, c0flag)

#  Calling arguments:
pointer im		# pointer to image being updated
pointer	act		# ACTion data structure
pointer	kw		# KeyWord data structure
pointer cam
pointer	nam		# File NAMe structure
pointer	gpix	# File GoodPIXel structure
bool	c0flag		# only print phot missing message for c0h files

#  Local variables
char	obuf[SZ_LINE]		# Buffer for message construction

#  Function used:
long	clktime()	# required to force min/max header update
int	envgets()		# Get environment variable
#int	strncmp()

errchk	gf_iastr, u_error, gf_iaddr

begin

	# Mark all the "action" keywords in the output image to indicate 
	# whether the processing step was performed
	if ( DOMASK(act) ) {
	    iferr ( call gf_iastr ( im, "MASKCORR", "COMPLETE" ) ) 
		call u_error ( "Error in updating MASKCORR flag" )
	}

	if ( DOATOD(act) ) {
	    iferr ( call gf_iastr ( im, "ATODCORR", "COMPLETE" ) ) 
		call u_error ( "Error in updating ATODCORR flag" )
	}
	if ( DOWF4T(act) ) {
	    iferr ( call gf_iastr ( im, "WF4TCORR", "COMPLETE" ) ) 
		call u_error ( "Error in updating WF4TCORR flag" )
	}
	if ( DOBLEV(act) ) {
	    iferr ( call gf_iastr ( im, "BLEVCORR", "COMPLETE" ) )
		call u_error ("Error in updating BLEVCORR flag")
	}
	if ( DOBIAS(act) ) {
	    iferr ( call gf_iastr ( im, "BIASCORR", "COMPLETE" ) ) 
		call u_error ( "Error in updating BIASCORR flag" )
	}
	if ( DODARK(act) ) {
	    iferr ( call gf_iastr ( im, "DARKCORR", "COMPLETE" ) )
		call u_error ( "Error in updating DARKCORR flag" )
	}
	if ( DOFLAT(act) ) {
	    iferr ( call gf_iastr ( im, "FLATCORR", "COMPLETE" ) )
		call u_error ( "Error in updating FLATCORR flag" )
	}

	if ( DOSHAD(act) ) {
	    iferr ( call gf_iastr ( im, "SHADCORR", "COMPLETE" ) )
		call u_error ( "Error in updating SHADCORR flag" )
	}
	if ( DOPHOT(act) ) {
	    # if in pipeline, populate PHOTTAB with thruput filename
	    if (envgets (PODPS_ENVIRON, obuf, SZ_FNAME) >= 1) {
		iferr ( call gf_iastr ( im, "PHOTTAB", THRU_TBL(nam) ) )
		    call u_error ( "Error in updating PHOTTAB" )
	    }
	    iferr ( call gf_iastr ( im, "DOPHOTOM", "COMPLETE" ) )
		call u_error ( "Error in updating DOPHOT flag" )
	}

	if ( DOHIST(act) ) {
	    iferr ( call gf_iastr ( im, "DOHISTOS", "COMPLETE" ) )
		call u_error ( "Error in updating DOHIST flag" )
	}

	# Update the statistical keywords
	# (n.b. these group parameters must be updated for each element)
	# Update image header structure
	iferr ( call gf_iaddr ( im, "GOODMIN", GOODMIN(kw) ) )
	    call u_error ( "Error in writing GOODMIN" )
	iferr ( call gf_iaddr ( im, "GOODMAX", GOODMAX(kw) ) )
	    call u_error ( "Error in writing GOODMAX" )

	iferr ( call gf_iaddr ( im, "DATAMEAN", DATAMEAN(kw) ) )
	    call u_error ( "Error in writing DATAMEAN" )

	#iferr ( call gf_iaddl ( im, "GPIXELS", N_GOODPIXEL(kw) ) )
	iferr ( call gf_iaddl ( im, "GPIXELS", N_GOODPIX(gpix) ) )
	    call u_error ( "Error in writing GPIXELS" )
	iferr ( call gf_iaddl ( im, "SOFTERRS", N_SOFTERROR(kw) ) )
	    call u_error ( "Error in writing SOFTERRS" )
	iferr ( call gf_iaddl ( im, "CALIBDEF", N_CALDEFECT(kw) ) )
	    call u_error ( "Error in writing CALIBDEF" )
	iferr ( call gf_iaddl ( im, "STATICD", N_STATICDEF(kw) ) )
	    call u_error ( "Error in writing STATICD" )
	iferr ( call gf_iaddl ( im, "ATODSAT", N_SATURATE(kw) ) )
	    call u_error ( "Error in writing ATODSAT" )
	iferr ( call gf_iaddl ( im, "DATALOST", N_DATALOST(kw) ) )
	    call u_error ( "Error in writing DATALOST" )
	iferr ( call gf_iaddl ( im, "BADPIXEL", N_BADPIXEL(kw) ) )
	    call u_error ( "Error in writing BADPIXEL" )
	iferr ( call gf_iaddl ( im, "OVERLAP", N_OVERLAP(kw) ) )
	    call u_error ( "Error in writing OVERLAP" )

	# Update the DEZERO, BIASEVEN, BIASODD keywords
	# (n.b. these group parameters must be updated for each element)
	iferr ( call gf_iaddr ( im, "DEZERO", DEZERO(kw) ) )
	    call u_error ( "Error in writing DEZERO" )
	iferr ( call gf_iaddr ( im, "BIASEVEN", BIASEVEN(kw) ) )
	    call u_error ( "Error in writing BIASEVEN" )
	iferr ( call gf_iaddr ( im, "BIASODD", BIASODD(kw) ) )
	    call u_error ( "Error in writing BIASODD" )

	# Write PHOTMODE and other photometry keyword 
	iferr ( call gf_ipstr ( im, "PHOTMODE", PHOTMODE(kw) ) )
	    call u_error ( "Error in writing PHOTMODE" )
	iferr ( call gf_iaddr (im, "PHOTFLAM", PHOTFLAM(kw)) )
	    call u_error ( "Error in writing PHOTFLAM" )
	iferr ( call gf_iaddr (im, "PHOTZPT", PHOTZPT(kw)) )
	    call u_error ( "Error in writing PHOTZPT" )
	iferr ( call gf_iaddr (im, "PHOTPLAM", PHOTPLAM(kw)) )
	    call u_error ( "Error in writing PHOTPLAM" )
	iferr ( call gf_iaddr (im, "PHOTBW", PHOTBW(kw)) )
	    call u_error ( "Error in writing PHOTBW" )
	iferr ( call gf_iaddr (im, "ZP_CORR", DELTAMAG(kw)) )
	    call u_error ( "Error in writing ZP_CORR" )

	# The following block is removed. IM_LIMTIME is set up before entering
	# the do loop for groups, and once IM_MIN and IM_MAX updated, there 
	# is no need to call gf_iaddr (im, "DATAMIN", MINVAL(kw))! 
	# -CYZhang 14/10/93
	IM_LIMTIME(im) = clktime (long(0))
	IM_MIN(im)     = MINVAL(kw)
	IM_MAX(im)     = MAXVAL(kw)
	iferr ( call gf_iaddr ( im, "DATAMIN", MINVAL(kw) ) )
	    call u_error ( "Error in writing DATAMIN" )
	iferr ( call gf_iaddr ( im, "DATAMAX", MAXVAL(kw) ) )
	    call u_error ( "Error in writing DATAMAX" )

#	IM_LIMTIME (OUT_IMG_P(im)) = IM_MTIME(OUT_IMG_P(im)) + 1
#	IM_LIMTIME (OUT_DQF_P(im)) = IM_MTIME(OUT_DQF_P(im)) + 1
#	if (DOHIST(act)) {
#	    IM_LIMTIME (HIST_IMG_P(im))= IM_MTIME(HIST_IMG_P(im)) + 1
#	}
end

################################################################################
#										
# U_UPDTKW2 -- Update image statistics keywords in the output Image. 		
#
#      History: 								
#	 9 Oct 1993 by CYZhang	Initial implementation				

procedure u_updtkw2 (im, kw, stat, c0flag)

#  Calling arguments:
pointer im		# pointer to image being updated
pointer	kw		# KeyWord data structure
pointer	stat		# Sectional statistics structure
bool	c0flag		# only print phot missing message for c0h files

errchk	u_error, gf_iaddr, gf_iaddl

begin

	# Update the sectional statistical keywords -- CYZ 2/9/93
	# (n.b. these group parameters must be updated for each element)
	iferr ( call gf_iaddr ( im, "MEDIAN", S_GMEDIAN(stat) ) )
	    call u_error ( "Error in writing MEDIAN" )
	iferr ( call gf_iaddr ( im, "MEDSHADO", S_SMEDIAN(stat) ) )
	    call u_error ( "Error in writing MEDSHADO" )
	iferr ( call gf_iaddr ( im, "HISTWIDE", S_HISTWIDE(stat) ) )
	    call u_error ( "Error in writing HISTWIDE" )
	iferr ( call gf_iaddr ( im, "SKEWNESS", S_SKEW(stat) ) )
	    call u_error ( "Error in writing SKEWNESS" )
	iferr ( call gf_iaddr ( im, "MEANC10", S_MEANC10(stat) ) )
	    call u_error ( "Error in writing MEANC10" )
	iferr ( call gf_iaddr ( im, "MEANC25", S_MEANC25(stat) ) )
	    call u_error ( "Error in writing MEANC25" )
	iferr ( call gf_iaddr ( im, "MEANC50", S_MEANC50(stat) ) )
	    call u_error ( "Error in writing MEANC50" )
	iferr ( call gf_iaddr ( im, "MEANC100", S_MEANC100(stat) ) )
	    call u_error ( "Error in writing MEANC100" )
	iferr ( call gf_iaddr ( im, "MEANC200", S_MEANC200(stat) ) )
	    call u_error ( "Error in writing MEANC200" )
	iferr ( call gf_iaddr ( im, "MEANC300", S_MEANC300(stat) ) )
	    call u_error ( "Error in writing MEANC300" )
	iferr ( call gf_iaddr ( im, "BACKGRND", S_BCKGRD(stat) ) )
	    call u_error ( "Error in writing BACKGRND" )
end

################################################################################
#										
# U_UPDTKW3 -- Update uncorrected bias keywords in the output Image. 		
#
#      History: 								
#	 2 May 2007  by WJHack	Initial implementation				

procedure u_updtkw3 (im, kw)

#  Calling arguments:
pointer im		# pointer to image being updated
pointer	kw		# KeyWord data structure
#pointer	stat		# Sectional statistics structure
#bool	c0flag		# only print phot missing message for c0h files

errchk	u_error, gf_iaddr, gf_iaddl

begin
	iferr ( call gf_iaddr ( im, "BIASEVNU", BIASEVEN(kw) ) )
	    call u_error ( "Error in writing BIASEVNU" )
	iferr ( call gf_iaddr ( im, "BIASODDU", BIASODD(kw) ) )
	    call u_error ( "Error in writing BIASODDU" )
end


################################################################################
#										
#  U_STRPWHT -- Strip trailing whitespace from a string.  Note: string is 	
#		modified in-place.  						
#										
#  History: 								
#	15 Jul 1992 by RAShaw	Initial implementation				

procedure u_strpwht (pstr)

#  Calling argument:
char	pstr[ARB]                       # photmode string

#  Local variable:
int	ip                              # string pointer

#  Function called:
int	strlen()			# return length of string

begin
        for (ip=strlen(pstr);  ip > 0 && pstr[ip] == ' ';  ip=ip-1)
	    ;
        pstr[ip+1] = EOS
end
