include <imhdr.h>
include <imio.h>
include <error.h>
include "wrdata.h"

#################################################################################
# WR_ADDGRP --	Add group suffix to file name.  				#

procedure wr_addgrp ( rootname, element, name, outimage )

include "wrincl.h"

char	rootname[SZ_FNAME]	# rootname
int	element			# element number
char	name[SZ_FNAME]		# complete name
bool	outimage		# flag for output image

errchk	strcpy, strcat, wr_error

begin
	call strcpy ( rootname, name, SZ_FNAME )

	switch ( element ) {	# select group extension
	    case 1: {
		if ( outimage ) {
		    switch ( NGROUP ) {
			case 1: call strcat("[1/1]", name, SZ_FNAME )
			case 2: call strcat("[1/2]", name, SZ_FNAME )
			case 3: call strcat("[1/3]", name, SZ_FNAME )
			case 4: call strcat("[1/4]", name, SZ_FNAME )
			default: call wr_error ("bad number of groups")
		    }
		} else
		    call strcat("[1]", name, SZ_FNAME )
	    }
	    case 2:
		call strcat("[2]", name, SZ_FNAME )
	    case 3:
		call strcat("[3]", name, SZ_FNAME )
	    case 4:
		call strcat("[4]", name, SZ_FNAME )
	    default: 
		call wr_error ( "unexpected group element number" )
	}
end


#################################################################################
# WR_UPDATEKW -- Update Keywords in the output Image.  This puts 'DONE' in 	#
#                the appropriate output image processing flags and sets the 	#
#                statistics and photometry keywords.  				#

procedure wr_updatekw ( im, c0flag, grpnum )

include "wrincl.h"

#  Calling arguments:
pointer im		# pointer to image being updated
bool	c0flag		# only print phot missing message for c0h files
int	grpnum		# current group number

#  Local variables:
long	clktime()	# required to force min/max header update
char	photmode[SZ_LINE]
char	text[SZ_LINE]	# warning messages

errchk	impstr, wr_error, imputr, imputl, wr_gphotmode, wr_photom

begin
	#  Mark all the "action" keywords in the output image to indicate 
	# whether the processing step was performed
	if ( DOMASK ) {
	    iferr ( call impstr ( im, "MASKCORR", "DONE" ) ) 
		call wr_error ( "Error in writing done mask flag" )
	}
	if ( DOATOD ) {
	    iferr ( call impstr ( im, "ATODCORR", "DONE" ) ) 
		call wr_error ( "Error in writing done atod flag" )
	}
	if ( DOBLEV ) {
	    iferr ( call impstr ( im, "BLEVCORR", "DONE" ) )
		call wr_error ("Error in writing done bias level flag")
	}
	if ( DOBIAS ) {
	    iferr ( call impstr ( im, "BIASCORR", "DONE" ) ) 
		call wr_error ( "Error in writing done bias flag" )
	}
	if ( DOPREF ) {
	    iferr ( call impstr ( im, "PREFCORR", "DONE" ) )
		call wr_error ( "Error in writing done preflash flag" )
	}
	if ( DOPURG ) {
	    iferr ( call impstr ( im, "PURGCORR", "DONE" ) )
		call wr_error ( "Error in writing done purge flag" )
	}
	if ( DODARK ) {
	    iferr ( call impstr ( im, "DARKCORR", "DONE" ) )
		call wr_error ( "Error in writing done dark flag" )
	}
	if ( DOFLAT ) {
	    iferr ( call impstr ( im, "FLATCORR", "DONE" ) )
		call wr_error ( "Error in writing done flat flag" )
	}
	if ( DOPHOT ) {
	    iferr ( call impstr ( im, "DOPHOTOM", "DONE" ) )
		call wr_error ( "Error in writing done phot flag" )
	}
	if ( DOSAT ) {
	    iferr ( call impstr ( im, "DOSATMAP", "DONE" ) )
		call wr_error ( "Error in writing done sat map flag" )
	}
	if ( DOHIST ) {
	    iferr ( call impstr ( im, "DOHISTOS", "DONE" ) )
		call wr_error ( "Error in writing done histos flag" )
	}

#  Update the statistical keywords
#	(n.b. these are group parameters and therefore must be updated for 
#	each element)

	iferr ( call imputr ( im, "DATAMIN", MINVAL ) )
	    call wr_error ( "Error in writing DATAMIN" )
	iferr ( call imputr ( im, "DATAMAX", MAXVAL ) )
	    call wr_error ( "Error in writing DATAMAX" )

	IM_LIMTIME ( im ) = clktime (long(0))		# update iraf header

	IM_MIN ( im ) = MINVAL	# update image header structure
	IM_MAX ( im ) = MAXVAL

	iferr ( call imputr ( im, "DEZERO", DEZERO ) )
	    call wr_error ( "Error in writing DEZERO" )
	if (!BOEFLAG) {
	    iferr ( call imputr ( im, "BIASEVEN", BIASEVEN ) )
		call wr_error ( "Error in writing BIASEVEN" )
	    iferr ( call imputr ( im, "BIASODD", BIASODD ) )
		call wr_error ( "Error in writing BIASODD" )
	} else {
	    if (c0flag) {
		call sprintf (text, SZ_LINE, 
		    "    Group [%1d]:  BIASEVEN = %9.7g;   BIASODD = %9.7g")
		    call pargi (grpnum)
		    call pargr (BIASEVEN)
		    call pargr (BIASODD)
		call wr_message (text)
	    }
	}

	iferr ( call imputr ( im, "GOODMIN", GOODMIN ) )
	    call wr_error ( "Error in writing GOODMIN" )
	iferr ( call imputr ( im, "GOODMAX", GOODMAX ) )
	    call wr_error ( "Error in writing GOODMAX" )

	if ( N_GOODPIXEL > 0 ) {
	    iferr ( call imputr ( im, "DATAMEAN", SUM / N_GOODPIXEL ) )
		call wr_error ( "Error in writing DATAMEAN" )
	}
	iferr ( call imputl ( im, "GPIXELS", N_GOODPIXEL ) )
	    call wr_error ( "Error in writing GPIXELS" )
	iferr ( call imputl ( im, "SOFTERRS", N_SOFTERROR ) )
	    call wr_error ( "Error in writing SOFTERRS" )
	iferr ( call imputl ( im, "CALIBDEF", N_CALIBDEFECT ) )
	    call wr_error ( "Error in writing CALIBDEF" )
	iferr ( call imputl ( im, "STATICD", N_STATICDEFECT ) )
	    call wr_error ( "Error in writing STATICD" )
	iferr ( call imputl ( im, "ATODSAT", N_ATODSAT ) )
	    call wr_error ( "Error in writing ATODSAT" )
	iferr ( call imputl ( im, "DATALOST", N_DATALOST ) )
	    call wr_error ( "Error in writing DATALOST" )
	iferr ( call imputl ( im, "BADPIXEL", N_BADPIXEL ) )
	    call wr_error ( "Error in writing BADPIXEL" )

#  Construct and update PHOTMODE keyword
	iferr ( call wr_gphotmode ( photmode ) )
	    call wr_error ( "Failed to construct PHOTMODE string" )
	iferr ( call impstr ( im, "PHOTMODE", photmode ) )
	    call wr_error ( "Error in writing PHOTMODE" )

#  Get photometry keywords from PHOTTAB table and update in 
#  output image headers
	if ( DOPHOT ) {
	    iferr ( call wr_photom ( im, c0flag, grpnum ) )
		call wr_error ( "Error in getting phot keywords" )
	}
end

#################################################################################
# WR_STRPWHT -- Strip trailing whitespace from a string.  			#
#										#
#	8/91	RAShaw		Initial code, replaces wr_photeq() boolean 	#
#				to compare two photmode strings, where one may	#
#				contain trailing blanks				#

procedure wr_strpwht ( pstr )

include "wrincl.h"

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


