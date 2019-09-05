include <imhdr.h>
include	<tbset.h>
include <imio.h>
include <error.h>
include "wrdata.h"

#################################################################################
# WR_INPUT --	Open selected element of input image for reading.  		#

pointer procedure wr_input ( element, name, opnflg )

include "wrincl.h"

int	element			# element number
char	name[SZ_FNAME]		# name of input number (fully qualified
				#    except for group element extension)
bool	opnflg			# flag for file open
char	fullname[SZ_FNAME]	# full filename 
char	text[SZ_LINE]
pointer	im, immap()

errchk	wr_addgrp, immap, sprintf, pargstr, wr_error

begin
	# add appropriate group element to file name
	call wr_addgrp ( name, element, fullname, false )

	iferr ( im = immap ( fullname, READ_ONLY, 0 ) ) {
	    call sprintf ( text, SZ_LINE, "Input File Open Error on: %s" )
	    call pargstr ( fullname )
	    opnflg = false
	    call wr_error ( text )
	}
	opnflg = true
	return ( im )
end

#################################################################################
# WR_OUTPUT --	Open selected element of output image for writing.  		#

pointer procedure wr_output ( element, name, opnflg, template )

include "wrincl.h"

int	element			# element number
char	name[SZ_FNAME]		# name of output file (fully qualified
				#    except for group element extension)
bool	opnflg			# flag for file open
pointer	template		# image template (usually the input image)
char	fullname[SZ_FNAME]	# full filename
char	text[SZ_LINE]
pointer	im, immap()

errchk	wr_addgrp, immap, sprintf, pargstr, wr_error

begin
	# add appropriate group element to file name
	call wr_addgrp ( name, element, fullname, true )

	iferr ( im = immap ( fullname, NEW_COPY, template ) ) {
	    call sprintf ( text, SZ_LINE, "Output File Open Error on: %s" )
	    call pargstr ( fullname )
	    call wr_error ( text )
	    opnflg = false
	}
	opnflg = true
	return ( im )
end

#################################################################################
# WR_OUTFILE --	Open output group parameter text file.  			#

int procedure wr_outfile ( element, name, opnflg )

include "wrincl.h"

int	element			# element number (a.k.a. group number)
char	name[SZ_FNAME]		# name of output file
bool	opnflg			# flag for file open
int	fd			# file descriptor
char	text[SZ_LINE]
int	open()

errchk	open, sprintf, pargstr, wr_error

begin
	# New file for first group...
	if ( element == 1 ) {	
	    iferr ( fd = open ( name, NEW_FILE, TEXT_FILE ) ) {
		call sprintf ( text, SZ_LINE,
		    "Output File Create Error on: %s" )
		call pargstr ( name )
		call wr_error ( text )
	    }

	# ...or append to same text file
	} else {
	    iferr ( fd = open ( name, APPEND, TEXT_FILE ) ) {
		call sprintf ( text, SZ_LINE,
		    "Output File Open Error on: %s" )
		call pargstr ( name )
		call wr_error ( text )
	    }
	}
	opnflg = true
	return ( fd )
end

#################################################################################
# WR_REFFILE -- Open selected element of Reference File for reading.  		#

pointer procedure wr_reffile ( element, name, opnflg )

include "wrincl.h"

int	element			# element number
char	name[SZ_FNAME]		# name of input image (fully qualified 
				#    except for group element extension)
bool	opnflg			# flag for file open
char	fullname[SZ_FNAME]	# full filename 
char	text[SZ_LINE]
int	numgroups		# number of groups in images (must be 4)
int	chip			# number of chip from DETECTOR keyword
int	imgeti()
bool	streq()
pointer	im, immap()

errchk	wr_addgrp, immap, sprintf, pargstr, wr_error,
		strupr, imgeti, wr_kwerr, imgstr, streq, wr_message

begin
	#  Add appropriate group element to file name
	call wr_addgrp ( name, element, fullname, false )

	iferr ( im = immap ( fullname, READ_ONLY, 0 ) ) {
	    opnflg = false
	    call sprintf ( text, SZ_LINE, 
			"Error Opening Reference File: %s" )
	    call pargstr ( fullname )
	    call wr_error ( text )
	}
	opnflg = true

	iferr ( numgroups = imgeti ( im, "GCOUNT" ) )
		call wr_kwerr ( "GCOUNT in Reference File" )

	if ( numgroups != 4 ) {
	    call sprintf ( text, SZ_LINE, 
		"Not Four Groups in Reference File: %s" )
	    call pargstr ( fullname )
	    call wr_error ( text )
	}

	iferr ( call imgstr ( im, "CDBSFILE", text, SZ_LINE ) )
		call wr_kwerr ( "CDBSFILE in Reference File" )
	call strupr ( text )
	if ( streq(text, "NO") ) {
	    call sprintf (text, SZ_LINE, 
		"CDBSFILE Flag not set in Reference File: %s" )
	    call pargstr ( fullname )
	    call wr_message ( text )
	}

	iferr ( chip = imgeti ( im, "DETECTOR" ) )
	    call wr_kwerr ( "DETECTOR in Reference File" )
	if ( chip != CHIPNUM ) {
	    call sprintf (text, SZ_LINE, 
		"DETECTOR value mismatch in Reference File: %s" )
	    call pargstr ( fullname )
	    call wr_message ( text )
	}

	iferr ( call imgstr ( im, "CAMERA", text, SZ_LINE ) )
	    call wr_kwerr ( "CAMERA in Reference File" )
	call strupr ( text )
	if ( ( streq(text,"WF") && !WFC ) || ( streq(text,"PC") && WFC ) ) {
	    call sprintf (text, SZ_LINE, 
		"CAMERA value mismatch in Reference File: %s" )
	    call pargstr ( fullname )
	    call wr_message ( text )
	}

	iferr ( call imgstr ( im, "MODE", text, SZ_LINE ) )
	    call wr_kwerr ( "MODE in Reference File" )
	call strupr ( text )
	if ( ( streq(text,"FULL") && !DOFULL ) ||
		( streq(text,"AREA") && DOFULL ) ) {
	    call sprintf (text, SZ_LINE, 
		"MODE value mismatch in Reference File: %s" )
	    call pargstr ( fullname )
	    call wr_message ( text )
	}

	return ( im )
end

#################################################################################
# WR_REFTAB -- Open Reference Table for reading.  				#

pointer procedure wr_reftab ( name, opnflg )

include "wrincl.h"

char	name[SZ_FNAME]		# name of input table
bool	opnflg			# flag for file open
char	text[SZ_LINE]		# buffer for error message
pointer	tbtopn ()
pointer	tp

errchk	tbtopn, sprintf, pargstr, wr_error

begin
	iferr ( tp = tbtopn ( name, READ_ONLY, NULL ) ) {
	    opnflg = false
	    call sprintf ( text, SZ_LINE, 
		"Error Opening Reference Table: %s" )
	    call pargstr ( name )
	    call wr_error ( text )
	}
	opnflg = true
	return ( tp )
end

#################################################################################
# WR_FINISHUP -- Closes any open files.  					#

procedure wr_finishup ()

include "wrincl.h"

errchk	wr_imunmap, wr_tbunmap

begin
        if ( II_OPEN )  call wr_imunmap ( II_IM,  II_OPEN )	# Input image
        if ( ID_OPEN )  call wr_imunmap ( ID_IM,  ID_OPEN )	# Input DQF

        if ( MD_OPEN )  call wr_imunmap ( MD_IM,  MD_OPEN )	# Static mask
        if ( A_OPEN )   call wr_imunmap ( A_IM,   A_OPEN )	# AtoD
        if ( BL_OPEN )  call wr_imunmap ( BL_IM,  BL_OPEN )	# EED file
        if ( BLD_OPEN ) call wr_imunmap ( BLD_IM, BLD_OPEN )
        if ( BI_OPEN )  call wr_imunmap ( BI_IM,  BI_OPEN )	# Bias
        if ( BD_OPEN )  call wr_imunmap ( BD_IM,  BD_OPEN )
        if ( PI_OPEN )  call wr_imunmap ( PI_IM,  PI_OPEN )	# Preflash
        if ( PD_OPEN )  call wr_imunmap ( PD_IM,  PD_OPEN )
        if ( RI_OPEN )  call wr_imunmap ( RI_IM,  RI_OPEN )	# SuperPurge
        if ( RD_OPEN )  call wr_imunmap ( RD_IM,  RD_OPEN )
        if ( DI_OPEN )  call wr_imunmap ( DI_IM,  DI_OPEN )	# Dark
        if ( DD_OPEN )  call wr_imunmap ( DD_IM,  DD_OPEN )
        if ( FI_OPEN )  call wr_imunmap ( FI_IM,  FI_OPEN )	# Flat Field
        if ( FD_OPEN )  call wr_imunmap ( FD_IM,  FD_OPEN )

	if ( PH_OPEN )  call wr_tbunmap ( PH_TAB, PH_OPEN )	# Phot table

        if ( OI_OPEN )  call wr_imunmap ( OI_IM,  OI_OPEN )	# Output image
        if ( OD_OPEN )  call wr_imunmap ( OD_IM,  OD_OPEN )	# Output DQF
        if ( H_OPEN )   call wr_imunmap ( H_IM,   H_OPEN )	# Output Histos
        if ( SD_OPEN )  call wr_imunmap ( SD_IM,  SD_OPEN )	# Output SatMap
	if ( G_OPEN )	call wr_close   ( G_FILE, G_OPEN )	# gp text file
end


#################################################################################
# WR_IMUNMAP -- Un-map an image and set its boolean open flag to false.  	#

procedure wr_imunmap ( im, opnflg )

include "wrincl.h"

pointer	im			# image pointer
bool	opnflg			# open flag

errchk	imunmap, wr_error

begin
	iferr ( call imunmap ( im ) )
	    call wr_error ( "imunmap call failed" )
	opnflg = false
end

#################################################################################
# WR_TBUNMAP -- Un-map an image and set its boolean open flag to false.  	#

procedure wr_tbunmap ( tp, opnflg )

include "wrincl.h"

pointer	tp			# table pointer
bool	opnflg			# open flag

errchk	tbtclo, wr_error

begin
	iferr ( call tbtclo ( tp ) )
	    call wr_error ( "tbunmap call failed" )
	opnflg = false
end

#################################################################################
# WR_CLOSE -- Close a text file and set its boolean open flag to false.  	#

procedure wr_close ( fd, opnflg )

include "wrincl.h"

int	fd			# file descriptor
bool	opnflg			# open flag

errchk	close, wr_error

begin
	iferr ( call close ( fd ) )
		call wr_error ( "wr_close call failed" )
	opnflg = false
end

