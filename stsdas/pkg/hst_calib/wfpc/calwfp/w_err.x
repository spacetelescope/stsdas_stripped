include <error.h>
include "wrdata.h"

#################################################################################
# WR_KWERR --	Report a error associated with accessing a keyword.  		#

procedure wr_kwerr( name )

include "wrincl.h"

char	name[ARB]
char	text[SZ_LINE]

errchk	sprintf, pargstr, wr_error

begin
	call sprintf ( text, SZ_LINE, "Error with Keyword %s" )
	call pargstr ( name )
	call wr_error ( text )
end

#################################################################################
# WR_MESSAGE -- Report a message along with the root name of the image being 	#
#		processed and the name of the program.  			#

procedure wr_message ( message )

include "wrincl.h"

char	message[SZ_LINE]	# message to be printed from caller

errchk	printf, pargstr, flush

begin
	call printf ( "---  Message From: CALWFP   While Processing: %s\n" )
	call pargstr ( ROOT )

	call printf ( "---  Message: %s\n" )
	call pargstr ( message )
	call flush ( STDOUT )
end

#################################################################################
# WR_ERROR -- Report an error situation along with the root name of the 	#
#	      image being processed and the name of the program.  		#

procedure wr_error ( message )

include "wrincl.h"

char	message[SZ_LINE]	# message to be printed from caller

errchk	eprintf, pargstr, flush, strcpy

begin
	call eprintf ( "*** Error From: CALWFP   While Processing: %s\n" )
	call pargstr ( ROOT )

	call eprintf ( "*** Error: %s\n" )
	call pargstr ( message )
	call flush ( STDERR )

# clear rootname global used in messages
	call strcpy ( "", ROOT, SZ_FNAME )
	call error (EA_FATAL, "Iraf Error Handler Called")
end


