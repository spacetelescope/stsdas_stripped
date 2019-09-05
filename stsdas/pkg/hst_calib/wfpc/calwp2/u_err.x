#			File:	u_err.x

include <error.h>

#################################################################################
#										#
#  U_KWERR --	Report a error associated with accessing a keyword.  		#
#										#
#  Last Modified: 								#
#	12 Aug 1992 by RAShaw	Initial implementation				#

procedure u_kwerr (name)

#  Calling argument:
char	name[ARB]			# keyword name

#  Local variable:
char	text[SZ_LINE]			# full text of error message

errchk	u_error

begin

	call sprintf (text, SZ_LINE, "Error accessing keyword: %s")
	    call pargstr (name)

	call u_error (text)

end


#################################################################################
#										#
#  U_MESSAGE --	Report a message to STDOUT.  					#
#										#
#  Last Modified: 								#
#	12 Aug 1992 by RAShaw	Initial implementation				#

procedure u_message (message)

#include	"u_context.h"

#  Calling argument:
char	message[SZ_LINE]	# message to be printed from caller

errchk	printf, pargstr, flush

begin

#	call printf ("---  Message From: CALWP-2  While Processing: %s\n")
#	    call pargstr (ROOT)

	call printf ("---  %s\n")
	    call pargstr (message)

	call flush (STDOUT)

end


#################################################################################
#										#
#  U_WARN --	Report a message along with the root name of the image being 	#
#		processed and the name of the program.  			#
#										#
#  Last Modified: 								#
#	12 Aug 1992 by RAShaw	Initial implementation				#

procedure u_warn (message)

include	"u_context.h"

#  Calling argument:
char	message[SZ_LINE]	# message to be printed from caller

errchk	printf, pargstr, flush

begin

	call printf ("*** WARNING From: CALWP-2  While Processing: %s\n")
	    call pargstr (ROOT)

	call printf ("***      %s\n")
	    call pargstr (message)

	call flush (STDOUT)

end


#################################################################################
#										#
#  U_ERROR --	Report an error condition along with the root name of the 	#
#		image being processed.  					#
#										#
#  Last Modified: 								#
#	12 Aug 1992 by RAShaw	Initial implementation				#
#	16 Dec 1993 by CYZhang	Add call xer_reset before call error		#
#	29 Jan 1994 by CYZhang	Replace eprintf with putline and putci		#

procedure u_error (message)

include	"u_context.h"

#  Calling argument:
char	message[SZ_LINE]	# message to be printed from caller

errchk	eprintf, pargstr, flush, strcpy

begin

#	call eprintf ("*** ERROR From: CALWP2  While Processing: %s\n")
#	    call pargstr (ROOT)

#	call eprintf ( "***      %s\n" )
#	    call pargstr (message)

        call putline (STDERR, "*** ERROR From: CALWP2  While Processing: ")
        call putline (STDERR, ROOT)
        call putci (STDERR, '\n')
 
        call putline (STDERR, "***      " )
        call putline (STDERR, message)
        call putci (STDERR, '\n')

	call flush (STDERR)
	call xer_reset ()
	call error (EA_FATAL, "IRAF Error Handler Called")

end


