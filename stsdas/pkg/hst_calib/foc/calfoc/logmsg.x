# logmsg -- print a message
# This routine prints the root name of the observation, prints the
# message which is the input argument, and flushes STDOUT.
#
# Note that this uses the common block xcroot.
#
# Phil Hodge, 25-Jan-1992  Extracted from calfoc.x.

procedure logmsg (message)

char	message[ARB]		# i: message to be printed
#--
char	rootname[SZ_FNAME]	# root name of the observation
common	/xcroot/ rootname	# root name, to be included in message

begin
	call printf (rootname)
	call printf (" ")
	call printf (message)
	call printf ("\n")
	call flush (STDOUT)
end
