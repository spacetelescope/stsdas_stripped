# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# YERROR -- call error
# This routine calls the iraf error subroutine.  The error number and
# error message are arbitrary except that the message will be truncated
# if it is longer than SZ_LINE.
#
# Phil Hodge, 23-May-1989  Subroutine created.

procedure yerror (ecode, errmes)

int	ecode			# I: Error code.
#				# i: text of error message
%      character*(*) errmes
#--
pointer sp
pointer message		# spp equivalent of error message
pointer omess		# buffer for original error message

begin
	call smark (sp)
	call salloc (message, SZ_LINE, TY_CHAR)
	call salloc (omess, SZ_LINE, TY_CHAR)

	call f77upk (errmes, Memc[message], SZ_LINE)
	call error (ecode, Memc[message])

	# There's no point in calling sfree.
end
