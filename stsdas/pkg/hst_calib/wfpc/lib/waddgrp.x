#################################################################################
#										#
#  W_ADDGRP --	Add group specification to image file name.  			#
#										#
#	Initial code:	9/91 by RAShaw						#

procedure w_addgrp (inimage, group)

# Calling arguments:
char	inimage[SZ_FNAME]		# Data filename
int	group				# Size of datafile string

# Local variables:
char	im_clstr[SZ_FNAME]		# Filename, minus group & section specs
char	section[SZ_FNAME]		# image section

begin
	call imgsection (inimage, section, SZ_FNAME)
	call imgcluster (inimage, im_clstr, SZ_FNAME)
	call sprintf (inimage, SZ_FNAME, "%s[%d]%s")
	    call pargstr (im_clstr, SZ_FNAME)
	    call pargi (group)
	    call pargstr (section, SZ_FNAME)
end


