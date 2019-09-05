include	<imio.h>

# Copyright restrictions apply - see stsdas$copyright.stsdas 

# PIXNXTGRP -- Set the image name to the next group
#
# This procedure changes the name of the image in the parameter structure
# to the next group in the same image. If there are no more groups in the
# image, it does not change the name and returns an error status.
#
# B.Simon	05-Jul-90	Original

int procedure pixnxtgrp (par, im)

pointer	par		# i: Parameter file descriptor
pointer im		# i: Current image file descriptor
#--
int	status, group, ngroup
pointer	sp, image, cluster

int	wrt_param()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (cluster, SZ_FNAME, TY_CHAR)

	# Get the group number and the total number of groups

	group = IM_CLINDEX(im)
	ngroup = IM_CLSIZE(im)

	if (group >= ngroup) {
	    status = ERR

	} else {
	    # EXtract the cluster from the old image name (BPS 08.01.91)

	    call rdstr_param (par, "image", Memc[image], SZ_FNAME)
	    call imgcluster (Memc[image], Memc[cluster], SZ_FNAME)

	    # Create the new image name

	    call sprintf (Memc[image], SZ_FNAME, "%s[%d]")
	    call pargstr (Memc[cluster])
	    call pargi (group+1)

	    status = wrt_param (par, "image", Memc[image])
	}

	call sfree (sp)
	return (status)

end
