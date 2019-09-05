# COUNTGRP -- Count total number of groups in a list of images
#
# B.Simon	21-Aug-91	Original

int procedure countgrp (list)

pointer	list		# i: image template descriptor
#--
int	total, count
pointer	sp, tp, image

int	imtgetim()
pointer	tp_open()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	total = 0
	call imtrew (list)

	# Retrieve each image name from the image template list
	# Then use the group template routines to determine the 
	# number of groups in each image

	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    tp = tp_open (Memc[image], 0, count)
	    total = total + count
	    call tp_close (tp)
	}

	call imtrew (list)
	call sfree (sp)
	return (total)
end
