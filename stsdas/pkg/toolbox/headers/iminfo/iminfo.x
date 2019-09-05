include	<error.h>
include	<imhdr.h>

# IMINFO -- Read selected contents of an image header and print on STDOUT.

procedure iminfo()

int	list
pointer	sp, template, image
int	imtopen(), imtgetim()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (template, SZ_LINE, TY_CHAR)

	call clgstr ("images", Memc[template], SZ_LINE)
	list = imtopen (Memc[template])


	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    iferr (call impinfo (Memc[image]))
		call erract (EA_WARN)
	    call flush (STDOUT)
	}

	call imtclose (list)
end
