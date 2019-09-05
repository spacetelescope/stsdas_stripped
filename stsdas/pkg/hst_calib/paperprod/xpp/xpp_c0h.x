include	"xpp.h"
# This function opens the FOC image which has been processed the
# most in the pipeline.  

pointer procedure xpp_c0h (root, img_name, d0h_ext, c0h_ext, c1h_ext, imtype, img_ext)

char    root[SZ_FNAME]
char    c0h_ext[SZ_EXT]
char    c1h_ext[SZ_EXT]
char    d0h_ext[SZ_EXT]
char	img_name[SZ_FNAME]
char	img_ext[SZ_EXT]
char	imtype[SZ_FNAME]

pointer	c0h
char    fname[SZ_FNAME]

bool	streq()
int	access()
pointer	immap()
begin

	call strcpy (root, fname, SZ_FNAME)
	call strcat (c1h_ext, fname, SZ_FNAME)

	call strcpy (c1h_ext, img_ext, SZ_EXT)
	if (access (fname, 0, 0) == NO) {
	    # Try to find a c0h image to work from
		call strcpy (root, fname, SZ_FNAME)
		call strcat (c0h_ext, fname, SZ_FNAME)

		call strcpy (c0h_ext, img_ext, SZ_EXT)
		if (access (fname, 0, 0) == NO) {
	    	    call strcpy (root, fname, SZ_FNAME)
	    	    call strcat (d0h_ext, fname, SZ_FNAME)
		    call strcpy (d0h_ext, img_ext, SZ_EXT)
		    if (access (fname, 0, 0) == NO) {
			# if there is no c1h, c0h, or d0h image, exit
			c0h = NULL
			call strcpy("", img_name, SZ_FNAME)
			call error(0,"PPLIST: No FOC Image to process...")
		    } else {
			# found a c0h image, so we use it...
			if(streq(imtype,"fits") )
				call strcat("[0]", fname, SZ_FNAME)
			call strcpy(fname, img_name, SZ_FNAME)
			c0h = immap (fname, READ_ONLY, 0)
		    }
		} else {
			# otherwise, use the c0h image
			# found a c0h image, so we use it...
			if(streq(imtype,"fits") )
				call strcat("[0]", fname, SZ_FNAME)
			call strcpy(fname, img_name, SZ_FNAME)
			c0h = immap (fname, READ_ONLY, 0)
		}
	} else {
		# found a c1h image, so we use it...

		# found a c0h image, so we use it...
		if(streq(imtype,"fits") )
			call strcat("[0]", fname, SZ_FNAME)
		call strcpy (fname, img_name, SZ_FNAME)
		c0h = immap (fname, READ_ONLY, 0)

	}


	return(c0h)
end
