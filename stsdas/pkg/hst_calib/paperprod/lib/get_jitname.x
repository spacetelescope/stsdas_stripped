include <imhdr.h>

define	SZ_EXT	20

pointer procedure get_jitname (rootname, jit_fname)

char	rootname[ARB]
char	jit_fname[ARB]
pointer	jih

char	fname[SZ_FNAME]
char    jih_ext[SZ_EXT]
char    jit_ext[SZ_EXT]
int	i, len


pointer	immap()
int	strlen()
int	access()


begin
	call strcpy ("_jif.fits", jih_ext, SZ_EXT)
        call strcpy ("_jit.fits", jit_ext, SZ_EXT)

        call strcpy (rootname, fname, SZ_FNAME)
        call strcat (jih_ext, fname, SZ_FNAME)

        if (access (fname, 0, 0) == NO) {
        # see if the rootname needs to be changed to have a 'j' at the end
	# WJH 9-Jan-1997

                len = strlen (rootname)
                i = len - 1
  		call strcpy (rootname, fname, i)
                call strcat ("j", fname, SZ_FNAME)

                call strcpy (rootname, jit_fname, i)
		call strcat ("j", jit_fname, SZ_FNAME)

                call strcat (jih_ext, fname, SZ_FNAME)
                
                # if it fails now, change extensions and look for a GEIS file

                if (access (fname, 0, 0) == NO) {
		   call strcpy (".jih", jih_ext, SZ_EXT)
		   call strcpy (".jit", jit_ext, SZ_EXT)
		   call strcpy (rootname, fname, SZ_FNAME)
		   call strcat (jih_ext, fname, SZ_FNAME)

		   if (access (fname, 0, 0) == NO) {
			len = strlen (rootname)
                	i = len - 1
  			call strcpy (rootname, fname, i)
                	call strcat ("j", fname, SZ_FNAME)

                	call strcpy (rootname, jit_fname, i)
			call strcat ("j", jit_fname, SZ_FNAME)
			call strcat(jit_ext, jit_fname, SZ_FNAME)

                	call strcat (jih_ext, fname, SZ_FNAME)

			if (access (fname, 0, 0) == NO) {
				jih = NULL

			} else {
				jih = immap(fname, READ_ONLY, 0)

			}
		   } else {
			jih = immap(fname, READ_ONLY, 0)
			call strcpy(rootname, jit_fname, SZ_FNAME)
			call strcat(jit_ext, jit_fname, SZ_FNAME)
		   }
	        } else {
		# This appends the proper fits extension to the file name 
		# prior to opening the image
 	          call pp_oms(fname)

		  jih = immap (fname, READ_ONLY, 0)
	          call strcat (jit_ext, jit_fname, SZ_FNAME)
		}

        } else { 
		# This appends the proper fits extension to the file name 
		# prior to opening the image
		call pp_oms(fname)

                jih = immap (fname, READ_ONLY, 0)
		call strcpy (rootname, jit_fname, SZ_FNAME)
	        call strcat (jit_ext, jit_fname, SZ_FNAME)
	}

	return(jih)

end
