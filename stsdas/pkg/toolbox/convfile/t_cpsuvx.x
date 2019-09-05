include	<imhdr.h>
include <error.h>

define SZ_EXTN 3
# T_CPSUVX --  Transfer and convert images (imh, or geis)from a SUN node 
#              to a VAX one. If the iraf network is not up, it can
#	       convert a file on the same node, provide that the pixels
#	       were transfer via FTP (or other means) with binary mode
#
# Nelson Zarate		Sep 89
# Fixed    hhhvax_sun
#          hhhsun_vax
#          imhsun_vax
#          imhvax_sun    Feb 1992 
#			 to convert double precision files. Under
#          some circumtances the transfer did not finished and a panic
#	   was issued.
#          Added for all the convertion routines the 'feed_user' verbose.
#	   
#          5-11-97, RLW.  OIF Format changed to machine independent format
#                         for IRAF 2.11.  Removed OIF portion of code.
#                         STF format still binary dependent.


define SZ_MESSAG 80

procedure t_cpsuvx ()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list
bool	verbose					# Print operations?

char	image1[SZ_PATHNAME]			# Input image name
char	image2[SZ_PATHNAME]			# Output image name
char	dirname1[SZ_PATHNAME], dirname2[SZ_PATHNAME]
char    extn[SZ_EXTN], messag[SZ_MESSAG]
int	list1, list2, root_len, nch, strncmp(), fnextn()

int	imtopen(), imtgetim(), imtlen(), isdirectory(), fnldir()
bool	clgetb()
string  oiffile1 "Conversion of IRAF OIF images not supported."
string  oiffile2 "\n\nConvert these files to FITS using wfits to move to a VAX.\n\n"

begin
	# Get input and output image template lists.

	call clgstr ("input", imtlist1, SZ_LINE)
	call clgstr ("output", imtlist2, SZ_LINE)
	verbose = clgetb ("verbose")

	if (isdirectory (imtlist2, dirname2, SZ_PATHNAME) > 0) {
	    list1 = imtopen (imtlist1)
	    if (imtlen(list1) == 0) { 
	       call printf ("No files selected '%s'\n")
		  call pargstr(imtlist1)
	       call erract(EA_WARN)
	    }
	    while (imtgetim (list1, image1, SZ_PATHNAME) != EOF) {

		# Strip the image section first because fnldir recognizes it
		# as part of a directory.  Place the input image name
		# without a directory or image section in string dirname1.

		call get_root (image1, image2, SZ_PATHNAME)
		root_len = fnldir (image2, dirname1, SZ_PATHNAME)
		call strcpy (image2[root_len + 1], dirname1, SZ_PATHNAME)

		call strcpy (dirname2, image2, SZ_PATHNAME)
		call strcat (dirname1, image2, SZ_PATHNAME)
	        nch = fnextn (image1, extn, SZ_EXTN)
		if (nch == 0) {
		   call sprintf (messag, SZ_MESSAG,
			    "No extension specify for %s")
		       call pargstr(image1)
		   call error (13, messag)
		}
	           
	        if (strncmp (extn, "imh", SZ_EXTN) == 0) {
# 		   iferr (call imhsun_vax (image1, image2, verbose))
            		call eprintf (oiffile2)
			call error(0, oiffile1)
                        break
	 	} else {
	           iferr (call hhhsun_vax (image1, image2, verbose))
                         break
	        }
	    }
	    call imtclose (list1)

	} else {
	    # Expand the input and output image lists.

	    list1 = imtopen (imtlist1)
	    list2 = imtopen (imtlist2)

	    if (imtlen (list1) != imtlen (list2)) {
		if (imtlen(list1) == 1)
	        call imtclose (list1)
	        call imtclose (list2)
	      call error (0, "The input or output specification is not correct")
	    } else 

	       # Do each set of input/output images.

	       while ((imtgetim (list1, image1, SZ_PATHNAME) != EOF) &&
		   (imtgetim (list2, image2, SZ_PATHNAME) != EOF)) {
	           nch = fnextn (image1, extn, SZ_EXTN)
		   if (nch == 0) {
		      call sprintf (messag, SZ_MESSAG,
			    "No extension specify for %s")
		           call pargstr(image1)
		       call error (13, messag)
		   }
	              
	           if (strncmp (extn, "imh", SZ_EXTN) == 0) {
# 		      iferr (call imhsun_vax (image1, image2, verbose))
            		call eprintf (oiffile2)
			call error(0, oiffile1)
                        break
	 	   } else {
	              iferr (call hhhsun_vax (image1, image2, verbose))
			    break
	           }
	       }

	    call imtclose (list1)
	    call imtclose (list2)
	}
	if (verbose) call printf("\n")
end
