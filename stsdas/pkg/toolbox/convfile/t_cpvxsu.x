include	<imhdr.h>
include <error.h>

define  SZ_EXTN 3
define  SZ_MESSAG 80
# T_CPVXSU -- Procedure to read a template for matching filenames
# on a local node and send the ieee data types images to a remote
# node using IRAF networking. The output files can also reside in the
# local machine for later transfer using FTP for example.
#
# Nelson Zarate		Aug 89
#
# Fixed    hhhvax_sun
#          hhhsun_vax
#          imhsun_vax
#          imhvax_sun    Feb 1992 
#			 to convert double precision files. Under
#          t_cpvxsu    Move clgstr("pixdir"...) to the imh block in this
#		       routine, where it is relevant. It was confusing when
#		       prompting the user on a GEIS (hhh) file.  Nov 18 1992.
#	
#          some circumtances the transfer did not finished and a panic
#	   was issued.
#          Added for all the convertion routines the 'feed_user' verbose.
#	   
#		   5-11-97, RLW.  OIF Format changed to machine independent format
#						  for IRAF 2.11.  Removed OIF portion of code.
#						  STF format still binary dependent.

procedure t_cpvxsu ()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list
bool	verbose					# Print operations?

char	image1[SZ_PATHNAME]			# Input image name
char	image2[SZ_PATHNAME]			# Output image name
char	dirname1[SZ_PATHNAME], dirname2[SZ_PATHNAME]
char	extn[SZ_EXTN]
char    temp[SZ_PATHNAME]
pointer list1, list2, imtopen()
int	root_len, nch

int	imtgetim(), imtlen(), isdirectory(), fnldir(), fnextn(), strncmp()
bool	clgetb()
string  oiffile1 "Conversion of IRAF OIF images not supported."
string  oiffile2 "\n\nConvert these files to FITS using wfits to move to a SUN. \n\n"

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

		# Do not allow image sections in this application

		call imgsection (image1, temp, SZ_PATHNAME)
	        if (temp[1] != EOS) {
		    call eprintf ("ERROR: No section specification allow: %s\n")
			   call pargstr(image1)
		    next 
		} else {
		    call get_root (image1, image2, SZ_PATHNAME)
		    root_len = fnldir (image2, dirname1, SZ_PATHNAME)
		    call strcpy (image2[root_len + 1], dirname1, SZ_PATHNAME)

		    call strcpy (dirname2, image2, SZ_PATHNAME)
		    call strcat (dirname1, image2, SZ_PATHNAME)

  	            nch = fnextn (image1, extn, SZ_EXTN)
		    if (nch == 0) {
		       call sprintf (temp, SZ_PATHNAME,
				 "No extension specify for %s")
		           call pargstr(image1)
		       call error (13, temp)
		    }
	           
	            if (strncmp (extn, "imh", SZ_EXTN) == 0) {
#	               call clgstr ("pixdir", pixdir, SZ_FNAME)
# 		       iferr (call imhvax_sun (image1, image2, pixdir, 
#                              verbose))
                          call eprintf (oiffile2)
                          call error(0, oiffile1)
			  break
	 	    } else {
	               iferr (call hhhvax_sun (image1, image2, verbose))
                          break
		    }
		}
	    }
	    call imtclose (list1)

	} else {
	    # Expand the input and output image lists.

	    list1 = imtopen (imtlist1)
	    list2 = imtopen (imtlist2)

	    if (imtlen (list1) != imtlen (list2)) {
	        call imtclose (list1)
	        call imtclose (list2)
	        call error(0,"The input or output specification is not correct")
	    } else

	       # Do each set of input/output images.

	       while ((imtgetim (list1, image1, SZ_PATHNAME) != EOF) &&
		   (imtgetim (list2, image2, SZ_PATHNAME) != EOF)) {

		   call imgsection (image1, temp, SZ_PATHNAME)
	           if (temp[1] != EOS) {
		       call eprintf ("ERROR: No section specification allow: %s\n")
			      call pargstr(image1)
		       next
		   } else {
	              nch = fnextn (image1, extn, SZ_EXTN)
		      if (nch == 0) {
		         call sprintf (temp, SZ_PATHNAME,
				 "No extension specify for %s")
		             call pargstr(image1)
		         call error (13, temp)
		      }
	           
	              if (strncmp (extn, "imh", SZ_EXTN) == 0) {
#  		         iferr (call imhvax_sun (image1, image2, pixdir,
#			       verbose))
                    	    call eprintf (oiffile2)
                            call error(0, oiffile1)
 			    break
	 	      } else {
	                 iferr (call hhhvax_sun (image1, image2, verbose))
		 	    break
		      }
		   }
	       }

	    call imtclose (list1)
	    call imtclose (list2)
	}
	if (verbose) call printf("\n")
end
