#  t_psfextr - Extract a subsection of an image to be used in the PSF library
#
#  	"Ye spread and span like the catholic man who hath mightily won
#	 God out of knowledge and good out of infinite pain"
#							- Sidney Lanier
#
#  Description:
#  ------------
#  This task is simply copy a subsection (or the whole image) of an input image
#  to a new image with a new set of header keywords.  The output image is 
#  to be stored in a PSF library.
#  
#  Date		Author			Description
#  ----		------			-----------
#  20-Jan-1992  J.-C. Hsu		design and coding
# ------------------------------------------------------------------------------

procedure t_psfextr ()

pointer	fin, fout 		# file template pointers
int	nfin			# number of input files 
real	xcenter, ycenter	# center of the subsection
int	xsize, ysize		# dimensions of the subsection
char	origin[SZ_LINE]		# origin of the input file
bool	allflag			# if entire image will be extracted
char	dummy[SZ_FNAME]		# PSF header template
#==============================================================================
begin

	# announce start of the task
	call printf ("*** PSFEXTR - Version 1.0 ***\n")

	# get CL parameters and related quantities
	call psfextr_in (fin, fout, nfin, xsize, ysize, xcenter, ycenter, 
			origin, allflag, dummy)
 
	# do the extraction
	call psfextr_do (fin, fout, nfin, xsize, ysize, xcenter, ycenter, 
			origin, allflag, dummy)

	# close file templates
	call imtclose (fin)
	call imtclose (fout)

	# announce completion of task
	call printf ("PSF extraction task completed with no error\n")
end
