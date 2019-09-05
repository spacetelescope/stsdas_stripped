#  psfextr_in -- Read parameters for the task psfextr.
#
#  Description:
#  ------------
#  Read CL parameters and do necessary checking and conversions.
#  
#  Input CL parameters:
#  -----------------
#
#  "infile"		Input science data file template name
#  "outfile"		Output science data file template name
#  "xcenter"		X coordinate of the subsection center
#  "ycenter"		Y coordinate of the subsection center
#  "xsize"		X dimension of the subsection
#  "ysize"		Y dimension of the subsection
#  "origin"		origin of the input file
#  "dummy"		PSF files deader template
#
#  Date		Author			Description
#  ----		------			-----------
#  20-Jan-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure psfextr_in (fin, fout, nfin, xsize, ysize, xcenter, ycenter, 
			origin, allflag, dummy)

pointer	fin, fout 		# output: file template pointers
int	nfin			# output: number of input files 
real	xcenter, ycenter	# output: center of the subsection
int	xsize, ysize		# output: dimensions of the subsection
char	origin[SZ_LINE]		# output: origin of the input file
bool	allflag			# output: if entire image will be extracted
char	dummy[SZ_FNAME]		# output: PSF header template

int	nfout

pointer	imtopenp() 
int	imtlen(), clgeti()
real	clgetr()
#==============================================================================
begin

	# open file templates and find out how many files are in the templates
	fin = imtopenp ("infile")
	fout = imtopenp ("outfile")

	nfin = imtlen (fin)
	nfout = imtlen (fout)

	# check input parameters, the following files can not be empty
	if (nfin < 1)
	    call error (1, "blank input file template")
	if (nfout < 1)
	    call error (1, "blank output file template")

	# check the numbers of files in each template are consistent/reasonable
	# the number of output files should be the same as input
	if (nfin != nfout)
	    call error (1, "mismatch no. of files: input and output")

	# read other CL parameters
	xcenter = clgetr ("xcenter")
	ycenter = clgetr ("ycenter")
        xsize = clgeti ("xsize")
        ysize = clgeti ("ysize")
	call clgstr ("origin", origin, SZ_LINE)
	call clgstr ("dummy", dummy, SZ_LINE)

	allflag = (xsize <= 0 || ysize <= 0)
end
