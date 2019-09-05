include <imhdr.h>
include	<mach.h>

#  psfextr_do - Perform image subsection extraction and copy to a new image
#
#  Description:
#  ------------
#  This is mostly a file housekeeping routine for the psf extraction task.
#  It takes care of input/output files open/close, check for dimensions, read/
#  write data from/to files, allocate memory spaces etc.
#  
#  Date		Author			Description
#  ----		------			-----------
#  20-Jan-1992  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------

procedure psfextr_do (fin, fout, nfin, xsize, ysize, xcenter, ycenter, 
			origin, allflag, dummy)

pointer	fin, fout 		# input: file template pointers
int	nfin 			# input: number of files in the input template
real	xcenter, ycenter	# input: center of the subsection
int	xsize, ysize		# input: dimensions of the subsection
char	origin[SZ_LINE]		# input: origin of the input file
bool	allflag			# input: if entire image will be extracted
char	dummy[SZ_FNAME]		# input: PSF header template
 
int	x0, y0			# lower left corner of the subsection
int	xcorner, ycorner
pointer arrdata 
pointer	ipin, ipout
real	minval, maxval
char	ifile[SZ_FNAME], ofile[SZ_FNAME]
int	nchar, i
pointer sp, ip, iptmpl
	
pointer	immap()
pointer	imgs2r(), imps2r()
int	imtgetim()
#==============================================================================
begin

	# allocate array space
	call smark (sp)

	iptmpl = immap (dummy, READ_ONLY, 0)		

	# loop all input files 
	do i = 1, nfin {

	    # read the next file name in the template list
	    nchar = imtgetim (fin, ifile, SZ_FNAME) 
            nchar = imtgetim (fout, ofile, SZ_FNAME) 

	    # open input file
	    ipin = immap (ifile, READ_ONLY, 0)		

	    # input file must be 2-D data and dimensions larger than
	    # specified range
	    if (IM_NDIM(ipin) != 2)
		call error (1, "input is not 2-D data")

	    # copy the whole input image
	    if (allflag) {
	    	x0 = 1
	    	y0 = 1
	    	xsize = IM_LEN(ipin,1)
	    	ysize = IM_LEN(ipin,2)
	        xcorner = nint (xcenter - real(xsize)/2. + EPSILON)
	    	ycorner = nint (ycenter - real(ysize)/2. + EPSILON)

	    # copy a subsection
	    } else {
	        x0 = nint (xcenter - real(xsize)/2. + EPSILON)
	    	y0 = nint (ycenter - real(ysize)/2. + EPSILON)
		xcorner = x0
		ycorner = y0
	    	if (x0 < 1 || y0 < 1 || IM_LEN(ipin,1) < x0+xsize-1 || 
		    IM_LEN(ipin,2) < y0+ysize-1) {
		    call error (1, 
		    "image subsection is too large or too close to corner/side")
		}
	    }

	    # open output image
	    ipout = immap (ofile, NEW_COPY, iptmpl)	
	    IM_NDIM(ipout) = 2
	    IM_LEN(ipout,1) = xsize
	    IM_LEN(ipout,2) = ysize

	    # read data from the input file and move to the output file
	    # output is always real
	    arrdata = imgs2r (ipin, x0, x0+xsize-1, y0, y0+ysize-1)
	    ip = imps2r (ipout, 1, xsize, 1, ysize)
	    call amovr (Memr[arrdata], Memr[ip], xsize*ysize)
	    call alimr (Memr[arrdata], xsize*ysize, minval, maxval)
		
	    # Add keywords to the output file header
	    call psf_header (ipin, ipout, x0, y0, xcorner, ycorner, 
				xcenter, ycenter, origin, minval, maxval)

	    # close files
	    call imunmap (ipin)
	    call imunmap (ipout)

	    # print out message of which files been created
	    call printf ("psfextr: output file %s is created\n")
	    	call pargstr (ofile)
	}
	call imunmap (iptmpl)
	call sfree (sp)
end
