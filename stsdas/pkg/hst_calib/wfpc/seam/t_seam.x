include	"wmosaic.h"

#  t_seam --  smooth the boundaries between WFPC frames after the mosaic
#
#  Description:
#  ------------
#  
#  Input CL parameters:
#  -----------------
#
#  "infile"     Input file name
#  "outfile"    Output file name
#  "length"	length of the stitch
#  "width"	total number of stitches used for background determination
#  "delta"	discrimination level for "bad" pixels
#  "npbg"	number of pixels used to determine the background in each stitch
#
#  Date		Author			Description
#  ----		------			-----------
#  26-Feb-1993  J.-C. Hsu       Version 1.0: design and coding
#  25-Jun-1993  J.-C. Hsu       Version 1.1: use better border equations
#  28-Apr-2003  J.-C. Hsu       Version 1.2: make it work for FITS file
#------------------------------------------------------------------------------

procedure t_seam()

pointer	ipin
pointer	ipout
char	fin[SZ_FNAME]
char	fout[SZ_FNAME]
char	str_camera[SZ_FNAME]
pointer	pic
real	outbuf[SZ_OUT, SZ_OUT]
int	camera
int	length 		# length of the "stitch" in number of pixels
int	width		# width of the "stitch" in number of pixels
real	delta		# discrimination level for bad pixels
int	npbg		# number of pixels used as background in each row
char    text[SZ_LINE], tstring[SZ_LINE]
	
int	clgeti()
real	clgetr()
bool	streq()
pointer	gf_map()
pointer	imgs2r(), imps2r()
long    clktime()
#==============================================================================
begin

	# read the input parameters
	call clgstr("infile", fin, SZ_FNAME)
	call clgstr("outfile", fout, SZ_FNAME)
        length = clgeti("length")
        width = clgeti("width")
        delta = clgetr("delta")
	npbg = clgeti("npbg")

	# open input file
	ipin = gf_map(fin, READ_ONLY, 0)
	ipout = gf_map(fout, NEW_COPY, ipin)

	iferr (call imgstr(ipin, "CAMERA", str_camera, SZ_FNAME))
	    call error(1, "No camera keyword.  This task only works on WF/PC images.")
	if (streq(str_camera, "PC")) 
	    camera = PC
	else if (streq(str_camera, "WF")) 
	    camera = WF
	else
	    call error(1, "Camera name not recognized.  This task only works on WF/PC images.")

	# read the input data
	pic = imgs2r(ipin, 1, SZ_OUT, 1, SZ_OUT)
	call amovr(Memr[pic], outbuf, SZ_OUT*SZ_OUT)

	# do the smoothing
	call mosaic_seam(camera, outbuf, length, width, delta, npbg)

	# write the result to the output file
	call amovr(outbuf, Memr[imps2r(ipout, 1, SZ_OUT, 1, SZ_OUT)],
			SZ_OUT*SZ_OUT)

        # record the time this task is run and parameters used
        call cnvtime(clktime(0), tstring, SZ_LINE)
        call sprintf(text, SZ_LINE, "Ran task SEAM, version %s, at %s")
            call pargstr(VERSION)
            call pargstr(tstring)
        call gf_iputh(ipout, "HISTORY", text)
        call sprintf(text, SZ_LINE, 
	      "parameters used: length= %d, width=%d, delta=%5.2f, npbg=%d")
            call pargi(length)
            call pargi(width)
            call pargr(delta)
            call pargi(npbg)
        call gf_iputh(ipout, "HISTORY", text)
 
	# close the input/output file
	call gf_unmap(ipin)
	call gf_unmap(ipout)
end
