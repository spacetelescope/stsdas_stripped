include	<imhdr.h>
include	<imio.h>
include <fset.h>		# for checking whether output is redirected
include	<foc.h>

# LISTAREA -- Print section of an image
#
# D. Giaretta, 01-Aug-1987	Original SPP version
# Phil Hodge, 19-June-1990	Replace sig_figs with pformat.
# Phil Hodge, 10-Oct-1990	Get data as double; use nint for %d format.
# Phil Hodge, 17-July-1991	Set sampbeg & linebeg to 1.
# Phil Hodge, 25-July-1991	Don't print form feed; print fewer newlines.

procedure t_listarea()

char	imlist[SZ_LINE]		# image list
char	pformat[SZ_FNAME]	# format to use for printing
int	page_width		# page width available
#--
pointer	sp
pointer	list			# image template list pointer
pointer	foc_coord
int	lenfmt			# length of string pformat
char	image[SZ_FNAME]		# single image name
char	tstring[SZ_FNAME]	# temporary string for adding " " to pformat
pointer	imtopen()
int	imtgetim()
pointer	im, x_foc_immap()
int	clgeti(), envgeti(), fstati()
int	strlen()

begin
	call smark (sp)

	call clgstr ("input", imlist, SZ_LINE)
	call clgstr ("pformat", pformat, SZ_FNAME)

	if (fstati (STDOUT, F_REDIR) == YES)
	    page_width = clgeti ("pagewidth")
	else
	    page_width = envgeti ("ttyncols")

	# Convert the format from Fortran to SPP, if necessary.
	call strlwr (pformat)
	call tbbftp (pformat, pformat)

	# Make sure there's at least one blank at beginning or end of pformat.
	lenfmt = strlen (pformat)
	if (pformat[1] != ' ' && pformat[lenfmt] != ' ') {
	    call strcpy (" ", tstring, SZ_FNAME)
	    call strcat (pformat, tstring, SZ_FNAME)
	    call strcpy (tstring, pformat, SZ_FNAME)
	}

	list = imtopen (imlist)

	# special treatment for FOC coords
	call salloc (foc_coord, SZ_FOC_COORD, TY_STRUCT)
	FOC_COORD_INIT(foc_coord) = false

	while (imtgetim (list, image, SZ_FNAME) != EOF) {
	    im = x_foc_immap (image, foc_coord, READ_ONLY, NULL)
	    call xlist (im, foc_coord, pformat, page_width )

	    call imunmap (im)
	}

	call imtclose (list)
	call sfree (sp)
end

#  XLIST -- prints a section of an image given either the
#  absolute coordinates or coordinates relative to the start
#  of the image. 

procedure xlist (im, foc_coord, pformat, page_width )

pointer	im		# i: input image
pointer	foc_coord	# i: foc coord stuff
char	pformat[ARB]	# i: print format
int	page_width	# i: page width
#--
int	i, j, k, l
int	nppg		# number of pages per one row
int	col1, col2	# 1st and last col being printed in current page
int	ncol		# no. of cols being printed in page

int	nlines, npix, sinc, linc, sampbeg, linebeg, soff1, soff2
int	dtype		# data type of image
char	colformat[SZ_FNAME]
pointer	line
long	v[IM_MAXDIM]
bool	onecol
bool	int_format		# is there a "d" in the print format?
char	d_char, o_char, x_char	# 'd', 'o', 'x'
int	imgnld(), imgnlx()
int	junk, ip, nchar
char	percent_char
int	stridx(), ctoi()

begin
	linebeg = 1		# was FOC_LINEBEG(foc_coord)
	sampbeg = 1		# was FOC_SAMPBEG(foc_coord)
	linc    = FOC_INC2(foc_coord)
	sinc    = FOC_INC1(foc_coord)
	npix    = FOC_NPIX1(foc_coord)
	nlines  = FOC_NPIX2(foc_coord)
	soff1	= int (FOC_SOFF1(foc_coord))
	soff2	= int (FOC_SOFF2(foc_coord))

	dtype = IM_PIXTYPE(im)

	# Set a flag if the print format is integer (or octal or hex)
	# so we will know that we should round the value before printing.
	d_char = 'd'
	o_char = 'o'
	x_char = 'x'
	if ((stridx (d_char, pformat) > 0) ||
	    (stridx (o_char, pformat) > 0) ||
	    (stridx (x_char, pformat) > 0))
	    int_format = true
	else
	    int_format = false

	percent_char = '%'
	ip = stridx (percent_char, pformat) + 1
	junk = ctoi (pformat, ip, nchar)
	nchar = abs (nchar) + 1			# add one for space
	colformat[1] = percent_char
	call sprintf (colformat[2], SZ_FNAME-1, "%dd")
	    call pargi (nchar)

	onecol = IM_NDIM(im) == 1 && npix == nlines
	if (onecol)
	    npix = 1
	
	nppg = max (1, (page_width-7)/nchar)

	for (i=1; i<=((npix+nppg-1)/nppg); i=i+1) {

	    if (i > 1)
		call printf ("\n")
	    call printf ("Image:  %s\n")
		call pargstr (IM_NAME(im))

	    col1 = (i-1)*nppg + 1
	    col2 = min (i*nppg, npix)
	    ncol = col2 - col1 + 1

	    call printf (" Sample")
	    do j = col1, col2 {
		call printf (colformat)
		    call pargi ((j-1)*sinc+sampbeg+soff1)
	    }
	    call printf ("\nLine\n")

	    call amovki (1, v, IM_MAXDIM)
	    
	    if (onecol) {

		if (dtype == TY_COMPLEX)
		    junk = imgnlx (im, line, v)
		else
		    junk = imgnld (im, line, v)

		do k = 1, nlines {
		    call printf ("%7d")
			call pargi ((k-1)*linc+soff2+linebeg)

		    if (dtype == TY_COMPLEX) {
			call printf (pformat)
			    call pargx (Memx[line+k-1])
		    } else if (int_format) {
			call printf (pformat)
			    call pargi (nint (Memd[line+k-1]))	# round off
		    } else {
			call printf (pformat)
			    call pargd (Memd[line+k-1])
		    }

		    call printf ("\n")
		}

	    } else {
	    
		do k = 1, nlines {

		    call printf ("%7d")
			call pargi ((k-1)*linc+soff2+linebeg)

		    if (dtype == TY_COMPLEX)
			junk = imgnlx (im, line, v)
		    else
			junk = imgnld (im, line, v)

		    do l = col1, col2 {
			if (dtype == TY_COMPLEX) {
			    call printf (pformat)
				call pargx (Memx[line+k-1])
			} else if (int_format) {
			    call printf (pformat)
				call pargi (nint (Memd[line+l-1]))	# round
			} else {
			    call printf (pformat)
				call pargd (Memd[line+l-1])
			}
		    }
		    call printf ("\n")
		}
	    }
	}		
end
