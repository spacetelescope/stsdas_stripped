###########################################################################
#                    Center for Astrophysical Sciences
#                        Johns Hopkins University


#  Synopsis:	procedure replot (gfd, gt, pix, npts, x1, x2)
#		pointer	gfd
#		pointer	gt
#		real	pix[ARB]
#		real	x1, x2
#		int	npts

#  Description:	REPLOT -- Replot the current array

#  Arguments:	pointer	gfd, gt		Pointers for graphics control
#		real	pix[ARB]	The data array to plot
#		real	x1, x2		Beginning and ending x coordinates
#		int	npts		Number of points in pix

#  Returns:	None

#  Notes:	Adapted from splot in the onedspec IRAF package

#  History:	June	1987	Gerard Kriss

###########################################################################

procedure replot (gfd, gt, pix, npts, x1, x2)

pointer	gfd
pointer	gt
real	pix[ARB]
real	x1, x2
int	npts

begin
	call gclear (gfd)
	call gswind (gfd, x1, x2, INDEF, INDEF)
	call gascale (gfd, pix, npts, 2)
	call gt_swind (gfd, gt)
	call gt_labax (gfd, gt)
	call gvline (gfd, pix, npts, x1, x2)
end
