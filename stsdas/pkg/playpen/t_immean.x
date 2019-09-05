include <imhdr.h>

# t_immean -- compute mean of an image
# This task computes the mean value of an image and puts the mean value
# back into the immean par file so it can be used by another task.
#
# Phil Hodge, 17-Jan-1991  Original.

procedure t_immean()

char	input[SZ_FNAME]
bool	verbose
#--
pointer im, x
double	sum, mean
long	v[IM_MAXDIM]
int	i
int	naxis1, npix
pointer immap()
int	imgnld()
bool	clgetb()

begin
	call clgstr ("input", input, SZ_FNAME)
	verbose = clgetb ("verbose")

	im = immap (input, READ_ONLY, NULL)

	naxis1 = IM_LEN(im,1)
	npix = naxis1
	do i = 2, IM_MAXDIM
	    npix = npix * IM_LEN(im,i)

	do i = 1, IM_MAXDIM
	    v[i] = 1

	sum = 0.d0
	while (imgnld (im, x, v) != EOF) {
	    do i = 0, naxis1-1
		sum = sum + Memd[x+i]
	}
	call imunmap (im)

	mean = sum / npix
	call clputd ("mean", mean)

	if (verbose) {
	    call printf ("%25.17g\n")
		call pargd (mean)
	}
end
