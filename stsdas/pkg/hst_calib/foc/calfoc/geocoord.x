include <imhdr.h>
include <math.h>

# geo_coord -- correct the coordinate parameters
# This routine modifies the coordinate parameters of the geometrically
# corrected image depending on the linear part of the distortion averaged
# over the middle portion of the image.  This is appropriate for normal
# image mode, not long-slit spectrographic mode.  The geometric correction
# is applied to CRPIX, the CD matrix and ORIENTAT in the geometrically
# corrected image.
#
# Phil Hodge, 26-Oct-1990  Subroutine created.
# Phil Hodge, 13-May-1991  Also update ORIENTAT.
# Phil Hodge,  9-Aug-1991  Move geo_get_val to geofindcrpix.x; include option
#			to get coord parameters from the geo correction file;
#			pass naxis1 & naxis2 to geo_der & geo_find_crpix.
# Phil Hodge,  7-Apr-1994  Spectrographic mode moved to geo_scale.

procedure geo_coord (g_im, geopt)

pointer g_im			# i: imhdr pointer for corrected image
pointer geopt			# i: imhdr pointer for geo correction file
#--
real	ocrpix[2]		# old values of crpix
real	ncrpix[2]		# new values of crpix
real	ocd[2,2]		# old CD matrix
real	ncd[2,2]		# new CD matrix
real	o_n[2,2]		# derivatives of old coords wrt new coords
real	orientat		# position angle of Y axis, eastward from north
int	naxis1, naxis2		# size of g_im
int	i, j, k
bool	found			# was ocrpix found in geo correction file?
real	imgetr()
errchk	imgetr

begin
	# Get "old" coordinate paramters, i.e. same as in uncorrected image.
	ocrpix[1] = imgetr (g_im, "crpix1")
	ocrpix[2] = imgetr (g_im, "crpix2")
	ocd[1,1] = imgetr (g_im, "cd1_1")
	ocd[1,2] = imgetr (g_im, "cd1_2")
	ocd[2,1] = imgetr (g_im, "cd2_1")
	ocd[2,2] = imgetr (g_im, "cd2_2")

	naxis1 = IM_LEN(g_im,1)
	naxis2 = IM_LEN(g_im,2)

	# Find ocrpix in geo correction file.  That file contains coordinates
	# in uncorrected image at pixel corners of corrected image.
	call geo_find_crpix (geopt, naxis1, naxis2, ocrpix, ncrpix, found)

	# It may be that ocrpix is outside the image, in which case
	# found will be set to false.
	if (!found)
	    call logmsg ("info (geo_coord):  reference pixel is outside image")

	# Compute derivatives.
	call geo_der (geopt, naxis1, naxis2, o_n)

	# Multiply CD matrix by derivatives of old pixel coordinates
	# with respect to new coords.
	do j = 1, 2
	    do i = 1, 2
		ncd[i,j] = 0.
	do j = 1, 2
	    do i = 1, 2
		do k = 1, 2
		    ncd[i,j] = ncd[i,j] + ocd[i,k] * o_n[k,j]

	# tan (orientat) = l / m = cd1_2 / cd2_2
	orientat = atan2 (ncd[1,2], ncd[2,2])
	orientat = RADTODEG (orientat)
	if (orientat < 0.)
	    orientat = orientat + 360.

	# Put values back into g_im.
	if (found) {
	    call imputr (g_im, "crpix1", ncrpix[1])
	    call imputr (g_im, "crpix2", ncrpix[2])
	}
	call imputr (g_im, "cd1_1", ncd[1,1])
	call imputr (g_im, "cd1_2", ncd[1,2])
	call imputr (g_im, "cd2_1", ncd[2,1])
	call imputr (g_im, "cd2_2", ncd[2,2])
	call imaddr (g_im, "orientat", orientat)
end
