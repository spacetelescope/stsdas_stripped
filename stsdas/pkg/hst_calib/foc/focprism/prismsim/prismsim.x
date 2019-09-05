include <imhdr.h>
include	"prismsim.h"

# PRISMSIM -- Simulate FOC Objective Prism spectra and images.
# Written by Dave Bazell.
# Phil Hodge, 29-Oct-1993  Move most cl get calls to this routine.
# Phil Hodge,  1-Jul-1994  Get area from par file.

procedure prismsim()

pointer	sp
pointer spectrum		# name of file containing spectrum
pointer output			# output image name
pointer obsmode			# observation mode string
pointer prism			# keyword for prism name
pointer dispfile		# name of file containing dispersion
real	offset			# offset from undispersed position
real	texp			# exposure time
real	area			# telescope area in sq cm
pointer	pix, counts
int	npix1, npix2		# size of output image
#--
pointer im
pointer lpt

pointer wave_disp		# pointer to array of wavelengths
pointer pix_disp		# pointer to array of pixel locations
real	wstart, wstop, dw
int	n_disp			# size of wave_disp, pix_disp arrays
int	line
pointer	impl2r(), immap()
real	clgetr()
int	clgeti()

begin
	# Allocate Memory
	call smark (sp)

	call salloc (spectrum, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (obsmode, SZ_FNAME, TY_CHAR)
	call salloc (prism, SZ_FNAME, TY_CHAR)
	call salloc (dispfile, SZ_FNAME, TY_CHAR)

	# Read in reference spectrum that is to be made into a prism image
	call clgstr ("spectrum", Memc[spectrum], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("obsmode", Memc[obsmode], SZ_FNAME)

	npix1 = clgeti ("npix1")
	npix2 = clgeti ("npix2")
	if (npix2 > MAXWAVE) {
	    call eprintf ("warning:  npix2 reduced to upper limit of %d\n")
		call pargi (MAXWAVE)
	    npix2 = MAXWAVE
	}
	call salloc (pix, npix2, TY_REAL)	# scratch space for calccounts
	call salloc (counts, npix2, TY_REAL)

	# Get Simulation Parameters
	wstart = clgetr ("wstart")
	wstop = clgetr ("wstop")
	if (wstop < wstart)
	    call error (1, "Ending wavelength less than starting wavelength")
	if (wstop == wstart)
	    call error (1, "Specified wavelength range is zero")
	dw = clgetr ("dw")
	offset = clgetr ("offset")
	texp = clgetr ("texp")
	area = clgetr ("area")

	# Get the prism name from the observation mode string.
	call wh_prism (Memc[obsmode], Memc[prism], SZ_FNAME)
	# Get the name of the dispersion file from the dispfiles pset.
	call clgstr (Memc[prism], Memc[dispfile], SZ_FNAME)
	if (Memc[dispfile] == EOS || Memc[dispfile] == ' ') {
	    call eprintf (
	"must specify file name for parameter `%s' in dispfiles pset\n")
		call pargstr (Memc[prism])
	    call error (1, "")
	}
	# Read in dispersion relation.  c1 and c2 are the column names; i.e.,
	# we're assuming this is a text file.
	call getdisp (Memc[dispfile], "c1", "c2", wave_disp, pix_disp, n_disp)

	# Calculate counts.
	call calccounts (Memc[spectrum], Memc[obsmode], npix2,
		Memr[wave_disp], Memr[pix_disp], n_disp,
		wstart, wstop, dw, offset, texp, area,
		Memr[pix], Memr[counts])

	# Done with dispersion info; deallocate memory.
	call mfree (wave_disp, TY_REAL)
	call mfree (pix_disp, TY_REAL)

	# Open the output image.
	im = immap (Memc[output], NEW_IMAGE, 0)

	IM_NDIM(im) = 2
	IM_LEN(im,1) = npix1
	IM_LEN(im,2) = npix2
	IM_PIXTYPE(im) = TY_REAL

	# Copy spectrum to image
	do line = 1, npix2 {
	    lpt = impl2r (im, line)
	    call aclrr (Memr[lpt], npix1)
	    Memr[lpt+npix1/2] = Memr[counts+line-1]
	}

	# Convolution is done separately.

	# Close image
	call imunmap (im)

	# Free memory
	call sfree (sp)
end
