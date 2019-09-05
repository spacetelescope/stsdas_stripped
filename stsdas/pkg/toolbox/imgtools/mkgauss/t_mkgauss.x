# Copyright(c) 1992 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<imhdr.h>
include	<math.h>
include	<mach.h>

# MKGAUSS -- Generate a simple image having a source of Gaussian type plus
# Gaussian white noise.
# Some procedures are in outbx.x

procedure t_mkgauss ()

pointer	sp		# Memory stack pointer
pointer	out_name	# Output image name string pointer

pointer	out_im		# Ouput image descriptor

int	npix, nlin 	# Image size in each dimension
int	narr		# Memory size for the array 
pointer	pta		# Array pointer

real	pos1, pos2	# Position of Gaussian fuction
real	amp		# Amplitude of G. f.
real	sgm1, sgm2	# Sigmas of G. f. 
real	fwhm1, fwhm2	# Full widths at half max of G. f.

real	rms		# Noise (zero-mean) rms
long	seed		# Seed for random numbers

int	line		# Line number
pointer	ptl		# Line pointer

int	fstati(), clgeti()
real	clgetr()
long	clgetl()

pointer	immap(), impl2r()

define	NDIM	2 	# The number of image dimension

begin
	# For properly print messages

	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize the dynamic memory stack

	call smark (sp)
	call salloc (out_name, SZ_FNAME, TY_CHAR)

	# Get output image name

	call clgstr ("outim", Memc[out_name], SZ_FNAME)

	# Get array size in each dim. and source parameters

	npix =  clgeti ("n1")
	nlin =  clgeti ("n2")
	pos1 =  clgetr ("pos1")
	pos2 =  clgetr ("pos2")
	amp =  clgetr ("amp")

	sgm1 =  clgetr ("sigma1")
	if (sgm1 <= EPSILONR) {
	    fwhm1 =  clgetr ("fwhm1")
	    sgm1 = fwhm1 / sqrt (8.0 * log (2.0))
	}
	 
	sgm2 =  clgetr ("sigma2")
	if (sgm2 <= EPSILONR) {
	    fwhm2 =  clgetr ("fwhm2")
	    sgm2 = fwhm2 / sqrt (8.0 * log (2.0))
	} 

	rms =  clgetr ("rms")
	seed =  clgetl ("seed")

	call printf ("seed = %d\n")
	    call pargl (seed)

	# Open output image

  	out_im = immap (Memc[out_name], NEW_IMAGE, 0)

	# Set image header parameters

	IM_NDIM(out_im) = NDIM
	IM_LEN(out_im,1) = npix
	IM_LEN(out_im,2) = nlin
	IM_PIXTYPE(out_im) = TY_REAL 

	# Dynamic memory allocation for the array, in common /Mem/

	narr = npix * nlin
	call malloc (pta, narr, TY_REAL)

	# Generate test data 

	sgm1 = max (sgm1, EPSILONR)
	sgm2 = max (sgm2, EPSILONR)
	call imdata (Memr[pta], npix, nlin, pos1, pos2, amp, sgm1, sgm2, 
	    rms, seed)

	# write	output data to image

	do line = 1, nlin {
	    # Allocate the output image line
	    ptl = impl2r (out_im, line)

	    # Assign values to each line
	    call outr_b (Memr[pta], Memr[ptl], npix, line)
	}

	# Free dynamic memory

	call mfree (pta, TY_REAL)

	# Close the image

	call imunmap (out_im)

	# Free dynamic memory stack

	call sfree (sp)
end

# Generate data for image, a Gaussian function centered at (pos1, pos2).
# Gaussian noise may be added.

procedure imdata (a, n1, n2, pos1, pos2, amp, sgm1, sgm2, rms, seed)

real	a[n1,n2]	# Array to hold data
int	n1, n2 		# Array size in each dim.
real	pos1, pos2	# Position of Gaussian function
real	amp		# Amplitude of Gaussian function
real	sgm1, sgm2	# Sigmas of Gaussian function
real 	rms		# Noise rms
long	seed		# Seed for random numbers

int	i, j
real	gj, r1, r2

real	urand()		# Function to generate random numbers uniformly on (0,1)

begin

	# Generate Gaussian function plus Gaussian noise

	do j = 1, n2 {
	    gj = amp * exp (-0.5 * ((j - pos2) / sgm2) ** 2)
	    do i = 1, n1 {
             	r1 = urand (seed)
             	r2 = urand (seed)
	        a[i,j] = gj * exp (-0.5 * ((i - pos1) / sgm1) ** 2) + 
	            rms * sqrt(-2.0 * log (r1)) * cos (TWOPI * r2)
	    }
	}
end	
