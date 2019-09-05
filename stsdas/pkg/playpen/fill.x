include	<imhdr.h>
include	<imset.h>

define	EPS	1.e-10
define	PIX	Memr[$1+($3-1)*sizex+($2-1)]	# 2-d array element

# FILL -- Non-linear sky smoother.
#
# This routine implements the Baade-Lucy method of filling "pits"
# (pixels very below the average background) by local flux reshuffling.
# The algorithm is described in: 1st ESO/ST-ECF Data Analysis Workshop, 
# Garching 17-19 April 1989, proceedings edited by P.Grosbol, F.Murtagh 
# and R. Warmels.
#
# The input images are given by an image template list. The output is either 
# a matching list of images or a directory. The number of input images may 
# be either one or match the number of output images. 
#
# 18 Jan 1991  -  I.Busko  -  Task created

procedure t_fill()

char	imtlist1[SZ_LINE]			# Input image list
char	imtlist2[SZ_LINE]			# Output image list/directory
int	size					# Filtering size
real	threshold				# Treshold
int	niter					# Iterations
bool	verbose					# Verbose ?

char	image1[SZ_PATHNAME]			# Input image name
char	image2[SZ_PATHNAME]			# Output image name
char	dirname1[SZ_PATHNAME]			# Directory name
char	dirname2[SZ_PATHNAME]			# Directory name

int	list1, list2, root_len

int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory(), clgeti()
bool	clgetb()
real	clgetr()

begin
	# Get task parameters
	call clgstr ("input", imtlist1, SZ_LINE)
	call clgstr ("output", imtlist2, SZ_LINE)
	size	  = clgeti ("size")
	threshold = clgetr ("threshold")
	niter     = clgeti ("niter")
	verbose	  = clgetb ("verbose")

	# If the output string is a directory, generate names for
	# the new images accordingly.
	if (isdirectory (imtlist2, dirname2, SZ_PATHNAME) > 0) {
	    list1 = imtopen (imtlist1)
	    while (imtgetim (list1, image1, SZ_PATHNAME) != EOF) {

		# Place the input image name, without a directory prefix, 
		# in string dirname1.
		root_len = fnldir (image1, dirname1, SZ_PATHNAME)
		call strcpy (image1[root_len + 1], dirname1, SZ_PATHNAME)

		# Assemble output image name.
		call strcpy (dirname2, image2, SZ_PATHNAME)
		call strcat (dirname1, image2, SZ_PATHNAME)

		# Do it.
		call fill_image (image1, image2, size, threshold, niter, verbose)
	    }
	    call imtclose (list1)

	} else {

	    # Expand the input and output image lists.
	    list1 = imtopen (imtlist1)
	    list2 = imtopen (imtlist2)
	    if (imtlen (list1) != imtlen (list2)) {
	        call imtclose (list1)
	        call imtclose (list2)
	        call error (0, "Number of input and output images not the same")
	    }

	    # Do each set of input/output images.
	    while ((imtgetim (list1, image1, SZ_PATHNAME) != EOF) &&
		   (imtgetim (list2, image2, SZ_PATHNAME) != EOF)) {
		call fill_image (image1, image2, size, threshold, niter, verbose)
	    }

	    call imtclose (list1)
	    call imtclose (list2)
	}
end


# FILL_IMAGE  -  Does the actual operation in one image.

procedure fill_image (input, output, size, threshold, niter,verbose)

char	input[ARB]			# Input image
char	output[ARB]			# Output image
int	size				# Filtering box size
real	threshold			# Treshold
int	niter				# Iterations
bool	verbose				# Print the operation

pointer	imin,imout			# IMIO pointers
pointer	ipix, opix			# pixel i/o buffer
pointer	sp
pointer	don,i2t,j2t			# donation table
int	sizex, sizey			# image size
int	i1,j1,i2,j2,i3,j3,it,i,j
int	l1_i2, l2_i2
int	l1_j2, l2_j2
int	l1_i3, l2_i3
int	l1_j3, l2_j3
int	iter, size2
int	ndonor, rcount
real	donation, min_donation

pointer	immap(), imgs2r(), imps2r()

begin
	# Alloc area for donation table.
	call smark (sp)
	i = size + 1
	i = i * i
	call salloc (don, i, TY_REAL)
	call salloc (i2t, i, TY_INT)
	call salloc (j2t, i, TY_INT)
	size2 = size / 2

	# Open input image. No boundary extension is used, to enforce
	# strict flux conservation.
	imin = immap (input, READ_ONLY, 0)
	if (IM_NDIM(imin) != 2) {
	    call imunmap (imin)
	    call error (0, "Input image section is not two-dimensional")
	}
	sizex = IM_LEN(imin,1)
	sizey = IM_LEN(imin,2)

	# Open output image with same header as input image.
	imout = immap (output, NEW_COPY, imin)
	IM_LEN(imout,1) = sizex
	IM_LEN(imout,2) = sizey
	call sprintf (IM_TITLE(imout), SZ_IMTITLE, "Background filled %s")
	    call pargstr (input)

	if (verbose) {
	    call printf ("%s -> %s\n")
	        call pargstr (input)
	        call pargstr (output)
	}

	# Read input image, copy it to output buffer and close it.
	ipix = imgs2r (imin,  1, sizex, 1, sizey)
	opix = imps2r (imout, 1, sizex, 1, sizey)
	call amovr (Memr[ipix], Memr[opix], sizex*sizey)
	call imunmap (imin)

	# Iteration loop.
	do iter = 1, niter {
	    if (verbose) {
	        call printf ("\rIteration: %d ")
	            call pargi (iter)
	        call flush (STDOUT)
	    }

	    # Main algorithm loop: looks for recipients.
	    do j1 = 1, sizey {
	        do i1 = 1, sizex {
	            if (PIX(opix,i1,j1) < threshold) {

	                # Found a recipient: look for donors around it.
	                ndonor = -1
	                l1_j2 = max (1,     j1-size2)	# Limits of square box
	                l2_j2 = min (sizey, j1+size2)	# around recipient.
	                l1_i2 = max (1,     i1-size2)
	                l2_i2 = min (sizex, i1+size2)
	                do j2 = l1_j2, l2_j2 {
	                    do i2 = l1_i2, l2_i2 {
	                        if (PIX(opix,i2,j2) > threshold) {

	                            # Found a donor: count its recipients.
	                            rcount = 0
	                            l1_j3 = max (1,     j2-size2)   # Limits of
	                            l2_j3 = min (sizey, j2+size2)   # square box
	                            l1_i3 = max (1,     i2-size2)   # around 
	                            l2_i3 = min (sizex, i2+size2)   # donor.
	                            do j3 = l1_j3, l2_j3 {
	                                do i3 = l1_i3, l2_i3 {
	                                    if (PIX(opix,i3,j3) < threshold) 
	                                        rcount = rcount + 1
	                                }
	                            }
	                            # Compute donation and store in table.
	                            donation = PIX(opix,i2,j2) - threshold
	                            donation = donation / rcount
	                            ndonor = ndonor + 1
	                            Memr[don+ndonor] = donation
	                            Memi[i2t+ndonor] = i2
	                            Memi[j2t+ndonor] = j2
	                        }
	                    }
	                }
	                if (ndonor >= 0) {
	                    # Pick up smallest donation from table.
	                    min_donation = 1./EPS
	                    do it = 0, ndonor {
	                        if (Memr[don+it] < min_donation)
	                            min_donation = Memr[don+it]
	                    }
	                    # Subtract it from each donor.
	                    do it = 0, ndonor {
	                        i = Memi[i2t+it]
	                        j = Memi[j2t+it]
	                        PIX(opix,i,j) = PIX(opix,i,j) - min_donation
	                    }
		            # Credit all donations to recipient at i1,j1.
	                    PIX(opix,i1,j1) = PIX(opix,i1,j1) + 
						(min_donation * (ndonor + 1))
	                }
	            }
	        }
	    }
	}
	if (verbose) {
	    call printf ("\n")
	}

	# Close output images and dealloc memory.
	call imunmap (imout)
	call sfree (sp)
end
