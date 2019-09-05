include <imhdr.h>
include	<imset.h>

define	EPS	1.e-15
define	PIX	Memr[opix+($2-1)*sizex+($1-1)]	# output image pixel
define	SIZE_LIST	200			# pixel list size
define	SET_PIX		1			# pixel modifying
define	SUB_PIX		2			# modes

# HSUBTRACT  --  Histogram-based sky background subtraction.
#
# This routine implements the Baade-Lucy histogram-based background
# subtraction algorithm. It is described in: 1st ESO/ST-ECF Data Analysis 
# Workshop, Garching 17-19 April 1989, proceedings edited by P.Grosbol, 
# F.Murtagh and R. Warmels.
#
# The input images are given by an image template list. The output is either 
# a matching list of images or a directory. The number of input images may 
# be either one or match the number of output images. 
#
# 24 Jan 1991  -  I.Busko  -  Task created

procedure t_hsubtract()

char	imlisti[SZ_LINE]			# Input image list
char	bklisti[SZ_LINE]			# Input background  list
char	imlisto[SZ_LINE]			# Output image list/directory
real	hmin					# Minimum
real	hmax					# Maximum
real	binsize					# Histogr. bin size
real	kmax					# Upper cutoff bin
bool	verbose					# Verbose ?

char	image1[SZ_PATHNAME]			# Input image name
char	imageb[SZ_PATHNAME]			# Input backgr. image name
char	image2[SZ_PATHNAME]			# Output image name
char	dirname1[SZ_PATHNAME]			# Directory name
char	dirname2[SZ_PATHNAME]			# Directory name

int	listi, listbk, listo, root_len

int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory()
bool	clgetb()
real	clgetr()

begin
	# Get task parameters
	call clgstr ("input", imlisti, SZ_LINE)
	call clgstr ("background", bklisti, SZ_LINE)
	call clgstr ("output", imlisto, SZ_LINE)
	hmin    = clgetr ("min")
	hmax    = clgetr ("max")
	binsize = clgetr ("binsize")
	kmax    = clgetr ("kmax")
	verbose	= clgetb ("verbose")

	# If the output string is a directory, generate names for
	# the new images accordingly.
	if (isdirectory (imlisto, dirname2, SZ_PATHNAME) > 0) {
	    listi  = imtopen (imlisti)
	    listbk = imtopen (bklisti)
	    if (imtlen (listi) != imtlen (listbk)) {
	        call imtclose (listi)
	        call imtclose (listbk)
	        call error (0, "Number of input and background images not the same")
	    }

	    while ((imtgetim (listi, image1, SZ_PATHNAME) != EOF) && 
		   (imtgetim (listbk, imageb, SZ_PATHNAME) != EOF)) {

		# Place the input image name, without a directory prefix, 
		# in string dirname1.
		root_len = fnldir (image1, dirname1, SZ_PATHNAME)
		call strcpy (image1[root_len + 1], dirname1, SZ_PATHNAME)

		# Assemble output image name.
		call strcpy (dirname2, image2, SZ_PATHNAME)
		call strcat (dirname1, image2, SZ_PATHNAME)

		# Do it.
		call h_back (image1, imageb, image2, hmin, hmax, 
			     binsize, kmax, verbose)
	    }
	    call imtclose (listi)
	    call imtclose (listbk)

	} else {

	    # Expand the input, sky and output image lists.
	    listi  = imtopen (imlisti)
	    listbk = imtopen (bklisti)
	    listo  = imtopen (imlisto)
	    if (imtlen (listi) != imtlen (listo)) {
	        call imtclose (listi)
	        call imtclose (listo)
	        call error (0, "Number of input and output images not the same")
	    } else if (imtlen (listi) != imtlen (listbk)) {
	        call imtclose (listi)
	        call imtclose (listbk)
	        call error (0, "Number of input and background images not the same")
	    }

	    # Do each set of input/output images.
	    while ((imtgetim (listi, image1, SZ_PATHNAME) != EOF) &&
		   (imtgetim (listbk,imageb, SZ_PATHNAME) != EOF) &&
		   (imtgetim (listo, image2, SZ_PATHNAME) != EOF)) {
		call h_back (image1, imageb, image2, hmin, hmax, 
			     binsize, kmax, verbose)
	    }

	    call imtclose (listi)
	    call imtclose (listbk)
	    call imtclose (listo)
	}
end


# H_BACK  -  Does the actual operation in one image.

procedure h_back (input, back, output, hmin, hmax, binsize, kmax, verbose)

char	input[ARB]			# Input image
char	back[ARB]			# Input backgr. image
char	output[ARB]			# Output image
real	hmin				# minimum
real	hmax				# maximum
real	binsize				# histog. bin size
real	kmax				# upper cutoff bin
bool	verbose				# Print the operation

pointer	imin,imbck, imout		# IMIO pointers
pointer	ipix, ipixb, opix		# pixel i/o buffer
pointer	hist, histb			# image and backgr. histograms
pointer	v_prob				# probability vector
pointer	laver, lx, ly			# rank list
int	sizex, sizey			# image section size
int	sizexb, sizeyb			# backgr. section size
int	i,j,k,k1,kmin,l
int	nbins
int	lp, lsize
long	cpu, clock			# time variables
real	hstep, hsum, hsumb, alpha, prob
real	aux, aux2, aver

pointer	immap(), imgs2r(), imps2r()
long	cputime(), clktime()

begin
	# Open input and background images.
	imin  = immap (input, READ_ONLY, 0)
	imbck = immap (back,  READ_ONLY, 0)
	if ((IM_NDIM(imin) != 2) || (IM_NDIM(imbck) != 2)) {
	    call imunmap (imin)
	    call imunmap (imbck)
	    call error (0, "Input image section is not two-dimensional")
	}
	sizex  = IM_LEN(imin,1)
	sizey  = IM_LEN(imin,2)
	sizexb = IM_LEN(imbck,1)
	sizeyb = IM_LEN(imbck,2)

	# Open output image with same header as input image.
	imout = immap (output, NEW_COPY, imin)
	IM_LEN(imout,1) = sizex
	IM_LEN(imout,2) = sizey
	call sprintf (IM_TITLE(imout), SZ_IMTITLE, "Background subtr. %s")
	    call pargstr (input)

	if (verbose) {
	    call printf ("%s -> %s\n")
	        call pargstr (input)
	        call pargstr (output)
	    call flush (STDOUT)
	}

	cpu   = cputime (0)
	clock = clktime (0)

	# Read input and sky images.
	ipix  = imgs2r (imin,  1, sizex,  1, sizey)
	ipixb = imgs2r (imbck, 1, sizexb, 1, sizeyb)

	# Find maxima and minima for histogram construction.
#	These are being input as task parameters.
#	call alimr (Memr[ipix],  sizex *sizey,  hmin, hmax)
#	call alimr (Memr[ipixb], sizexb*sizeyb, bck_min, bck_max)
#	hmin = min (hmin, bck_min)
#	hmax = max (hmax, bck_max)

	# Build histograms. First, alloc space.
	nbins = int ((hmax - hmin) / binsize) + 1
	if (verbose) {
	    call printf ("Building histograms with %d bins...\n")
	        call pargi (nbins)
	    call flush (STDOUT)
	}
	call calloc (hist,    nbins, TY_INT)
	call calloc (histb,   nbins, TY_INT)
	call malloc (v_prob,  nbins, TY_REAL)
	call ahgmr (Memr[ipix],  sizex *sizey,  Memi[hist],  nbins,hmin,hmax)
	call ahgmr (Memr[ipixb], sizexb*sizeyb, Memi[histb], nbins,hmin,hmax)

	# Close background image.
	call imunmap (imbck)

	# Get output buffer, transfer input pixels to it and close input image.
	opix = imps2r (imout, 1, sizex, 1, sizey)
	call amovr (Memr[ipix], Memr[opix], sizex*sizey)
	call imunmap (imin)

	# Compute alpha and generate probability vector.
	hstep = (hmax - hmin) / real (nbins - 1)
	k = int ((kmax - hmin) / hstep)
	hsum = 0.			# accumulate image histogram
	do i = 0, k
	    hsum = hsum + real (Memi[hist+i])
	hsumb = 0.			# accumulate backgr. histogram
	do i = 0, k
	    hsumb = hsumb + real (Memi[histb+i])
	if (hsumb > 0)
	    alpha = hsum / hsumb
	else
	    alpha = 0.
	do i = 0, nbins-1 {
	    aux = real(Memi[hist+i])
	    if (aux > 0.)
	        Memr[v_prob+i] = alpha * real(Memi[histb+i]) / aux
	    else
	        Memr[v_prob+i] = 1.
	}

	# First, zero all pixels definitely below the background.
	do j = 1, sizey {
	    do i = 1, sizex {

	        # Locate pixel bin and its associated probability.
	        k1 = int((PIX(i,j) - hmin) / hstep)

	        # Out-of-bounds can generate floating point errors (IB 7/17/00)
	        if (k1 >=0 && k1 < nbins) {
	            prob = Memr[v_prob + k1]
	            if ((k1 <= int(kmax)) || (prob >= 1.))
	                PIX(i,j) = 0.
	        }
	    }
	}

	# Process each histogram bin. First, create empty list.
	kmin = (kmax - hmin) / hstep + 1
	do k = kmin, nbins-1 {
	    if (verbose) {
	        call printf ("\rProcessing bin %d ... ")
	            call pargi (k)
	        call flush (STDOUT)
	    }
	    call malloc (laver, SIZE_LIST, TY_REAL)
	    call malloc (lx,    SIZE_LIST, TY_INT)
	    call malloc (ly,    SIZE_LIST, TY_INT)
	    lp = -1
	    lsize = SIZE_LIST
	
	    # Look for pixels in bin.
	    do j = 2, sizey-1 {
	        do i = 2, sizex-1 {
	            if (PIX(i,j) > 0.) {
	                # Locate pixel bin.
	                k1 = (PIX(i,j) - hmin) / hstep
	                if (k1 == k) {
	                    # Pixel is in bin; compute average flux
	                    # around it and store in list.
	                    aver = 0.
	                    aver = aver + PIX(i-1,j)
	                    aver = aver + PIX(i+1,j)
	                    aver = aver + PIX(i-1,j-1)
	                    aver = aver + PIX(i,j-1)
	                    aver = aver + PIX(i+1,j-1)
	                    aver = aver + PIX(i-1,j+1)
	                    aver = aver + PIX(i,j+1)
	                    aver = aver + PIX(i+1,j+1)
	                    aver = aver / 8
	                    call h_bump_pointer (lp, lsize, laver, lx, ly)
	                    Memr[laver+lp] = aver
	                    Memi[lx+lp]    = i
	                    Memi[ly+lp]    = j
	                }	            
	            }
	        }
	    }

	    if (lp > 0) {
	        # Zero lowest pixels.
	        k1 = int (alpha * real(Memi[histb+k]))
	        if (k1 > 1) {
	            do l = 1, k1
	                call h_setpix (Memr[laver], Memi[lx], Memi[ly], lp+1, 
					0., SET_PIX, opix, sizex)
	        }

	        # Subtract from highest pixels the expectation value 
	        # for bins 1...k.
	        aux  = 0.
	        aux2 = 0.
	        do l = 0, k {
	            aux  = aux + real(Memi[histb+l]) * (real(l) * hstep + hmin)
	            aux2 = aux + real(Memi[histb+l])
	        }
	        aux = aux / aux2
	        if ((k1+1) < lp) {
	            do l = k1+1, lp
	                call h_setpix (Memr[laver], Memi[lx], Memi[ly], lp+1, 
					aux, SUB_PIX, opix, sizex)
	        }
	    }

	    # Free list space.
	    call mfree (laver, TY_REAL)
	    call mfree (lx, TY_INT)
	    call mfree (ly, TY_INT)
	}

	if (verbose) {
	    call printf ("\n%7.2f CPU seconds,  %5.2f elapsed minutes.\n")
	        call pargr (real (cputime (cpu)) / 1000.)
	        call pargr (real (clktime (clock)) / 60.)
	    call flush (STDOUT)
	}

	# Close output images and dealloc memory.
	call imunmap (imout)
	call mfree (hist, TY_INT)
	call mfree (histb, TY_INT)
	call mfree (v_prob,  TY_REAL)
end


# H_BUMP_POINTER  --  Bump list pointer and eventually realloc list space.

procedure h_bump_pointer (lp, lsize, laver, lx, ly)

int	lp, lsize
pointer	laver, lx, ly

begin
	lp = lp + 1
	if (lp > lsize) {
	    lsize = lsize + SIZE_LIST
	    call realloc (laver, lsize, TY_REAL)
	    call realloc (lx,    lsize, TY_INT)
	    call realloc (ly,    lsize, TY_INT)
	}
end


# H_SETPIX  --  Subtract value from, or set value of, lowest non-INDEF 
# pixel in list. If value=0., zero pixel instead. Also flag the pixel 
# with INDEF.

procedure h_setpix (list, lx, ly, slist, value, flag, opix, sizex)

real	list[ARB]
int	lx[ARB], ly[ARB]
int	slist
real	value
int	flag
pointer	opix
int	sizex

int	i, ilow
real	low

begin
	low = 1./ EPS
	do i = 1, slist {
	    if ((!IS_INDEFR (list[i])) && (list[i] < low)) {
	        low  = list[i]
	        ilow = i
	    }
	}

	switch (flag) {
	case SET_PIX:
	    PIX(lx[ilow], ly[ilow]) = value
	case SUB_PIX:
	    PIX(lx[ilow], ly[ilow]) = PIX(lx[ilow], ly[ilow]) - value
	default:
	}

	list[ilow] = INDEF
end
