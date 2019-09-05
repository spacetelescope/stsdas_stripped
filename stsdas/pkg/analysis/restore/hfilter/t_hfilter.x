include	<imset.h>
include	<imhdr.h>

define	S_CLIP		2.5			# background sigma clipping
define	N_CLIP		3			# no. of rejection cicles

define	STORAGE_TYPES	"|memory|disk"
define	STOR_MEM	1
define	STOR_DISK	2

define	H_TYPE		TY_REAL			# H-transform storage type
define	HT_TYPE		real
define	HT		Memr[ht+$1-1]			     # H vector addr.
define	HX		Memr[ht+$1+(($4)-1)*($2)*3+($3-1)*3] # H 2-dim addr.
define	HY		Memr[ht+$1+(($4)-1)*($2)*3+($3-1)*3+1]
define	HC		Memr[ht+$1+(($4)-1)*($2)*3+($3-1)*3+2]

define	SCAT		Memr[scatter+($1-1)*3 +($2-1)]	# scatter array 
define	S		Memd[s       +($1-1)*3 +($2-1)]	# elements
define	S2		Memd[s2      +($1-1)*3 +($2-1)]
define	N		Memi[n       +($1-1)*3 +($2-1)]
define	ARR		Memr[arr     +($2-1)*$3+($1-1)]

define	AR	Memr[$1+($3-1)*blksize+($2)-1]		# real array element
define	AR1	Memr[$1+($3-1)*(blksize+1)+($2)-1]	# real array element
define	AX	Memx[$1+($3-1)*blksize+($2)-1]		# complex array element

define	FORWARD		 1				# sense of H-transform
define	BACKWARD	-1

# Number of H-transform coefficients as a function of order.
define NCOEFF		int (((2.**($1))**2)*3./4.)



# HFILTER  --  H-transform adaptive filter.
#
#
# 15 Apr 91	I. Busko  -  Task created.
# 06 Aug 91	   "	  -  Threshold filter gain.
# 05 Sep 91	   "	  -  Spectral filter.
# 06 Dez 91	   "	  -  Iterative rejection in sky section.
# 15 Jan 92        "      -  Sky section taken from input.

procedure t_hfilter()

char	imlisti[SZ_LINE]		# input image list
char	imlisto[SZ_LINE]		# output image list/directory
char	sky[SZ_LINE]			# sky section
int	blk				# block size for H-transform
int	storage				# storage type
real	threshold			# threshold filter parameter
real	gain				# filter gain
real	spfactor			# spectral filter factor
bool	tfil				# use threshold filter ?
bool	spfilter			# use spectral filter ?
bool	verb

int	iaux
real	aux

char	input[SZ_PATHNAME]		# input image name
char	output[SZ_PATHNAME]		# output image name
char	dirnamei[SZ_PATHNAME]		# directory name
char	dirnameo[SZ_PATHNAME]		# directory name
char	str[SZ_LINE]
int	listi, listo, root_len
int	strdic()
int	imtopen(), imtgetim(), imtlen()
int	fnldir(), isdirectory(), clgeti()
real	clgetr(), h_log2()
bool	clgetb()

begin
	# Input parameters.
	call clgstr ("input", imlisti, SZ_LINE)
	call clgstr ("output", imlisto, SZ_LINE)
	blk  = clgeti ("blksize")
	aux  = h_log2 (real(blk))
	iaux = int (aux)
	aux  = aux - real(iaux)
	if (aux > 0.)
	    call error (0, "blksize is not integer power of two")
	call clgstr ("storage", str, SZ_LINE)
	storage = strdic (str, str, SZ_LINE, STORAGE_TYPES)
	if (storage == 0)
	    storage = STOR_MEM
	tfil = clgetb ("thfilter")
	if (tfil) {
	    threshold = clgetr ("thresh")
	    gain      = clgetr ("gain")
	    call clgstr ("skysec", sky, SZ_LINE)
	}
	spfilter = clgetb ("spfilter")
	if (spfilter)
	    spfactor = clgetr ("spfactor")
	verb = clgetb ("verbose")

	# If the output string is a directory, generate names for
	# the new images accordingly.

	if (isdirectory (imlisto, dirnameo, SZ_PATHNAME) > 0) {
	    listi = imtopen (imlisti)
	    while (imtgetim (listi, input, SZ_PATHNAME) != EOF) {

		# Place the input image name, without a directory prefix, 
		# in string dirnamei.
		root_len = fnldir (input, dirnamei, SZ_PATHNAME)
		call strcpy (input[root_len + 1], dirnamei, SZ_PATHNAME)

		# Assemble output image name.
		call strcpy (dirnameo, output, SZ_PATHNAME)
		call strcat (dirnamei, output, SZ_PATHNAME)

		# Do it.
	        call h_filt (input, output, storage, blk, tfil, sky, 
	                       threshold, gain, spfilter, spfactor, verb) 
	    }
	    call imtclose (listi)

	} else {

	    # Expand the input and output image lists.
	    listi = imtopen (imlisti)
	    listo = imtopen (imlisto)
	    if (imtlen (listi) != imtlen (listo)) {
	        call imtclose (listi)
	        call imtclose (listo)
	        call error (0, "Number of input and output images not the same")
	    }

	    # Do each set of input/output images.
	    while ((imtgetim (listi, input,  SZ_PATHNAME) != EOF) &&
		   (imtgetim (listo, output, SZ_PATHNAME) != EOF)) {
	        call h_filt (input, output, storage, blk, tfil, sky,
	                       threshold, gain, spfilter, spfactor, verb)

	    }

 	    call imtclose (listi)
	    call imtclose (listo)
	}
end



# H_FILT --  Main routine. Filter one image in H-transform space.

procedure h_filt (input, output, storage, blksize, tfil, back, thresh,
                    gain, spfilter, spfactor, verb)

char	input[ARB]			# input image
char	output[ARB]			# output image
char	back[ARB]			# background section
int	storage				# storage
int	blksize				# block size
real	thresh				# threshold parameter
real	gain				# filter gain
real	spfactor			# spectral filter factor
bool	tfil				# use threshold filter ?
bool	spfilter			# use spectral filter ?
bool	verb

pointer	imin, imback, imout		# IMIO pointers
pointer	im, ib, io			# image buffers
pointer	it, tpi, tpo, tpb		# temporary storage
pointer	scatter				# scatter list
pointer	s, s2, n			# scatter quantities
pointer	ht				# H-transform
int	isizex,isizey			# input image size
int	bsizex,bsizey			# input background section size
int	order				# order being operated on
int	norder				# maximum order
int	ncoeff				# no. of coefficients
int	nblx, nbly			# no. of blocks in each axis
int	size,size1			# size of 2-dim order in transform
int	offset, offset1			# offset in H-transform vector
int	cicle				# cicle of sky rejection
int	i, j, iblx, ibly, ibla 
int	i1, i2, j1, j2
int	i1old,i2old,j1old,j2old
long	cpu, clock			# time variables
HT_TYPE	h1, h21, h22, h23, h24		# coeffs. for spectral filter
HT_TYPE	hax, hay, hac
HT_TYPE	haux, hsg, htg

pointer	immap(), imgs2r(), imps2r() 
pointer	h_transform()
real	h_log2()
int	h_off()
long	cputime(), clktime()
bool	streq()

begin
	if (verb) {
	    call printf ("%s -> %s      ")
	        call pargstr (input)
	        call pargstr (output)
	    call flush (STDOUT)
	}

	norder = h_log2 (real(blksize))		# number of orders
	cpu   = cputime (0)
	clock = clktime (0)
	im = NULL
	io = NULL
	ib = NULL

	# Open input image and get its size. Boundary extension
	# is used to enforce an integer number of blocks in array.
	imin = immap (input, READ_ONLY, 0)
	if (IM_NDIM(imin) != 2)
	    call error (0, "Input image section is not 2-dimensional.")
	call imseti (imin, IM_TYBNDRY, BT_REFLECT) 
	call imseti (imin, IM_NBNDRYPIX, blksize)
	isizex = IM_LEN(imin,1)
	isizey = IM_LEN(imin,2)
	if (mod(isizex, blksize) != 0)
	    isizex = blksize * (isizex / blksize + 1)
	if (mod(isizey, blksize) != 0)
	    isizey = blksize * (isizey / blksize + 1)
	# If memory-intensive, read full data
	if (storage == STOR_MEM)
	    tpi = imgs2r (imin, 1, isizex, 1, isizey)

	# Open output image with same header as input image.
	# Its size, however, will be an integer multiple of
	# blksize, regardless of the input image size. The extra
	# lines/columns come from boundary extension in
	# the input image.
	imout = immap (output, NEW_COPY, imin)
	IM_LEN(imout,1) = isizex
	IM_LEN(imout,2) = isizey
	call sprintf (IM_TITLE(imout), SZ_IMTITLE, "H-filter of %s")
	    call pargstr (input)
	# If memory-intensive, open buffer for full data
	if (storage == STOR_MEM)
	    tpo = imps2r (imout, 1, isizex, 1, isizey)

	# Initial procedure for threshold filter: read sky section,
	# compute its H-transform, and build scatter estimates for 
	# each order.

	if (tfil) {

	    if (streq (back, "input")) {
	        # Background section is input image itself.
	        imback = imin
	        bsizex = isizex
	        bsizey = isizey 
	        if (storage == STOR_MEM)
	            tpb = tpi
	    } else {
	        # Read background section from its file.
	        imback = immap (back, READ_ONLY, 0)
	        if (IM_NDIM(imin) != 2)
	            call error (0, "Background section is not 2-dimensional.")
	        call imseti (imback, IM_TYBNDRY, BT_REFLECT) 
	        call imseti (imback, IM_NBNDRYPIX, blksize)
	        bsizex = IM_LEN(imback,1)
	        bsizey = IM_LEN(imback,2)
	        if (mod(bsizex, blksize) != 0)
	            bsizex = blksize * (bsizex / blksize + 1)
	        if (mod(bsizey, blksize) != 0)
	            bsizey = blksize * (bsizey / blksize + 1)
	        # If memory-intensive, open buffer for full data
	        if (storage == STOR_MEM)
	            tpb = imgs2r (imback, 1, bsizex, 1, bsizey)
	    }

	    # Alloc memory for scatter quantities
	    call malloc (scatter, 3*norder, TY_REAL)
	    call malloc (s,  3*norder, TY_DOUBLE)
	    call malloc (s2, 3*norder, TY_DOUBLE)
	    call malloc (n,  3*norder, TY_INT)

	    # Initialize scatter for first cicle of deviant
	    # point rejection.
	    do order = 1, norder {
	        do i = 1, 3 {
	            SCAT(order,i) = 1.e8
	        }
	    }

	    # Compute scatter quantities in N_CLIP rejection cicles.
	    do cicle = 1, N_CLIP {
	        if (verb) {
	            call printf ("\rDoing sky rejection cycle %d...")
	                call pargi (cicle)
	            call flush (STDOUT)
	        }

	        # Zero sums.
	        call amovkd (double(0.), S(1,1),  3*norder)
	        call amovkd (double(0.), S2(1,1), 3*norder)
	        call amovki (0,          N(1,1),  3*norder)

	        # Read section block by block.
	        nblx = bsizex / blksize
	        nbly = bsizey / blksize	
	        do ibly = 1, nbly {
	            do iblx = 1, nblx {
	                i1 = (iblx -1) * blksize + 1
	                i2 = i1 + blksize - 1
	                j1 = (ibly -1) * blksize + 1
	                j2 = j1 + blksize - 1
	                call h_imget (ib, imback, i1, i2, j1, j2, storage, 
	                              bsizex, bsizey, tpb)

	                # Compute H-transform.
	                ht = h_transform (ib, blksize, FORWARD)

	                # Scan coefficients and update sums.
	                offset = 0
	                do order = norder, 1, -1 {
	                    ncoeff = NCOEFF(order)
	                    do j = 1, ncoeff, 3 {
	                        do i = 1, 3 {
	                            haux = HT(offset+i+j-1)
	                            # Clip off deviant coefficients.
	                            if (abs (real (haux)) < 
	                                S_CLIP * SCAT(order,i)) { 
	                                N(order,i)  = N(order,i)  + 1
	                                S(order,i)  = S(order,i)  + haux
	                                S2(order,i) = S2(order,i) + haux * haux
	                            }
	                        }
	                    }
	                    offset = offset + ncoeff
 	                }
	            }
	        }

	        # Compute scatter for each order
	        do order = 1, norder {
	            do i = 1, 3 {
	                if (N(order,i)  > 3) {
	                    S2(order,i) = (S2(order,i) - ((S(order,i) ** 2) /
	                                   N(order,i))) / (N(order,i) - 1)
	                    if (S2(order,i) > 0.)
	                        SCAT(order,i) = sqrt(S2(order,i))
	                    else
	                        SCAT(order,i) = 0.
	                } else {
	                    SCAT(order,i) = 0.
	                }
	            }
	        }
	    }

	    # Close background section and de-alloc auxiliary memory
	    if (streq (back, "input")) {
	    } else {
	        call imunmap (imback)
	    }
	    if (storage == STOR_MEM) {
	        call mfree (ib, TY_REAL)
	    }
	    call mfree (ht, H_TYPE)
	    call mfree (s,  TY_DOUBLE)
	    call mfree (s2, TY_DOUBLE)
	    call mfree (n,  TY_INT)
	}

	# Main filter loop. Process input image block by block

	nblx = isizex / blksize
	nbly = isizey / blksize	
	if (verb) {
	    ibla = 1
	    call printf ("\n")
	}
	i1old = 0

	do ibly = 1, nbly {
	    do iblx = 1, nblx {
	        if (verb) {
	            call printf ("\rProcessing block %d of %d...")
	                call pargi (ibla)
	                call pargi (nblx*nbly)
	            call flush (STDOUT)
	            ibla = ibla + 1
	        }

	        # Read one block
	        i1 = (iblx -1) * blksize + 1
	        i2 = i1 + blksize - 1
	        j1 = (ibly -1) * blksize + 1
	        j2 = j1 + blksize - 1
 	        call h_imget (im, imin,  i1, i2, j1, j2, storage, 
	                      isizex, isizey, tpi)
	        call h_imput (io, imout, i1, i2, j1, j2, storage, 
	                      isizex, isizey, tpo, 
	                      i1old, i2old, j1old, j2old)
	        i1old = i1
	        i2old = i2
	        j1old = j1
	        j2old = j2

	        # Compute its H-transform
	        ht = h_transform (im, blksize, FORWARD)

	        # Apply filter 

	        htg = HT_TYPE(1. - gain)

	        # Work each order downwards.
	        do order = norder-1, 1, -1 {

	            # Compute offset for given order.
	            offset1 = h_off (order+1, norder)

	            # Threshold filter.
	            if (tfil) {
	                ncoeff = NCOEFF(order+1)
	                do j = 1, ncoeff, 3 {
	                    do i = 1, 3 {
	                        if (abs (HT(offset1+i+j-1)) < 
	                                 HT_TYPE (SCAT(order+1,i) * thresh))
	                            HT(offset1+i+j-1) = HT(offset1+i+j-1) * htg
	                    }
	                }
	            }

	            # Spectral filter.
	            if (spfilter && (order >= (norder-1))) {

	                hsg = HT_TYPE(sqrt (2.*spfactor) / 4.)

	                # Compute offset and sizes for lower order.
	                offset  = h_off (order,   norder)
	                size    = int (sqrt (real (NCOEFF(order)   / 3.)))
	                size1   = int (sqrt (real (NCOEFF(order+1) / 3.)))

	                # Work on each 4X4 cell.
	                do j = 1, size {
	                    j1 = 2*j
	                    do i = 1, size {
	                        i1 = 2*i
	                        # Get coefficients for the cell
	                        h1  = HX(offset,  size,  i,  j)
	                        h21 = HX(offset1, size1, i1-1, j1-1)
	                        h22 = HX(offset1, size1, i1,   j1-1)
	                        h23 = HX(offset1, size1, i1-1, j1)
	                        h24 = HX(offset1, size1, i1,   j1)
	                        # Compute inequalities
	                        haux = abs (h21 + h22 + h23 + h24)
	                        hax  = abs (h21 - h22 + h23 - h24)
	                        hay  = abs (h21 + h22 - h23 - h24)
                                hac  = abs (h21 - h22 - h23 + h24)
	                        # X coefficients
	                        if (abs(h1) < (hsg * haux - 0.25 * hax)) {
	                            HX(offset1, size1, i1-1, j1-1) = 0.
	                            HX(offset1, size1, i1,   j1-1) = 0.
	                            HX(offset1, size1, i1-1, j1)   = 0.
	                            HX(offset1, size1, i1,   j1)   = 0.
	                        }
	                        # Y coefficents
	                        if (abs(h1) < (hsg * haux - 0.25 * hay)) {
	                            HY(offset1, size1, i1-1, j1-1) = 0.
	                            HY(offset1, size1, i1,   j1-1)   = 0.
	                            HY(offset1, size1, i1-1, j1)   = 0.
	                            HY(offset1, size1, i1,   j1)     = 0.
	                        }
	                        # C coefficents
	                        if (abs(h1) < (hsg * haux - 0.25 * hac)) {
	                            HC(offset1, size1, i1-1, j1-1) = 0.
	                            HC(offset1, size1, i1,   j1-1) = 0.
	                            HC(offset1, size1, i1-1, j1)   = 0.
	                            HC(offset1, size1, i1,   j1)   = 0.
	                        }
	                    }
	                }
	            }
 	        }

	        # Go back to data space
	        it = h_transform (ht, blksize, BACKWARD)
	        call amovr (Memr[it], Memr[io], blksize*blksize)
	        call mfree (it, TY_REAL)
	        call mfree (ht, H_TYPE)
	    }
	}

	# Close images.
	call h_imput (io, imout, i1, i2, j1, j2, storage, 
	              isizex, isizey, tpo, 
	              i1old, i2old, j1old, j2old)
	call imunmap (imin)
	call imunmap (imout)
	if (storage == STOR_MEM) {
	    call mfree (im, TY_REAL)
	    call mfree (io, TY_REAL)
	}
                          
	# De-alloc memory
	if (tfil)
	    call mfree (scatter, TY_REAL)

	if (verb) {
	    call printf ("%7.2f CPU seconds,  %5.2f elapsed minutes.\n")
	        call pargr (real (cputime (cpu)) / 1000.)
	        call pargr (real (clktime (clock)) / 60.)
	    call flush (STDOUT)
	}
end

	


# H_TRANSFORM  --  Compute recursively forward or backward H-transform.
#
# Routine allocs memory for the resulting array, and returns a
# pointer to it. User has to explicitly mfree this array after
# usage. 
#
# Type of array is as follows:
#
#    Sense         Input      Output
#
#    FORWARD       TY_REAL    H_TYPE
#    BACKWARD      H_TYPE     TY_REAL
#

pointer procedure h_transform (im, size, sense)

pointer	im				# array to be transformed
int	size				# it is size X size
int	sense				# forward/backward

pointer	ht				# return pointer
int	norder,order
int	psize				# present size of reduced array
long	htlast				# pointer to last ht element

real	h_log2()

begin
	norder = h_log2 (real(size))

	switch (sense) {

	case FORWARD:
	    call malloc (ht, size*size, H_TYPE)
	    htlast = 0
	    psize  = size
	    do order = norder, 1, -1 {
	        call h_reduce (im, psize, ht, htlast)
	        psize = psize / 2
	    }
	    htlast = htlast + 1
	    HT(htlast) = HT_TYPE(Memr[im])	# zero-th order coefficient

	case BACKWARD:
	    call malloc (ht, size*size, TY_REAL)
	    Memr[ht] = Memr[im+size*size-1]
	    do order = 1, norder
	        call h_expand (ht, size, im, order)

	default:

	}

	return (ht)
end




# H_REDUCE  --  Given an array of size 'size X size', reduce it
# to an array 'size/2 X size/2', filling up the H-transform
# vector with the corresponding coefficients.

procedure h_reduce (arr, size, ht, htlast)

pointer	arr				# input real array
pointer	ht				# output H-transform vector
int	size				# array is size X size
long	htlast				# pointer to last used
					# ht element
int	i,j
real	h0,hx,hy,hc

begin
	do j = 1, size, 2 {
	    do i = 1, size, 2 {
	        # Compute the four H-transform coefficients
	        h0 = (ARR(i,j,size)   + ARR(i+1,j,size) + 
	              ARR(i,j+1,size) + ARR(i+1,j+1,size)) / 4.
	        hx = (ARR(i,j,size)   - ARR(i+1,j,size) + 
	              ARR(i,j+1,size) - ARR(i+1,j+1,size)) / 4.
	        hy = (ARR(i,j,size)   + ARR(i+1,j,size) - 
	              ARR(i,j+1,size) - ARR(i+1,j+1,size)) / 4.
	        hc = (ARR(i,j,size)   - ARR(i+1,j,size) - 
	              ARR(i,j+1,size) + ARR(i+1,j+1,size)) / 4.
	        # Store them
	        htlast = htlast + 1
	        HT(htlast) = HT_TYPE(hx)
	        htlast = htlast + 1
	        HT(htlast) = HT_TYPE(hy)
	        htlast = htlast + 1
	        HT(htlast) = HT_TYPE(hc)
	        # Zero order goes to original array, sized to half
	        ARR((i+1)/2,(j+1)/2,size/2) = h0
	    }
	}
end




# H_EXPAND  --  Expand one order of H-transform vector.

procedure h_expand (arr, size, ht, order)

pointer	arr				# output real array
int	size				# it is size X size
pointer	ht				# pointer to H-transform vector
int	order				# order

int	ncoeff				# no. coefficients
int	norder				# maximum order
int	nbl				# no. of 4 X 4 blocks
int	i, j, k, i1, j1, sz
long	offset				# offset in H-transform vector
long	htp				# pointer to 4 X 4 block
real	h0, hx, hy, hc

long	h_off()
real	h_log2()

begin
	# Compute offset in H-transform vector, and no. of 
	# coefficients for given order.
	norder = h_log2 (real(size))
	offset = h_off (order,norder)
	ncoeff = NCOEFF(order)

	# Fill up array elements
	nbl = sqrt (real((ncoeff / 3)))
	sz = size / nbl
	htp = offset + 1
	do j = 1, nbl {
	    do i = 1, nbl {
	        i1 = (i - 1) * sz + 1
	        j1 = (j - 1) * sz + 1

	        # First, get coefficients
	        h0 = ARR(i1,j1,size)        # zero-th order is stored in
	        hx = real(HT(htp))          # first sub-array element
	        hy = real(HT(htp+1))
	        hc = real(HT(htp+2))

	        # Compute the four array elements. They will be 
	        # stored in the upper-left corners of the four 
	        # sub-arrays.
	        k = sz / 2 
	        ARR(i1,  j1,  size) = h0 + hx + hy + hc
	        ARR(i1+k,j1,  size) = h0 - hx + hy - hc
	        ARR(i1,  j1+k,size) = h0 + hx - hy - hc
	        ARR(i1+k,j1+k,size) = h0 - hx - hy + hc

	        htp = htp + 3
	    }
	}
end



# H_IMGET  --  Read image section. The read is performed either directly 
# from disk, or from an internal buffer.

procedure h_imget (im, image,  i1, i2, j1, j2, storage, 
                   sizex, sizey, tp)

pointer	im				# buffer pointer
pointer	image				# IMIO pointer
int	i1,i2,j1,j2			# sub-array indices
int	storage				# operation mode
int	sizex, sizey			# image size
pointer	tp				# buffer pointer

int	nlin, ncol, j
long	bof, of1, of2 

pointer	imgs2r()

begin
	switch (storage) {

	case STOR_DISK:
	    # Buffer is handled by imio, reading directly from disk.
	    im = imgs2r (image, i1, i2, j1, j2)

	case STOR_MEM:
	    # Buffer was already created and filled up with full
	    # image by caller. Routine has to return a pointer to
	    # section described by i1,i2,j1,j2.
	    if (im != NULL)
	        call mfree (im, TY_REAL)
	    nlin = j2 - j1 + 1
	    ncol = i2 - i1 + 1
	    call malloc (im, nlin * ncol ,TY_REAL)
	    bof = (j1 - 1) * sizex + (i1 - 1)
	    of1 = 0
	    of2 = 0
	    do j = 1, nlin {
	        call amovr (Memr[tp+bof+of1], Memr[im+of2], ncol)
	        of1 = of1 + sizex
	        of2 = of2 + ncol
	    }

	default:
	    im = NULL

	}
end




# H_IMPUT  --  Write image section. Use either imio, or internal
# explicit buffering.

procedure h_imput (io, image,  i1, i2, j1, j2, storage, 
                   sizex, sizey, tp, i1o, i2o, j1o, j2o)

pointer	io				# buffer pointer
pointer	image				# IMIO pointer
int	i1,i2,j1,j2			# sub-array indices
int	storage				# operation mode
int	sizex, sizey			# image size
pointer	tp				# buffer pointer
int	i1o, i2o, j1o, j2o		# previous sub-array

int	nlold,ncold
int	nlin, ncol, j
long	bof, of1, of2

pointer	imps2r()

begin
	switch (storage) {

	case STOR_DISK:
	    # Buffer is handled by imio.
	    io = imps2r (image, i1, i2, j1, j2)

	case STOR_MEM:
	    # Buffer was already created by caller. Routine has to 
	    # fill it up and return a new pointer to section described 
	    # by i1,i2,j1,j2. On first call, do not copy buffer.
	    if (i1o != 0) {
	        nlold = j2o - j1o + 1
	        ncold = i2o - i1o + 1
	        bof = (j1o - 1) * sizex + (i1o - 1)
	        of1 = 0
	        of2 = 0
	        do j = 1, nlold {
	            call amovr (Memr[io+of2], Memr[tp+bof+of1], ncold)
	            of1 = of1 + sizex
	            of2 = of2 + ncold
	        }
	    }
	    if (io != NULL)
	        call mfree (io, TY_REAL)
	    nlin = j2 - j1 + 1
	    ncol = i2 - i1 + 1
	    call malloc (io, nlin * ncol ,TY_REAL)

	default:
	    io = NULL

	}
end



# H_OFFPIX  --  Compute offset in H-transform vector for a given 
# pixel i,j and order. Offsets begin with 1, to be compatible with
# HT macro definition.

int procedure h_offpix (i, j, order, norder, blksize)

int	i, j, order, norder, blksize

int	oi, oj, size, size1
int	off, h_off()

begin
	off   = h_off (order, norder)
	size  = int (sqrt (real (NCOEFF(order) / 3.)))
	size1 = blksize / size

	oj = j / size1
	if (mod (j, size1) == 0)
	    oj = oj - 1

	oi = i / size1
	if (mod (i, size1) == 0)
	    oi = oi - 1

	off = off + oj*size*3 + oi*3 + 1
	return (off)
end



# H_OFF  --  Compute offset in H-transform vector for a given order.
#
#   BETTER IF IMPLEMENTED AS A LOOK-UP TABLE !!

int procedure h_off (order, norder)

int	order, norder

int	i, offset

begin
	offset = 0
	if (order == norder)
	    return (offset)

	i = norder
	while (i > order) {
	    offset = offset + NCOEFF(i)
	    i = i - 1
	}

	return (offset)
end



# LOG2  --  Base 2 logarithm

real	procedure h_log2 (x)
real	x

begin
	return (log(x) / log(2.))
end


