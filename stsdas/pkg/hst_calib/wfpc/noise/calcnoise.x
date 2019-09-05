include <imhdr.h>
include <imset.h>
include	<mach.h>

#################################################################################
#										#
#  CALCNOISE -- This routine is used to determine the noise characteristics 	#
#		of the data by examining the way short-scale fluctuations 	#
#		in a flat field (or other) frame vary with data number.  The 	#
#		empirical results may be compared with various noise models.	#
#		This routine is based upon FORTRAN code written by K.Horne.  	#
#										#
#	SPP implementation:	7/91 by RAShaw					#
#										#

procedure calcnoise (mean, sigma, jpts, mk, group, xsize, ysize, ksig, list)

# Calling arguments:
real	mean[jpts]		# O: Array of bin means 
real	sigma[jpts]		# O: Array of all sigmas
int	jpts			# I: Anticipated number of bins for all images
int	mk			# O: Total number of bins for all images
int	group			# I: Group number of images
int	xsize			# I: Size of box in X-dimension
int	ysize			# I: Size of box in Y-dimension
real	ksig			# I: Kappa-sigma rejection threshold
pointer	list			# List of input image rootnames

# Local variables:
int	badbits			# User-selected DQF bits
char	datextn[SZ_FNAME]	# Filename extension for images
pointer	dqf			# Pointer to input DQFs
char	dqfextn[SZ_FNAME]	# Filename extension for DQFs
bool	dqfflag			# Reference DQF files for pixel masking?
pointer	dqlines			# Pointer to DQF subsection
char	grpch[SZ_FNAME]		# Cluster index in brackets
int	iy, jim, junk, k	# Loop indexes
char	image[SZ_FNAME]		# Input image rootname
pointer	in			# Pointer to input images
pointer input			# Pointer to input image filenames
pointer	lines			# Pointer to image subsection
pointer	mn			# Array of bin means in each subsection
int	nimages			# Number of input images
int	npts			# Number of pixels per box
int	nxbox, nybox		# Number of bins along x,y axes
#int	ngroup			# Number of groups per image
char	section[SZ_FNAME]	# Image sub-section string
pointer	sig			# Measured sigmas in each subsection
pointer	sp			# Pointer to begining of stack memory
int	x1, x2, y1, y2		# Limits of mapped image sub-section
int	begnxy[2]		# Begining x and y pixel numbers
int	endxy[2]		# Ending x and y pixel numbers
int	step[2]			# Step size in X and Y

# List of functions used:
bool	clgetb()		# Get boolean value from the CL
pointer	imgs2i()		# Get image subsection, type int
pointer	imgs2r()		# Get image subsection, type real
pointer	gf_map()		# Map an image into memory
int	imtlen()		# Determine the number of images in a list
int	imtgetim()		# Get next image filename from image template

errchk	gf_map, imgsection, subsectn

begin

#  Get list of input images
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	nimages = imtlen (list)

#  Set selected DQF flags
	dqfflag = clgetb ("usedqf")
	if (dqfflag) 
	    call dqfinit (badbits, datextn, dqfextn)
	mk = 0
	call strcpy (EOS, grpch, SZ_FNAME)
	if (!IS_INDEFI (group)) {
	    call sprintf (grpch, SZ_FNAME, "[%d]")
		call pargi (group)
	} 

#  Map input images (and associated DQF files, if needed)
	do jim = 1, nimages {
	    junk = imtgetim (list, Memc[input], SZ_FNAME)

#  Get the image cluster (i.e. root minus group and section specifications), 
#  and append the requested group number
	    call imgcluster (Memc[input], image, SZ_FNAME)
	    if (!IS_INDEFI (group)) 
		call strcat (grpch, image, SZ_FNAME)
	    in = gf_map (image, READ_ONLY, 0)
	    if (dqfflag) {
		call splicstr (image, image, datextn, dqfextn)
		dqf = gf_map (image, READ_ONLY, 0)
	    } else 
		dqf = NULL
	    call strcpy (EOS, image, SZ_FNAME)

#  Get boundaries of images for calculation.  Default to the entire image.  
	    begnxy[1] = 1
	    begnxy[2] = 1
	    endxy[1]  = IM_LEN[in,1]
	    endxy[2]  = IM_LEN[in,2]
	    step[1]   = 1
	    step[2]   = 1
	    call imgsection (Memc[input], section, SZ_FNAME)
	    if (section[1] != EOS) 
		call subsectn (section, begnxy, endxy, step, 2)

#  Set up accumulation variables
	    npts   = xsize * ysize
	    nxbox  = int ((endxy[1] - begnxy[1] + 1) / xsize)
	    nybox  = int ((endxy[2] - begnxy[2] + 1) / ysize)
	    call calloc (mn,  nxbox, TY_REAL)
	    call calloc (sig, nxbox, TY_REAL)

#  Loop over bins
	    x1 = begnxy[1]
	    x2 = nxbox * xsize + begnxy[1] - 1
	    do iy = 0, nybox-1 {
		y1 = iy * ysize + 1
		y2 = y1 + ysize - 1

#  Get an image section of "ysize" lines
		lines = imgs2r (in, x1, x2, y1, y2)
		if (dqfflag) {
		    dqlines = imgs2i (dqf, x1, x2, y1, y2)

#  Compute statistics on each portion of the image
		    call dqboxstat (Memr[lines], Memi[dqlines], Memr[mn], 
				Memr[sig], nxbox, xsize, ysize, ksig, badbits)
		} else 
		    call boxstat (Memr[lines], Memr[mn], Memr[sig], nxbox, 
				xsize, ysize, ksig)

#  Retrieve calculations from accumulation variables
		do k = 0, nxbox-1 {
		    if ((!IS_INDEFR(Memr[mn+k])) && (!IS_INDEFR(Memr[sig+k]))) {
			mk = mk + 1
			mean[mk]  = Memr[mn+k]
			sigma[mk] = Memr[sig+k]
		    }
		}
	    }

# Close the image and release memory
	    call gf_unmap (in)
	    if (dqfflag) 
		call gf_unmap (dqf)
	    call mfree (mn,  TY_REAL)
	    call mfree (sig, TY_REAL)
	}

#  Release stack memory
	call sfree (sp)
	call imtrew (list)

end


#################################################################################
#										#
#  BOXSTAT --	This routine is used to determine the mean and standard 	#
#		deviation of subsections of the image and returning the 	#
#		results in the variables MEAN and SIGMA.			#
#										#
#	Initial code:	6/91 by RAShaw						#
#										#

procedure boxstat (data, mean, sigma, nxbox, xsize, ysize, ksig)

# Calling arguments:
real	data[xsize*nxbox,ysize]	# I: Pointer to input image
real	mean[nxbox]		# O: Mean within boxes  
real	sigma[nxbox]		# O: Sqrt(variance) within boxes  
int	nxbox			# I: Number of boxes per line
int	ysize			# I: Number of image lines 
int	xsize			# I: Number of image lines 
real	ksig			# I: Kappa-sigma rejection threshold

#  Local variables:
int	box			# Running index of boxes
int	indx, ix, iy, iu	# Loop indexes
int	ngpix			# Number of non-rejected pixels used in average
int	npts, nxpts		# Number of pixels per box
pointer	sp			# Pointer to begining of stack memory
pointer	work			# Working array

# Functions called:
int	aravr()

begin

#  Initialize working array
	npts  = xsize * ysize
	nxpts = xsize * nxbox
	call smark (sp)
	call salloc (work, npts, TY_REAL)
	call aclrr (Memr[work], npts)
	call aclrr (mean,  nxbox)
	call aclrr (sigma, nxbox)

#  Loop over each box 
	do box = 1, nxbox {
	    indx = 0
	    iu   = (box - 1) * xsize

#  Fill working array with box values
	    do iy = 1, ysize {
		do ix = 1, xsize {
		    Memr[work+indx] = data[iu+ix,iy]
		    indx = indx + 1
		}
	    }

#  Compute average and deviation within working array.  
	    if (IS_INDEFR (ksig)) 
		call aavgr (Memr[work], indx, mean[box], sigma[box])
	    else
		ngpix = aravr (Memr[work], indx, mean[box], sigma[box], ksig)
	}
	call sfree (sp)
end


#################################################################################
#										#
#  DQBOXSTAT --	This routine is used to determine the mean and standard 	#
#		deviation of subsections of the image and returning the 	#
#		results in the variables MEAN and SIGMA.  Pixel values flagged 	#
#		in the DQFs will be excluded from the calculation.  		#
#										#
#	Initial code:	6/91 by RAShaw						#
#										#

procedure dqboxstat (data, dqf, mean, sigma, nxbox, xsize, ysize, ksig, 
			badbits)

# Calling arguments:
real	data[xsize*nxbox,ysize]	# I: Input image section
int	dqf[xsize*nxbox,ysize]	# I: Pointer to DQF image
real	mean[nxbox]		# O: Mean within boxes  
real	sigma[nxbox]		# O: Sqrt(variance) within boxes  
int	nxbox			# I: Number of boxes per line
int	xsize			# I: Number of pixels per line
int	ysize			# I: Number of image lines 
real	ksig			# I: Kappa-sigma rejection threshold
int	badbits			# I: Bit-coded DQ indicators

#  Local variables:
int	bflag			# User-selected DQF flag(s) @pixel
int	box			# Running index of boxes
int	indx, ix, iy, iu	# Loop indexes
int	ngpix			# Number of non-rejected pixels used in average
int	npts, nxpts		# Number of pixels per box
pointer	sp			# Pointer to begining of stack memory
pointer	work			# Working array

# Functions called:
int	aravr()

begin

#  Initialize working array
	npts  = xsize * ysize
	nxpts = xsize * nxbox
	call smark (sp)
	call salloc (work, npts, TY_REAL)
	call aclrr (Memr[work], npts)
	call aclrr (mean,  nxbox)
	call aclrr (sigma, nxbox)

#  Loop over each box
	do box = 1, nxbox {
	    indx = 0
	    iu   = (box - 1) * xsize

#  Fill working array with non-flagged data:
	    do iy = 1, ysize {
		do ix = 1, xsize {

#  Select user-chosen Data Quality bits:
		    bflag = dqf[ix+iu,iy]
		    call aandki (bflag, badbits, bflag, 1)
		    if (bflag == 0) {
			Memr[work+indx] = data[ix+iu,iy]
			indx = indx + 1
		    }
		}
	    }

#  Compute average and deviation within working array.  `mean' and `sigma' 
#  are set to INDEF if the number of non-rejected points is zero or one, 
#  respectively.  
	    if (IS_INDEFR (ksig)) 
		call aavgr (Memr[work], indx, mean[box], sigma[box])
	    else
		ngpix = aravr (Memr[work], indx, mean[box], sigma[box], ksig)
	}
	call sfree (sp)
end

