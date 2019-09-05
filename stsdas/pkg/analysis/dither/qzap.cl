procedure qzap(inlist,outlist)

#  Cosmic ray cleaning routine using median filtering.
#  This is basically reinventing the wheel -- like the IRAF routine
#  'cosmicrays' (but unlike xzap), this routine does a "flux ratio"
#  comparison between the pixel being zapped and the surrounding
#  pixels to check to see whether the candidate is really a CR or
#  might instead just be a faint star.  Thus it is not as necessary
#  to mask out objects before zapping, and in general the parameter
#  nobjsigma (which controls object masking) can be set to zero.
#  Qzap also makes multiple passes through the zapping process, but
#  does so very inefficiently.
#
#  Latest revision:  8 October 1993
#  Mark Dickinson
#
#  Calls scripts fileroot.cl, iterstat.cl, minv.cl

string	inlist	    {prompt="Image(s) for cosmic ray cleaning"}
string	outlist	    {prompt="Output image(s)"}
int	zboxsz	    {5,min=3,prompt="Box size for zapping"}
real	nsigma	    {5.,min=0.,prompt="Number of sky sigma for zapping threshold"}
real	nnegsigma   {0.,min=0.,prompt="Number of sky sigma for negative zapping"}
bool	fluxcompare {yes,prompt="Make flux ratio comparison between CR and local pixels?"}
real	fluxratio   {0.1,prompt="Flux ratio threshold of local median to CR candidate"}
int	niter       {1,min=1,prompt="Number of zapping iterations"}
int     skyfiltsize {15,min=0,prompt="Median filter size for local sky evaluation"}
int	skysubsample{1,min=0,prompt="Block averaging factor before median filtering"}
int	nrings	    {0,min=0,prompt="Number of pixels to flag as buffer around CRs"}
real	nobjsigma   {0.,min=0.,prompt="Number of sky sigma for object identification"}
int	ngrowobj    {0,min=0,prompt="  Number of pixels to flag as buffer around objects"}
string	statsec	    {"",prompt="Image section to use for computing sky sigma"}
bool	deletemask  {no, prompt="Delete CR mask after execution?"}
bool	verbose     {yes,prompt="Verbose output?"}
bool	checklimits {yes,prompt="Check min and max pix values before filtering?"}
real	zmin        {-32768.,prompt="Minimum data value for fmedian"}
real	zmax        {32767.,prompt="Minimum data value for fmedian"}

struct	*inimglist
struct	*outimglist
struct	*statlist

begin

	string	ilist,olist		# equal inlist, outlist
	real	skysig,skymode
	real 	crthresh,objthresh,negthresh
	real	dmin,dmax
	real	nbox,nbox2
	int	nin
	string 	infile, outfile, img, imgex, outimg, statfile
	string	workimg
	int	ix,iy
	string	cutsec
	real	flagval
	bool	maskobj, maskneg
	int	ii,nzap,nzaptot

# Get query parameters.

	ilist = inlist
	olist = outlist

# Check input parameters and set initial values.

	if (nnegsigma > 0.) 
		maskneg = yes 
	   else 
		maskneg = no
	if (nobjsigma == 0.) {
		print ("No object masking will be used during zapping.")
		maskobj = no
	   } else {
		maskobj = yes
	}
	flagval = fluxratio + 1.

# Expand file lists into temporary files.

	infile =  mktemp("tmp$zap")
	outfile = mktemp("tmp$zap")
	sections (ilist,option="fullname",>infile)
	nin = sections.nimages
	sections (olist,option="fullname",>outfile)
	if (nin != sections.nimages) 
		error(0,"ERROR:   Numbers of input and output images do not match.")
	inimglist = infile
	outimglist = outfile

# Loop through input files:

	while (fscan(inimglist,img) != EOF) {

# Strip extension off input file name.

		fileroot (img,validim+)
		imgex = img
		img = fileroot.root

# Read name of output file.

		if (fscan(outimglist,outimg) == EOF) {
			print ("ERROR:   Problem scanning output file name in xzap.")
			return
		}
		fileroot (outimg,validim+)
		outimg = fileroot.root

		if (verbose) print ("Working on ",img," ---ZAP!--> ",outimg)

# Calulate sky mean and RMS noise using iterative sigma rejection.

		if (verbose) print ("     Computing statistics.")
		iterstat (img//statsec,nsigrej=5.,maxiter=10,
			verbose-,print=verbose)
		skymode = iterstat.mean
		skysig  = iterstat.sigma

		if (verbose) print ("     Subtracting local sky.")

# If skyfiltsize > 1, median filter image to produce local sky, then subtract that from the image.

		if (skyfiltsize > 1) {

# If skysubsample > 1, block average input image to smaller size.

		  if (skysubsample > 1) {
			if (verbose) print ("        Block averaging.")
			blkavg (img,"_blk_"//img,skysubsample,skysubsample,option="average")
			workimg = "_blk_"//img
		   } else {
			workimg = img
		  }

# First, check limits of data range.  Wildly large (positive or negative) data values
# will screw up fmedian calculation unless checklimits = yes and zmin and zmax are set
# to appropriate values, e.g. zmin=-32768, zmax=32767.   Note that the old fmedian
# bug which occurred if zmin=hmin and zmax=hmax has been fixed in IRAF version 2.10.1.
# The pipe to dev$null, however, is included because V2.10.1 and .2 have debugging 
# print statements accidentally left in the fmedian code.  

		  if (checklimits) {
			minmax (img,force+,update+,ve-)
			if (verbose) print("     Data minimum = ",minmax.minval,
						" maximum = ",minmax.maxval)
			if (minmax.minval < zmin) {
				dmin = zmin
			   } else { 
				dmin = minmax.minval
			}
			if (minmax.maxval > zmax) {
				dmax = zmax
			   } else { 
				dmax = minmax.maxval
			}
			if (verbose) print ("     Truncating data range ",dmin," to ",dmax)
		     } else {
			dmin = INDEF
			dmax = INDEF
		  }

		  fmedian (workimg,"_sky_"//workimg,xw=skyfiltsize,yw=skyfiltsize,
			 boundary="nearest",hmin=-32768,hmax=32767,
			 zmin=dmin,zmax=dmax,>&"dev$null")

# If we have block averaged, block replicate median filtered image back to original size.

		  if (skysubsample > 1) {
			if (verbose) print ("        Block reproducing.")
			imdelete ("_blk_"//img,ver-)
			blkrep ("_sky_"//workimg,"_sky_"//img,skysubsample,skysubsample)
			imdelete ("_sky_"//workimg,ver-)
			imgets(img,"i_naxis1")
			ix = int(imgets.value)
			imgets(img,"i_naxis2")
			iy = int(imgets.value)
			cutsec = "[1:"//ix//",1:"//iy//"]"
			imcopy ("_sky_"//img//cutsec,"_sky_"//img,ver-)
		  }

		  imarith (img,"-","_sky_"//img,"_sub_"//img)

		} else {

# ...or, just subtract the constant sky value computed above from the image.

		  imarith (img,"-",skymode,"_sub_"//img)

		}

# Main zapping loop:

	nzaptot = 0

	for (ii=1; ii<=niter; ii+=1) {

		if (verbose && niter > 1) print ("--- Iteration number ",ii)

# Check data limits again on sky subtracted image...

		if (checklimits) {
			minmax ("_sub_"//img,force+,update+,ve-)
			if (verbose) print("     Data minimum = ",minmax.minval,
						" maximum = ",minmax.maxval)
			if (minmax.minval < zmin) {
				dmin = zmin
			   } else { 
				dmin = minmax.minval
			}
			if (minmax.maxval > zmax) {
				dmax = zmax
			   } else { 
				dmax = minmax.maxval
			}
			if (verbose) print ("     Truncating data range ",dmin," to ",dmax)
		     } else {
			dmin = INDEF
			dmax = INDEF
		}

# Median filter image to produce file _fmed_//img

		if (verbose) print ("     Median filtering.")
		fmedian ("_sub_"//img,"_fmed_"//img,xw=zboxsz,yw=zboxsz,
			 boundary="reflect",hmin=-32768,hmax=32767,
			 zmin=dmin,zmax=dmax,>&"dev$null")

# Take difference to produce "unsharp masked" image _cr_//img
		imarith ("_sub_"//img,"-","_fmed_"//img,"_cr_"//img)

# Threshold _cr_//img at nsigma*skysig to make CR mask _peaks_//img.  
#    Potential CRs --> 1
#    "Blank sky"   --> 0
# Note that crthresh will be positive by definition.
		if (verbose) print ("     Masking potential CR events.")
		imcopy ("_cr_"//img,"_peaks_"//imgex,ve-)
		crthresh = nsigma*skysig   
		imreplace ("_peaks_"//imgex,0.,upper=crthresh,lower=INDEF)
		imreplace ("_peaks_"//imgex,1.,lower=crthresh,upper=INDEF)
# Make PL format version of _peaks_//img and delete IMH format version.
		imcopy   ("_peaks_"//imgex,"_peaks_"//img//".pl",ve-)
		imdelete ("_peaks_"//imgex,ve-)


# If fluxcompare==yes, apply the criteron that a detected peak is truly a cosmic ray only
# if median value of the surrounding pixels of the sky subtracted image (i.e. the pixel
# value in _sub_//img) is less than fluxratio times the CR candidate's value in excess 
# of the local median (i.e. the pixel value in _cr_//img).

		if (fluxcompare) {

			if (verbose) print ("     Making flux ratio comparison.")
			imarith ("_fmed_"//img,"/","_cr_"//img,"_fluxrat_"//img,ver-)
			imarith ("_fluxrat_"//img,"/","_peaks_"//img//".pl","_fluxrat_"//img,
				divzero=flagval,ver-)
			imreplace ("_fluxrat_"//img,0.,lower=INDEF,upper=fluxratio)
			imreplace ("_fluxrat_"//img,1.,lower=fluxratio,upper=INDEF)
			minv ("_fluxrat_"//img,"_crmask_"//img//".pl")
			imdelete ("_peaks_"//img//".pl",ve-)
			imdelete ("_fluxrat_"//img,ver-)

		   } else {

# Otherwise, the crmask is just a copy of the _peaks_ mask.

			imrename ("_peaks_"//img//".pl","_crmask_"//img//".pl",ver-)
		}

# Object masking:  create mask identifying where objects might be.

		if (maskobj) {

			if (verbose) print ("     Creating object mask.")
		        objthresh = nobjsigma*skysig  

			imcopy ("_fmed_"//img,"_obj_"//img,ver-)
			imreplace ("_obj_"//img,0,lower=INDEF,upper=objthresh)
			imreplace ("_obj_"//img,1,lower=objthresh,upper=INDEF)
			if (ngrowobj != 0) {
				nbox = 2 * ngrowobj + 1
				nbox2 = 1./ (nbox * nbox)
				boxcar ("_obj_"//img,"_obj_"//img,nbox,nbox)
				imreplace ("_obj_"//img,1,lower=nbox2,upper=INDEF)
			}

# Invert mask to make "objects" --> 0 and "sky" --> 1.

			minv ("_obj_"//img,"_obj_"//img//".pl")
			imdelete ("_obj_"//img,ver-)
			imarith ("_crmask_"//img//".pl","*","_obj_"//img//".pl",
				 "_crmask_"//img//".pl")
		}

		imdelete ("_fmed_"//img,ve-)

# Grow additional buffer region around identified CRs.
		if (nrings > 0) {
			if (verbose) print ("     Growing mask rings around CR hits.")
			nbox = 2 * nrings + 1
			nbox2 = nbox * nbox
			imarith ("_crmask_"//img//".pl","*",nbox2,"_crmask_"//img//".pl")
			boxcar ("_crmask_"//img//".pl","_crmask_"//img//".pl",
				xw=nbox,yw=nbox)
			imreplace ("_crmask_"//img//".pl",1,lower=1,upper=INDEF)
		}

# Identify negative pixels if desired.  No "rings" are grown around negative 
		if (maskneg) {
			if (verbose) print ("     Masking deviant negative pixels.")
			imcopy ("_cr_"//img,"_neg_"//img,ve-)
			negthresh = -1.*nnegsigma*skysig
			imreplace ("_neg_"//img,0.,
				lower=negthresh,upper=INDEF)
			imreplace ("_neg_"//img,1.,
				upper=negthresh,lower=INDEF)
			imcopy ("_neg_"//img,"_neg_"//img//".pl",ve-)
			imdelete ("_neg_"//imgex,ve-)
			if (maskobj) imarith ("_neg_"//img//".pl","*","_obj_"//img//".pl",
				 "_neg_"//img//".pl")
			imarith ("_crmask_"//img//".pl","+","_neg_"//img//".pl",
				 "_crmask_"//img//".pl")
			imreplace ("_crmask_"//img//".pl",1,lower=1,upper=INDEF)
		}

		if (maskobj) imdelete ("_obj_"//img//".pl",ve-)
		if (maskneg) imdelete ("_neg_"//img//".pl",ve-)

		imstat ("_crmask_"//img//".pl",fields="npix",
			lower=1,upper=1,format-) | scan (nzap)
		nzaptot = nzaptot + nzap
		if (verbose) {
			if (nzap==1)
				print ("     Replacing 1 CR hit with local median.")
			   else
				print ("     Replacing ",nzap," CR hits with local median.")
		}


# If nzap = 0, there's no point in doing any more work, so we clean up and exit loop.

		if (nzap == 0) {
			if (ii==1) {
				imrename ("_crmask_"//img//".pl","crmask_"//img//".pl",ver-)
			   } else {
				imdelete ("_crmask_"//img//".pl",ver-)
			}
			imdelete ("_cr_"//img,ve-)
			break
		}
			
# If nzap > 0, then we remove the detected cosmic ray hits.
# Multiply CR mask by _crmask_//img to make "comic rays only" image _cronly_//img

		imarith ("_cr_"//imgex,"*","_crmask_"//img//".pl",
			 "_cronly_"//imgex)

# Subtract _cronly_//img from data to produce clean image "outimg". 
#      Note that this effectively replaces the masked regions with the local
#      median, since _cronly_//img = img - _fmed_//img.

		if (niter > 1) {
			imarith ("_sub_"//img,"-","_cronly_"//img,"_sub_"//img)
			imdelete ("_cronly_"//img,ve-)
		}

# If this is first iteration, rename _crmask_ --> crmask_.   Otherwise, add the _crmask_
# to the already existing crmask_.

		if (ii==1) {
			imrename ("_crmask_"//img//".pl","crmask_"//img//".pl",ver-)
		   } else {
			imarith  ("crmask_"//img//".pl","+","_crmask_"//img//".pl",
				  "crmask_"//img//".pl",ver-)
			imdelete ("_crmask_"//img//".pl",ver-)
		}

		imdelete ("_cr_"//img,ve-)

	}

# End of iteration loop.   Now, if any CRs were zapped, and if we did more than
# one iteration, then add the sky back in and create the output image.

	if (nzaptot > 0) {
		if (niter > 1) {
			if (img == outimg) imrename (img,"__"//img,ver-)
			imarith ("_sub_"//img,"+","_sky_"//img,outimg,ver-)
			if (img == outimg) imdelete ("__"//img,ver-)
				

# Otherwise, if we only did one iteration, simply subtract the _cronly_ image from
# the original input image to create the output.

	   	   } else {
			imarith (img,"-","_cronly_"//img,outimg)
			imdelete ("_cronly_"//img,ve-)
		}
	   } else {
		if (img != outimg) imcopy (img,outimg,ver-)
	}

# Record CR mask name in headers of input and output images

		hedit (outimg,"CRMASK","crmask_"//img//".pl",add+,ver-,update-)
		if (img != outimg) hedit (img,"CRMASK","crmask_"//img//".pl",add+,ver-,update-)

# Clean up.

		imdelete ("_sub_"//img,ve-)
		if (skyfiltsize > 1) imdelete ("_sky_"//img,ver-)
		if (deletemask) {
			imdelete ("crmask_"//img//".pl",ve-)
		   } else {
			if (niter > 1) imreplace ("crmask_"//img//".pl",1,lower=1,upper=INDEF)
		}
		if (verbose) print ("     Done.")
	}


	inimglist = ""
	outimglist = ""
	statlist = ""

end
