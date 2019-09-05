#			  INTENSITY.CL
#			----------------
#
#   INPUT parameters:
#
#	* IMAGE    	image cube
#	* MIMAGE	image cube from which mask will be made
#	* THRESHOLD	threshold above which mask value will be set to one
#	* NONEG		switch to mask out negative intensities in image
#	* NMIN		minimum number of channels passed for further 
#			processing 
#
#   OUTPUT parameters:
#
#	* NTHRESH	plane of the number of channels passed after 
#			merely thresholding
#	* NRCHAN	plane of the number of channels passed after all
#			corrections to the mask
#	* MASK		final corrected mask cube
#	* IMASK		masked image cube 
#	* TIMAP		plane of total intensity for every masked profile
#
#
# This IRAF script was developed by Diane Gilmore and Ron Allen of the
# Space Telescope Science Institute in 1990.
#

	procedure intensity(image,mimage,nthresh,nrchan,mask,imask,timap)
# =======================================================================

  file image = ""     {prompt="input data cube filename"}
  file mimage = ""    {prompt="input cube for determining mask"}
  file nthresh = ""   {prompt="output channel map after thresholding"}
  file nrchan = ""    {prompt="output channel map after corrections"}
  file mask = ""      {prompt="output mask cube"}
  file imask = ""     {prompt="output masked image cube"}
  file timap = ""     {prompt="output total intensity map"}
  real threshold = 1. {prompt="threshold for masking mimage"}
  bool noneg = no   {prompt="exclude neg values when creating mask? [yes/no]"}
  int  nmin = 2       {prompt="minimum acceptable number of channels"}
  file logfile = ""   {prompt="output logfile (null string = screen)"}
  struct *head        {prompt="Do Not enter a value"}

  begin

# Declare the intrinsic parameters:
 
	file  im                # input data cube
	file  mim               # input cube for mask-making
	file  msk		# output mask cube
	file  imsk		# output masked image cube
	file  nthr		# output plane of number of channels passed
 	file  imap		# output total intensity pane
	file  nrch		# output plane of final number of channels
                                #             passed after all corrections
	file  tmp,otmp,mtmp     # files for temporary storage
        file  msktmp,ntmp       # files for temporary storage
	file  logf              # output message file
	file  outf		# temporary file for header info
	real  thresh,negthresh	# positive and negative threshold
	real  deltav            # channel width in km/sec read from header
	int   dimen		# header parameter giving dimensions of im
	int   xmax,ymax,zmax    # header parameters describing size of im
        int   z,x	        # counters
	int   zmtwo		# an index
	int   nm		# the minimum acceptable number of channels
	bool  nn	        # switch to eliminate the negative image
				# intensities.

# Get query parameters:
	
	im = image
	mim = mimage
	thresh = threshold
	nthr = nthresh
	nn = noneg
	nm = nmin
	nrch = nrchan
	msk = mask
	imsk = imask
	imap = timap
	logf = logfile
	if (logf == "") {
	   logf = "STDOUT"
	}

	time		      # print starting time
	set clobber=yes

# Check access to input file, and get header parameters describing	
# the size of the input file:

	outf = mktemp("outf")
	if(access(im)) {
	   hselect(im,"NAXIS,NAXIS[123]",exp=yes,>outf) # write to temprorary file
	} else {
	   error(1,"Input image missing or incorrect")
	}
	xmax = 1		# Initialize header keyword parameters
	ymax = 1
	zmax = 1
	dimen = 0

# Read the temporary file outf using a list structured parameter.

	head = outf   		# Access the temporary file
	while (fscan(head,dimen,xmax,ymax,zmax) != EOF) {
	   print("dimensions of image = ",dimen,>logf)
	   print("xmax = ",xmax,>>logf)
	   print("ymax = ",ymax,>>logf)
	   print("zmax = ",zmax,>>logf)
	   if(dimen != 3) {     # check the dimensionality of input cube
	      error(1,"Image is not three dimensional")
	   }
	}
	delete(outf,v-)

# Get header parameters describing the size of the mimage file:

	outf = mktemp("outf")     	# rewrite temporary file
	if(access(mim)) {	
	   hselect(mim,"NAXIS,NAXIS[123]",exp=yes,>outf)
	} else {
	   error(1,"Input image missing or incorrect")
	}

# Read the temporary file outf using a list structured parameter.

	xmax = 1
	ymax = 1
	zmax = 1
	dimen = 0
	head = outf   			# access temporary file
	while (fscan(head,dimen,xmax,ymax,zmax) != EOF) {  # read it
	   print("dimensions of mimage = ",dimen,>>logf)
	   print("xmax = ",xmax,>>logf)
	   print("ymax = ",ymax,>>logf)
	   print("zmax = ",zmax,>>logf)
	   if(dimen != 3) {		# check dimensionality of second cube
	      error(1,"Image is not three dimensional")
	   }
	}
	delete(outf,v-)

# Get header parameter describing the channel width.  Assume value is
# in units of meters/second.  If it is not there - assume 1 km/sec 
# channel width.

	outf = mktemp("outf")	
	hselect(im,"CDELT3",exp=yes,> outf) # write to temporary file
	deltav = 0.0
	head = outf			    # access the temporary file
	while(fscan(head,deltav) != EOF) {  # read it
	   deltav = deltav/1000.0           # in km/sec
	   print("deltav = ",deltav,>> logf)
	}
	if(deltav == 0.0) {
	   print("Header parameter missing in image: CDELT3",>> logf)
	   print("Using deltav = 1 km/sec",>> logf)
	   deltav = 1.0                     # in km/sec
	}
	delete(outf,v-)

# COMPUTE MASK =======================================================
	
	print(" COMPUTING MASK CUBE..........................",>> logf)

# create place-holder image with the same header as the cube used for mask.

	imcopy(mim,msk,v-)   

# Apply threshold to the smoothed image cube to create a mask cube with 
# values of one or zero.

	negthresh = -thresh
# bjc - imreplace
	imreplace(msk,0.,imaginary=0.,lower=negthresh,upper=thresh)
	imreplace(msk,1.,imaginary=0.,lower=INDEF,upper=negthresh)
	imreplace(msk,1.,imaginary=0.,lower=thresh,upper=INDEF)
#imreplace(msk,0.,0.,lower=negthresh,upper=thresh)
#imreplace(msk,1.,0.,lower=INDEF,upper=negthresh)
#imreplace(msk,1.,0.,lower=thresh,upper=INDEF)

# Add history records to update the mask file:

	print("IRAF  Mask Cube") | stfhistory(msk,"STDIN")
	print("IRAF  threshold used: "//thresh) | stfhistory(msk,"STDIN")

# COMPUTE NUMBER OF CHANNELS SURVIVING THE THRESHOLDING OPERATION =====

	print (" COMPUTING NUMBER OF CHANNELS THRESHOLDED .... ",>> logf)

# This will be a 2-dimensional image containing, at every pixel, the 
# total number of channels above the threshold.

# Sum over the velocity dimension of the mask file:

	blkavg(msk,nthr,1,1,zmax,1,1,1,1,option="sum",>> logf)
	print (" BLKAVG successful in sum over velocity. . . . ",>> logf)
	imcopy(nthr//"[*,*,1]",nthr,v-)
        print (" IMCOPY successful to turn BLKAVG into 2D. ",>> logf)

# Add history records to update the nthresh file:

	print ("IRAF  Channel Map after thresholding") | 
		stfhistory(nthr,"STDIN")

# EXCLUDE NEGATIVE VALUES IN IMAGE FROM THE MASK =====================

	if (nn)  {
	   
	   msktmp = mktemp("msktmp")
           print (" MKTMP successful. ",>> logf)
# bjc -imcalc
	   imcalc(input=im,output=msktmp,equals="im1.gt.0.",v+,>> logf)
           print (" IMCALC successful. ",>> logf)
	   imarith(operand1=msk,op="*",operand2=msktmp,result=msk,
                   pixtype="real",calctype="real",v+,>> logf)
           print (" IMARITH successful. ",>> logf)
	   imdel(msktmp,go+,v-)

# Add history records to update the mask file:

	   print("IRAF  Negative values excluded from mask ") |
		  stfhistory(msk,"STDIN")

	}

# CORRECT MASK =======================================================

	print (" CORRECTING MASK FOR MINIMUM CHANNELS",>> logf)

# Get current nrchan map.

	ntmp = mktemp("ntmp")
        print( " MKTEMP successful.",>> logf)
	if (nn)  {
	   blkavg(input=msk,output=ntmp,b1=1,b2=1,b3=zmax,b4=1,b5=1,b6=1,
               b7=1,option="sum",>> logf)
	   print (" BLKAVG successful in sum over mask",>> logf)
	   imcopy(input=ntmp//"[*,*,1]",output=ntmp,v-)
           print (" IMCOPY successful to turn BLKAVG into 2D. ",>> logf)
	} else {
	   imcopy(nthr,ntmp,v-)
	}

# Set mask profile pixels to zero where nthresh < nmin

	mtmp = mktemp("mtmp")
        print( " MKTEMP successful.",>> logf)
	imcalc(input=ntmp,output=mtmp,equals="(im1.ge."//nm//")",v+,>> logf)
        print (" IMCALC successful. ",>> logf)
	imarith(operand1=msk,op="*",operand2=mtmp,result=msk,v+,>> logf)
        print (" IMARITH successful. ",>> logf)
	imdel(mtmp,go+,v-)
	imdel(ntmp,go+,v-)

# Add history records to update the mask file:

	print ("IRAF  Minimum number channels: "//nm) | 
		stfhistory(msk,"STDIN")

# Correct the mask for single outliers along the velocity axis.  These
# are profiles where a single channel survives thresholding when
# adjacent pixels (velocity channels) do not survive.  

# The first and last planes of the mask must be handled separately:
# Correct the first plane for mask profiles cases: {1,0,0} and {1,0,1}
# i.e., the first pixel survived alone, OR the first and third survived.  In
# each of these cases, we set the first pixel to zero because it
# constitutes a single outlier.

	print("OUTLIER CORRECTION OF MASK ...................",>> logf)

	otmp = mktemp("otmp")
        print( " MKTEMP successful.",>> logf)
	imcalc(output=otmp,input=msk//"[*,*,1],"//msk//"[*,*,2]",
            equals="im1-((im1.eq.1.)*(im2.eq.0.))",v+,>> logf)
        print (" IMCALC successful in eliminating outliers. ",>> logf)
	imcopy(input=otmp,output=msk//"[*,*,1]",v+)
	imdel(otmp,go+,v-)

# Correct the last plane for mask cases: {0,0,1} and {1,0,1}

 	otmp = mktemp("otmp")
        print( " MKTEMP successful.",>> logf)
	imcalc(output=otmp,
            input=msk//"[*,*,"//zmax-1//"],"//msk//"[*,*,"//zmax//"]",
            equals="im2-((im1.eq.0.)*(im2.eq.1.))",v+,>> logf)
        print (" IMCALC successful in eliminating outliers. ",>> logf)
	imcopy(input=otmp,output=msk//"[*,*,"//zmax//"]",v+)
	imdel(otmp,go+,v-)

# Correct for mask profile cases  {0,1,0}, that is, when any single 
# channel survived, but its neighbors both did not.

	zmtwo = zmax-2
        for (z=1; z<=zmtwo; z+=1)   {

           tmp = mktemp("tmp")
           print( " MKTEMP successful.",>> logf)
	   imcalc(output=tmp,equals="im2-((im1.eq.0.)*(im2.eq.1.)*(im3.eq.0))",
  input=msk//"[*,*,"//z//"],"//msk//"[*,*,"//z+1//"],"//msk//"[*,*,"//z+2//"]",
               v+)
           print (" IMCALC successful at eliminating outliers. ",>> logf)
	   imcopy(input=tmp,output=msk//"[*,*,"//z+1//"]",v+)
	   imdel(tmp,go+,v-)

	}

# COMPUTE NRCHAN  =====================================================

	print (" COMPUTING NUMBER OF SURVIVING CHANNELS.......",>> logf)

# Sum over the third dimension of the mask file:

	blkavg(input=msk,output=nrch,b1=1,b2=1,b3=zmax,b4=1,b5=1,b6=1,b7=1,
            option="sum",>> logf)
	print (" BLKAVG successful in sum over mask surviving channels",>> logf)
	imcopy(input=nrch//"[*,*,1]",output=nrch,v-)

# Add history records to update the nrchan file:

	print ("IRAF  Channel Map") | stfhistory(nrch,"STDIN")

# COMPUTE IMAP ========================================================

	print (" COMPUTING TOTAL INTENSITY MAP................",>> logf)

#  Imap is the total intensity of the input data cube over all velocity 
#  channels, for channels whose mask value is 1.  For those channels,
#  the intensity of the pixels in the input cube used for the mask 
#  lies above the threshold. 

# Apply the mask to the input data cube.
	
	imarith(operand1=im,op="*",operand2=msk,result=imsk,
            pixtype="real",calctype="real",v+,>> logf)
        print (" IMARITH successful computing total intensity map. ",>> logf)

# Add history records to update the masked intensity file:

	print ("IRAF  Masked Intensity Map") | stfhistory(imsk,"STDIN")
	print ("IRAF  Image file used: "//im) | stfhistory(imsk,"STDIN")
	print ("IRAF  Mask file used: "//msk) | stfhistory(imsk,"STDIN")

# Sum the image planes of the imask image together:

	blkavg(input=imsk,output=imap,b1=1,b2=1,b3=zmax,b4=1,b5=1,b6=1,b7=1,
            option="sum",>> logf)
	print (" BLKAVG successful in sum over mask channels",>> logf)
	imcopy(input=imap//"[*,*,1]",output=imap,v+)

# Multiply the final image by the summing interval, deltav, to complete 
# the sum.

	imarith(operand1=imap,op="*",operand2=abs(deltav),result=imap,
            pixtype="real",calctype="real",v+,>> logf)

# Add history records to update the imap file:

	print ("IRAF  Total Intensity Map") | stfhistory(imap,"STDIN")
	
	time	          	# print the finish time

	end

