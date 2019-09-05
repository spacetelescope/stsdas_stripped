#			      VELOCITY.CL
#			    ---------------
#
#   INPUT parameters:
#   	
#       * IMASK    masked intensity cube. 
#       * NRCHAN   plane containing the number of channels passed at each
#		   pixel (x,y).
#	* TIMAP    plane of total intensity at each pixel.
#	* VVECTOR  (optional) file of velocities for each velocity
#	           plane.
#       * MASKCORR switch to determnine whether to correct for profile
#		   masking.
#	* CORRECT  switch to determine whether to correct for velocity-
#                  smoothing.  
#	* VSIG     (optional) If correct = yes, this is the sigma of the
#		   velocity - smoothing gaussian.  	
#
#   OUTPUT parameters:
#	
#	* VMEAN    plane of intensity-weighted mean velocity at every pixel.
#	* VPEAK    plane of velocities at peak intensity of each profile
#	* VDISP	   plane of velocity dispersion at each pixel.
#	* LOGFILE  message file 
#
#
#   This IRAF script was developed by Diane Gilmore and Ron Allen at
#   the Space Telescope Science Institute in 1990.
#

	procedure velocity(imask,nrchan,timap,vpeak,vmean,vdisp)
#  ========================================================================

  file imask = ""      {prompt="input masked intensity cube filename"}
  file nrchan  = ""    {prompt="input corrected channel map filename"}
  file timap = ""      {prompt="input total intensity map filename"}
  file vpeak = ""      {prompt="output peak velocity map filename"}
  file vmean = ""      {prompt="output mean velocity map filename"}
  file vdisp = ""      {prompt="output velocity dispersion map filename"}
  file vvector = ""    {prompt="input velocity vector ascii filename \
                                (null string => will attempt to use header"}
  bool maskcorr = yes  {prompt="correct dispersion for masking on profile"}
  bool correct = no    {prompt="correct dispersion for gaussian \
				velocity smooth? [yes/no]"}
  real vsig = 0.       {prompt="sigma of velocity-smoothing gaussian"}
  file logfile = ""    {prompt="logfile (null string = screen)"}
  struct *head         {prompt="Do Not enter a value"}
  struct *list         {prompt="Do Not enter a value"}

  begin

# Declare intrinsic variables:

	file   imsk		# input masked intensity cube
	file   mx		# peak velocity cube
	file   mxtmp  		# temporary storage files
	file   imap		# input total intensity map
	file   vv		# velocity vector 
	file   vm		# mean velocity plane
	file   vd               # velocity dispersion plane
	file   vp		# peak velocity plane
	file   vtmp,gtmp,mtmp   # temporary velocity storage files
        file   dtmp,vdtmp,ptmp  # temporary velocity storage files
	file   v0d,v1d,v2d      # velocity storage files
	file   nrch		# channel map
	file   btmp,ctmp	# temporary storage files
	file   root1,roott	# dispersion images corrected for
				# masking in the velocity profiles
	file   logf		# message output file
	file   outf		# temporary file for storage of header data
	real   crval3,deltav	# velocity channel width parameters
	int    crpix3		# input file header parameter
	real   vel,velocity[100] # velocity vector values
        real   vs,vsigsq	# sigma of velocity-smoothing gaussian kernel
        real   azero,aone,atwo,az,azz  # temporary terms in masking correction
	int    zd               # size of input image along z (velocity)
	int    dimen,xmax,ymax,zmax    # input file header dimensions
	int    z		# counter
	bool   c		# switch to compute corection for velocity
				# smoothing
	bool   mc               # switch to compute correction to the
			        # dispersion for masking the profiles

	string expression	# long string used in imcalc


# Get query parameters:

	imsk = imask
	nrch = nrchan
	imap = timap
	vv = vvector
	vp = vpeak
	vm = vmean
	vd = vdisp
	vs = vsig
	c = correct
	mc = maskcorr
	logf = logfile                # if no logfile, send messages to screen
	if (logf == "") {
	   logf = "STDOUT"
	}

	time     		# print start time
	set clobber=yes         # overwrite existing ascii files

# Retrieve the image dimensions from the header.

	outf = mktemp("outf")	# create temporary file
	hselect(imsk,"NAXIS,NAXIS[123]",exp=yes,>outf)  # read header of im
	xmax = 1
	ymax = 1
	zmax = 1
	dimen = 0

# Read the temporary file outf using a list structured parameter.

	head = outf   			# access the temporary file
	while (fscan(head,dimen,xmax,ymax,zmax) != EOF) { # read it
	   print("dimensions of image = ",dimen,>logf)
	   print("xmax = ",xmax,>>logf)
	   print("ymax = ",ymax,>>logf)
	   print("zmax = ",zmax,>>logf)
	   if(dimen != 3) {		# check dimensions of im
	      error(1,"Image is not three dimensional",>>logf)
	   }
	}
	delete(outf,v-)

# Get velocity values supplied by the user (if not using header-supplied 
# parameters).

	if(vv != "") {
	   list = vv	  # access the list file with channel velocities
	   for(i=1; i<=zmax; i+=1) {	# begin loop over channel number
	      if(fscan(list,vel)==EOF) {  	# read list file 
	         error(1,"Not enough values in velocity file",>>logf)
	      } else {		  	
	      velocity[i] = vel		# set up velocity vector
	      }
 	   } 
           deltav = velocity[2] - velocity[1]                      
	} else {

# Get header velocity parameters to compute velocity of each plane
# if not using user-supplied values.

	   outf = mktemp("outf")	# create temporary file
	   hselect(imsk,"CDELT3",exp=yes,>>outf)  # header values to temp file
	   deltav = 0.0
	   head = outf			# access temporary file 
	   while(fscan(head,deltav) != EOF) {  # read temp file
	      deltav = deltav/1000.0           # in km/sec
	      print("deltav = ",deltav,>>logf)  
	   }
	   if(deltav == 0.0) {          # check that header value exists
	      print("Header parameter missing in image: CDELT3",>>logf)
	      print("Using deltav = 1 km/sec",>>logf)
	      deltav = 1.0                     #in km/sec
	   }
	   delete(outf,v-)

	   outf = mktemp("outf")    # create temp file
	   hselect(imsk,"CRPIX3",exp=yes,>>outf)	# header value to temp file
	   crpix3 = 0
	   head = outf			# access temp file
	   while(fscan(head,crpix3) != EOF) {   # read temp file
	      print("crpix3 = ",crpix3,>>logf)	
	   }
	   if(crpix3 == 0) {		# check that header value exists
	      error(1,"Header parameter missing in image: CRPIX3",>>logf)
	   }
	   delete(outf,v-)

	   outf = mktemp("outf")   	# create temp file
	   hselect(imsk,"CRVAL3",exp=yes,>>outf)  # header value to temp file
	   crval3 = 0.0
	   head = outf			# access temp file
	   while(fscan(head,crval3) != EOF) {  # read temp file
	      crval3 = crval3/1000.0           # in km/sec
	      print("crval3 = ",crval3,>>logf) 
	   }
	   if(crval3 == 0.0) {          # check that header value exists
	      error(1,"Header parameter missing in image: CRVAL3",>>logf)
	   }
	   delete(outf,v-)

	   for(i=1; i<=zmax; i+=1) {	# compute velocity vector
	      velocity[i] = real(i - crpix3) * deltav + crval3
	   }
 	}

	print("Velocity values:",>>logf) 
	for (i=1; i<=zmax; i+=1) {
           print(velocity[i],>>logf)
	}

# COMPUTE PEAK AND INTENSITY-WEIGHTED MEAN VELOCITIES =====================

	print("COMPUTE PEAK and MEAN VELOCITIES .................",>>logf)

# Compute the maximum intensity map (used to calculate the peak 
# velocity) by looping through the velocity planes and comparing
# two planes at a time.  At each pass, save the highest value at each
# (x,y) pixel in a temprorary file.

	mx = mktemp("mx")   		# create temp files
        imcalc(output=mx,equals="max(abs(im1),abs(im2))",
            input=imsk//"[*,*,1],"//imsk//"[*,*,2]",v-)
                      			# compare the first two planes

        for( z=3; z<=zmax; z+=1)   {    # loop through all other planes
	   mxtmp = mktemp("mxtmp")
	   imcalc(output=mxtmp,equals="max(abs(im1),abs(im2))",
               input=mx//","//imsk//"[*,*,"//z//"]",v-)
	   imdel(mx,go+,v-)
	   mx = mktemp("mx")
	   imcopy(mxtmp,mx,v-)
	   imdel(mxtmp,go+,v-)
	}


# Compute the velocity at the peak intensity.  Loop throough the
# velocity channels and find where the intensity is peak.  At that
# point, multiply by the corresponding velocity.

	imcopy(imsk,vp,v-)		# create empty image file with
	imreplace(images=vp,value=0.,imaginary=0.) 	# same header as image.
	imcopy(imsk,vm,v-)		# create empty image file with
	imreplace(images=vm,value=0.,imaginary=0.) 	# same header as image.

        for( z=1; z<=zmax; z+=1)   {

	   ptmp = mktemp("ptmp")        # create temp file	        
           imcalc(output=ptmp,equals="(abs(im1).eq.im2)*(im3.ne.0.)",
               input=imsk//"[*,*,"//z//"],"//mx//","//nrch,v-)
                                        # locate peak intensity pixels
	   vel = velocity[z]		# get channel velocity
	   imarith(operand1=ptmp,op="*",operand2=vel,result=ptmp,pixtype="real",
                  calctype="real",v+,>>logf)    # multiply by velocity 
	   imcopy(ptmp,vp//"[*,*,"//z//"]",v-)
	   imdel(ptmp,go+,v-)

# Weight the velocity by intensity, avoiding channels which have been
# masked out (i.e. use the masked intensity cube for intensity-weighting):


	   mtmp = mktemp("mtmp")
 	   imarith(operand1=imsk//"[*,*,"//z//"]",op="*",operand2=vel,
               result=mtmp,pixtype="real",calctype="real",v+,>>logf)
	   imcopy(mtmp,vm//"[*,*,"//z//"]",v-)
	   imdel(mtmp,go+,v-)

	}

	imdel(mx,go+,v-)

# Form the peak velocity product:

        blkavg(input=vp,output=vp,b1=1,b2=1,b3=zmax,b4=1,b5=1,b6=1,b7=1,
            option="sum")   # sum through profiles
	imcopy(vp//"[*,*,1]",vp,v-)		      # collapse to a plane

# Add history records to the header of the peak velocity map:

	print ("IRAF  Map of Velocity at Peak Intensity") |
		stfhistory(vp,"STDIN")

# Form the mean velocity product:

	blkavg(input=vm,output=vm,b1=1,b2=1,b3=zmax,b4=1,b5=1,b6=1,b7=1,
            option="sum")   # sum through profiles
	imcopy(vm//"[*,*,1]",vm,v-)		      # collapse to a plane

# Multiply the final image by the summing interval, deltav, to complete 
# the sum. Divide by the total intensity map to normalize.

	imarith(operand1=vm,op="*",operand2=abs(deltav),result=vm,
            pixtype="real",calctype="real",v+,>>logf)
	imarith(operand1=vm,op="/",operand2=imap,result=vm,
            calctype="real",pixtype="real",v+,>>logf)

# Add history records to the header of the mean velocity map:

	print("IRAF  Mean Velocity Map") | stfhistory(vm,"STDIN")

	
# COMPUTE THE VDISP PRODUCT ============================================

	print("COMPUTING VELOCITY DISPERSION ................",>>logf)

# Compute the velocity for every velocity plane, and use it to compute
# the dispersion in velocity.  The residual in velocity is computed one
# plane at a time, and then the planes are summed:

	imcopy(imsk,vd,v-)		      # make 3-D empty image with same
	imreplace(images=vd,value=0.,imaginary=0.) # header as input image cube

        for( z=1; z<=zmax; z+=1)   {
	   vel = velocity[z]		      # get channel velocity
	   dtmp = mktemp("dtmp")
	   imcalc(output=dtmp,equals="im2*sqr(im1-"//vel//")",
               input=vm//","//imsk//"[*,*,"//z//"]",v-)
	   imcopy(dtmp,vd//"[*,*,"//z//"]",v-)
	   imdel(dtmp,go+,v-)
	}
	
	blkavg(input=vd,output=vd,b1=1,b2=1,b3=zmax,b4=1,b5=1,b6=1,b7=1,
            option="sum")
	imcopy(vd//"[*,*,1]",vd,v-)

# Multiply the final image by the summing interval, deltav, to complete 
# the sum.  Then divide by the total intensity map to normalize, and
# take the square root to calculate dispersion.

	imarith(operand1=vd,op="*",operand2=abs(deltav),result=vd,
            pixtype="real",calctype="real",v+,>>logf)
	imarith(operand1=vd,op="/",operand2=imap,result=vd,
            pixtype="real",calctype="real",v+,>>logf)

        vdtmp = mktemp("vdtmp")
	imcalc(output=vdtmp,equals="sqrt(im1)",input=vd,v-)
	imdel(vd,go+,v-)	
	imcopy(vdtmp,vd,v-)
	imdel(vdtmp,go+,v-)

# Add history records to the header of the output dispersion map:
	
	print ("IRAF  Velocity Dispersion Map") | stfhistory(vd,"STDIN")


# CORRECT VDISP FOR MASKING  ==========================================
# In this section the velocity dispersion is corrected for profile masking.
# The ratio of the uncorrected dispersion to the intrinsic dispersion
# has been determined over a range of values of the ratio of the number of 
# surviving channels (nrchan) to the intrinsic dispersion.  This correction
# curve can be fit well by a parabola with coefficients listed below.  
# The solution for intrinsic dispersion as a function of nrchan and 
# uncorrected dispersion is a quadratic whose roots are computed in this
# section.  The correction is then applied to the dispersion values
# where enough channels remain to evaluate the profile width with some
# certainty.

# The dispersion and nrchan must both be in pixels.

	if (mc) {
	   print("  CORRECT VDISP FOR MASKING ..................",>>logf)

# Coefficients for terms in the quadratic root:

	   azero = -0.00810445
	   aone  =  0.3278082
	   atwo  = -0.02680988
	   az = 4.*azero
	   azz = 2.*azero

# Convert the uncorrected dispersion to units of pixels:

	   v0d = mktemp("v0d")
	   imarith(operand1=vd,op="/",operand2=abs(deltav),result=v0d,
               pixtype="real",calctype="real",v+,>>logf)
	   imdel(vd,go+,v-) 		   # keep this

# Compute quadratic terms:

	   btmp = mktemp("btmp")
	   imarith(operand1=nrch,op="*",operand2=aone,result=btmp,
               pixtype="real",calctype="real",v+,>>logf)
	   imarith(operand1=v0d,op="-",operand2=btmp,result=btmp,
               pixtype="real",calctype="real",v+,>>logf)

	   ctmp = mktemp("ctmp")
	   imcalc(output=ctmp,equals="sqr(im1)",input=nrch,v-)
	   imarith(operand1=ctmp,op="*",operand2=atwo,result=ctmp,
               pixtype="real",calctype="real",v+,>>logf)
	   imarith(operand1=ctmp,op="*",operand2=az,result=ctmp,
               pixtype="real",calctype="real",v+,>>logf)

# Compute the positive quadratic root which represents the corrected velocity
# dispersion:

	   root1 = mktemp("root1")
	   roott = mktemp("roott")
	   imcalc(output=roott,equals="sqr(im1)-im2",
               input=btmp//","//ctmp,v+,>>logf)
	   imcalc(output=root1,equals="if im1<=0. then 0. else (im2+sqrt(im1))",
                  input=roott//","//btmp,v+,>>logf)
#	   imcalc(root1,"(im1+sqrt(sqr(im1)-im2))",btmp//","//ctmp,v-)
	   imarith(operand1=root1,op="/",operand2=azz,result=root1,
               pixtype="real",calctype="real",v+,>>logf)
	
# Compute the correction factor sigma/sigma_prime

#	   corfac = mktemp("corfac")
#	   imcalc(corfac,"(im1-im2)/im2",root1//","//v0d,v-)
	   
# Set vdisp = 0 at pixels where the uncorrected dispersion is poorly
# determined...(i.e. where the number of channels <= twice the
# uncorrected profile width.  For these profiles, the dispersion
# is ill-determined.)

           v1d = mktemp("v1d")
           imcalc(output=v1d,equals="im2*((im1/im2).ge.2.)",
               input=nrch//","//v0d,v-)

# Compute the ratio map of sigma_prime/sigma.

	   v2d = mktemp("v2d")
           imarith(operand1=v1d,op="/",operand2=root1,result=v2d,
               pixtype="real",calctype="real",v+,>>logf)

# Set vdisp = correcrted dispersion value if 0 > ratio > 1, otherwise
# leave it uncorrected.

   	   expression = "(im1*((im3>0.).and.(im3<=1.)))+"//
                        "(im2*((im3<=0.).or.(im3>1.)))"
	   imcalc(output=vd,equals=expression,
               input=root1//","//v1d//","//v2d,v-)

           imdel(v0d,go+,v-)
	   imdel(v1d,go+,v-)
	   imdel(v2d,go+,v-)

# Convert vd back to km/sec.

	   imarith(operand1=vd,op="*",operand2=abs(deltav),result=vd,
               pixtype="real",calctype="real",v+,>>logf)

	   imdel(btmp,go+,v-)
	   imdel(ctmp,go+,v-)
 	   imdel(root1,go+,v-)
	   imdel(roott,go+,v-)

# Add history records to the header of the output dispersion map:

	   print ("IRAF  Corrected for profile masking ") | 
		   stfhistory(vd,"STDIN")

	}

# CORRECT VDISP FOR GAUSSIAN VELOCITY SMOOTHING =======================

# Take the square root to find the true velocity dispersion:

	if (c)  {
	   print("  CORRECT VDISP FOR SMOOTHING ..............",>>logf)
	   vs = vs * deltav
	   vsigsq = vs * vs
     	   gtmp = mktemp("gtmp")
	   imcalc(output=gtmp,equals="sqrt(sqr(im1) - "//vsigsq//")",
               input=vd,v-)
	   imdel(vd,go+,v-)		#Put in as a test
	   imcopy(gtmp,vd,v-)
	   imdel(gtmp,go+,v-)

# Add history records to the header of the output dispersion map:

	   print ("IRAF  Corrected for velocity smoothing") | 
 		   stfhistory(vd,"STDIN")
	}

	time	            # print end time

  end

