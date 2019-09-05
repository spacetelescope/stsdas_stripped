#	 		     SMOOTH.CL
#			   -------------
#
# DESCRIPTION:  See the help file for user documentation.
#
# INPUT parameters:
#
#	* IMAGE		image cube
#	* SMOOTHV	switch to determine whether to smooth in velocity
#	* SMOOTHS	switch to determine whether to smooth in space
#	* KERNEL	if smoothv=yes, ascii file with seven-point 
#			smoothing function (default value "gauss.kern"
#			is a sample file containing the values from a 
#                       seven-point gaussian kernel.)
#	* NORM	 	if smoothv=yes, divisor for normalizing velocity
#			smoothed product
#	* FWHMI		if smooths=yes, this is the FWHM of the input image
#	* FWHMO		if smooths=yes, this is the spatially-smoothed
#			image FWHM.
# OUTPUT parameters:
#
#	* VSMOOTH	if smoothv=yes, velocity-smoothed image
#	* SMMAP		if smooths=yes, spatially-smoothed image
#	* LOGFILE       output message file (optional)
#
#
# NOTE:
#
#     In this version, the parameters "xaspp" and "yaspp" are assumed to 
#  be the same value; that is, the number of arcseconds per pixel in each 
#  direction are assumed to be the same.
#
#    This script assumes that the header parameters NAXIS1,CDELT1,and
#  CDELT2 are in the header of the input image.  Use the task "hedit"
#  to insert them if they are not already there.
#
#    This IRAF script was developed by Diane Gilmore and Ron Allen
# at the Space Telescope Science Institute in 1990.
#

      procedure smooth(image)
# ======================================================================

  file   image            {prompt="input data cube filename"}
  bool   smoothv = no     {prompt="smooth velocity profiles? [yes/no]"}
  bool   smooths = no     {prompt="perform spatial smoothing? [yes/no]"}
  file   vsmooth = ""     {prompt="output velocity smoothed image filename"}
  file   kernel = "gauss.kern" {prompt="velocity-smoothing kernel filename"}
  real   norm = 1.        {prompt="velocity normalization divisor"}
  real   fwhmi            {prompt="input map fwhm (arcseconds)"}
  real   fwhmo            {prompt="fwhm after spatial smooth (arcseconds)"}
  file   smmap = ""       {prompt="spatially-smoothed output cube filename"}
  file   logfile = ""     {prompt="filename for output messages"}
  struct *head            {prompt="Do Not enter a value"}

  begin

# Declare the intrinsic parameters:
 
	file   im		     # imput image cube
        file   smm		     # spatially-smoothed output image
	file   vsm		     # velocity-smoothed output image
	file   k		     # kernel file
	file   logf		     # message file
	file   outf		     # temporary file to store header values
	file   tmp,vtmp		     # temporary filenames
	real   sigout		     # sigma of the spatially smoothed image
        real   sigin		     # sigma of the input image
	real   sigsm  		     # sigma of the smoothing gaussian
	real   n		     # normalization constant for velocity smooth
	real   fwhmin		     # FWHM of the input image 
	real   fwhmout		     # FWHM of spatially smoothed image
	real   const		     # normalization factor for spatial smooth
	real   xaspp,yaspp,aspp	     # image scale header values (arcsec/pix)
	int    dimen                 # dimensionality of the input image 
	int    xmax,ymax,zmax        # image size header values 
 	int    z,x		     # counters
	bool   smv,sms               # switches to turn on/off velocity
				     # and spatial smoothing.

# Check to see that proper packages are loaded

	if (! defpac ("images")) {
		print ("iraf images package must be loaded...")
		bye
	}

	if (! defpac ("proto")) {
		print ("noao proto package must be loaded...")
		bye
	}

	if (! defpac ("tools")) {
		print ("stsdas tools package must be loaded...")
		bye
	}

# Get query parameters:
	
	im = image
	smv = smoothv
	vsm = vsmooth
	k = kernel
	n = norm
	sms = smooths
	fwhmin  = fwhmi
	fwhmout = fwhmo
	smm = smmap

 # send output to screen if no logfile given

	logf = logfile
	if(logf == "") {       
           logf="STDOUT"
	} 

# Get header parameters describing the size of the input file:

	set clobber=yes
	time                              # print the starting time
	outf = mktemp("outf")             # make temporary file
	hselect(im,"NAXIS,NAXIS[123]",exp=yes,>outf) # select header dimension keyword

	xmax = 1		          # initialize dimension values
	ymax = 1
	zmax = 1
	dimen = 0

# Read the temporary file outf using a list structured parameter.

	head = outf   			  # access the temporary file
	while (fscan(head,dimen,xmax,ymax,zmax) != EOF) {   #read it
	   print("dimensions = ",dimen,> logf)        
	   print("xmax = ",xmax,>> logf)
	   print("ymax = ",ymax,>> logf)
	   print("zmax = ",zmax,>> logf)
	   if(dimen != 3) {
	      error(1,"Image is not three dimensional")
	   }
	}
	
	delete(outf,v-)			  # delete temporary file
	outf = mktemp("outf")             # recreate temporary file
	hselect(im,"CDELT1,CDELT2",exp=yes,>outf)  # read header keywords
	xaspp = 0.0			  # initialize header values
	yaspp = 0.0

	head = outf			  # access the temporary file
	while(fscan(head,xaspp,yaspp) != EOF) {    # read it
	   xaspp = abs(xaspp)		  # take absolute value
	   yaspp = abs(yaspp)		
	   print("number of arcseconds per pixel in x and y:",
                  xaspp,yaspp,>> logf)
	   if(xaspp != yaspp) {		  # tests for spatial scales
	      error(1,"Image has unequal x and y spatial scales")
	   } else if((xaspp == 0.)||(yaspp == 0.)) { 
	      error(1,"Image has no CDELT2 and/or CDELT3 scale parameter")
	   } else {			 
	      aspp = xaspp*3600.0         # units in arcsec/pix
	   }
	} 
	delete(outf,v-)	

# DO VELOCITY SMOOTHING IN INPUT DATA CUBE =============================

	if (smv)  {
	   print (" SMOOTHING IN VELOCITY .............. ",>> logf)

# Create an empty vsm cube with the same header as the image

	   imcopy(im,vsm,v-)
	   imreplace(vsm,0.,0.)

# Loop to convolve one y-z plane at a time with the kernel, k.

   	   for (x=1; x<=xmax; x+=1)  {
	      if (access(k)) {
	         vtmp = mktemp("vtmp")
	         convolve(im//"["//x//",*,*]",vtmp,k)
	         imcopy(vtmp,vsm//"["//x//",*,*]",v-)
	         imdel(vtmp,go+,v-)
	      } else {
		 error(1,"Kernel file not found")
	      }
	   }

#  Normalize the smoothing function unless divisor = 1.
	
	   if (n != 1.) {
	      imarith(vsm,"/",n,vsm,pixtype="real",calctype="real",v-)
	   }
	;

# Add history record to output file:

	print ("IRAF  Velocity-smoothed image") | stfhistory(vsm,"STDIN")
	print ("IRAF  Smoothing function: "//kernel) | stfhistory(vsm,"STDIN")
	print ("IRAF  Velocity normalization factor: "//n) | 
                stfhistory(vsm,"STDIN")

  	}

# DO SPATIAL SMOOTHING ON A COPY OF THE INPUT IMAGE ====================

	if (sms)  {
	   print (" SMOOTHING IN SPACE ................ ",>> logf)

# Create an empty smmap with same header as image

	   imcopy(im,smm,v-)
	   imreplace(smm,0.,0.)

# Calculate the sigma of the smoothing function needed to produce
# an output smoothed cube with a sigma of `sigout'

	   sigout = (fwhmout/aspp)/2.35482        # in pixels
	   sigin  = (fwhmin/aspp)/2.35482 	  # in pixels
	   sigsm = sqrt(sigout**2 - sigin**2)     # in pixels

# Loop over the velocity dimension (z), and compute the smoothed image plane

	   for( z=1; z<=zmax; z+=1)   {

	      tmp = mktemp("tmp")

# use result of velocity smooth step if it was done, otherwise use the
# input image 

	      if (smv)  {
                 gauss(vsm//"[*,*,"//z//"]",tmp,sigsm,nsigma=4.,
                       boundary="reflect")
	      } else { 
                 gauss(im//"[*,*,"//z//"]",tmp,sigsm,nsigma=4.,
                       boundary="reflect")  
	      }
  	      imcopy(tmp,smm//"[*,*,"//z//"]",v-)
              imdel(tmp,go+,v-)       
	   
	   }

# Normalize to preserve the peak intensity (i.e. the flux density/beam)
# in the smoothed cube:

	   const = (fwhmout**2)/(fwhmin**2)
	   imarith(smm,"*",const,smm,calctype="real",pixtype="real",v-)

# Add history record to output file:
# BMAJ and BMIN are the FWHM in degrees, and BPA is set to zero for a
# circular output beam shape.

	   print ("IRAF  Spatially smoothed image") | stfhistory(smm,"STDIN")
	   hedit(smm,"BMAJ",(fwhmo/3600.),add+,del-,ver-,sh+,up+,>>logf)
	   hedit(smm,"BMIN",(fwhmo/3600.),add+,del-,ver-,sh+,up+,>>logf)
	   hedit(smm,"BPA",0.0,add+,del-,ver-,sh+,up+,>>logf)
#	   print ("IRAF  BMAJ="//(fwhmo/3600.)//"  BMIN="//(fwhmo/3600.) |
#                  stfhistory(smm,"STDIN")

	}
	;

	time  	                 

  end
