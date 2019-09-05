procedure sampinfo (image)

# Look at headers for individual IMSETs in an IR MULTIACCUM image
# and report useful information like the SAMPNUM, SAMPTIME, DELTATIM
#
# M. Dickinson, 5/26/99

file	image	{prompt="Image for extracting sample information"}

begin

	string	img, sampseq, msg
	int	i, nsamp, sampnum, nextend, nimsets
	real	exptime, samptime, deltatime

	# Get query parameter
	img = image

	# Make sure necessary packages are loaded
	if (!defpac("images")) images
	if (!defpac("imutil")) imutil

	# Check the OBSMODE keyword to make sure the image is a MultiAccum
	imgets (img//"[0]", "OBSMODE")
	if (imgets.value != "MULTIACCUM") {
	    msg = "Image " // img // " is not a MULTIACCUM data set."
	    error (1, msg)
	}

	# Read the NEXTEND, NSAMP, SAMP_SEQ, and EXPTIME keywords from
	# the primary header
	imgets (img//"[0]", "NEXTEND")
	nextend = int(imgets.value)
	imgets (img//"[0]", "NSAMP")
	nsamp = int(imgets.value)
	imgets (img//"[0]", "SAMP_SEQ")
	sampseq = imgets.value
	imgets (img//"[0]", "EXPTIME")
	exptime = real(imgets.value)

	# Make sure NSAMP and NEXTEND are consistent
	if (nsamp != nextend/5) {
	    print ("NOTE: Image ", img, " has fewer than NSAMP IMSETs,")
	    print ("and thus may not be a complete MULTIACCUM data sequence.")
	    print ("Reporting information only for the available IMSETs.")
	    print (" ")
	}

	nimsets = nextend / 5

	# Print information header lines
	printf ("IMAGE                NEXTEND   SAMP_SEQ   NSAMP   EXPTIME\n")
	printf ("%-18s     %3d     %8s     %2d    %.3f\n",
		img, nextend, sampseq, nsamp, exptime)
	printf ("IMSET   SAMPNUM    SAMPTIME   DELTATIME\n")

	# Loop over the IMSETS, reading and printing sample info
	for (i=1; i<=nimsets; i+=1) {
	     hselect (img//"[SCI,"//i//"]", "SAMPNUM,SAMPTIME,DELTATIM", yes) |	scan (sampnum, samptime, deltatime)
	     printf ("  %2d%9d%14.3f%12.3f\n", i, sampnum, samptime, deltatime)
	}

end

