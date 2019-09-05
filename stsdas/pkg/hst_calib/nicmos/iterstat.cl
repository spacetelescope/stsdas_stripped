procedure iterstat(image)

# Script to find image statistics excluding deviant pixels
# 4 August 1992 by John Ward
# Minor modifications 4 August 1992 MD
# Various subsequent variations.
# Latest revision:  18 Aug 1993 MD
# Modifications for public release: Howard Bushouse, 13 April 1999

string	image	{prompt="Input image(s)"}
real	nsigrej	{5.,min=0.,prompt="Number of sigmas for limits"}
int	maxiter	{10,min=1,prompt="Maximum number of iterations"}
bool	print	{yes,prompt="Print final results?"}
bool	verbose	{yes,prompt="Show results of iterations?"}
real	lower	{INDEF,prompt="Initial lower limit for data range"}
real	upper	{INDEF,prompt="Initial upper limit for data range"}
int	npix	{prompt="Returned value of npix"}
real	mean	{prompt="Returned value of mean"}
real	sigma	{prompt="Returned value of sigma"}
real	median	{prompt="Returned value of median"}
real	valmode	{prompt="Returned value of mode"}
#          	Must be "valmode" to avoid conflict w/ omnipresent task 
#		parameter "mode" 
struct	*inimglist

begin

	string	imglist		# equals image
	string	infile		# temporary list for files
	string  img		# image name from fscan
	real	mn		# mean from imstat
	real	sig		# stddev from imstat
	real	med		# midpt from imstat
	real	mod		# mode from imstat
	real	ll		# lower limit for imstat
	real	ul		# upper limit for imstat
	int	nx, npx		# number of pixels used
	int	m		# dummy for countdown

	# Get query parameter
	imglist = image

	# Load necessary packages
	if (!defpac("images")) images
	if (!defpac("imutil")) imutil

	# Expand file lists into temporary files.
	infile =  mktemp("tmp$iterstat")
	sections (imglist,option="fullname",>infile)
	inimglist = infile

	# Loop through images
	while (fscan(inimglist,img) != EOF) {

	       # Compute image statistics
	       imstat(img,fields="mean,stddev,npix,midpt,mode",lower=lower,
		      upper=upper,format-) | scan(mn,sig,npx,med,mod)

	       if (verbose)
		   print (img,": mean=",mn," rms=",sig," npix=",npx,
			  " median=",med," mode=",mod)

	       # Loop over rejection cycles
	       m = 1
	       while (m <= maxiter)  {

		      ll = mn - (nsigrej*sig)
		      ul = mn + (nsigrej*sig)
		      if (lower != INDEF && ll < lower) ll = lower
		      if (upper != INDEF && ul > upper) ul = upper
		      imstat(img,fields="mean,stddev,npix,midpt,mode",lower=ll,
			     upper=ul,format-) | scan(mn,sig,nx,med,mod)

		      if (nx == npx)
			  break

		      if (verbose)
			  print (img,": mean=",mn," rms=",sig," npix=",nx,
				 " median=",med," mode=",mod)

		      npx = nx
		      m = m + 1
	       }

	       if (print && !verbose) 
		   print (img,": mean=",mn," rms=",sig," npix=",nx," median=",
			  med," mode=",mod)

	       # Store results in output parameters
               npix = npx
	       mean = mn
	       sigma = sig
	       median = med
	       valmode = mod

	} # End of loop over images

	# Clean up temp files
	delete (infile, verify-, >& "dev$null")
	inimglist = ""

end

