# DERIV  --  create the derivative image of an input, or more precisely
#            compute the difference of each pixel with the four pixels
#            sharing a side and save the largest absolute value
#
# V1.0 Release June 1998
# V1.1 	Attach the imtype extension to the output file of imarith.  With no
#	specified extension it will default to hhh.  JC Hsu, Jan 19, 2000.

procedure deriv(inlist)

string	inlist	    {prompt="Image(s) for cosmic ray cleaning"}
bool	verbose     {yes, prompt="Verbose output?"}

struct	*inimglist

begin

	string	ilist, infile, img, hold, outimg, imtp
#	string	hold2, imcalc_in
	int ii

	# get imtype
	show ("imtype") | scan (imtp)

	# Get query parameters.
	ilist = inlist

	# Expand file lists into temporary files.
	infile =  mktemp("tmp$deriv")
	sections (ilist, option="fullname", > infile)
	inimglist = infile

	# Loop through input files:
	while (fscan(inimglist, img) != EOF) {

	    # Strip extension off input file name.
	    fileroot (img, validim+)
	    img = fileroot.root
	    if (verbose) print ("deriv: Working on ", img)

	    # Main deriv loop:
	    outimg = img+"_deriv."+imtp
	    imarith(img,"*","0",outimg);
	    for (ii=-1; ii<=1; ii+=2) {
	    	hold = mktemp("tmp$deriv")+"."+imtp
		imshift(img,hold,ii,0)
		imarith(img,"-",hold,hold)
#	        imcalc_in = outimg//","//hold
#		imcalc(imcalc_in,hold2,"max(im1,abs(im2))")
		imfunction(hold,hold,"abs",verbose-)
		imarith(outimg,"max",hold,outimg)
		imdel(hold)
            }

	    for (ii=-1; ii<=1; ii+=2) {
	    	hold = mktemp("tmp$deriv")+"."+imtp
		imshift(img,hold,0,ii)
		imarith(img,"-",hold,hold)
#	        imcalc_in = outimg//","//hold
#		imcalc(imcalc_in,hold2,"max(im1,abs(im2))")
		imfunction(hold,hold,"abs",verbose-)
		imarith(outimg,"max",hold,outimg)
		imdel(hold)
            }
	}

	del(infile)
end
