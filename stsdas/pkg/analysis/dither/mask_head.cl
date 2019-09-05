# MASK_HEAD - Given a list of images and masks, this task place the name of the
#             of the corresponding mask in the image and converts the mask
#             to a pl file, inverting the mask values if requested.
#             This task is primarily a response to imcombine, which cannot
#             take a list of masks, but instead requires the mask name
#             to be included as an image header keyword
#
# V1.0  release June 1998
# V1.1  Use the original mask file names in imcalc, instead of using the 
#	root name, also clean up the code.  JC Hsu, Jan 19, 2000

procedure mask_head (inlist, masklist)

string	inlist		{prompt="Image names"}
string  masklist	{prompt="Mask names"}
bool    invert		{yes, prompt="invert masks?"} 
bool	verbose		{yes, prompt="Verbose output?"}

struct	*inimglist
struct  *inmasklist
begin

	string	ilist, infile, img, in2file
        string  mask, mlist, img_root, mask_pl
        bool    verb

	# Get query parameters.
        verb = verbose
	ilist = inlist
	mlist = masklist

	# Expand file lists into temporary files.
	infile =  mktemp("tmp$inv")
	sections (ilist, option="fullname", > infile)
	inimglist = infile

	in2file =  mktemp("tmp$inv")
	sections (mlist, option="fullname", > in2file)
	inmasklist = in2file

	# Loop through input files:
	while (fscan(inimglist, img) != EOF) {

	    # Strip extension off input file name.
	    fparse(img)
	    img_root = fparse.root
            if (fscan(inmasklist, mask) == EOF){
            	print ("ran out of masks!")
                exit
            } else {
                fparse(mask)
                mask_pl=fparse.root//".pl"

                if (invert) 
		    imcalc(mask, mask_pl, "if (im1 .gt. 0) then 0 else 1",ver-)
                else
		    imcalc(mask, mask_pl, "if (im1 .gt. 0) then 1 else 0",ver-)
	    	hedit (img_root, "BPM", mask_pl, add+, ver-, show-)
	    }
	    if (verb)
                print ("mask_head: working on ", img)
	}
end
