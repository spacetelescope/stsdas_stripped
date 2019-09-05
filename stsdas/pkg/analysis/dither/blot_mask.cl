# 
# blot_mask -  Given a list of blotted images, this creates a mask
#              for each one based on regions where the sky is identically zero.
#              Also multiplies by a static mask.
# V1.0  release June 1998

procedure deriv(blotlist,static)


string  blotlist    {prompt="image files"}
string  static      {prompt="Static mask"}
bool	verbose     {yes,prompt="Verbose output?"}
#float   c_val       {0,prompt="CR val to ignore"}

struct	*inimglist
begin

	string	ilist,infile,img,hold,hold2,imcalc_in,in2file,junk
        string  smask,mlist,img_root
	int ii,jj

# Get query parameters.

	ilist = blotlist
	smask = static
# Expand file lists into temporary files.

	infile =  mktemp("tmp$inv")
	sections (ilist,option="fullname",>infile)
	inimglist = infile


# Loop through input files:

	while (fscan(inimglist,img) != EOF) {

# Strip extension off input file name.

		fparse(img)
		img_root = fparse.root
                junk = mktemp("tmp$inv")
                imcalc(img_root//"_bl",junk,"if (im1 .eq. 0.0) then 0 else 1")
		imarith (img_root//"_cr.pl","*",junk,img_root//"_cr.pl")
		imarith (img_root//"_cr.pl","*",smask,img_root//"_cr.pl")
		imdel (junk)
	}


end
