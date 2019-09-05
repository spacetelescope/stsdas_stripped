# DRIZ_CR  -- mask blemishes in dithered data by comparison of an image
#             with a model image and the derivative of the model image.
#
# V1.0     -- released June 1998
# V1.01    -- error in imcalc quotation fixed -- 30 June 1998
# V1.02    -- updated to handle cps   --  26 Oct 1998
# V1.03    -- added ability to weight using flatfield  -- A. Fruchter
# V1.04    -- explicitly specify the ".pl" extension in the last imcalc, 
#		and imtype for _bl and _bl_deriv files.  -- JC Hsu, 21 Jan 2000
# V1.05    -- added ability to change suffixes for input and output files  --  26 Jun 2000
# V1.06    -- updated calls to imdel to explicitly turn of verification

procedure driz_cr(inlist,group)

string	inlist	    	{prompt="Image(s) for cosmic ray cleaning"}
string  group       	{prompt="Group being cleaned"}
real    gain = 7    	{prompt="Detector gain, e-/ADU"}
real    rn   = 5    	{prompt="Read noise in electrons"}
char    SNR = "4.0 3.0" {prompt="signal-to-noise ratio"}
char    scale = "0.5 0.4"   {prompt="scaling factor applied to the derivative"}
char    units = "counts"    {prompt="counts or cps"}
string  backg = "backgrnd"  {prompt="Background header keyword"}
string  expkey = "exptime"  {prompt="exposure time keyword"}
bool    corr        	{no,prompt="Create corrected image?"}
bool    flat            {no, prompt="weight statistics by flat field image?"}
char    flt_im = ""     {prompt="flat field image name"}
char	bl_suffix = "_bl"          {prompt="Suffix for input blotted image"}
char	deriv_suffix = "_bl_deriv" {prompt="Suffix for output derivative image"}
char	cr_suffix = "_cr"          {prompt="Suffix for output cosmic ray pixel mask"}
char	cor_suffix = "_cor"        {prompt="Suffix for corrected image"}
bool	verbose     	{yes,prompt="Verbose output?"}

struct	*inimglist

begin

	string	ilist,infile,img,outimg,imcalin,imcalout,inimg
        string 	tmp1,tmp2,tmp3,tmp4,tmp5,img0,grp,cos_var1,cos_var2
	string	imtp, bl, bl_deriv
        real 	mult1, mult2, back,adu, snr1, snr2, a1, a2, read, expmult
	int 	i,j

	# get the image type
	show ("imtype") | scan (imtp)

	# Get query parameters.
	ilist = inlist
        grp = group
        adu = gain
        read = rn
        i = fscan(SNR, snr1, snr2)
        j = fscan(scale, mult1, mult2)

	# create cosmic.calc
        
	# Expand file lists into temporary files.
	infile =  mktemp("tmp$deriv")
	sections (ilist,option="fullname",>infile)
	inimglist = infile

	# Loop through input files:
	while (fscan(inimglist,img) != EOF) {

	    # Strip extension off input file name.

	    fparse (img)
	    img0 = fparse.root
	    if (verbose) print ("Working on ",img )

	    # Main deriv loop:
	    tmp1 = mktemp("drz")+"."+imtp
	    tmp2 = mktemp("drz")+"."+imtp
	    bl = img0+bl_suffix+"."+imtp
	    bl_deriv = img0+deriv_suffix+"."+imtp
	    outimg = img0+cr_suffix+"."+imtp

	    # now prepare first string
            inimg = img+"["+grp+"]"
            imgets(inimg,backg)
            back = real(imgets.value)
            if (units == "counts") 
		expmult=1
            else if (units == "cps") {
            	imgets(inimg,expkey)
            	expmult = real(imgets.value)
            } else 
		print("units kewyord not recognized")

	    # If there is a flat field image:
            if (flat) {
            cos_var1 = "if (abs(im1-im2) .gt. "//mult1//" * im3 + ("//snr1
            cos_var2 = "*sqrt(im4)* sqrt("//adu//"*abs(im2*"//expmult//" + "//back//"*"//expmult//")+im4*"//read//"*"//read//")/"//adu//")/"//expmult//") then 0 else 1"
	    tmp4 = mktemp("drz")

            print(cos_var1,cos_var2, > tmp4)
            imcalin = inimg//","+bl+","+bl_deriv+","+flt_im

            cos_var1 = "if ((abs(im1-im2) .gt. "//mult2//" * im3 + ("//snr2
            cos_var2 = "*sqrt(im5)* sqrt("//adu//"*abs(im2*"//expmult//" + "//back//"*"//expmult//")+im5*"//read//"*"//read//")/"//adu//")/"//expmult//") .and. (im4 .lt. 9)) then 0 else 1"
	    tmp5 = mktemp("drz")

            print(cos_var1, cos_var2, > tmp5)
            imcalc (imcalin, tmp1, "@"//tmp4, verb-)

	    imcalout = img0+cr_suffix+".pl"
            convolve(tmp1, tmp2, "", "1 1 1", "1 1 1", bilin=yes, radsym=no)

            imcalin = inimg+","+bl+","+bl_deriv+","+tmp2+","+flt_im
            imcalc (imcalin, imcalout, "@"//tmp5, verb-)

	    if (corr) {
	     	s1 = inimg+","+img0+cr_suffix+".pl,"+bl
	     	imcalc(s1,img0//cor_suffix,"if (im2 .eq. 0) then im3 else im1", 
			verb-)
	    }

	    # If not using a flat field image:
            } else {
            cos_var1 = "if (abs(im1-im2) .gt. "//mult1//" * im3 + ("//snr1
            cos_var2 = "* sqrt("//adu//"*abs(im2*"//expmult//" + "//back//"*"//expmult//")+"//read//"*"//read//")/"//adu//")/"//expmult//") then 0 else 1"
	    tmp4 = mktemp("drz")

            print(cos_var1, cos_var2, > tmp4)
            imcalin = inimg//","+bl+","+bl_deriv

            cos_var1 = "if ((abs(im1-im2) .gt. "//mult2//" * im3 + ("//snr2
            cos_var2 = "* sqrt("//adu//"*abs(im2*"//expmult//" + "//back//"*"//expmult//")+"//read//"*"//read//")/"//adu//")/"//expmult//") .and. (im4 .lt. 9)) then 0 else 1"
	    tmp5 = mktemp("drz")

            print(cos_var1, cos_var2, > tmp5)
            imcalc (imcalin, tmp1, "@"//tmp4, verb-)

	    imcalout = img0+cr_suffix+".pl"
            convolve(tmp1, tmp2, "", "1 1 1", "1 1 1", bilin=yes, radsym=no)

            imcalin = inimg+","+bl+","+bl_deriv+","+tmp2
            imcalc (imcalin, imcalout, "@"//tmp5, verb-)

	    if (corr) {
	     	s1 = inimg+","+img0+cr_suffix+".pl,"+bl
	     	imcalc(s1,img0//cor_suffix,"if (im2 .eq. 0) then im3 else im1", 
			verb-)
	    }

            }
            imdel (tmp1, verify=no, go_ahead=yes)
            imdel (tmp2, verify=no, go_ahead=yes)
	    del (tmp4)
	    del (tmp5)
	}

	del (infile)
end
