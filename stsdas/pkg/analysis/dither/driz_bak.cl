# DRIZ_CR  -- mask blemishes in dithered data by comparison of an image
#             with a model image and the derivative of the model image.
#
# V1.0     -- released June 1998
# V1.01    -- error in imcalc quotation fixed -- 30 June 1998

procedure driz_cr(inlist,group)

string	inlist	    {prompt="Image(s) for cosmic ray cleaning"}
string  group       {prompt="Group being cleaned"}
real    gain = 7       {prompt="Detector gain, e-/ADU"}
real    rn   = 5     {prompt="Read noise in electrons"}
char    SNR = "4.0 3.0" {prompt="signal-to-noise ratio"}
char    scale = "0.5 0.4" {prompt="scaling factor applied to the derivative"}
char    units = "counts" {prompt="counts or cps"}
string  backg = "backgrnd"    {prompt="Background header keyword"}
string  expkey = "exptime" {prompt="exposure time keyword"}
bool    corr        {no,prompt="Create corrected image?"}
bool	verbose     {yes,prompt="Verbose output?"}

struct	*inimglist

begin

	string	ilist,infile,img,hold,hold2,outimg,imcalin,imcalout,inimg
        string tmp1,tmp2,tmp3,tmp4,tmp5,img0,grp,cos_var1,cos_var2,cos_var3
        real mult1, mult2, back,adu, snr1, snr2, a1, a2, read, expmult
	int ii,jj,i,j

# Get query parameters.
	ilist = inlist
        grp = group
        adu = gain
        read = rn
        i = fscan(SNR, snr1, snr2 )
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


	   print ("the image =",img)
	   print ("the image =",img0)
# Main deriv loop:


	   tmp1 = mktemp("drz")
	   tmp2= mktemp("drz")
	   outimg = img0//"_cr"
#	print (outimg)
#      now prepare first string
          inimg = img//"["//grp//"]"
          imgets(inimg,backg)
          back = real(imgets.value)
          if (units == "counts") expmult=1
          else if (units == "cps") {
            imgets(inimg,expkey)
            expmult = real(imgets.value)
          } else print("units kewyord not recognized")
          cos_var1 = "if (abs(im1-im2) .gt. "("//mult1//" * im3 + "//snr1
          cos_var2 = "* sqrt("//adu//"*abs(im2 + "//back//")+"//read//"*"//read//")/"//adu//")/"//expmult then 0 else 1"
	  tmp4= mktemp("drz")
          print(cos_var1,cos_var2)
          print(cos_var1,cos_var2,>tmp4)
          imcalin = inimg//","//img0//"_bl,"//img0//"_bl_deriv"
#         print ("imcalin =",imcalin)
          cos_var1 = "if ((abs(im1-im2) .gt. "("//mult2//" * im3 + "//snr2
          cos_var2 = "* sqrt("//adu//"*abs(im2 + "//back//")+"//read//"*"//read//")/"//adu//")/"//expmult .and. (im4 .lt. 9)) then 0 else 1"
	  tmp5= mktemp("drz")
          print(cos_var1,cos_var2)
          print(cos_var1,cos_var2,>tmp5)
          imcalc (imcalin,tmp1,"@"//tmp4)
	  imcalout = img0//"_cr.pl"
          print ("done with first imcalc")
          convolve(tmp1,tmp2,"","1 1 1","1 1 1",bilin=yes,radsym=no)
          print ("done with convolve")
          imcalin = inimg//","//img0//"_bl,"//img0//"_bl_deriv,"//tmp2
#         print ("imcalin =",imcalin)
          imcalc (imcalin,imcalout,"@"//tmp5)
	  if (corr) 
	     imcalc(inimg//","//img0//"_cr,"//img0//"_bl",img0//"_cor","if (im2 .eq. 0) then im3 else im1")
          imdel (tmp1)
          imdel (tmp2)
	  del (tmp4)
	  del (tmp5)
	}
del (infile)

end
