#COR_SHFT --- find shift by cross-correlating corrected image with blotted image
#

procedure cor_shft(inlist,group,outfile)

string	inlist	    {prompt="Image(s) for shift check"}
string  group       {prompt="Group being checked"}
char    outfile      {prompt="output file name"}
char    suffix       {"_cor",prompt="suffix for corrected image"}
char    units        {"counts",prompt="counts or cps"}
real    cps_cut      {0.,prompt="CPS cut for zeroing pixel"}
string  exp_wd      {"exptime",prompt="Exposure keyword"}
bool    sig2n       {no,prompt=">Estimate s/n of each pixel?"}
bool	verbose     {yes,prompt="Verbose output?"}

struct	*inimglist
struct  *list

begin

	string	ilist,infile,img,hold,hold2,outimg,imcalin,imcalout,inimg
        string tmp1,tmp2,tmp3,tmp4,tmp5,img0,grp,cos_var1,cos_var2,cos_var3
        string cor_img,cr_img,bl_img, name, exp_word
        struct line
	char imcals
        real mult1, mult2, back,adu, snr1, snr2, a1, a2, read, exptime
        real xsh, xerr, ysh, yerr, rot, xdsh, ydsh, cpsc, min_val
	int ii,jj,i,j, ig 
        file out
	bool do_sqrt

# Get query parameters.
	ilist = inlist
        grp = group
        out = outfile
        do_sqrt = sig2n
        cpsc = cps_cut
        exp_word=exp_wd
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


#	   print ("the image =",img)
#	   print ("the image =",img0)
# Main deriv loop:


	   tmp1 = mktemp("drz")
	   tmp2= mktemp("drz")
	   tmp3 = mktemp("drz")
	   tmp4 = mktemp("drz")
	   cor_img = img0//suffix
           bl_img = img0//"_bl"
           inimg = img//"["//grp//"]"
	   if (do_sqrt) {
              imgets(inimg,exp_word,mode="h")
              if (units == "counts") min_val = cpsc * real(imgets.value)
              else min_val = cpsc
	      imcals = "if (im1 <"//min_val//") then 0 else sqrt(im1)"
	      imcalc(cor_img,tmp3,imcals)
	      imcalc(bl_img,tmp4,imcals)
	   } else {
	     imcopy (cor_img,tmp3)
	     imcopy (bl_img,tmp4)
	   }
 
#	print (outimg)

           crossdriz.dinp=no
           crossdriz.dref=no
           crossdriz.maxtheta=0.
           crossdriz.mintheta=0.
	   crossdriz(tmp3,tmp4,tmp1)
           hedit (tmp1,"ORG_TITL",img0, add+, show-, update+, verify-)
           hedit (tmp1,"ORG_TITL",img0, add+, show-, update+, verify-)
           hedit (tmp1, "ORG_GRP", grp, add+, show-, update+, verify-)
           hedit (tmp1, "ORG_GRP", grp, add+, show-, update+, verify-)
 	   shiftfind(tmp1,tmp2)
           list=tmp2
           i=fscan(list,line)
           i = fscan (line, name, ig, xdsh, xerr, ydsh, yerr) 
           print ("delta offsets ",name," ", xdsh," ",ydsh)
	   imgets(tmp1,"BLOTXSH",mode="h")
	   xsh = xdsh + real(imgets.value)
	   imgets(tmp1,"BLOTYSH",mode="h")
	   ysh = ydsh + real(imgets.value)
	   imgets(tmp1,"BLOTROT",mode="h")
           rot = real(imgets.value)
           imgets(tmp1,exp_word,mode="h")
           exptime = real(imgets.value)
           printf ("%s %2d %8.2f %8.2f %8.3f %8.2f %7.3f %7.3f\n",name,grp, xsh,ysh,rot,exptime,xdsh,ydsh,>>out)
           imdel (tmp1)
	   imdel (tmp3)
	   imdel (tmp4)
           del (tmp2)
	}
del (infile)

end
