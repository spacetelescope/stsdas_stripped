procedure	sampdiff (image1, outimage)

# Take the difference between consecutive samples in a NICMOS MULTIACCUM image.
# Ordinarily one uses this with image1 = image2, i.e. to take the
# difference between samples 1 and 2, 2 and 3, 3 and 4 ... n-1 and n.
# However two separate images may also be input.
#
# Mark Dickinson
#
# Last modified 6 Sept 98 to make image2 a hidden parameter, since it is 
# almost always set equal to image1.
#
# Howard Bushouse, 7 May 2003: Modified to load all necessary packages.

string	image1		{prompt="First image"}
string	outimage	{prompt="Output image"}
string	image2		{"", prompt="Second image (default is same as first image)"}

begin

	string	im1, im2, outim
	int	nex1, nex2
	int	i
	string	exlist1, exlist2

	if (!defpac("images")) images
	if (!defpac("imutil")) imutil
	if (!defpac("stsdas")) stsdas motd-
	if (!defpac("toolbox")) toolbox
	if (!defpac("imgtools")) imgtools
	if (!defpac("mstools")) mstools

	exlist1 = ""
	exlist2 = ""

	im1 = image1
	im2 = image2
	if (im2=="") {
	    im2 = im1
	    print ("Taking first sample differences of images: ",im1)
	} else {
	    print ("Taking sample differences between different images: ",
		   im1," and ",im2)
	}
	outim = outimage

	imgets (im1//"[0]","NEXTEND")
	nex1 = int(imgets.value) / 5
	imgets (im2//"[0]","NEXTEND")
	nex2 = int(imgets.value) / 5
	if (nex1 != nex2) error (0,"ERROR:  Mismatched number of extensions.")

	for (i=1; i<nex1-1; i+=1) {
		exlist1 = exlist1 // i // ","
		exlist2 = exlist2 // i+1 // ","
	}
	exlist1 = exlist1 // nex1-1
	exlist2 = exlist2 // nex1

	msarith (im1, "-", im2, outim, list1=exlist1, list2=exlist2, crate-)

end
