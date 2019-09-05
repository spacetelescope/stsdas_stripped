procedure	sampcum (image, outimage, zeroimage, template)

# Accumulate image from a set of sample differences.
#
# Mark Dickinson
# Howard Bushouse, 7 May 2003: Modified to load all necessary packages

string	image		{prompt="First image"}
string	outimage	{prompt="Output image"}
string	zeroimage	{prompt="Image to use for zero read of output image"}
string	template	{prompt="Image to use as template for output image"}

begin

	string	im, outim, zim, tim, tmpim
	string	timext, outimext
	int	nexim, nextim
	int	i

	if (!defpac("images")) images
	if (!defpac("imutil")) imutil
	if (!defpac("stsdas")) stsdas motd-
	if (!defpac("toolbox")) toolbox
	if (!defpac("tools")) tools

	im = image
	outim = outimage
	zim = zeroimage
	tim = template

	tmpim = mktemp("sampcum")

	imgets (im//"[0]","NEXTEND")
	nexim = int(imgets.value) / 5
	imgets (tim//"[0]","NEXTEND")
	nextim = int(imgets.value) / 5
	if (nexim+1 != nextim)
	    error (0,"ERROR:  Mismatched number of extensions.")

	fparse (tim, verbose-)
	timext = fparse.root // ".fits"
	fparse (outim, verbose-)
	outimext = fparse.root // ".fits"
	copy (timext, outimext)

	imcopy (zim, outim//"[sci,"//nextim//"][*,*]")

	for (i=nexim; i>=1; i-=1) {

	     imarith (outim//"[sci,"//i+1//"]", "+", im//"[sci,"//i//"]", tmpim,
		      title="", hparams="", pixtype="", calctype="", noact-)
	     hedit (tmpim//"[0]", "NEXTEND", add-, delete+, ver-, show-, up+)
	     hedit (tmpim//"[0]", "EXTEND",  add-, delete+, ver-, show-, up+)
	     imcopy (tmpim, outim//"[sci,"//i//"][*,*]")
	     imdelete (tmpim, ver-, >& "dev$null")
		
	}


end
