procedure saolpr(frame)

int	frame	{1,prompt="Frame to dump"}

file	saocmap	{"",prompt="SAOImage color map"}
string	device	{"stdplot",prompt="Device to print image on"}
bool	append	{no,prompt="Append to previous plot"}
real	left 	{0.,prompt="Left edge of image"}
real	right 	{1.,prompt="Right edge of image"}
real	bottom 	{0.,prompt="Bottom edge of image"}
real	top 	{1.,prompt="Top edge of image"}
real	min	{INDEF,prompt="Minum value to display"}
real	max	{INDEF,prompt="Maximum value to display"}

begin
	file	igi_script
	file	image

	igi_script = mktemp ("tmp$saolpr_igi_script")
	image = mktemp ("tmp$saolpr_image")
	
	# Dump the image.
	dsimg (frame, image)

	# Now create the igi script.
	print ("zsection "//image, > igi_script)
	if (min != INDEF)
	    print ("zrange "//min//" "//max, >> igi_script)
	else
	    print ("zrange", >> igi_script)
	if (saocmap != "")
	    print ("saocmap "//saocmap, >> igi_script)
	print ("fitpix "//left//" "//right//" "//bottom//" "//top,
	       >> igi_script)
	print ("limits", >> igi_script)
	print ("pixmap", >> igi_script)

	# Now render it.
	igi (device=device, metacode="", append=append, < igi_script,
	     >& "dev$null")

	imdelete (image, verify=no, >& "dev$null")
	delete (igi_script, verify=no, >& "dev$null")
end
