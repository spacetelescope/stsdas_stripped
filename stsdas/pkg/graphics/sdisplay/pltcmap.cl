procedure pltcmap (cmap)

#  pltcmap -- Plot the color map.  The color map is in the Celco form
#  -- an image (3x256).  igi is used to do the plot.  Optionally print
#  the igi script before executing it.

#  3 December 1992 ZGL
#  28 July 1993  Move to graphics.stplot and add test for stplot package.

file	cmap	{"", prompt = "Color map image file"}

string	device	{"stdgraph", prompt="graphics device"}
bool	fullrange {no, prompt="Does the colormap fill the range?"}
bool	prtigi	{no, prompt = "Print igi commands?"}

begin
	file	cmapim
	file	igicmd
	int	i2

	# Make sure the stplot package is loaded for igi.
	if( !defpac("stplot")) {
	    print( "Error: stplot package needs to be loaded!" )
	    bye
	}

	if (fullrange) {
	    i2 = 256
	} else {
	    i2 = 218
	}

	cmapim = cmap

	# Use a temporary file for igi commands
	igicmd = mktemp ("tmp$scic")

	# Build the igi command file
	print ("LIMITS 0 "+i2+1+" -.05 1.05", > igicmd)
	print ("LOCATION 0.1 0.98 0 1", >> igicmd)

	print ("ysection "+cmapim+"[1:"+i2+",1]", >> igicmd)
	print ("VPAGE 0 1 .1 .375", >> igicmd)
	print ("BOX", >> igicmd)
	print ("ylabel Red", >> igicmd)
	print ("color 1", >> igicmd)
	print ("CONNECT", >> igicmd)

	print ("color 0", >> igicmd)
	print ("ysection "+cmapim+"[1:"+i2+",2]", >> igicmd)
	print ("VPAGE 0 1 .375 .65", >> igicmd)
	print ("BOX 0 2", >> igicmd)
	print ("ylabel Green", >> igicmd)
	print ("color 2", >> igicmd)
	print ("CONNECT", >> igicmd)

	print ("color 0", >> igicmd)
	print ("ysection "+cmapim+"[1:"+i2+",3]", >> igicmd)
	print ("VPAGE 0 1 .65 .925", >> igicmd)
	print ("BOX 0 2", >> igicmd)
	print ("ylabel Blue", >> igicmd)
	print ("color 3", >> igicmd)
	print ("CONNECT", >> igicmd)

	print ("color 0", >> igicmd)
	print ("VPAGE 0 1 0 1", >> igicmd)
	print ("LOCATION 0.1 0.98 .65 .925", >> igicmd)
	print ("title "+cmapim, >> igicmd)

	if (prtigi) {
	    print (igicmd)
	    type (igicmd)
	}

	# Draw the plot
	igi (<igicmd, device=device)

	# Delete the igi command file
	delete (igicmd)
end
