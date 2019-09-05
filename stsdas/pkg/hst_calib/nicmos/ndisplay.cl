# NDISPLAY: Display a science image with DQ flags superimposed
#
# This script calls the standard IRAF display task to display a
# science image and then calls the task markdq to
# overlay DQ flags on the display.
#
# Version 1.0: H.Bushouse - March 1997
#

procedure ndisplay (image, frame)

file	image	{prompt = "Image to display"}
int	frame	{prompt = "Frame number"}

begin

  # Declarations.
  file img
  int  ifr
  int  icol
  int  i1, i2, i3
  string ver

  # Make sure the appropriate tasks are loaded.
  if(!defpac("nicmos") || !defpac("tv") ) {
    print( "Error: tv and nicmos packages must be loaded!" )
    bye
  }
    
  # Get the image name and frame number.
  img = image
  ifr = frame

  # Display the science image
  display (img, ifr)

  # Construct the appropriate DQ image name
  i1 = stridx("[", img)
  if (i1 > 0) {
      i2 = stridx(",", img)
      if (i2 > 0) {
	  i3 = stridx("]", img)
	  ver = substr(img,i2+1,i3-1)
      } else
	  ver = str(1)
      img = substr(img,1,i1)
      img = img // "DQ," // ver // "]"
      #print (img)
  } else
      img = img // "[DQ,1]"

  # Display the DQ flags
  markdq (img, ifr)

end
