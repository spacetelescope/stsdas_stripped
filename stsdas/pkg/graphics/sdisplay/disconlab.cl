procedure disconlab (image)

file	image	{prompt = "Image to display"}

bool	disp	{yes, prompt = "Display image?"}
int	frame	{1, prompt = "Frame number"}

bool	cont	{yes, prompt = "Draw contours?"}
file	cimage	{prompt = "Image for computing contours"}
string	contcol	{"red", min = "red|green|blue|yellow|white", prompt = "Color of contours (red|green|blue|yellow|white)"}

bool	label	{yes, prompt = "Draw coordinate labels?"}
file	limage	{prompt = "Image for labeling"}
string	lablcol	{"blue", min = "red|green|blue|yellow|white", prompt = "Color of coordinate labels (red|green|blue|yellow|white)"}

bool	doapers	{no, prompt = "Draw the HST instrument apertures?"}
file	aimage	{prompt = "Image for computing aperture scale"}
string	apercol	{"yellow", min = "red|green|blue|yellow|white", prompt = "Color of science apertures (red|green|blue|yellow|white)"}
string	apers	{"@stsdaslib$siaper_def_apers.txt",prompt = "Apertures to display"}
string	center	{"ota", prompt = "Aperture to center on"}
string	ra	{"", prompt = "RA of the center aperture"}
string	dec	{"", prompt = "DEC of the center aperture"}
real	roll	{0.0, prompt ="Spacecraft roll"}

real	left	{0.1, prompt = "Left edge of viewport", min=0, max=1}
real	right	{0.9, prompt = "Right edge of viewport", min=0, max=1}
real	bottom	{0.1, prompt = "Bottom edge of viewport", min=0, max=1}
real	top	{0.9, prompt = "Top edge of viewport", min=0, max=1}

string	Version	{"10Aug95",prompt="Date of Installation"}

begin

  # Declarations.
  file  iimg, icim, ilim
  int  ifr
  real  il, ir, ib, it
  string  ccol, cdev
  real  xs, ys, xc, yc
  bool  icon, ilab, iaper

  # Make sure the appropriate tasks are loaded.
  if( !defpac("stplot") || !defpac("tv") ) {
    print( "Error: tv and stplot packages need to be loaded!" )
    bye
  }
    
  # Figure out how to display the image.
  iimg = image

  il = left; ir = right; ib = bottom; it = top

  xs = ir - il; ys = it - ib
  xc = (il + ir) / 2.; yc = (ib + it) / 2.

  # Display the image
  if (disp) {
      ifr = frame
      display (iimg, ifr, xsize=xs, ysize=ys, xcenter=xc, ycenter=yc, fill=yes)
  }

  # Whether the images has been redisplayed or not, we need to determine
  # where the image is in the display.
  imdisp_pos( iimg )
  il = imdisp_pos.left
  ir = imdisp_pos.right
  ib = imdisp_pos.bottom
  it = imdisp_pos.top
  iimg = imdisp_pos.section

  icon = cont
  ilab = label
  iaper = doapers

  # Draw contours
  if (icon) {
    icim = cimage
    if (icim == "")
      icim = iimg

    ccol = substr (contcol, 1, 1)
    cdev = "imd" + ccol

    newcont (icim,device=cdev,preserve-,perimeter-,wcslab-,
             left=il,right=ir,bottom=ib,top=it)
  }

  # Draw the telescope instrument apertures.
  if (iaper) {

    ilim = aimage
    if (ilim == "")
      ilim = iimg

    ccol = substr (apercol, 1, 1)
    cdev = "imd" + ccol

    siaper (ilim, apertures=apers, device=cdev, left=il, right=ir, 
            bottom=ib, top=it, roll=roll, center=center, ra=ra, dec=dec) 
  }

  # Label with coordinates
  if (ilab) {

    ilim = limage
    if (ilim == "")
      ilim = iimg

    ccol = substr (lablcol, 1, 1)
    cdev = "imd" + ccol

    wcslab(ilim,frame=frame, device=cdev,vl=il,vr=ir,
	   vb=ib,vt=it,fill+,append-)
  }

end
