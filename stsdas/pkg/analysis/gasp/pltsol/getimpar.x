# GET_IMPAR -- Get parameter from image header

procedure get_impar (im, crpix1, crpix2, crval1, crval2,
			   x_pixel_size, y_pixel_size, plate_scale)

pointer	im		# image descriptor
double	crpix1, crpix2	# pixel position for reference point (pixel)
double	crval1, crval2	# equatorial coordinates of reference point (degrees)
real	x_pixel_size	# x coordinate pixel size (mm)
real	y_pixel_size	# y coordinate pixel size (mm)
double	plate_scale	# plate scale [arcs/mm]

real	imgetr()
double	imgetd()

begin

	crpix1 = imgetd (im, "CRPIX1  ")
	crpix2 = imgetd (im, "CRPIX2  ")
	crval1 = imgetd (im, "CRVAL1  ")
	crval2 = imgetd (im, "CRVAL2  ")
	x_pixel_size = imgetr (im, "XPIXELSZ")
	y_pixel_size = imgetr (im, "YPIXELSZ")
	plate_scale  = imgetd (im, "PLTSCALE")

end
