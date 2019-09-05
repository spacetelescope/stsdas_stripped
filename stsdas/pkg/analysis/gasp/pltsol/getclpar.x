# GET_IMPAR -- Get parameter from the cl

procedure get_clpar (crpix1, crpix2, crval1, crval2,
			   x_pixel_size, y_pixel_size, plate_scale)

double	crpix1, crpix2	# pixel position for reference point (pixel)
double	crval1, crval2	# equatorial coordinates of reference point (degrees)
real	x_pixel_size	# x coordinate pixel size (mm)
real	y_pixel_size	# y coordinate pixel size (mm)
double	plate_scale	# plate scale [arcs/mm]

real	clgetr()
double	clgetd()

begin

	crpix1 = clgetd ("crpix1")
	crpix2 = clgetd ("crpix2")
	crval1 = clgetd ("crval1")
	crval2 = clgetd ("crval2")
	x_pixel_size = clgetr ("xpixelsz")
	y_pixel_size = clgetr ("ypixelsz")
	plate_scale = clgetd ("pltscale")
end
