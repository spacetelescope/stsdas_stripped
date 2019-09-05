include <math.h>
include <mii.h>
include <gset.h>
include "mapdat.h"

procedure mwp (gp)

#  mwp -- Draw a map of the world in Mollweide projection.
#  Continental outlines are from the NCAR database.

pointer	sp
pointer	gp				# Graphics descriptor
pointer	md				# Outline data descriptor
real	xs, ys, ar			# Device aspect
real	vl, vr, vb, vt			# Viewport
real	wl, wr, wb, wt			# Window
int	sihp[4]
real	srhp[4]
pointer	svp				# SPP coordinates
pointer	mihp, mrhp, mvp			# mii data
int	nihp, nrhp, nvp
int	junk
int	npts, type
real	minlat, maxlat, minlon, maxlon	# Extremes of polyline
real	lat, lon			# Coordinates (degrees)
int	i
int	np
int	ip

int	read(), miipksize(), open()
real	ggetr()

begin
	call smark (sp)

	# Device size
	xs = ggetr (gp, "xs");  ys = ggetr (gp, "ys")

#	call printf ("%f %f\n")
#	    call pargr (xs)
#	    call pargr (ys)

	# Adjust the vertical size to account for device aspect
	# and projection
	ar = 0.2 * xs / ys

	vl = 0.0;  vr = 1.0
	vb = 0.5 - ar;  vt = 0.5 + ar

	# Set the viewport
	call gsview (gp, vl, vr, vb, vt)

	# Set the window
	wl = -2.0;  wr = 2.0;  wb = -1.0;  wt = 1.0
	call gswind (gp, wl, wr, wb, wt)
        
	# Draw the grid
	call mwgrid (gp)

	nihp = miipksize (2, MII_SHORT)
	call malloc (mihp, nihp, TY_CHAR)
	nrhp = miipksize (4, MII_REAL)
	call malloc (mrhp, nrhp, TY_CHAR)

	# Open the World outline data
	md = open (MapData, READ_ONLY, BINARY_FILE)

	np = 0

	while (read (md, Memc[mihp], nihp) != EOF) {
	    # Each polyline in the outline file
	    np = np + 1
	    
	    # Unpack the number of points and outline type
	    call miiupk (Memc[mihp], sihp, 2, MII_SHORT, TY_INT)
	    npts = sihp[1]
	    type = sihp[2]

	    # Ignore political boundaries
	    if (type != 1)
	        next

	    # Get the extremes of the polyline
	    junk = read (md, Memc[mrhp], nrhp)
	    call miiupk (Memc[mrhp], srhp, 4, MII_REAL, TY_REAL)

	    minlat = srhp[1]
	    maxlat = srhp[2]
	    minlon = srhp[3]
	    maxlon = srhp[4]

#	    call printf ("%d %d %d %9.3f %9.3f %9.3f %9.3f\n")
#		call pargi (np)
#		call pargi (npts)
#		call pargi (type)
#		call pargr (minlat)
#		call pargr (maxlat)
#		call pargr (minlon)
#		call pargr (maxlon)

	    # Allocate vectors for coordinates
	    call malloc (svp, npts, TY_REAL)
	    nvp = miipksize (npts, MII_REAL)
	    call malloc (mvp, nvp, TY_CHAR)

	    # Get the polyline
	    junk = read (md, Memc[mvp], nvp)
	    call miiupk (Memc[mvp], Memr[svp], npts, MII_REAL, TY_REAL)

	    ip = svp

	    lat = Memr[ip]
	    ip = ip + 1
	    lon = Memr[ip]
	    ip = ip + 1

	    call gmwmov (gp, lon, lat)

	    do i = 2, npts / 2 {
		# Each point in the polyline
		lat = Memr[ip]
		ip = ip + 1
		lon = Memr[ip]
		ip = ip + 1

		call gmwdrw (gp, lon, lat)
	    }

	    call mfree (svp, TY_REAL)
	    call mfree (mvp, TY_CHAR)
	}

	call mfree (mihp, TY_CHAR)
	call mfree (mrhp, TY_CHAR)
	call close (md)

	call sfree (sp)
end


procedure mwgrid (gp)

#  mwgrid -- Draw a grid on the Molleweide projection.  Parallels of
#  lattitude and meridians of longitude are 15 degrees apart and drawn in
#  dotted lines.  The outline of the edge of the globe is solid.

pointer	gp

real	lat, lon

begin
	# Box around viewport
	call gamove (gp, -2.0, -1.0)
	call gadraw (gp, +2.0, -1.0)
	call gadraw (gp, +2.0, +1.0)
	call gadraw (gp, -2.0, +1.0)
	call gadraw (gp, -2.0, -1.0)

	call gseti (gp, G_PLTYPE, GL_SOLID)

	# Draw the outline
	lon = 180.0
	lat = -90.0
	call gmwmov (gp, lon, lat)

	for (lat = -88.0;  lat <= 90.0;  lat = lat + 2.0)
	    call gmwdrw (gp, lon, lat)

	lon = -180.0
	lat =  -90.0
	call gmwmov (gp, lon, lat)

	for (lat = -88.0;  lat <= 90.0;  lat = lat + 2.0)
	    call gmwdrw (gp, lon, lat)

	call gseti (gp, G_PLTYPE, GL_DOTTED)

	# Draw parallels of lattitude
	for (lat = -75.0;  lat <= 75.0;  lat = lat + 15.0) {
	    lon = -180.0
	    call gmwmov (gp, lon, lat)
	    lon = +180.0
	    call gmwdrw (gp, lon, lat)
	}

	# Draw meridians of longitude
	# Central meridian reaches the poles 
	call gmwmov (gp, 0.0, -90.0)
	call gmwdrw (gp, 0.0, +90.0)

	# Odds reach 60 degrees lattitude
	for (lon = -165.0;  lon <= -15.0;  lon = lon + 30.0) {
	    lat = -60.0
	    call gmwmov (gp, lon, lat)
	    for (lat = -58.0;  lat <= +60.0;  lat = lat + 2.0)
		call gmwdrw (gp, lon, lat)
	}

	for (lon = 15.0;  lon <= 165.0;  lon = lon + 30.0) {
	    lat = -60.0
	    call gmwmov (gp, lon, lat)
	    for (lat = -58.0;  lat <= +60.0;  lat = lat + 2.0)
		call gmwdrw (gp, lon, lat)
	}

	# Evens reach 75 degrees lattitude
	for (lon = -150.0;  lon <= -30.0;  lon = lon + 30.0) {
	    lat = -75.0
	    call gmwmov (gp, lon, lat)
	    for (lat = -73.0;  lat <= +75.0;  lat = lat + 2.0)
		call gmwdrw (gp, lon, lat)
	}

	for (lon = 30.0;  lon <= 150.0;  lon = lon + 30.0) {
	    lat = -75.0
	    call gmwmov (gp, lon, lat)
	    for (lat = -73.0;  lat <= +75.0;  lat = lat + 2.0)
		call gmwdrw (gp, lon, lat)
	}

	call gseti (gp, G_PLTYPE, GL_SOLID)
end


procedure gmwmov (gp, lon, lat)

#  gmwmov -- Pen-up move in longitude and lattitude using Molleweide
#  projection.

pointer	gp
real	lon, lat

real	x, y

begin
	call mwcart (lon, lat, x, y)

#	call printf ("%9.3f, %9.3f --> %9.3f, %9.3f\n")
#	    call pargr (lon)
#	    call pargr (lat)
#	    call pargr (x)
#	    call pargr (y)

	call gamove (gp, x, y)
end


procedure gmwdrw (gp, lon, lat)

#  gmwmov -- Pen-down move (draw) in longitude and lattitude using Molleweide
#  projection.

pointer	gp
real	lon, lat

real	x, y

begin
	call mwcart (lon, lat, x, y)

#	call printf ("%9.3f, %9.3f --> %9.3f, %9.3f\n")
#	    call pargr (lon)
#	    call pargr (lat)
#	    call pargr (x)
#	    call pargr (y)

	call gadraw (gp, x, y)

end


procedure mwcart (lon, lat, x, y)

#  mwcart -- Transform longitude and lattitude in degrees using
#  Molleweide projection to rectangular coordinates in the range
#  (-2:2,-1:1).

real	lat, lon
real	x, y

real	rlat, rlon

begin
	rlat = DEGTORAD(lat)
	rlon = DEGTORAD(lon)

	y = sin (rlat)
	x = atan2 (sin (rlon), cos (rlon)) * cos (rlat) * 2.0 / PI
end
