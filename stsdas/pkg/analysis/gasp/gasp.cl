#{ GASP.CL -- Script to set up tasks in the GASP package
#
#  12/6/91 Hide stgindx and overlay.  ZGL
#  6/2/92 Hide extcpy since extgst now includes allcols.  ZGL
#  4/26/93  Remove obsolete tasks extcpy, overlay, repos, cdrom.  ZGL

package         gasp

set 	pltsol	= "gasp$pltsol/"

task	pltsol	= "pltsol$x_pltsol.e"

task	extgst,
	intrep,
	pxcoord,
	sgscind,
	makewcs,
	xyeq,
	eqxy	= "gasp$x_cdgasp.e"

task	regions	= "gasp$regions.cl"
task	stgindx	= "gasp$stgindx.cl"
hidetask stgindx

task	targets	= "gasp$targets.cl"
task	copyftt	= "gasp$copyftt.cl"

#task	$getimage = "gasp$stub.cl"
task	getimage = "gasp$getimage.cl"
task	$xgtimage = "$/usr/local/bin/getimage"
hidetask xgtimage
 
clbye()
