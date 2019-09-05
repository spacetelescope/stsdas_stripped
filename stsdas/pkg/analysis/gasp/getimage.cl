procedure getimage (name,ra,dec,hsize,vsize)

# GETIMAGE - get an image from the CD-ROM all sky survey
#
# This script collects parameters and sets up the call to
# the foreign task GetImage.  See the documentation for the 
# stand-alone version to see the meaning of the execution options.
#
# May 25, 1994
#

char	name {prompt = 'Prefix of output file name - up to 4 chars'}
real	ra {min = 0.0, max = 23.99999999, 
		prompt = 'Ra of center of field in hours'}
real	dec {min = -90.0, max = 90.0, 
		prompt = 'Dec of center of field in degrees'}
real	hsize = 14.5 {prompt = 'Horizontal size in arcminutes'}
real	vsize = 14.5 {prompt = 'Vertical size in arcminutes'}
char	equinox = 'J2000' {mode = 'h', enum = 'B1950|J2000', 
		prompt = 'Equinox of spedified ra and dec'}
char	filtype = 'geis' {mode = 'h', enum = 'fits|geis', 
		prompt = 'Output file type?'}
bool	exitopt = yes {mode = 'h',
		prompt = 'Unmount the CD-ROM after completion?'}
bool	cntropt = yes {mode = 'h',
		prompt = 'Only extract image closest to a plate center?'}
char	mnt_pnt = "$HOME/cdrom" {mode = 'h',
		prompt = 'Mount point for your local CD-ROM reader?'}

begin

	char n
	real r
	real d
	real h
	real v
	char e
	char e_out
	char t
	char t_out
	bool o
	char o_out
	bool c
	char c_out
	char m
	char m_out
	char linename
	char linename_out


	# variables used to convert ra and dec to strings
	int 	hrs
	int 	min
	real 	sec
	int 	sign
	int 	degs
	int 	dmin
	real 	dsec
	real 	tmp
	int	itmp

	n = getimage.name	# get all the parameters
	r = getimage.ra
	d = getimage.dec
	h = getimage.hsize
	v = getimage.vsize
	e = getimage.equinox
	t = getimage.filtype
	o = getimage.exitopt
	c = getimage.cntropt
	m = getimage.mnt_pnt
	
	if (o == yes) 		# the unmount option
		o_out = ""
	else
		o_out = "-u"
	if (c == yes)		# the extract-all or only center option
		c_out = "-c"
	else
		c_out = "-a"
	if (e == "J2000")	# the equinox option
		e_out = "-j"
	else
		e_out = "-b"
	if (t == "fits")	# the filetype option
		t_out = "-f"
	else
		t_out = "-g"
	m_out = "-m"		# the mount point option
	linename_out = "-i"	# the non-interactive mode
	linename = mktemp("gsp")# create a temporary file for name, ra, etc.

	tmp = r			# convert ra
	hrs = tmp
	tmp = tmp - hrs
	tmp = 60 * tmp
	min = tmp
	tmp = tmp - min
	sec = 60 * tmp		
	itmp = sec * 100.0 + 0.5 # round to ss.ss seconds
	sec = itmp
	sec = sec / 100.0
	tmp = d			# convert dec
	if ( tmp < 0) {
		sign = -1
		tmp = -tmp
	}
	else
		sign = 0
	degs = tmp
	tmp  = tmp - degs
	if (sign == -1)
		degs = -degs
	tmp  = 60 * tmp
	dmin = tmp
	tmp  = tmp - dmin
	dsec = 60 * tmp		
	itmp = dsec * 100.0 + 0.5 # round to ss.ss seconds
	dsec = itmp
	dsec = dsec / 100.0

	print(n," ",hrs,min,sec,degs,dmin,dsec,h,v, >> linename)

	xgtimage(linename_out,linename,m_out,m,c_out,e_out,t_out,o_out)

	delete(linename)	# delete the temporary file
end
