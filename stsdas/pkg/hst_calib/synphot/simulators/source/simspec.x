include	<imhdr.h>
include	"simtwo.h"
include "grating.h"
include "limit.h"

#* HISTORY *
#* B.Simon	05-Jul-95	derived from simimg
#* B.Simon	05-Dec-95	added central order and wavelength
#* B.Simon	19-Nov-97	added noise parameter

# SIMSPEC -- Two dimensional spectrograph simulator

procedure simspec ()

#--
pointer	obsmode		# telescope observation mode
pointer	input		# object description file
pointer	output		# output image name
real	exptime		# exposure time
int	nread		# number of readouts
real	cenwave		# central wavelength
int	cenorder	# central order
bool	lsf_flag	# use line spread function?
bool	dsf_flag	# use detector point spread function?
bool	calcback	# Add calculated background to output?
bool	calcnoise	# Add calculated noise to output?
bool	quant		# round results to whole number?
bool	verbose		# Print diagnostic messages?
double	det_ra		# telescope right ascension
double	det_dec		# telescope declination
double	det_ang		# telescope position angle
pointer	noise		# optional noise model
pointer	backfile	# background file
pointer	noisefile	# noise file
pointer	wavtable	# wavelength array table
pointer	magband		# passband of object magnitude
pointer	magform		# form of object magnitude
pointer	colnames	# input file column names (if table)
real	dynrange	# dynamic range of extended objects
int	nsub		# number of pixel subdivisions
real	eshine		# fraction of maximum earthlight
pointer	time		# time of observation
long	seed		# random number seed
pointer	spectrum	# default spectrum
pointer	psfcat		# catalog of point spread functions
pointer	lsfcat		# catalog of line spread functions
pointer	dsfcat		# catalog of detector point spread functions
pointer	detcat		# catalog of detector information
pointer	apercat		# catalog of aperture shapes
pointer	gratecat	# catalog of grating dispersion parameters
pointer	limitcat	# catalog of count rate limits
pointer	flatcat		# catalog of flatfield files
pointer	zodtab		# table of zodiacal flux
pointer	earthtab	# earthlight spectrum (flux / sec^2)
pointer	thermtab	# thermal background (flux / sec^2)
pointer	grftable	# instrument graph table
pointer	cmptable	# component name table
real	hstarea		# total telescope area

bool	skycoord
double	apscale, jd
int	nx, ny, nwave, degree
pointer	sp, flatfile, zband, mode, ncode, pcode, ocode, mcode, zcode 
pointer	grating, wave, thruput, psf, lsf, dsf, apshape, limit, mw, im, out
real	counts

data	skycoord  / false /
string	raform	  RA_UNITS
string	notband   "Observation mode is not a bandpass"
string	nomagband "Magnitude passband is blank"

bool	clgetb(), isblank()
double	clgetd(), daytime()
int	clgeti()
pointer	psfimage(), lsfimage(), dsfimage(), immap(), imps2r()
real	clgetr()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (obsmode, SZ_LINE, TY_CHAR)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (noise, SZ_FNAME, TY_CHAR) 
	call salloc (backfile, SZ_FNAME, TY_CHAR) 
	call salloc (noisefile, SZ_FNAME, TY_CHAR) 
	call salloc (wavtable, SZ_FNAME, TY_CHAR) 
	call salloc (magband, SZ_FNAME, TY_CHAR) 
	call salloc (magform, SZ_FNAME, TY_CHAR) 
	call salloc (colnames, SZ_FNAME, TY_CHAR) 
	call salloc (time, SZ_FNAME, TY_CHAR)
	call salloc (spectrum, SZ_FNAME, TY_CHAR) 
	call salloc (psfcat, SZ_FNAME, TY_CHAR) 
	call salloc (lsfcat, SZ_FNAME, TY_CHAR) 
	call salloc (dsfcat, SZ_FNAME, TY_CHAR) 
	call salloc (detcat, SZ_FNAME, TY_CHAR) 
	call salloc (apercat, SZ_FNAME, TY_CHAR) 
	call salloc (gratecat, SZ_FNAME, TY_CHAR) 
	call salloc (limitcat, SZ_FNAME, TY_CHAR) 
	call salloc (flatcat, SZ_FNAME, TY_CHAR) 
	call salloc (zodtab, SZ_FNAME, TY_CHAR)
	call salloc (earthtab, SZ_FNAME, TY_CHAR)
	call salloc (thermtab, SZ_FNAME, TY_CHAR)
	call salloc (grftable, SZ_FNAME, TY_CHAR)
	call salloc (cmptable, SZ_FNAME, TY_CHAR)

	call salloc (flatfile, SZ_FNAME, TY_CHAR) 
	call salloc (zband, SZ_FNAME, TY_CHAR)
	call salloc (mode, SZ_LINE, TY_CHAR)
	call salloc (limit, LEN_L, TY_REAL)
	call salloc (grating, LEN_G, TY_REAL)
	call salloc (ncode, SZ_LINE, TY_INT)
	call salloc (pcode, 3*SZ_LINE, TY_INT)
	ocode = pcode
	mcode = pcode + SZ_LINE
	zcode = pcode + 2 * SZ_LINE

	# Read task parameters from main parameter file

	call clgstr ("obsmode", Memc[obsmode], SZ_LINE)
	call clgstr ("input", Memc[input], SZ_LINE)
	call clgstr ("output", Memc[output], SZ_FNAME)

	exptime = clgetr ("exptime")
	nread = clgeti ("nread")

	cenwave = clgetr ("cenwave")
	cenorder = clgeti ("cenorder")

	lsf_flag = clgetb ("lsf_flag")
	dsf_flag = clgetb ("dsf_flag")

	calcback = clgetb ("calcback")
	calcnoise = clgetb ("calcnoise")
	quant = clgetb ("quant")
	verbose = clgetb ("verbose")

	det_ra = clgetd ("det_ra")
	det_dec = clgetd ("det_dec")
	det_ang = clgetd ("det_ang")

	call clgnone ("noise", Memc[noise], SZ_FNAME)
	call clgnone ("backfile", Memc[backfile], SZ_FNAME)
	call clgnone ("noisefile", Memc[noisefile], SZ_FNAME)
	call clgnone ("wavetab", Memc[wavtable], SZ_FNAME)

	# Read model parameter file

	call clgstr ("magband", Memc[magband], SZ_FNAME)
	call clgstr ("magform", Memc[magform], SZ_FNAME)
	call clgstr ("colnames", Memc[colnames], SZ_FNAME)
	dynrange = clgetr ("dynrange")
	nsub = clgeti ("nsub")

	# Read background parameter file

	eshine = clgetr ("eshine")
	call clgstr ("time", Memc[time], SZ_FNAME)
	seed = clgeti ("seed")

	# Read catalog and file parameter file

	call clgstr ("spectrum", Memc[spectrum], SZ_FNAME)
	call clgstr ("psfcat", Memc[psfcat], SZ_FNAME)
	call clgstr ("lsfcat", Memc[lsfcat], SZ_FNAME)
	call clgstr ("dsfcat", Memc[dsfcat], SZ_FNAME)
	call clgstr ("detcat", Memc[detcat], SZ_FNAME)
	call clgstr ("apercat", Memc[apercat], SZ_FNAME)
	call clgstr ("gratecat", Memc[gratecat], SZ_FNAME)
	call clgnone ("limitcat", Memc[limitcat], SZ_FNAME)
	call clgnone ("flatcat", Memc[flatcat], SZ_FNAME)
	call clgnone ("zodtab", Memc[zodtab], SZ_FNAME)
	call clgnone ("earthtab", Memc[earthtab], SZ_FNAME)
	call clgnone ("thermtab", Memc[thermtab], SZ_FNAME)

	# Read reference data parameter file

	call clgstr ("grtbl", Memc[grftable], SZ_FNAME)
	call clgstr ("cmptbl", Memc[cmptable], SZ_FNAME)
	hstarea = clgetr( "area" )

	# Process the task parameters

	call put_hstarea (hstarea)
	call set_message (verbose)

	call getnaked (Memc[obsmode], Memc[mode], SZ_LINE)
	call chkform (Memc[magform])
	call angtodeg (raform, det_ra)
	jd = daytime (Memc[time])

	# Retrieve the detector size and scale

	call getscale (Memc[detcat], Memc[mode], apscale, nx, ny)

	# Read the shape of the aperture from its catalog

	call apershape (Memc[apercat], Memc[mode], apshape)

	# Read the grating dispersion parameters

	call getgrating (Memc[gratecat], Memc[mode], Memr[grating])

	# Modify the dispersion parameters to get 
	# the desired central wavelength

	call tiltgrating (cenorder, cenwave, Memr[grating])

	# Read the limits catalog

	call getlimits (Memc[limitcat], Memc[mode], Memr[limit])

	# Retrieve the flat field 

	call getflat (Memc[flatcat], Memc[mode], Memc[flatfile], SZ_FNAME)

	# Retrieve the psf images for this observation

	psf = psfimage (Memc[psfcat], Memc[mode], apscale, dynrange, nsub)

	# Retrieve the lsf images for this observation

	if (lsf_flag) {
	    lsf = lsfimage (Memc[lsfcat], Memc[mode], apscale, dynrange, nsub)
	} else {
	    lsf = NULL
	}

	if (dsf_flag) {
	    dsf = dsfimage (Memc[dsfcat], Memc[mode], apscale, dynrange)
	} else {
	    dsf = NULL
	}

	# Initialize table cache and synphot variables

	call inisyntab
	call undefsynvar

	# Compile the observation mode and magnitude passband expressions

	if (calcback) {
	    call strcpy (ZOD_BAND, Memc[zband], SZ_FNAME)
	} else {
	    call strcpy ("1.0", Memc[zband], SZ_FNAME)
	}

	if (isblank (Memc[magband]))
	    call printerr_str (nomagband, Memc[magband])

	call setmodewarn (NO)	# Turn off msg for unused keywords

	call expcompile (Memc[mode], Memi[ocode], SZ_LINE)
	call expcompile (Memc[magband], Memi[mcode], SZ_LINE)
	call expcompile (Memc[zband], Memi[zcode], SZ_LINE)

	# Compile the noise expression

	if (Memc[noise] == EOS) {
	    call rdnoise (Memc[mode], Memc[grftable], Memc[cmptable], 
			  exptime, nread, Memi[ncode], SZ_LINE)

	} else {
	    call compnoise (Memc[noise], exptime, nread, Memi[ncode], SZ_LINE)
	}

	# Compute the wavelength and thruput arrays

	call getwavelen (Memc[wavtable], Memc[grftable], Memc[cmptable],
			 Memi[pcode], 3, SZ_LINE, wave, nwave)

	call salloc (thruput, nwave, TY_REAL)
	call syncalc (Memi[ocode], SZ_LINE, NULL, nwave, Memr[wave],
		      Memc[grftable], Memc[cmptable], Memr[thruput], degree)

	call setmodewarn (YES)
	if (degree != 0)
	    call printerr_str (notband, Memc[mode])

	# Remove the aperture thruput from the total thruput

	call dropaper (Memc[mode], Memc[grftable], Memc[cmptable], 
		       nwave, Memr[wave], Memr[thruput])

	# Compute the coordinate transformation from world 
	# to detector coordinates

	call coordtrans (skycoord, det_ra, det_dec, det_ang, 
			 apscale, nx, ny, mw)

	# Open / create output image

	im = immap (Memc[output], NEW_FILE, NULL)

	IM_NDIM(im) = 2
	IM_LEN(im,1) = nx
	IM_LEN(im,2) = ny
	IM_PIXTYPE(im) = TY_REAL

	# Get outout image buffer

	out = imps2r (im, 1, nx, 1, ny)
	call aclrr (Memr[out], nx*ny)

	# Read input list and add sources to output image

	call rd_source (nwave, Memr[wave], Memr[thruput], Memc[input], 
			Memc[spectrum], Memc[magband], Memc[magform], 
			Memc[colnames], Memc[grftable], Memc[cmptable], 
			exptime, psf, lsf, mw, apshape, apscale, 
			Memr[grating], dynrange, nsub, nx, ny, Memr[out])

	# Add background from file to output image

	if (Memc[backfile] != EOS)
	    call fileops (Memr[out], nx, ny, Memc[backfile], ADDOP)

	# Add calculated background from outside telescope to output image

	if (calcback)
	    call specbackgd (Memr[out], nx, ny, Memc[zodtab], det_ra, 
			     det_dec, jd, Memc[earthtab], eshine, 
			     exptime, apshape, apscale, Memr[grating], 
			     Memc[grftable], Memc[cmptable], nwave, 
			     Memr[wave], Memr[thruput])

	# Multiply output by flat field

	if (Memc[flatfile] != EOS)
	    call fileops (Memr[out], nx, ny, Memc[flatfile], MULOP)

	# Add calculated background from inside telescope to output image

	if (calcback) {
	    call inbackgd (Memc[thermtab], exptime, apscale, nwave, 
			   Memr[wave], Memr[thruput], counts)

	    call aaddkr (Memr[out], counts, Memr[out], nx * ny)
	}

	# Add noise from file to output image

	if (Memc[noisefile] != EOS)
	    call fileops (Memr[out], nx, ny, Memc[noisefile], ADDOP)

	# Add calculated noise to output image

	if (calcnoise)
	    call addnoise (Memr[out], nx, ny, quant, seed, Memi[ncode])

	# Convolve output image with detector point spread function

	call dsfconv (dsf, nx, ny, Memr[out])

	# Check count rate limits and print warning

	call chklimits (Memr[limit], exptime, nx, ny, Memr[out])

	# Write WCS and observation mode to image header

	call mw_saveim (mw, im)
	call imastr (im, "OBSMODE", Memc[mode])

	# Close files and free memory

	call imunmap (im)
	call mw_close (mw)
	call clssyntab

	call free_func (apshape)
	call mfree (wave, TY_REAL)
	call mfree (psf, TY_INT)
	if (lsf != NULL)
	    call mfree (lsf, TY_INT)

	call sfree (sp)
end
