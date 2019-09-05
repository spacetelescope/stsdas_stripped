include <imhdr.h>
include <tbset.h>
include "objcalib.h"		# defines prism types and file types
define	SZ_MODE		11	# size of string for prism mode

# t_objcalib -- driver for Peter Jacobsen's calib subroutine
# Warren Hack,  Original.
# Phil Hodge,  7-Sep-1993  Use obsmode, include makephot, allow tables.
# Phil Hodge,  1-Jul-1994  Get area from par file.

procedure t_objcalib()

pointer sp			# stack pointer, for scratch space
char	input[SZ_FNAME]		# name of input spectrum
char	output[SZ_FNAME]	# name of output spectrum
char	obsmode[SZ_FNAME]	# observation mode
real	yzero			# offset from undispersed position
real	wfirst			# starting wavelength in output
real	wlast			# ending wavelength in output
real	dw			# wavelength spacing in output
bool	calibrate		# correct the photometry as well as rebin?
real	texp			# exposure time
real	encirc			# encircled energy fraction
real	area			# telescope area in sq cm
char	n_wl[SZ_COLNAME]	# column name for wavelength
char	n_flux[SZ_COLNAME]	# column name for flux
char	grftbl[SZ_FNAME]	# graph table name
char	cmptbl[SZ_FNAME]	# component table name
#--
char	prism[SZ_MODE]		# name of prism (fuv96, etc)
char 	dispfile[SZ_FNAME]	# calibration file for dispersion
int	nwx			# max number of elements in output array
pointer wave_disp, pix_disp	# wavelengths & pixel positions for disp file
pointer wave_phot, fac_phot	# wavelengths & photometry for phot file
pointer u_temp, y2disp		# scratch for spline
int	n_disp, n_phot		# number of values in calibration arrays
pointer cts			# array of input flux values
pointer cts_tmp			# array of photometrically corrected flux val
pointer wave			# array of wavelengths
pointer fl			# array of fluxes
int	status			# error return code (zero is OK) from calib
int	np			# number of values read from input
int	npix1			# length of first dimension of image
int	i
real	asumr()
real	clgetr()
bool	clgetb()

pointer inimg, oimg, immap()
pointer ix, ox, imgl1r(), impl1r(), imgl2r()

pointer intbl, otbl
pointer icp_flux		# column pointer for flux in input table
pointer ocp_wl, ocp_flux	# col ptrs for wavelength & flux in output table
pointer tbtopn()
int	tbpsta()

int	file_type		# specifies image or table

begin
	call smark(sp)

	# Get cl parameter values.
	call clgstr ("input", input, SZ_FNAME)
	call clgstr ("output", output, SZ_FNAME)
	call clgstr ("obsmode", obsmode, SZ_FNAME)
	yzero = clgetr ("yzero")
	wfirst = clgetr ("wfirst")
	wlast = clgetr ("wlast")
	dw = clgetr ("dw")
	calibrate = clgetb ("calibrate")
	if (calibrate) {
	    texp = clgetr ("texp")
	    encirc = clgetr ("encirc")
	    area = clgetr ("area")
	    call clgstr ("graphtab", grftbl, SZ_FNAME)
	    call clgstr ("comptab", cmptbl, SZ_FNAME)
	} else {
	    texp = 0.
	    encirc = 0.
	    grftbl[1] = EOS
	}

	nwx = nint ((wlast - wfirst) / dw + 1.)

	# Allocate memory for arrays.
	call salloc (wave_phot, nwx, TY_REAL)
	call salloc (fac_phot, nwx, TY_REAL)

	# Determine what parameter name to use to get the name of
	# the dispersion file from the dispfiles pset.
	call wh_prism (obsmode, prism, SZ_MODE)

	# Get the name of the dispersion file.
	call clgstr (prism, dispfile, SZ_FNAME)
	if (dispfile[1] == EOS || dispfile[1] == ' ') {
	    call eprintf (
	"must specify file name for parameter `%s' in dispfiles pset\n")
		call pargstr (prism)
	    call error (1, "")
	}

	# Allocate space for wavelengths and flux of rebinned spectrum.
	call salloc (wave, nwx, TY_REAL)
	call salloc (fl, nwx, TY_REAL)

	# Read in the dispersion file.  This routine allocates wave_disp
	# and pix_disp.
	call getdisp (dispfile, "c1", "c2", wave_disp, pix_disp, n_disp)

	# Allocate scratch space for spline and splint.
	call salloc (u_temp, n_disp, TY_REAL)	# used only by spline
	call salloc (y2disp, n_disp, TY_REAL)

	# Initialize the spline interpolation.  The 2.e30 values are flags
	# to say use a "natural" spline.
	call spline (Memr[wave_disp], Memr[pix_disp], n_disp, 2.e30, 2.e30,
		Memr[u_temp], Memr[y2disp])

	# Compute the photometric correction.
	if (calibrate) {
	    call obj_mkphot (obsmode, grftbl, cmptbl,
			wfirst, dw, nwx, encirc, area,
			Memr[wave_phot], Memr[fac_phot], n_phot)
	} else {
	    wave_phot = NULL
	    fac_phot = NULL
	}

	# Determine what type of input file is being used, and open it.
	call what_kind (input, inimg, intbl, file_type)

	# It is an image --
	if (file_type == TYPE_IMAGE) {

	    # Create output image.
	    oimg = immap (output, NEW_COPY, inimg)
	    IM_NDIM(oimg) = 1

	    # Get input data.
	    if (IM_NDIM(inimg) == 1) {

		np = IM_LEN(inimg,1)
		ix = imgl1r (inimg)
	
		# Photometrically calibrate and rebin the array.
		# Note that the input is ix.
		if (calibrate) {
		    call malloc (cts_tmp, np, TY_REAL)
		    call calib (Memr[ix], Memr[cts_tmp],
				np, yzero, texp, wfirst, dw, nwx,
				Memr[wave_disp], Memr[pix_disp], n_disp,
				Memr[wave_phot], Memr[fac_phot], n_phot,
				Memr[y2disp], Memr[wave], Memr[fl], status)
		    call mfree (cts_tmp, TY_REAL)
		} else {			# just rebin
		    call rebin (Memr[ix], np, yzero, wfirst, dw, nwx,
				Memr[wave_disp], Memr[pix_disp], n_disp,
				Memr[wave_phot], Memr[fac_phot], n_phot,
				Memr[y2disp], Memr[wave], Memr[fl], status)
		}

	    } else {

		# For a 2-D image, we sum along the first dimension.

		npix1 = IM_LEN(inimg,1)
		np = IM_LEN(inimg,2)

		# Allocate memory for the 1-D spectrum.
		call malloc (cts, np, TY_REAL)

		# Sum along lines to fill in the spectrum.
		do i = 1, np {
		    ix = imgl2r (inimg, i)
		    Memr[cts+i-1] = asumr (Memr[ix], npix1)
		}
	
		# Photometrically calibrate and rebin the array.
		# Note that the input is cts.
		if (calibrate) {
		    call malloc (cts_tmp, np, TY_REAL)
		    call calib (Memr[cts], Memr[cts_tmp],
				np, yzero, texp, wfirst, dw, nwx,
				Memr[wave_disp], Memr[pix_disp], n_disp,
				Memr[wave_phot], Memr[fac_phot], n_phot,
				Memr[y2disp], Memr[wave], Memr[fl], status)
		    call mfree (cts_tmp, TY_REAL)
		} else {			# just rebin
		    call rebin (Memr[cts], np, yzero, wfirst, dw, nwx,
				Memr[wave_disp], Memr[pix_disp], n_disp,
				Memr[wave_phot], Memr[fac_phot], n_phot,
				Memr[y2disp], Memr[wave], Memr[fl], status)
		}

		call mfree (cts, TY_REAL)
	    }
	    if (status != 0)
		call error (1, "fatal error calling subroutine calib")

	    IM_LEN(oimg,1) = nwx
	    ox = impl1r (oimg)
	    call amovr (Memr[fl], Memr[ox], nwx)	# copy to output
	    call imaddr (oimg, "CRPIX1", 1.)
	    call imaddr (oimg, "CRVAL1", wfirst)
	    call imaddr (oimg, "CD1_1", dw)
	    call imastr (oimg, "CTYPE1", "LAMBDA")	

	    call imunmap (oimg)
	    call imunmap (inimg)

	} else if (file_type == TYPE_TABLE) {

	    # Get the column names.
	    call clgstr ("n_wl", n_wl, SZ_COLNAME)
	    call clgstr ("n_flux", n_flux, SZ_COLNAME)

	    # Find flux column in input.
	    call tbcfnd (intbl, n_flux, icp_flux, 1)
	    if (icp_flux == NULL) {
		call tbtclo (intbl)
		call eprintf ("Column `%s' not found in input table;\n")
		    call pargstr (n_flux)
		call error (1, "check value of parameter n_flux.")
	    }

	    # Create output table.  Note that this does not use the
	    # input table as a template.  If the input is a text file,
	    # however, set the type of output to text also.
	    otbl = tbtopn (output, NEW_FILE, NULL)
	    if (tbpsta (intbl, TBL_WHTYPE) == TBL_TYPE_TEXT)
		call tbpset (otbl, TBL_WHTYPE, TBL_TYPE_TEXT)

	    # Create columns.
	    call tbcdef (otbl, ocp_wl, n_wl,
			"Angstroms", "%9.3f", TY_REAL, 1, 1)
	    call tbcdef (otbl, ocp_flux, n_flux,
			"flam", "", TY_REAL, 1, 1)
	    call tbtcre (otbl)

	    # Allocate space for counts from input.
	    np = tbpsta (intbl, TBL_NROWS)
	    call malloc (cts, np, TY_REAL)

	    # Read data from input table.
	    do i = 1, np
		call tbegtr (intbl, icp_flux, i, Memr[cts+i-1])

	    # Done with input table; close it.
	    call tbtclo (intbl)

	    # Photometrically calibrate and rebin the array.
	    if (calibrate) {
		call malloc (cts_tmp, np, TY_REAL)
		call calib (Memr[cts], Memr[cts_tmp],
			np, yzero, texp, wfirst, dw, nwx,
			Memr[wave_disp], Memr[pix_disp], n_disp,
			Memr[wave_phot], Memr[fac_phot], n_phot,
			Memr[y2disp], Memr[wave], Memr[fl], status)
		call mfree (cts_tmp, TY_REAL)
	    } else {				# just rebin
		call rebin (Memr[cts], np, yzero, wfirst, dw, nwx,
			Memr[wave_disp], Memr[pix_disp], n_disp,
			Memr[wave_phot], Memr[fac_phot], n_phot,
			Memr[y2disp], Memr[wave], Memr[fl], status)
	    }
	    if (status != 0)
		call error (1, "fatal error calling subroutine calib")

	    # Write data to output table.
	    do i = 1, nwx {
		call tbeptr (otbl, ocp_wl, i, Memr[wave+i-1])
		call tbeptr (otbl, ocp_flux, i, Memr[fl+i-1])
	    }

	    # Close output table.
	    call tbtclo (otbl)

	    # Deallocate memory.
	    call mfree (cts, TY_REAL)
	    if (calibrate) {
		call mfree (wave_disp, TY_REAL)
		call mfree (pix_disp, TY_REAL)
	    }

	} else {
	    call eprintf ("no action taken\n")
	}

	call sfree (sp)
end
