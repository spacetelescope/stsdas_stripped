include	<error.h>
include	<imhdr.h>
include	<cif.h>
include	"../mkwave/mkw.h"
include	"ot.h"
include	"wid.h"

# Memory management.
define	Sx		Memc[sx]

#---------------------------------------------------------------------------
.help t_waveid 10Apr95 source
.ih
NAME
t_waveid -- GHRS-specific wavelength identification
.ih
DESCRIPTION
Below are programming notes.  For user-level help, see the task help
file.

The code is arranged on an interface paradigm, allowing easy use from
other SPP-based tasks.  The routines 'wid_alloc' and 'wid_free' manage
the WID object.  'wid_waveid' is the main algorithm.

The output table is managed by the set of routines 'wid_o_*'.

The line list is managed by the set of routines 'wid_ll_*'.

This task also uses the MKW object for creating artificial spectra.
.endhelp
#---------------------------------------------------------------------------
procedure t_waveid()

# Image IO
int	imgeti()		# Get integer-valued keyword.
real	imgetr()		# Ger real-valued keyword.
pointer	imgl1r()		# Get real-valued image data.
pointer	imgl1d()		# Get double-valued image data.
pointer	immap()			# Open an image.
pointer	oim			# Observation image descriptor.
pointer	wim			# Wavelength solution image descriptor.

# CIF IO
pointer	cif			# CIF object.
pointer	cif_alloc()		# Create the CIF object.
bool	cif_next()		# Get next set of files.

# Line List IO
pointer	ll			# Line list.
pointer	wid_ll_rtab()		# Read the line list table.

# WID Object
pointer	wid			# WID object.
pointer	wid_alloc()		# Create the WID object.

# OT Object.
pointer	ot			# WID OT Object.
pointer wid_o_alloc()		# Open the output table.

# MKW Object.
pointer	mkw			# MWK object.
pointer	mkw_alloc()		# Allocate the mkw object.

# Generic.
int	btoi()			# Convert boolean to integer.
bool	clgetb()		# Get boolean-valued parameter.
int	clgeti()		# Get integer-valued parameter.
real	clgetr()		# Get real-valued parameter.
int	clgwrd()		# Get dictionary indexed parameter.
bool	first_missing		# TRUE when missing first solution.
int	i			# Generic.
pointer	sp			# Stack pointer.
bool	strne()			# Strings not equal?
pointer	sx			# Generic string.

begin
	call smark (sp)
	call salloc (sx, SZ_PATHNAME, TY_CHAR)

	# Open up the main memory structure.
	wid = wid_alloc()
	
	# Setup the input images.
	cif = cif_alloc (1, 0)
	call clgstr ("input", CIF_p_file_list(cif), CIF_SZ_FNAME)
	call clgstr ("wave", CIF_in_file_list(cif,1), CIF_SZ_FNAME)
	call clgstr ("definputext", CIF_p_ext(cif), CIF_SZ_FNAME)
	call clgstr ("defwavext", CIF_in_ext(cif,1), CIF_SZ_FNAME)

	# Read the input line list.
	call clgstr ("lines", Sx, SZ_PATHNAME)
	ll = wid_ll_rtab (Sx)

	# Open the output table.
	call clgstr ("output", Sx, SZ_PATHNAME)
	ot = wid_o_alloc (Sx)

	# Get offset.
	WID_OFF(wid) = clgetr ("offset")
	WID_UNITS(wid) = clgwrd ("units", Sx, SZ_PATHNAME, WID_UNITS_DICT)
	WID_XCOR(wid) = btoi (clgetb ("cross_corr"))
	WID_MSHIFT(wid) = clgeti ("max_shift")

	# Get line profile search parameters
	WID_WIDTH(wid) = clgetr ("width")
	WID_RADIUS(wid) = clgetr ("radius")
	WID_THRESH(wid) = clgetr ("threshold")
	WID_NO_DUPS(wid) = btoi (clgetb ("no_dups"))

	# Handle interactive mode.
	WID_INTER(wid) = btoi (clgetb ("interactive"))

	# Setup the MWK object for creating artificial spectra.
	mkw = mkw_alloc()
	MKW_SIGMA(mkw) = 0.001d0

	# Go through all the input images, finding lines.
	first_missing = true
	oim = NULL
	wim = NULL
	while (cif_next (cif, CIF_NEXT_GROUP)) {

	    # Open the observation.
	    if (oim != NULL)
		call imunmap (oim)
	    iferr (oim = immap (CIF_p_file(cif), READ_ONLY, 0)) {
		call erract (EA_WARN)
		call eprintf ("\t...reading next input\n")
		next
	    }

	    # Open the next wavelength image if available.
	    if (CIF_in_status(cif,1) == CIF_OK) {
		if (wim != NULL)
		    call imunmap (wim)
		iferr (wim = immap (CIF_in_file(cif,1), READ_ONLY, 0)) {
		    call erract (EA_WARN)
		    call eprintf ("\t...cannot assign wavelengths to current input, reading next input.\n")
		    next
		}
	    }

	    # Else, use previous wavelength image, if one was opened.
	    else {
		if (wim == NULL)
		    call error (1, "waveid: No wavelength solution images available")
		if (first_missing) {
		    call printf ("waveid: No more wave solutions, using previous solution.\n")
		    first_missing = false
		}
	    }

	    # Consistency check.  Number of data points must be the
	    # same for the observation and solution.
	    if (IM_LEN(oim,1) != IM_LEN(wim,1)) {
		call eprintf ("waveid: Image sizes not the same between observation and solutions images\n\t'%s' and '%s'\n\t...reading next input")
		call pargstr (CIF_p_file(cif))
		call pargstr (CIF_in_file(cif,1))
		next
	    }
	    WID_NPIX(wid) = IM_LEN(oim,1)

	    # Consistency check.  The carrousel positions must be the
	    # same.
	    i = imgeti (oim, "carpos")
	    if (OT_NROWS(ot) <= 0) {
		OT_CARPOS(ot) = i
		call imgstr (oim, "grating", OT_GRAT(ot), SZ_LINE)
		call imgstr (oim, "aperture", OT_APER(ot), SZ_LINE)
	    } else if (OT_CARPOS(ot) != i) {
		call eprintf ("waveid: carrousel position of input '%s'(%d)\n\tdoes not match output table's %d.\n\t...Reading next input.\n")
		call pargstr (CIF_p_file(cif))
		call pargi (i)
		call pargi (OT_CARPOS(ot))
		next
	    }

            # Check grating.  The observations must all come from the same
            # grating.  The carrousel position should be sufficient.
	    # However, with the side 1 cross-strapped to side 2 carrousel
	    # positions, there is overlap between gratings.
            call imgstr (oim, "grating", Sx, SZ_PATHNAME)
            if (strne (Sx, OT_GRAT(ot))) {
                call eprintf ("waveid: grating %s of input %s\n\tdoes not match table's grating %s.\n\t...Reading next input.\n")
                call pargstr (Sx)
                call pargstr (CIF_p_file(cif))
                call pargstr (OT_GRAT(ot))
                next
            }
                
            # Check aperture.  The observations must all come from the same
            # aperture.
            call imgstr (oim, "aperture", Sx, SZ_PATHNAME)
            if (strne (Sx, OT_APER(ot))) {
                call eprintf ("waveid: aperture %s of input %s\n\tdoes not match table's aperture %s.\n\t...Reading next input.\n")
                call pargstr (Sx)
                call pargstr (CIF_p_file(cif))
                call pargstr (OT_APER(ot))
                next
            }
                
	    # Get order.
	    iferr (WID_M(wid) = imgeti (oim, "sporder")) {
		call erract (EA_WARN)
		call eprintf ("\tsetting order to 1\n")
		WID_M(wid) = 1
	    }

	    # Retrieve the input data.
	    WID_OBS_PTR(wid) = imgl1r (oim)
	    WID_S0(wid) = imgetr (oim, "sample")
	    WID_SD(wid) = imgetr (oim, "deltas")

	    # Get wavelength solution.
	    WID_WAVE_PTR(wid) = imgl1d (wim)

	    # Got all the input, find the lines.
	    call wid_waveid (wid, ll, ot, mkw)
	}

	# That's all folks.
	if (wim != NULL)
	    call imunmap (wim)
	if (oim != NULL)
	    call imunmap (oim)
	call mkw_free (mkw)
	call wid_ll_free (ll)
	call wid_o_free (ot)
	call cif_free (cif)
	call wid_free (wid)
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_waveid
#---------------------------------------------------------------------------
