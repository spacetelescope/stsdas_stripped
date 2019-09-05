include	<imhdr.h>
include	<cif.h>
include	<error.h>
include	"mkw.h"

# Memory management.
define	Cur_table		Memc[cur_table]

#---------------------------------------------------------------------------
.help t_mkwave 11Apr95 source
.ih
NAME
t_mkwave -- Make a spectrum based on a wavelenth image.
.ih
DESCRIPTION
The following are programming notes.  See the task help file for a
user-level description.

The bulk of this code is taken from the IRAF NOAO package, artdata,
specifically the task mk1dspec.

The code is based on an interface paradigm so that it can easily be used by
other SPP-based tasks.  The file, mkw.h, defines the memory structure.
The routines 'mkw_alloc' and 'mkw_free' manage the structure.  The
routine 'mkw_mkwave' is the main algorithm.  The routine 'mkw_rtab' is
a utility routine to read in a line list but in no other way provides
any specific functionality.
.endhelp
#---------------------------------------------------------------------------
procedure t_mkwave()

# File manipulation.
pointer	cif				# Coordinated Input File object.
pointer	cif_alloc()			# Create a CIF object.
bool	cif_next()			# Get next set of files.

# MKW object and manipulation.
pointer	mkw				# MKW object.
pointer	mkw_alloc()			# Create the MKW object.

# Image IO
pointer	imgl1d()			# Get 1-dimensional line from image.
pointer	immap()				# Open an image.
pointer	impl1r()			# Put 1-dimensional line to image.
pointer	out				# Ouput image descriptor.
pointer	wave				# Wavelength image descriptor.

# Tables IO
pointer	lines				# Line list table descriptor.
pointer	tbtopn()			# Open a table.

# Generic.
int	btoi()				# Convert boolean to integer.
bool	clgetb()			# Get boolean-valued parameter.
double	clgetd()			# Get double-valued parameter.
int	clgeti()			# Get integer-valued parameter.
long	clgetl()			# Get long-valued parameter.
bool	first_missing			# True if first line list table missing.
pointer	cur_table			# Name of current table.
pointer	sp				# Stack pointer.

begin
	call smark (sp)
	call salloc (cur_table, SZ_PATHNAME, TY_CHAR)
	
	# Create mkwave structure.
	mkw = mkw_alloc()
	
	# Setup the input/output files.
	cif = cif_alloc (1, 1)
	call clgstr ("input", CIF_p_file_list(cif), CIF_SZ_FNAME)
	call clgstr ("output", CIF_out_file_list(cif,1), CIF_SZ_FNAME)
	call clgstr ("lines", CIF_in_file_list(cif,1), CIF_SZ_FNAME)
	call strcpy ("tab", CIF_in_ext(cif,1), CIF_SZ_FNAME)
	
	# Get artificial spectrum characteristics.
	MKW_SUBSAMPLE(mkw) = 1.0d0 / clgeti ("nxsub")
	MKW_PEAK(mkw) = clgetd ("peak")
	MKW_SIGMA(mkw) = clgetd ("sigma")
	MKW_NSIGMA(mkw) = sqrt (2.d0 * log (clgetd ("dynrange")))
	MKW_CONT(mkw) = clgetd ("continuum")
	MKW_SLOPE(mkw) = clgetd ("slope")
	MKW_TEMP(mkw) = clgetd ("temperature")
	MKW_FNU(mkw) = btoi (clgetb ("fnu"))
	MKW_Z(mkw) = clgetd ("rv")
	if (clgetb ("z"))
	    MKW_Z(mkw) = 1 + MKW_Z(mkw)
	else {
	    MKW_Z(mkw) = MKW_Z(mkw) / 299792.5
	    MKW_Z(mkw) = sqrt ((1 + MKW_Z(mkw)) / (1 - MKW_Z(mkw))) 
	}
	
	# Loop through the files, creating artificial spectra.
	wave = NULL
	out = NULL
	lines = NULL
	while (cif_next (cif, CIF_NEXT_GROUP)) {

	    # Open the input wavelength data.
	    if (wave != NULL)
		call imunmap (wave)
	    iferr (wave = immap (CIF_p_file(cif), READ_ONLY, 0)) {
		call erract (EA_WARN)
		call eprintf ("\t..reading next input\n")
		next
	    }
	    MKW_WAVE_PTR(mkw) = imgl1d (wave)
	    MKW_NPIX(mkw) = IM_LEN(wave,1)
	    
	    # Open the next output file.
	    if (out != NULL)
		call imunmap (out)
	    if (CIF_out_status(cif,1) == CIF_SAME)
		call error (1, "mkwave: no output files specified.  Set a value for the 'output' parameter")

	    iferr (out = immap (CIF_out_file(cif,1), NEW_COPY, wave)) {
		call erract (EA_WARN)
		call eprintf ("mkwave: could not create file '%s'\n\t..reading next input\n")
		call pargstr (CIF_out_file(cif,1))
		next
	    }
	    MKW_SPEC_PTR(mkw) = impl1r (out)

	    # Open the line list table.  If there are no more in the list, use
	    # the last opened table.
	    if (CIF_in_status(cif,1) != CIF_OK) {

		# If this is the first "missing" table, print a warning
		# saying the current table will be used for all subsequent
		# input.
		if (first_missing && lines != NULL) {
		    call printf ("mkwave: lines file list empty.\n\tUsing table '%s' for all subsequent input\n")
		    call pargstr (Cur_table)
		    first_missing = false
		}
	    }

	    # Else, open the new line list table.
	    else {
		if (lines != NULL)
		    call tbtclo (lines)
		iferr (lines = tbtopn (CIF_in_file(cif,1), READ_ONLY, 0)) {
		    call erract (EA_WARN)
		    call eprintf ("mkwave: could not open table '%s'.\n\t..reading next input file.\n")
		    call pargstr (CIF_in_file(cif,1))
		    next
		}
		call strcpy (CIF_in_file(cif,1), Cur_table, CIF_SZ_FNAME)
		call mkw_rtab (lines, MKW_LINES_PTR(mkw),
			      MKW_INT_PTR(mkw), MKW_FWHM_PTR(mkw),
			      MKW_NLINES(mkw))
	    }

	    # Create the spectrum.
	    call mkw_mkwave (mkw, clgetl("seed"))

	}

	# That's all folks.
	if (wave != NULL)
	    call imunmap (wave)
	if (out != NULL)
	    call imunmap (out)

	if (lines != NULL)
	    call tbtclo (lines)
	
	if (MKW_LINES_PTR(mkw) != NULL)
	    call mfree (MKW_LINES_PTR(mkw), TY_DOUBLE)
	if (MKW_INT_PTR(mkw) != NULL)
	    call mfree (MKW_INT_PTR(mkw), TY_REAL)
	if (MKW_FWHM_PTR(mkw) != NULL)
	    call mfree (MKW_FWHM_PTR(mkw), TY_REAL)

	call mkw_free (mkw)
	call cif_free (cif)
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_mkwave
#---------------------------------------------------------------------------
