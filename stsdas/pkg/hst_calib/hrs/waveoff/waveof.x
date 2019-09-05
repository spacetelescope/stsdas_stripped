include <imhdr.h>
include	<cif.h>

# Convenience macros
define	Obs_data	Memd[obs_data]
define	Obs_file	CIF_p_file(in)
define	Pred		Memd[pred]
define	Wave_data	Memd[wave_data+$1-1]
define	Wave_file	CIF_in_file(in,1)
define	WAVE_EXISTS	(CIF_in_status(in,1) == CIF_OK)

#---------------------------------------------------------------------------
.help waveoff Jun94 source
.ih
NAME
waveoff -- Find offsets between observed and predicted wavelengths.
.ih
DESCRIPTION
The offset is given in the sense of the following:

.nf
	offset = predicted - observed.
.fi
.endhelp
#---------------------------------------------------------------------------
procedure waveoff (in, offsets, lines, cor_size, preview, gp)

pointer	in			# I:  CIF Object.
pointer	offsets			# I:  Offsets descriptor.
pointer	lines			# I:  PtNe line table descriptor.
int	cor_size		# I:  Half-size of the correlation function.
bool	preview			# I:  TRUE to view the correlation functions.
pointer	gp			# I:  Graphics descriptor.

# Declarations
int	checkdim()		# Get "real" dimension of image.
bool	cif_next()		# Get next set of input files.
double	dc			# Approximate linear dispersion.
double	deltas			# Sample dispersion.
double	imgetd()		# Get double-valued header parameter.
pointer	imgl1d()		# Get double-valued image vector.
pointer	immap()			# Open an image.
int	len			# Length of the data vectors.
pointer	obs_data		# Observation data.
pointer	obs_im			# Observation image object.
double	offset			# Shift.
pointer	pred			# Predicted observation.
double	soffset			# Offset in sample units.
bool	streq()			# Are strings equal?
char	sx[SZ_LINE]		# Generic.
double	value			# Correlation value.
pointer	wave_data		# Wavelength vector.
pointer	wave_im			# Wavelength vector image object.
int	wave_mode		# Mode to open the wavelength file.
double	woffset			# Offset in wavelength units.

begin
	# Open the wavelength file with the appropriate mode.
	wave_mode = READ_ONLY
	
	# Loop through all the input.
	pred = NULL
	while (cif_next (in, CIF_NEXT_GROUP)) {

	    # Open the observation.
	    iferr (obs_im = immap (Obs_file, READ_ONLY, NULL)) {
		call eprintf ("WARNING: waveoff: Could not open observation '%s'\n	skipping....\n")
		call pargstr (Obs_file)
		next
	    }
	    if (checkdim (obs_im) > 1) {
		call eprintf ("WARNING: waveoff: Image '%s' is not 1 dimensional\n	skipping....\n")
		call pargstr (Obs_file)
		next
	    }
	    
	    # Open the wavelength vector image.  If there isn't one, don't
	    # try it.
	    if (!WAVE_EXISTS) {
		call eprintf ("WARNING: waveoff: No wavelength vector specified for observation '%s'\n	skipping...\n")
		call pargstr (Obs_file)
		next
	    }
	    wave_im = immap (Wave_file, wave_mode, NULL)
	    if (checkdim (obs_im) > 1) {
		call eprintf ("WARNING: waveoff: Wavelength image '%s' is not 1 dimensional\n	skipping....\n")
		call pargstr (Obs_file)
		next
	    }
	    
	    # Sanity check, the wavelength and observations must be the
	    # same length.
	    if (IM_LEN(obs_im,1) != IM_LEN(wave_im,1)) {
		call eprintf ("WARNING: waveoff: observation '%s' is not the same length as\n	wavelength vector '%s', skipping....\n")
		call pargstr (Obs_file)
		call pargstr (Wave_file)
		next
	    }
	    len = IM_LEN(obs_im,1)

	    # Get the vectors.
	    obs_data = imgl1d (obs_im)
	    wave_data = imgl1d (wave_im)

	    # Create the predicted observation.
	    call realloc (pred, len, TY_DOUBLE)
	    call wo_pred_obs (Wave_data(1), len, lines, Pred)

	    # Calculate the linear dispersion.
	    dc = (Wave_data(len) - Wave_data(1)) / len

	    # Determine the gross (linear) offset between the
	    # predicted wavelengths and what is observed.
	    call wo_offset (Obs_file, Obs_data, Pred, len, cor_size,
			    preview, gp, offset, value)

	    # Calculate shift in other units, only if there is a shift.
	    if (IS_INDEFD (offset)) {
		woffset = INDEFD
		soffset = INDEFD
	    } else {
		
		# Calculate shift in wavelength.  Assume linear wavelength
		# dispersion.
		woffset = dc * offset

		# If this is a GHRS image, calculate offset in sample
		# space.
		soffset = INDEFD
		ifnoerr (call imgstr (obs_im, "instrume", sx, SZ_LINE)) {
		    if (streq (sx, "HRS")) {
			ifnoerr (deltas = imgetd (obs_im, "deltas")) {
			    soffset = deltas * offset
			}	    
		    }
		}
	    }
	    
	    # Write the results to the offset table
	    call wo_add_offsets (offsets, Obs_file, offset, value,
				 woffset, soffset)

	    # Close the images.
	    call imunmap (wave_im)
	    call imunmap (obs_im)
	}
	
	# That's all folks.
	if (pred != NULL)
	    call mfree (pred, TY_DOUBLE)
end	
#---------------------------------------------------------------------------
# End of waveoff
#---------------------------------------------------------------------------
