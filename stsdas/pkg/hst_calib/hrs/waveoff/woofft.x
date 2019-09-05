# Memory management
define	Corr		Memd[corr]

#---------------------------------------------------------------------------
.help wo_offset Nov93 source
.ih
NAME
wo_offset -- Find offset between observed and predicted wavelengths.
.endhelp
#---------------------------------------------------------------------------
double procedure wo_offset (in_file, obs, pred, len, max_offset, preview, gp,
			    offset, value)

pointer	in_file			# I:  Observation file name.
double	obs[len]		# I:  Observed wavelengths.
double	pred[len]		# I:  Predicted wavelengths.
int	len			# I:  Length of arrays.
int	max_offset		# I:  Maximum offset to search over.
bool	preview			# I:  True to graphically display correlations.
pointer	gp			# I:  Graphics descriptor.
double	offset			# O:  The offset in pixels.
double	value			# O:  The correlation value

pointer	corr			# Correlation function.
int	cs			# Half-Size of correlation function.
bool	wo_preview()		# Preview correlation function.
pointer	sp			# Stack Pointer.

begin
	call smark (sp)
	
	# Cross-correlate to find the shift.
	if (IS_INDEFI(max_offset))
	    cs = len / 4 - 1
	else if (len < max_offset*2+1)
	    cs = len / 4 - 1
	else
	    cs = max_offset
	call salloc (corr, cs*2+1, TY_DOUBLE)
	
	call wo_correlate (obs, pred, len, 1, cs, Corr)

	# Find the shift.
	call wo_maxcorr (Corr, cs, 1, INDEFR, INDEFI,
			 INDEFI, INDEFI, false, offset, value)

	# Show the correlation function if so desired.
	if (preview)
	    preview = wo_preview (in_file, Corr, cs, offset, value, gp)

	# That's all folks.
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of wo_offset
#---------------------------------------------------------------------------
