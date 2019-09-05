include	<cif.h>

#---------------------------------------------------------------------------
.help waveoff Oct93 source
.ih
NAME
waveoff -- Compute gross offsets between observered wavecals and predicted.
.ih
DESCRIPTION
See the task level help for task waveoff.  What follows is primarily
programming notes, if any.
.endhelp
#---------------------------------------------------------------------------
procedure t_waveoff()

# Declarations
pointer	cif_alloc()		# Allocated a CIF object.
bool	clgetb()		# Get boolean-valued parameter.
int	clgeti()		# Get integer-valued parameter.
pointer	gopen()			# Open the graphics device.
pointer	gp			# Graphics descriptor.
pointer	in			# Coordinated Input File (CIF) object.
pointer	lines			# The Line List object.
pointer	offsets			# WaveOff Offset object.
bool	preview			# TRUE to preview the correlation.
char	sx[SZ_PATHNAME]		# Generic string.
char	sy[SZ_PATHNAME]		# Generic string.
pointer	wo_alloc_line()		# Allocate the Line List object.
pointer	wo_alloc_offsets()	# Allocate the Offest object.

begin
	# Open the coordinated input object.
	in = cif_alloc (1, 0)
	call clgstr ("input", CIF_p_file_list(in), CIF_SZ_FNAME)
	call clgstr ("input_ext", CIF_p_ext(in,), CIF_SZ_FNAME)
	call clgstr ("wave", CIF_in_file_list(in,1), CIF_SZ_FNAME)
	call clgstr ("wave_ext", CIF_in_ext(in,1), CIF_SZ_FNAME)

	# Open the offsets table.
	call clgstr ("output", sx, SZ_PATHNAME)
	iferr (offsets = wo_alloc_offsets (sx)) {
	    call sprintf (sy, SZ_PATHNAME, "waveoff: could not open table '%s'")
	    call pargstr (sx)
	    call error (1, sy)
	}
	
	# Open the line list table
	call clgstr ("linetab", sx, SZ_PATHNAME)
	lines = wo_alloc_line (sx)

	# If previewing, open up the graphics device.
	preview = clgetb ("preview")
	if (preview) {
	    call clgstr ("device", sx, SZ_PATHNAME)
	    gp = gopen (sx, NEW_FILE, STDGRAPH)
	} else
	    gp = NULL
	
	# Find the offsets.
	call waveoff (in, offsets, lines, clgeti ("max_offset"), preview, gp)
	
	# That's all folks.
	if (gp != NULL)
	    call gclose (gp)
	call wo_free_line (lines)
	call wo_free_offsets (offsets)
	call cif_free (in)
end
#---------------------------------------------------------------------------
# End of t_waveoff
#---------------------------------------------------------------------------
