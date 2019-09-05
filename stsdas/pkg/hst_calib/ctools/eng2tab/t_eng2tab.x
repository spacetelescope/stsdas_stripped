include	<cif.h>

# Number of input files.
define	N_FILES			1

# Memory Management
define	Output_file		Memc[output_file]
define	Sx			Memc[sx]
define	Sy			Memc[sy]

#---------------------------------------------------------------------------
.help eng2tab May94 source
.ih
NAME
eng2tab -- Read HST engineering information and convert to readable tables.
.endhelp
#---------------------------------------------------------------------------
procedure t_eng2tab()

# Input file list handling.
pointer	cif_alloc()		# Allocate a CIF object.
bool	cif_next()		# Get next input file.
pointer	input			# Input file list object.

# Output table.
pointer	output_file		# Output file name.

# Bit unpack object.
pointer	bu_alloc()		# Allocate an GHRS UDL object.
pointer	bu			# The BIT UNPACK object.

# Generic.
bool	clgetb()		# Get boolean-valued parameter.
int	errget()		# Retrieve error information.
bool	fmt			# Format?
int	i			# Generic.
pointer	sp			# Stack Pointer.
pointer	sx, sy			# Generic string.

begin
	call smark (sp)
	call salloc (output_file, SZ_PATHNAME, TY_CHAR)
	call salloc (sx, SZ_COMMAND, TY_CHAR)
	call salloc (sy, SZ_COMMAND, TY_CHAR)
	
	# Get the input files.
	input = cif_alloc (N_FILES, 0)
	call clgstr ("input", CIF_p_file_list(input), CIF_SZ_FNAME)
	call clgstr ("defext", CIF_p_ext(input), CIF_SZ_FNAME)

	# Get the output table name.
	call clgstr ("output", Output_file, SZ_PATHNAME)
	
	# Open the BIT UNPACK object.
	call clgstr ("definition", Sx, SZ_PATHNAME)
	call clgstr ("items", Sy, SZ_PATHNAME)
	bu = bu_alloc (Sx, Sy)

	# Loop through each input, populating the bit unpack object.
	fmt = clgetb ("format")
	while (cif_next (input, CIF_NEXT_GROUP)) {
	    iferr (call bu_read_image (bu, CIF_p_file(input), fmt)) {
		i = errget (Sx, SZ_COMMAND)
		call eprintf ("%s\n")
		call pargstr (Sx)
		call eprintf ("WARNING: could not decode file '%s'\n    ....skipping\n")
		call pargstr (CIF_p_file(input))
		next
	    }
	    
	}

	# Save the BIT UNPACK object to a table.
	call bu_write_table (bu, Output_file, NEW_FILE, NULL)

	# That's all folks.
	call cif_free (input)
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_eng2tab
#---------------------------------------------------------------------------
