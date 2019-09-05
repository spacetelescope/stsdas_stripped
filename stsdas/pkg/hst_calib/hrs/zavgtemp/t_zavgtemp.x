include	<error.h>
include	<cif.h>

# Memory management.
define	Sx		Memc[sx]

#---------------------------------------------------------------------------
.help zavgtemp 29Mar95
.ih
NAME
zavgtemp -- Average temperature monitors
.endhelp
#---------------------------------------------------------------------------
procedure t_zavgtemp()

# Declarations
pointer	cif			# Coordinated Input File object.
pointer	cif_alloc()		# Create cif object.
bool	cif_next()		# Get next file.
pointer	im			# Image descriptor.
pointer	immap()			# Open an image.
pointer	keys			# Keys object.
pointer	ot			# Output Table object.
pointer	sp			# Stack pointer.
pointer	sx			# Generic string.
pointer	zt_k_alloc()		# Create a Keys object.
pointer	zt_ot_alloc()		# Output table object.

begin
	call smark (sp)
	call salloc (sx, SZ_PATHNAME, TY_CHAR)
	
	# Open file list.
	cif = cif_alloc (1, 0)
	call clgstr ("input", CIF_p_file_list(cif), CIF_SZ_FNAME)
	call clgstr ("ext", CIF_p_ext(cif), CIF_SZ_FNAME)

	# Get the header keyword list.  If blank, use the GHRS defaults.
	call clgstr ("keywords", Sx, SZ_PATHNAME)
	keys = zt_k_alloc (Sx)

	# Open the output table.
	call clgstr ("output", Sx, SZ_PATHNAME)
	ot = zt_ot_alloc (keys, Sx)

	# Loop through the input images, accumulating keyword values.
	while (cif_next (cif, CIF_NEXT_GROUP)) {

	    # Open image,
	    iferr (im = immap (CIF_p_file(cif), READ_ONLY, NULL)) {
		call erract (EA_WARN)
		call eprintf ("zavgtemp: image %s cannot be opened.\n\t...reading next image.\n")
		call pargstr (CIF_p_file(cif))
		next
	    }

	    # Accumulate the values.
	    call zt_k_accum (keys, im)

	    call imunmap (im)
	}

	# Write out the results.
	call zt_ot_wtab (ot, keys)

	# That's all folks.
	call zt_ot_free (ot)
	call zt_k_free (keys)
	call cif_free (cif)
	call sfree (sp)
end
#---------------------------------------------------------------------------
# End of t_zavgtemp 
#---------------------------------------------------------------------------
