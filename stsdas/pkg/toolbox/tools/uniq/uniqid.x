
# UNIQID -- Create a unique identifier
#
# B.Simon	27-Aug-87	First Code

procedure t_uniqid ()

pointer	value		# Identifier created by this task

pointer	sp

begin
	# Allocate storage space for strings

	call smark (sp)
	call salloc (value, SZ_FNAME+1, TY_CHAR)

	# Make the identifier from the current time

	call makeid (Memc[value], SZ_FNAME)

	# Write the identifier and free allocated memory

	call clpstr ("value", Memc[value])
	call sfree (sp)
end
