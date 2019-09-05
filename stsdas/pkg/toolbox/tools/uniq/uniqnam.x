include <ctype.h>

# UNIQNAME -- Create a unique file name
#
# B.Simon	27-Aug-87	First Code
# B.Simon	14-Oct-92	Add wfpc II

procedure t_uniqnam ()

pointer extension	# File name extension
pointer	instr		# HST instrument associated with this file
pointer	value		# File name created by this task

int	ip
pointer	sp, trailer

int	instcode(), access()

begin
	# Allocate storage space for strings

	call smark (sp)
	call salloc (instr, SZ_FNAME, TY_CHAR)
	call salloc (extension, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (trailer, SZ_FNAME, TY_CHAR)

	# Read input parameters

	call clgstr ("extension", Memc[extension], SZ_FNAME)
	call clgstr ("instr", Memc[instr], SZ_FNAME)

	# Strip leading white space from extension

	while (IS_WHITE(Memc[extension]))
	    extension = extension + 1

	# Add the code letter to the extension to form the trailer

	Memc[trailer] = instcode (Memc[instr])

	ip = 1
	if (Memc[extension] != '.' && Memc[extension] != EOS) {
	    Memc[trailer+ip] = '.'
	    ip = ip + 1
	}
	Memc[trailer+ip] = EOS
	call strcat (Memc[extension], Memc[trailer], SZ_FNAME)

	# Make a unique id from the current time. Add the trailer to create
	# a file name. Loop until we get a unique file name.

	repeat {
	    call makeid (Memc[value], SZ_FNAME)
	    call strcat (Memc[trailer], Memc[value], SZ_FNAME)
	} until (access (Memc[value], 0, 0) == NO)

	# Write the file name and free allocated memory

	call clpstr ("value", Memc[value])
	call sfree (sp)
end
