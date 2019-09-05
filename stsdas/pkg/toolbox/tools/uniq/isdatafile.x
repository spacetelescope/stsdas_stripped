# IS_DATAFILE -- Check to see if a file is the data portion of an image 
#
# B.Simon	19-Sep-90	First Code

bool procedure is_datafile (dfile)

char	dfile[ARB]	# i: Header file name
#--
bool	status
int	junk
pointer	sp, hfile, extension

bool	streq(), access()
int	fnextn(), fnroot()

begin
	# Allocate storage space for strings

	call smark (sp)
	call salloc (hfile, SZ_FNAME, TY_CHAR)
	call salloc (extension, SZ_FNAME, TY_CHAR)

	# Extract extension from data file name and check its legality

	junk = fnroot (dfile, Memc[hfile], SZ_FNAME)
	if (fnextn (dfile, Memc[extension], SZ_FNAME) != 3) {
	    status = false

	# Build data file name from header. Only two image
	# types are recognized -- OIF and STF

	} else if (streq (Memc[extension], "pix")) {
	    status = true
	    call strcat (".imh", Memc[hfile], SZ_FNAME)

	} else if (Memc[extension+2] == 'd') {
	    status = true
	    Memc[extension+2] = 'h'
	    call strcat (".", Memc[hfile], SZ_FNAME)
	    call strcat (Memc[extension], Memc[hfile], SZ_FNAME)

	} else {
	    status = false
	}

	if (status)	
	    status = access (Memc[hfile], 0, 0)

	call sfree (sp)
	return (status)
end
