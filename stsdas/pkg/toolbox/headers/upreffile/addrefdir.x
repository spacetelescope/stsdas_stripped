#* HISTORY *
#* B.Simon	30-Jun-92	Original
#* B.Simon	10-Apr-97	Added support for blank names and fits files
#* B.Simon	15-Jul-98	Added is_reffile check

# ADDREFDIR -- Add reference directory to file name

procedure addrefdir (ofile, nfile, maxch)

char	ofile[ARB]	# i: file name without directory
char	nfile[ARB]	# o: file name with directory
int	maxch		# i: declared length of output file name
#--
char	uscore
int	len
pointer	sp, root, ext, errmsg

data	uscore 	 / '_' /
string	badname  "Illegal file name (%s)"

bool	is_reffile()
int	fnroot(), fnextn(), strldx(), strlen()

begin
	# Blank filenames are treated as a special case 
	# and translated to "N/A"

	if (! is_reffile (ofile)) {
	    call strcpy (ofile, nfile, maxch)
	    return
	}

	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (ext, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Break input file name into root and extension

	if (fnroot (ofile, Memc[root], SZ_FNAME) == 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, badname)
	    call pargstr (ofile)
	    call error (1, Memc[errmsg])
	}

	if (fnextn (ofile, Memc[ext], SZ_FNAME) == 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, badname)
	    call pargstr (ofile)
	    call error (1, Memc[errmsg])
	}

	# Decide whether it is a fits file, a geis image or table based on 
	# the first letter of the extension. Get the instrument code from 
	# the letter preceding the last underscore if the file is a fits
	# file, from the last letter of the name if the file is an image 
	# or the second letter of the extension if the file is a table. 
	# Construct the directory name from the file type and instrument. 
	# Hope the reference file naming convention doesn't change (again)
	# or this code is in deep doo-doo.  

	if (Memc[ext] == 'f') {
	    len = strldx (uscore, Memc[root]) - 1

	    if (Memc[root+len-1] == 'm') {
		call strcpy ("?tab$", nfile, maxch)
	    } else {
		call strcpy ("?ref$", nfile, maxch)
	    }

	    nfile[1] = Memc[root+len-1]

	} else if (Memc[ext] == 'r' || Memc[ext] == 'b') {
	    len = strlen (Memc[root])
	    call strcpy ("?ref$", nfile, maxch)
	    nfile[1] = Memc[root+len-1]

	} else {
	    call strcpy ("?tab$", nfile, maxch)
	    nfile[1] = Memc[ext+1]
	}

	call strcat (ofile, nfile, maxch)

	call sfree (sp)
end
