# UNIQFILE -- Rename a file, giving it a unique name
#
# B.Simon	01-Dec-88	First Code
# B.Simon	23-Jan-89	Modified sprintf() for history
# B.Simon	25-Sep-89	Added verbose parameter

procedure t_uniqfile ()

pointer	fname		# File name
pointer	instr		# HST instrument associated with this file

bool	verbose, image
int	junk
long	aeon
pointer	sp, im, tp, oldname, newname, dirname
pointer	extension, newroot, date, history

data	aeon	/ 0 /

string  mformat	"%s -> %s\n"			  	# Verbose message fmt
string	hformat	"%s renamed to %s on %s"	  	# History record format

bool	clgetb()
int	instcode(), fnldir(), fnextn(), imaccess(), access()
long	clktime()
pointer	immap(), tbtopn()

begin
	# Allocate storage space for strings

	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (instr, SZ_FNAME, TY_CHAR)
	call salloc (oldname, SZ_FNAME, TY_CHAR)
	call salloc (newname, SZ_FNAME, TY_CHAR)
	call salloc (dirname, SZ_FNAME, TY_CHAR)
	call salloc (newroot, SZ_FNAME, TY_CHAR)
	call salloc (extension, SZ_FNAME, TY_CHAR)
	call salloc (date, SZ_FNAME, TY_CHAR)	
	call salloc (history, SZ_LINE, TY_CHAR)

	# Read input parameters

	call clgstr ("fname", Memc[fname], SZ_FNAME)
	call clgstr ("instr", Memc[instr], SZ_FNAME)
	verbose = clgetb ("verbose")

	# Get the directory name and see if file is an image

	call imgcluster (Memc[fname], Memc[oldname], SZ_FNAME)
	junk = fnldir (Memc[oldname], Memc[dirname], SZ_FNAME)

	image = imaccess (Memc[oldname], READ_ONLY) == YES &&
		imaccess (Memc[oldname], NEW_FILE) == YES

	# Add the code letter to the front of the extension

	Memc[extension] = instcode (Memc[instr])
	Memc[extension+1] = '.'
	if (fnextn (Memc[oldname], Memc[extension+2], SZ_FNAME) == 0)
	    Memc[extension+1] = EOS

	# Make a unique id from the current time. Add the extension to create
	# a file name. Check if this is a unique file name, and if so,
	# rename the file and exit the loop.

	repeat {
	    call makeid (Memc[newroot], SZ_FNAME)

	    call strcpy (Memc[dirname], Memc[newname], SZ_FNAME)
	    call strcat (Memc[newroot], Memc[newname], SZ_FNAME)
	    call strcat (Memc[extension], Memc[newname], SZ_FNAME)

	    call cnvtime (clktime (aeon), Memc[date], SZ_FNAME)
	    call sprintf (Memc[history], SZ_LINE, hformat)
	    call pargstr (Memc[oldname])
	    call pargstr (Memc[newname])
	    call pargstr (Memc[date])

	    if (image) {
		if (imaccess (Memc[newname], READ_ONLY) == NO) {
		    call imrename (Memc[oldname], Memc[newname])

		    # Add a history record to the image

		    im = immap (Memc[newname], READ_WRITE, 0)
		    call imputh (im, "HISTORY", Memc[history])
		    call imunmap (im)

		    break
		}
	    } else {
		if (access (Memc[newname], 0, 0) == NO) {
		    call rename (Memc[oldname], Memc[newname])

		    tp = tbtopn (Memc[newname], READ_WRITE, NULL)
		    call tbhadt (tp, "HISTORY", Memc[history])
		    call tbtclo (tp)

		    break
		}
	    }
	} 

	# Print message with old and new file names

	if (verbose) {
	    call printf (mformat)
		call pargstr (Memc[oldname])
		call pargstr (Memc[newname])
	}

	call sfree (sp)
end
