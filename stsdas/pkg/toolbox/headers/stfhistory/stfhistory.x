define	SZ_TEXT		2000

# STFHISTORY -- add text file to STF image header as HISTORY
#
# D. Giaretta 25-May-1988	Original
# B. Simon    18-Jan-1990	Modified to use imputh

procedure t_stfhistory()

pointer	input		# input image template
pointer	text		# text file with history text
bool	verbose		# diagnostic message switch
#--
int	fd, nc, linenum
pointer	sp, im, fname

string	keyword  "HISTORY"

bool	clgetb()
int	imtgetim(), getlongline()
pointer	imtopenp(), immap(), open()

begin

	# Allocate dynamic memory used to hold strings

	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (text, SZ_TEXT, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Read task parameters

	input = imtopenp ("input")
	call clgstr( "text", Memc[text], SZ_FNAME)
	verbose = clgetb ("verbose")

	# If text is contained in a file, open the file

	if (Memc[text] == '@')
	    fd = open (Memc[text+1], READ_ONLY, TEXT_FILE)
	else
	    fd = 0

	# Loop over all images named in template

	while (imtgetim (input, Memc[fname], SZ_FNAME) != EOF) {
	    im = immap (Memc[fname], READ_WRITE, NULL)

	    if (fd == 0) {
		call imputh (im, keyword, Memc[text])
	    } else {
		linenum = 0
		call seek (fd, BOF)
		repeat {
		    nc = getlongline (fd, Memc[text], SZ_TEXT, linenum)
		    if (nc == EOF)
			break
		    else {
			Memc[text+nc-1] = EOS	# remove newline char
		        call imputh (im, keyword, Memc[text])
		    }
		}
	    }
	    call imunmap (im)

	    # Print diagnostic message

	    if (verbose) {
		call printf ("History record added to %s\n")
		    call pargstr (Memc[fname])
		call flush (STDOUT)
	    }
	}

	# Task cleanup

	if (fd != 0)
	    call close (fd)

	call imtclose (input)
	call sfree (sp)
end
