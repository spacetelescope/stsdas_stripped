# HCHECK -- Perform a consistency check on an image header
#
# B.Simon	30-Apr-90	Original

procedure hcheck ()

#--
pointer	input		# Image template
pointer	chkfile		# Text file containing consistency checks

bool	title, hasgroup
int	fd, ngroup, igroup, iline, nc
pointer	sp, cluster, image, command, tp, im

bool	tp_fetch()
int	open(), imtgetim(), getlongline()
pointer	imtopenp(), tp_open(), immap()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (chkfile, SZ_FNAME, TY_CHAR)
	call salloc (cluster, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (command, SZ_COMMAND, TY_CHAR)

	# Read the task parameters

	input = imtopenp ("input")
	call clgstr ("chkfile", Memc[chkfile], SZ_FNAME)

	fd = open (Memc[chkfile], READ_ONLY, TEXT_FILE)

	# Check each image

	while (imtgetim (input, Memc[cluster], SZ_FNAME) != EOF) {

	    # Check each group in each image

	    tp = tp_open (Memc[cluster], 0, ngroup)

	    igroup = 1
	    hasgroup = false
	    while (tp_fetch (tp, Memc[image])) {
		if (igroup > 1 && ! hasgroup)
		    break

		title = true
		im = immap (Memc[image], READ_ONLY, 0)

		call seek (fd, BOF)
	        repeat {
		    nc  = getlongline (fd, Memc[command], SZ_COMMAND, iline)
		    if (nc <= 0)
			break

		    Memc[command+nc-1] = EOS
		    call docheck (im, igroup, Memc[command], hasgroup, title)
		}

		call imunmap (im)
		igroup = igroup + 1
	    }
	    call tp_close (tp)
	}	
	call sfree (sp)
end
