include	<fset.h>

#* HISTORY *
#* B.Simon	28-Jun-92	Original
#* B.Simon	30-Oct-97	Rewritten to run outside cdbs
#* B.Simon	06-Feb-98	Removed oldfile
#* B.Simon	15-Jul-98	Modified to handle new getref output
#* B.Simon	07-Dec-98	Allow image templates in delta file
#* B.Simon	11-Dec-98	Add source checking

# UPREFFILE -- Update an image with the names of the best reference files

procedure upreffile ()

#--
pointer	input		# input file, produced by getreffile
pointer	srclist		# list of sources whose keywords should be processed
bool	verify		# verify each change to the header?

int	ref
pointer	sp, key, value, template, file, oper, source
pointer	oldfile, errmsg, list, upr

string	allsources  "obc,okr,ref"
string	badsource   "WARNING Unrecognized source, processed anyway (%s)\n"
string	badimage    "Cannot open image (%s)"

bool	clgetb(), strne()
int	fstati(), open(), imtgetim(), rd_reffile(), word_match()
pointer	imtopen(), upr_open()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (srclist, SZ_FNAME, TY_CHAR)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (template, SZ_FNAME, TY_CHAR)
	call salloc (file, SZ_FNAME, TY_CHAR)
	call salloc (oper, SZ_FNAME, TY_CHAR)
	call salloc (source, SZ_FNAME, TY_CHAR)
	call salloc (oldfile, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	call post_exit_handler ()

	if (fstati (STDIN, F_REDIR) == NO) {
	    call clgstr ("input", Memc[input], SZ_FNAME)
	} else {
	    call strcpy ("STDIN", Memc[input], SZ_FNAME)
	}

	call clgstr ("source", Memc[srclist], SZ_FNAME)
	call strlwr (Memc[srclist])
	call check_source (allsources, Memc[srclist])

	verify = clgetb ("verify")

	ref = open (Memc[input], READ_ONLY, TEXT_FILE)

	upr = NULL
	Memc[oldfile] = EOS

	while (rd_reffile (ref, Memc[key], Memc[value], Memc[template], 
			   Memc[oper], Memc[source], SZ_FNAME) != EOF) {

	    # Skip change if source is not in source list

	    if (Memc[source] != EOS) {
		call strlwr (Memc[source])
		if (word_match (Memc[source], allsources) == 0) {
		    call eprintf (badsource)
		    call pargstr (Memc[source])

		} else if (word_match (Memc[source], Memc[srclist]) == 0) {
		    next
		}
	    }

	    # Loop over images. Cache image descriptor.

	    list = imtopen (Memc[template])
	    while (imtgetim (list, Memc[file], SZ_FNAME) != EOF) {
		if (strne (Memc[file], Memc[oldfile])) {
		    if (upr != NULL)
			call upr_close (upr)

		    upr =  upr_open (Memc[file], READ_WRITE, NULL)
		    call strcpy (Memc[file], Memc[oldfile], SZ_FNAME)
		}

		call upheader (upr, Memc[key], Memc[value], Memc[oper], verify)
	    }

	    call imtclose (list)
	}

	if (upr != NULL)
	    call upr_close (upr)

	call close (ref)
	call sfree (sp)
end
