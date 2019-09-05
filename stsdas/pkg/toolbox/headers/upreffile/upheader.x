define	ICODE		1
define	UCODE		2
define	DCODE		3

#* HISTORY *
#* B.Simon	28-Jun-92	Original
#* B.Simon	05-Apr-92	Allow keywords to be added
#* B.Simon	06-Feb-98	Removed oldfile
#* B.Simon	15-Jul-98	Modified to handle new getref file

# UPHEADER -- Update header keywords in a file

procedure upheader (upr, keyword, value, oper, verify)

pointer	upr		# i: file descriptor
char	keyword[ARB]	# i: keyword name
char	value[ARB]	# i: keyword value
char	oper[ARB]	# i: operation to perform to header
bool	verify		# i: verify before changing?
#--
bool	found, update
int	code
pointer	sp, key, val, fname, oldval, newval, errmsg

string	oplist    "|insert|update|delete|"
string  badoper   "Unknown operation (%s)"
string	asknoval  "\nOld value of %s.%s not found\nNew value of %s.%s = %s\n"
string	askisval  "\nOld value of %s.%s = %s\nNew value of %s.%s = %s\n"
string	askdelval "\nOld value of %s.%s = %s\nKeyword will be deleted\n"

bool	clgetb(), check_eq(), getqual()
int	strdic()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (val, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (newval, SZ_FNAME, TY_CHAR)
	call salloc (oldval, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	code = strdic (oper, Memc[val], SZ_FNAME, oplist)

	if (code == 0) {
	    call sprintf (Memc[errmsg], SZ_LINE, badoper)
	    call pargstr (oper)

	    call error (1, Memc[errmsg])
	}

	call strcpy (keyword, Memc[key], SZ_FNAME)
	call strcpy (value, Memc[val], SZ_FNAME)
	
	repeat {
	    # Add directory name to file
	    
	    call addrefdir (Memc[val], Memc[newval], SZ_FNAME)

	    # Read existing value of keyword
	    # Set to empty string if not found
	    
	    iferr {
		found = true
		call upr_getkey (upr, Memc[key], Memc[oldval], SZ_FNAME)
	    } then {
		found = false
		Memc[oldval] = EOS
	    }

	    if (found) {
		update = (! check_eq (Memc[oldval], Memc[newval])) ||
			 (code == DCODE)
	    } else {
		update = (code == ICODE)
	    }

	    # If old keyword value is same as new value, do not update
	    # Otherwise, ask user to verify update
	    
	    if (update && verify) {
		call clputb ("update", true)
		call upr_getname (upr, Memc[fname], SZ_FNAME)

		if (code == DCODE) {
		    call eprintf (askdelval)
		    call pargstr (Memc[fname])
		    call pargstr (Memc[key])
		    call pargstr (Memc[oldval])

		} else if (found) {
		    call eprintf (askisval)
		    call pargstr (Memc[fname])
		    call pargstr (Memc[key])
		    call pargstr (Memc[oldval])
		    call pargstr (Memc[fname])
		    call pargstr (Memc[key])
		    call pargstr (Memc[newval])

		} else {
		    call eprintf (asknoval)
		    call pargstr (Memc[fname])
		    call pargstr (Memc[key])
		    call pargstr (Memc[fname])
		    call pargstr (Memc[key])
		    call pargstr (Memc[newval])
		}

		update = clgetb ("update")
	    }

	    # Make update
	    
	    if (update) {
		if (code == DCODE) {
		    call upr_delkey (upr, Memc[key])
		} else {
		    call upr_putkey (upr, Memc[key], Memc[newval])
		}
	    }
	    
	} until (! getqual (Memc[key], Memc[val], SZ_FNAME))

	call sfree (sp)

end
