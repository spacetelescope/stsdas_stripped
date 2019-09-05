#* HISTORY *
#* B.Simon	01-Sep-92	original

# CHECKLIST -- Create a checklist of test to perform from input string

procedure checklist (keywords, tests, list, maxlist)

char	keywords[ARB]	# i: dictionary of possible tests
char	tests[ARB]	# i: names of tests to be performed
int	list[ARB]	# o: checklist of tests to be performed
int	maxlist		# i: declared length of checklist
#--
int	ic, itest
pointer	sp, testname, fullname

string	badtest  "WARNING: unrecognized test name ignored (%s)\n"
string	badsize  "checklist: number of tests exceeds length of checklist"

bool	isblank()
int	strdic(), word_fetch()

begin
	# A blank list of names means perform all tests

	if (isblank (tests)) {
	    call amovki (YES, list, maxlist)
	    return
	} else {
	    call amovki (NO, list, maxlist)
	}

	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (testname, SZ_FNAME, TY_CHAR)
	call salloc (fullname, SZ_FNAME, TY_CHAR)

	# Get each test name from list and look it up in dictionary
	# Set the corresponding variable in the checklist array to YES

	ic = 1
	while (word_fetch (tests, ic, Memc[testname], SZ_FNAME) > 0) {
	    itest = strdic (Memc[testname], Memc[fullname], SZ_FNAME, keywords)

	    if (itest == 0) {
		call eprintf (badtest)
		call pargstr (Memc[testname])

	    } else if (itest > maxlist) {
		call error (1, badsize)

	    } else {
		list[itest] = YES
	    }
	}

	call sfree (sp)
end
