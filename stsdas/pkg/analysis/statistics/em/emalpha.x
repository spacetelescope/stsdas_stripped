#* HISTORY *
#* B.Simon	24-Sep-92	original

# EMALPHA -- Set initial estimate of regression coefficients

procedure emalpha (emestim, alpha, nvar)

bool	emestim		# i: Read initial estimates from param file?
double	alpha[ARB]	# o: Regression estimates
int	nvar		# i: Number of coefficients in regression
#--
int	ic, jc, ivar, junk
pointer	sp, val, astring

string	notenuf  " Not enough initial estimates. Rest assumed to be zero.\n"
string	toomany  " Too many inital estimates. Rest were ignored.\n"

int	word_fetch(), ctod()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (val, SZ_FNAME, TY_CHAR)
	call salloc (astring, SZ_FNAME, TY_CHAR)

	# Set inital estimates to default values

	call aclrd (alpha, nvar)
	alpha[nvar+1] = 1.0
	
	# Read inital estimates from parameter file
	# Estimates are in the form of a comma delimeted string

	if (emestim) {
	    call clgstr ("alpha", Memc[astring], SZ_FNAME)

	    ic = 1
	    do ivar = 1, nvar {
		if (word_fetch (Memc[astring], ic, Memc[val], SZ_FNAME) <= 0) {
		    call printf (notenuf)
		    break
		}

		jc = 1
		junk = ctod (Memc[val], jc, alpha[ivar])
	    }

	    if (word_fetch (Memc[astring], ic, Memc[val], SZ_FNAME) > 0)
		call printf (toomany)
	}
	
	call sfree (sp)
end
