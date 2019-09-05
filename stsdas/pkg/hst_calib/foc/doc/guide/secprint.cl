# SECPRINT - add or change section numbers in help files - driven by
# the table of contents file
# Note this procedure creates a temporary file and renames it to the help
# file just processed

procedure secprint( tocfile )

file tocfile = "focdoc$FOC_TOC"
string guide = "FOC" {prompt=">guide name "}
struct	*inimglist      {prompt = "not user parameter" }
struct  *temphelp

#--

begin

string	ll, tok1, tok2, tok3, lastone
string	htok1, htok2, htok3, htok4
file    hfile, newfile
int	linect, junk

	inimglist 	= tocfile
	lastone = "############"
	tok2    = "#"
        tok3    = "#"

	# loop over all the names
	while ( fscan( inimglist,   tok1, tok2, tok3 ) != EOF ) {
	    if ( tok1 != "#" && substr(tok1, 1, 12) != lastone  ) {
		if ( tok1 == "*" ) {
			tok1 = tok2
			tok2 = tok3
		}
		lastone = tok1
		hfile = tok2//".hlp"
	        newfile = "temphelpfile.hlp"
		print ( hfile)
		if ( access( hfile ) ) {
	 	    delete ("temphelpfile.hlp")
		    chsect( hfile, tok1, guide, newfile )
		    rename ( newfile, hfile)
		}
	    }
	}

end
