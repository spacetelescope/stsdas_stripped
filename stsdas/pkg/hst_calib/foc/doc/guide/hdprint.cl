# HDPRINT -- create hd file driven by table of contents file

procedure hdprint( tocfile )

file tocfile = "focdoc$FOC_TOC"
struct	*inimglist      {prompt = "not user parameter" }

#--

begin

string	ll, tok1, tok2, tok3, lastone

	inimglist 	= tocfile
	lastone = "############"
	tok2    = "#"
        tok3    = "#"
	print ( "$defdir		= \"focdoc$/\" ")

	print ( "FOC_TOC\t\thlp=FOC_TOC")

	# loop over all the names
	while ( fscan( inimglist,   tok1, tok2, tok3 ) != EOF ) {
	    if ( tok1 != "#" && substr(tok1, 1, 12) != lastone  ) {
		if ( tok1 == "*" ) {
		    tok1 = tok2
		    tok2 = tok3
		}
		if ( strlen( tok2) > 7 )
	            ll = tok2//"\t\thlp="//tok2//".hlp"
		else
	            ll = tok2//"\t\t\thlp="//tok2//".hlp"
		lastone = tok1
	        print( ll )
	    }
	}
	print("focgeom\t\t\thlp=focgeom$focgeom.men,")
	print("\t\t\tpkg=focgeom$focgeom.hd")
	print("focphot\t\t\thlp=focphot$focphot.men,")
	print("\t\t\tpkg=focphot$focphot.hd")
	print("focutility\t\thlp=focutility$focutility.men,")
	print("\t\t\tpkg=focutility$focutility.hd")

end
