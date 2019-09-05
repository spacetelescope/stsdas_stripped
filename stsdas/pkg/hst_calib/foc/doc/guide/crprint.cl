# CRPRINT -- create script for printing GUIDE, driven by table of contents file.

procedure crprint( tocfile )

file tocfile = "focdoc$FOC_TOC"
###file output  = "output" {prompt=">name of output file in script"}
struct	*inimglist      {prompt = "not user parameter" }

#--

begin

string	ll, tok1, tok2, tok3, lastone
string	formfeed, redirect, op

###	delete( output)
	redirect = ",>>(output) )"
	formfeed = "\t print(\"\\f\""//redirect


print ( "procedure FOC_PRINT_GUIDE" )

op =  "file output = \"FOC_GUIDE.doc\" {prompt=\">output file for text\"} "
print ( op )
print ( "file graphout = \"FOC_GUIDE.figs\" {prompt=\">output for graphics\"}" )

print ( "begin")

###print( "string	hsaved")

print ("	if ( !deftask(\"igi\") )  plot")
print ( "\t print(\"\\f\",>(output) )" )
		
###print ("	hsaved = help.helpdb")

###print ("	help.helpdb = \"focdoc$FOC_GUIDE\" ")

print ( "	ty (\"focdoc$FOC_GUIDE_TITLE\""//redirect )
	    print( formfeed )
print ("	ty (\"focdoc$FOC_TOC\""//redirect )
	    print( formfeed )

	# set the listfiles as the file with separated names
	inimglist 	= tocfile
	lastone = "############"
	tok2    = "#"
        tok3    = "#"

	# loop over all the names
	while ( fscan( inimglist,   tok1, tok2, tok3 ) != EOF ) {
	    if ( tok1 != "#" && substr(tok1,1,12) != lastone  ) {
		if ( tok1 == "*" ) {
		    tok1 = tok2
		    tok2 = tok3
		}
		lastone = tok1
	        ll = "\t help(\""//tok2//"\""//redirect 
	        print( ll )
	        print( formfeed )
	    }
	}

print ("	ty (\"focdoc$FOC_INDEX\""//redirect )
	    print( formfeed )

###print ( "	help.helpdb = hsaved")

print ( "end")

end
