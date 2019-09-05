# PARPRINT - create task statement from lpar output

procedure parprint( tname , input)

string	tname = "listpix"
file 	input = "testpar.par"
struct	*parlist      {prompt = "not user parameter" }
struct  tline 		{length=160}
string	mode = "q"
#--

begin

string	ll, tok1, tok2, tok3, ttok
int	totlen

####	ty ( input )
	tline 	= tname//" "
	parlist = input

	# loop over all the names
	while ( fscan( parlist, tok1, tok2, tok3 ) != EOF ) {
	    if ( substr( tok1, 1, 1) == "(" ) {
		ll = substr( tok1, 2, strlen(tok1) )
		ttok = substr(tok3, 1, strlen(tok3)-1)
		ll = ll//"="//ttok//" "
	    } else {
		ll = tok3//" "
	    }
	    totlen = strlen(ll) + strlen(tline)
####	    print ( strlen(ll), strlen(tline), totlen )
	    if ( totlen > 80 ) {
		print ( tline//" \\" )
		tline = "        "//ll
	    } else {
		tline = tline//ll
	    }
	}

	print ( tline )
end
