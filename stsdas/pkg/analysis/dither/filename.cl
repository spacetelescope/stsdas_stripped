procedure filename(input)

# Routine to remove directory prefixes from a file name.

# 18 October 1996 by Andy Fruchter
# based on M. Dickinson's fileroot.cl

string	input	 	{prompt="File name"}
string  main		{"",prompt="Returned main filename"}

begin

	string	fname		# Equals filename
        string  newname
	string	revname		# Reversed version of input string
	int 	ilen,ipos,ic	# String position markers
	int	ii		# Counter


# Get query parameter.

	fname = input

# Reverse filename string character by character --> revname.

	ilen = strlen(fname)

# Now remove directory prefixes

      ipos = stridx("/",fname)
        if (ipos != 0) {
                ipos = ipos + 1
                main = substr(fname,ipos,ilen)
           } else {
             main = fname
           }
	newname = main
	print (newname)            

end
