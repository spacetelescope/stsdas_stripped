procedure checkwfpc(input)

string	input	{prompt = "Image name"}
struct	*ilist	# list of input file names

begin

file 	ifile	# temporary file to hold list of input file names
string	iname	# input file name from list of file names
string	root	# file name root
string	rex	# reference file extension
string	qex	# quality file extension
string	rname	# reference file name
string	qname	# quality file name
int	len_ext # length of the file extension
int	len_root

	# Make sure headers package is loaded
	if (! defpac ("headers") || ! defpac ("tools"))
	    error (1, "The headers and tools packages must be loaded")

	# create a list of file names that match the given pattern
	# redirect it to a temporary file
	ifile = mktemp("tmp$chk")
	files(input,>ifile)

	# read the temporary file using a list structured parameter
	# this assigns one line from the file to the parameter each
	# time it is used in the script
	ilist = ifile
	while (fscan(ilist,iname) != EOF) {

	    # Break the reference file name into its parts
	    fparse (iname)
	    len_ext = strlen(fparse.extension)

	    # GEIS file
	    if (substr(fparse.extension, len_ext, len_ext) == "h") {
	    	root = fparse.directory // fparse.root // "."
	    	rex =  substr (fparse.extension, 2, 4)
	    	qex = "b" // substr(rex, 2, 3)
	    	rname = root // rex
	    	qname = root // qex

	    # FITS file
	    } else {
		len_root = strlen(fparse.root)
	    	root = fparse.directory // substr(fparse.root, 1, len_root-3)
	    	rex =  substr (fparse.root, len_root-2, len_root)
	    	qex = "b" // substr(rex, 2, 3)
	    	rname = root // rex // fparse.extension
	    	qname = root // qex // fparse.extension
	    }

	    # Check reference image and its quality file, if any
	    keypar(rname//"[0]", "INSTRUME")
	    if (keypar.value == "WFPC2") {
		hcheck(rname, "wfpc$checkwfpc2.dat")
		if (access(qname) == YES) {
		    hdiff(rname, qname, keyword=" ", nofile+)
	 	} else {
		    print("\n File ", qname, " does not exist.\n")
		}
	    } else if (keypar.value == 'WFPC') {
		hcheck(rname, "wfpc$checkwfpc.dat")
		if (access(qname) == YES) {
		    hdiff(rname, qname, keyword=" ", nofile+)
	 	} else {
		    print("\n File ", qname, " does not exist.\n")
		}
	    } else {
		print("Wrong instrument: " // keypar.value)
	    }
	}

	# Delete temporary file
	delete(ifile)
end
