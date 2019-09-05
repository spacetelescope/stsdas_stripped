procedure ppdirbox (dirname)

string	dirname

begin
	print ("erase")
	print ("reset; fontset hard; vpage 0.0 1.0 0.05 0.98")
	print ("location 0 1 0 1")
	print ("justify 5")

	# print the version number
	print ("expand 0.5")
	printf ("vmove 0.10 -0.03; label 'Paper Product, %s'\n", pp_dads.version)

	# print the directory name, if it is not NULL
	if (dirname != "") {
	    print ("vmove 0.00 0.00")
	    print ("vdraw 0.20 0.00; vdraw 0.20 0.10; vdraw 0.00 0.10; vdraw 0.00 0.00")
	    print ("expand 0.4")
	    print ("vmove 0.10 0.085; label 'Directory Name'")
	    print ("vmove 0.10 0.015; label 'For STScI Use Only'")
	    print ("expand 1.2")
	    printf ("vmove 0.10 0.05; label '%s'\n", dirname)
	}
end
