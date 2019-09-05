procedure ppcover (propid, piname)

char	propid  
char	piname

begin
	string today

	time | scan (line)
	today = line

	print ("reset; fontset hard; vpage 0.0 1.0 0.05 0.98")
	       
	print ("location 0 1 0 1")
	print ("expand 0.4")
	print ("vmove 0. 1.; justify 3; label 'Space Telescope Science Institute Paper Product'")
	printf ("vmove 0.5 1.; justify 2; label '%s'", today)
	print ("vmove 1. 1.; justify 1; label 'Space Telescope Science Institute Paper Product'")
	print ("expand 3.")
	printf ("vmove 0.5 .9; justify 2; label '%s Proposal: %s'\n",
		"\\fB", propid)
	printf ("vmove 0.5 .75; label '%s PI: %s'\n",
		"\\fB", piname)
end
