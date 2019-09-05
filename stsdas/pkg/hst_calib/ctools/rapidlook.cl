procedure rapidlook (input, output, display)

file 	input	{prompt="Input multigroup image name"}
file 	output	{prompt="Output 2-D image name"}
string 	display	{"plot",prompt="Image display or surface plot?",
			       min="tv|plot"}


begin 	    

	string	dispmode
	bool	delete_im = no
	file 	temp1, temp2

	# check for needed packages
	temp1 = ""
	if (!defpac ("stplot"))
	    temp1=temp1//"stplot "
	if (!defpac ("imgtools"))
	    temp1=temp1//"imgtools "
	if (!defpac ("plot"))
	    temp1=temp1//"plot "
	if (!defpac ("tv"))
	    temp1=temp1//"tv "
	if (!defpac ("images"))
	    temp1=temp1//"images "
	if (strlen (temp1) > 0)
	    error (1, "rapidlook: Please load packages: "//temp1)

	temp1 = input
	if (!(access (temp1)))
	    error (1, "Input image not found.")
	temp2 = output

	# if no output name, assume output is not wanted
	if (temp2 == "") {
	    temp2 = mktemp("tmp$tmp")
	    delete_im = yes
	}

	# build the filenames for all groups and build a 2-D image
	grlist (image=temp1, members="-") | 
	    stack(input="@STDIN",output=temp2)

	dispmode = display
	if (dispmode == "plot") 
	    surface (image=temp2)
	else if (dispmode == "tv")
	    display (image=temp2,frame=1)
	else
	    error (1, "Invalid display mode.")

	if (delete_im)
	    imdelete(image=temp2)

end
