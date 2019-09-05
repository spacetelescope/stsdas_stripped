# SHOWSPIRAL -- Display HRS spiral search map
#--
procedure showspiral(in,out,disponly)

file     in		{prompt = "Input image name"}
file     out		{prompt = "Output image name"}
bool	 disponly	{no,prompt = "Redisplay map without recomputing"}

begin

file    file1, file2, file3

	# check for needed packages
	if (!defpac ("tv") || !defpac ("proto")) 
	    error (0, "Be certain that the tv and proto packages are loaded")

	file1 = in
	file2 = out

	# do the map construction
	if (!disponly) 
	    spiralmap (input=file1,output=file2)
	
        # display the map 
        file3 = file2 + "[*,-*]"
        display (file3,1,fill+,xcenter=0.5,ycenter=0.5,xsize=0.67,
                 ysize=0.67,xmag=1.0,ymag=1.0)
        tvmark (1,coords="",nxoffset=0,nyoffset=0,color=203,
                commands="hrs$spiralmap/showspiral.dat")


end
