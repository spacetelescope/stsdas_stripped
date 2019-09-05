#FILECONV.CL -- Script to set up tasks in the STSDAS.TOOLS.FILECONV package
# Modifications
# R. Williamson 13-Dec-1991 Creation
# RAShaw	20-Aug-93   Add "networking" task

procedure convfile()

string	mode="h"

begin

	package convfile

	task 	sun2vax,	        
		vax2sun,	        
		tconvert        = "convfile$x_convfile.e"

	cl()
end

