#  pickfile -- get the name of a selected file from the template
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  17-Aug-1995  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------
procedure pickfile ()

pointer	fin 			# file template pointer
char	filename[SZ_FNAME]	# output: output file name
int	nchar

pointer imtopenp()
int	clgeti()
int	imtrgetim()
#==============================================================================
begin
	fin = imtopenp ("input")
	nchar = imtrgetim (fin, clgeti("index"), filename, SZ_FNAME) 
	call clpstr ("output", filename)
	call imtclose (fin)
end
