#  countfiles -- count how many files are in the image template
#
#  Description:
#  ------------
#  
#  Date		Author			Description
#  ----		------			-----------
#  17-Aug-1995  J.-C. Hsu		design and coding
#------------------------------------------------------------------------------
procedure countfiles ()

pointer	fin 			# file template pointer

pointer imtopenp()
int	imtlen()
#==============================================================================
begin
	fin = imtopenp ("input")
	call clputi ("output", imtlen(fin))
	call imtclose (fin)
end
