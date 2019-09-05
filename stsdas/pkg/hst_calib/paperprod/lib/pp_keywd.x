# pp_keywd - Get a keyword's value from an image header and write at a 
# specified location

procedure pp_keywd (fd, x, y, im, keyword)

int	fd		# output file pointer
real	x, y		# location of the keyword value
pointer	im
char	keyword[ARB]

char	outstr[SZ_LINE]

begin
	iferr (call imgstr (im, keyword, outstr, SZ_LINE)) 
	    call strcpy (" ", outstr, SZ_LINE)
	call pp_label (fd, x, y, outstr)
end
