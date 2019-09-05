# pp_fmtkw - Get a keyword's value from an image header and write at
# a specified location with specified format

procedure pp_fmtkw (fd, x, y, im, keyword, fmt)

int	fd
real	x, y		# world coordinate of the string
char	keyword[ARB]		
pointer	im
char	fmt[ARB]	# format 

char	str[SZ_LINE]
char	text[SZ_LINE]
int	ival
real	rval
int	len

int	imgeti()
real	imgetr()
int	strlen()

begin
	len = strlen (fmt)
	call sprintf (text, SZ_LINE, "move %%d %%d; label '%s'\n")
	    call pargstr (fmt)

	# string format
	if (fmt[len] == 's') {
	    if (im == NULL) str[1] = EOS
	    else iferr (call imgstr (im, keyword, str, SZ_LINE)) str[1] = EOS
	    call fprintf (fd, text)
	        call pargr (x)
	        call pargr (y)
	        call pargstr (str)

	# integer format
	} else if (fmt[len] == 'd') {
	    if (im == NULL) ival = INDEFI
	    else {
		iferr (ival = imgeti (im, keyword)) ival = INDEFI
	    	else {
		    call fprintf (fd, text)
	            	call pargr (x)
	            	call pargr (y)
	            	call pargi (ival)
		}
	    }

	# float format
	} else {
	    if (im == NULL) rval = INDEF
	    else {
		iferr (rval = imgetr (im, keyword)) rval = INDEF
	    	else {
		    call fprintf (fd, text)
	                call pargr (x)
	                call pargr (y)
	                call pargr (rval)
		}
	    }
	}
end
