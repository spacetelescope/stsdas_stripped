# Plot the dither-chopping pattern

procedure t_dithchop ()

char	output[SZ_FNAME]	# output igi script file name
char	pattern[SZ_LINE, 11]	# dither-chopping pattern
int	npos			# number of positions
real    dithsize        # dither size (in arc seconds)
real    chopsize        # chop size (in arc seconds)

int	fd			# output file pointer
int	i, npatt

int	open()
int	clgeti()
real	clgetr()
bool	streq()

begin
	# read output igi script file name
	call clgstr ("igi_output", output, SZ_LINE)

	# read pattern and number of positions
	call clgstr ("pattern", pattern[1,1], SZ_LINE)
	npos = clgeti ("npos")
	dithsize = clgetr ("dithsize")
	chopsize = clgetr ("chopsize")
	npatt = 1

	call strupr (pattern[1,1])
	if (streq (pattern[1,1], "ALL")) {
	    npatt = 11
	    call strcpy ("SPIRAL-DITH", pattern[1,1], SZ_LINE)
	    call strcpy ("SQUARE-WAVE-DITH", pattern[1,2], SZ_LINE)
	    call strcpy ("XSTRIP-DITH", pattern[1,3], SZ_LINE)
	    call strcpy ("YSTRIP-DITH", pattern[1,4], SZ_LINE)
	    call strcpy ("ONE-CHOP", pattern[1,5], SZ_LINE)
	    call strcpy ("TWO-CHOP", pattern[1,6], SZ_LINE)
	    call strcpy ("FOUR-CHOP", pattern[1,7], SZ_LINE)
	    call strcpy ("EIGHT-CHOP", pattern[1,8], SZ_LINE)
	    call strcpy ("SPIRAL-DITH-CHOP", pattern[1,9], SZ_LINE)
	    call strcpy ("XSTRIP-DITH-CHOP", pattern[1,10], SZ_LINE)
	    call strcpy ("YSTRIP-DITH-CHOP", pattern[1,11], SZ_LINE)
	}

	# open the output file
	fd = open (output, APPEND, TEXT_FILE)

	# draw the pattern
	do i = 1, npatt {
	    call fprintf (fd, "erase\n")
	    call npp_dithchop (fd, pattern[1,i], npos, dithsize, chopsize)
	}

	# close the output file
	call close (fd)
end
