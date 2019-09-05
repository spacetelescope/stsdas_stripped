# Plot a gray scale bar

procedure t_gsbar ()

char	output[SZ_FNAME]
real    vleft, vright, vbottom, vtop    # the location of the gray scale bar
real    min, max        # data extremes represented by the gray scale bar
real    label_size      # the size of the label
bool    negative        # is the image negative (i.e. black represents bright
                        # instead of faint object)?

int	fd

int	open()
real	clgetr()
bool	clgetb()

begin
	# read output igi script file name
	call clgstr ("igi_output", output, SZ_LINE)

	# read location parameters
	vleft = clgetr ("vleft")
	vright = clgetr ("vright")
	vbottom = clgetr ("vbottom")
	vtop = clgetr ("vtop")

	# read data range
	min = clgetr ("min")
	max = clgetr ("max")

	# read text size
	label_size = clgetr ("label_size")

	# is this a negative or positive image
	negative = clgetb ("negative")

	# open the output file
	fd = open (output, APPEND, TEXT_FILE)

	# draw the gray scale bar
	call pp_gsbar (fd, vleft, vright, vbottom, vtop, min, max, label_size,
                        negative)

	call close (fd)
end
