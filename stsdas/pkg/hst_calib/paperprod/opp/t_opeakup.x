# Plot the peakup image

procedure t_opeakup ()

char	input[SZ_FNAME]
char	output[SZ_FNAME]
real	x1, x2, y1, y2

int	fd
pointer	im

pointer	immap()
int	open()
real	clgetr()

begin
	# read input file name and output igi script file name
	call clgstr ("input", input, SZ_LINE)
	call clgstr ("igi_output", output, SZ_LINE)

	# read x, y, and size
	x1 = clgetr ("x1")
	x2 = clgetr ("x2")
	y1 = clgetr ("y1")
	y2 = clgetr ("y2")

	# open the image, read keywords, and close input image
	im = immap (input, READ_ONLY, 0)

	# open the output file
	fd = open (output, APPEND, TEXT_FILE)

	# draw the peakup image
	call opp_peakup (im, fd, x1, x2, y1, y2)

	call imunmap (im)
	call close (fd)
end
