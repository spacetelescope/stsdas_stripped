# Plot a compass by using the CD matrix in an image header

procedure t_cdcompass ()

char	input[SZ_FNAME]		# input image name
char	output[SZ_FNAME]	# output igi script file name
real	x, y, size		# location and the size of the compass

real	cd[2,2]			# CD matrix of the input image
pointer	ip			# input image pointer
int	fd			# output file pointer

pointer	immap()
int	open()
real	clgetr()
real	imgetr()

begin
	# read input image and output igi script file name
	call clgstr ("input", input, SZ_LINE)
	call clgstr ("igi_output", output, SZ_LINE)

	# read x, y, and size
	x = clgetr ("x")
	y = clgetr ("y")
	size = clgetr ("size")

	# open the input image
	ip = immap (input, READ_ONLY, 0)

	# read the CD matrix
	cd[1,1] = imgetr (ip, "CD1_1")
	cd[1,2] = imgetr (ip, "CD1_2")
	cd[2,1] = imgetr (ip, "CD2_1")
	cd[2,2] = imgetr (ip, "CD2_2")

	# close input image
	call imunmap (ip)

	# open the output file
	fd = open (output, APPEND, TEXT_FILE)

	# draw the compass
	call pp_cdcompass (fd, x, y, size, cd)

	# close output file
	call close (fd)
end
