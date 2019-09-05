# Plot a compass

procedure t_compass ()

char	output[SZ_FNAME]	# output igi script file name
real	x, y, size		# location of the compass and its size
real	orientat		# orientation
bool	mir_revr	

int	fd			# output file pointer

int	open()
real	clgetr()
bool	clgetb()

begin
	# read output igi script file name
	call clgstr ("igi_output", output, SZ_LINE)

	# read x, y, size, orientat, and mir_revr
	x = clgetr ("x")
	y = clgetr ("y")
	size = clgetr ("size")
	orientat = clgetr ("orientat")
	mir_revr = clgetb ("mir_revr")

	# open the output file
	fd = open (output, APPEND, TEXT_FILE)

	# draw the compass
	call pp_compass (fd, x, y, size, orientat, mir_revr)

	# close the output file
	call close (fd)
end
