# Calculate the "usable" histogram by rejecting the highest and lowest counts

procedure t_gethist ()

char	input[SZ_FNAME]		# input image name
#char	output[SZ_FNAME]
real    lower           	# lower limit of the usable histogram
                        	# if lower = 0.01, the bottom 1% of the
                        	# original data points will be excluded
real    upper           	# upper limit of the usable histogram
                        	# if upper = 0.99, the top 1% of the
                        	# original data points will be excluded
int	nbins			# number of bins in the histogram

pointer	ip
#int	fd
pointer	hist			# the histogram
real	hmin, hmax

pointer	immap()
#int	open()
real	clgetr()
int	clgeti()

begin
	# read input image and output file name
	call clgstr ("input", input, SZ_FNAME)
	#call clgstr ("output", output, SZ_FNAME)

	# read upper and lower limits
	lower = clgetr ("lower")
	upper = clgetr ("upper")
	nbins = clgeti ("nbins")

	# allocate memory
	call malloc (hist, nbins, TY_REAL)

	# open the input image
	ip = immap (input, READ_ONLY, 0)

	# get the histogram
	call get_hist (ip, upper, lower, Memr[hist], nbins, hmin, hmax)

	# write the min/max as output parameters
	call clputr ("hmin", hmin)
	call clputr ("hmax", hmax)

	# close input image
	call imunmap (ip)

	# open the output file
	#fd = open (output, APPEND, TEXT_FILE)

	# close output file
	#call close (fd)

	# free the memory
	call mfree (hist, TY_REAL)
end
