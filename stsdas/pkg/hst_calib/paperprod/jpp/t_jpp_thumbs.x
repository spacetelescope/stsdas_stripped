# Interface to SPP based tasks for plotting thumbnails of input
#   exposures

procedure t_jpp_thumbs ()

char	output[SZ_LINE]
char	root[SZ_FNAME]
char    prodext[SZ_FNAME]

int	fd

int	open()

begin
	# read output igi script file name
	call clgstr ("igi_output", output, SZ_LINE)

	# read other input parameters
	call clgstr ("root", root, SZ_LINE)
	call clgstr ("prodext", prodext, SZ_FNAME)

	# open the output file
	fd = open (output, APPEND, TEXT_FILE)
    
	# draw the compass
	call jpp_thumbs (fd, root, prodext)

	call close (fd)
end
