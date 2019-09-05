# Plot the banner

procedure t_pp_pdfsection ()

char	output[SZ_FNAME]
char	id[SZ_LINE]
int	page
int count

int	fd

int	open()
int	clgeti()

begin
	# read output igi script file name
	call clgstr ("igi_output", output, SZ_LINE)

	# read other input parameters
	call clgstr ("id", id, SZ_LINE)

	page = clgeti ("page")
	count = clgeti ("count")

	# open the output file
	fd = open (output, APPEND, TEXT_FILE)

	# draw the compass
	call pp_pdfsection (fd, page, count, id)

	#call clputi ("page", page)

	call close (fd)
end

procedure t_pp_pdfbook ()

char	output[SZ_FNAME]
char	title[SZ_LINE]
int	page

int	fd

int	open()
int	clgeti()

begin
	# read output igi script file name
	call clgstr ("igi_output", output, SZ_LINE)

	# read other input parameters
	call clgstr ("title", title, SZ_LINE)

	page = clgeti ("page")

	# open the output file
	fd = open (output, APPEND, TEXT_FILE)

	# draw the compass
	call pp_pdfbook (fd, page, title)

	#call clputi ("page", page)

	call close (fd)
end
