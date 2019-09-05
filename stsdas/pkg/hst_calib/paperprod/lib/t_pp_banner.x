# Plot the banner

procedure t_pp_banner ()

char	output[SZ_FNAME]
char	visit[SZ_LINE]
char	obs[SZ_LINE]
char	prop[SZ_LINE]
char	instru[SZ_LINE]
char	timetag[SZ_LINE]
int	page

int	fd

int	open()
int	clgeti()

begin
	# read output igi script file name
	call clgstr ("igi_output", output, SZ_LINE)

	# read other input parameters
	call clgstr ("visit", visit, SZ_LINE)
	call clgstr ("obs", obs, SZ_LINE)
	call clgstr ("prop", prop, SZ_LINE)
	call clgstr ("instru", instru, SZ_LINE)
	call clgstr ("timetag", timetag, SZ_LINE)

	page = clgeti ("page")

	# open the output file
	fd = open (output, APPEND, TEXT_FILE)

	# draw the compass
	call pp_banner (fd, visit, obs, prop, instru, timetag, page)

	call clputi ("page", page)

	call close (fd)
end
