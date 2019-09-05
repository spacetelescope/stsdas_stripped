# PIXEDIT -- Screen editor for image pixels

procedure pixedit ()

#--
pointer	image		# image name
bool	silent		# don't ring bell when error occurs
bool	rdonly		# edit image read only
bool	inplace		# edit image in place

pointer	sp

bool	clgetb()

begin
	# Read task parameters

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	call clgstr ("image", Memc[image], SZ_FNAME)

	silent = clgetb ("silent")
	rdonly = clgetb ("rdonly")
	inplace = clgetb ("inplace")

	# Initialize window and message system

	call initscr
	call nosavewin
	call msg_create

	# Create top level object: the terminal

	call trm_init (Memc[image], silent, rdonly, inplace)

	# Enter message processing loop

	call msg_loop

	# Close down message and window systems

	call msg_destroy
	call endwin

	call sfree (sp)
end


