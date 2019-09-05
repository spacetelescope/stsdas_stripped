# common for the feed_user routines

int	tfd	      # tty descriptor
int	term          # terminal output descriptor
int	xpos, ypos    # x,y pos to message output
int	nlines 	      # total number of lines in image 
int	ngroups       # number of groups in geis image
int	oldtime       # last value from clktime()
int	geisfile      # YES, NO to indicate a GEIS file

common /fuser_comm/ tfd, term, xpos, ypos, nlines, ngroups, oldtime, geisfile
