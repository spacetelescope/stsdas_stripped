include <fset.h>
include <ttyset.h>

procedure feed_user_begin()

int     lines, ttyodes(), ttystati(), ttopen()

include "feed_user.com"

begin

	term = ttyodes ("terminal")
	lines = ttystati (term, TTY_NLINES)
	tfd = ttopen ("dev$tty", APPEND)

	# The message will be written from the lowest left hand corner
	xpos = 1
	ypos = lines
end

procedure feed_user(group, line)

int	group	# image group number
int	line	# image line

int	newtime, clktime()

include "feed_user.com"

begin
	newtime = clktime(oldtime)

	if (newtime > 1) {
	   # Position the cursor at (xpos, ypos)
	   call ttygoto (tfd, term, xpos, ypos)

	   # The message to write
	   if (geisfile == NO) {
	      call fprintf(tfd,"%d: %d")
	         call pargi(nlines)
	         call pargi(line)
	   } else {
	      call fprintf(tfd,"%dg:%d %d:%4.4d")
	         call pargi(ngroups)
	         call pargi(group)
	         call pargi(nlines)
	         call pargi(line)
	   }
	   call flush (tfd)
	   oldtime = oldtime + newtime
	}

end

procedure feed_last(group, line)

int	group	# image group number
int	line	# image line

int
include "feed_user.com"

begin
	# Position the cursor at (xpos, ypos)
	call ttygoto (tfd, term, xpos, ypos)

	# The message to write
	if (geisfile == NO) {
	   call fprintf(tfd,"%d: %d\r")
	      call pargi(nlines)
	      call pargi(line)
	} else {
	   call fprintf(tfd,"%dg:%d %d:%4.4d\r")
	      call pargi(ngroups)
	      call pargi(group)
	      call pargi(nlines)
	      call pargi(line)
	}
#	call ttygoto (tfd, term, xpos, ypos)
	call flush (tfd)

end

procedure feed_user_end()

include "feed_user.com"

begin
	   call close (tfd)
	   call ttyclose(term)
end
