# print out a simple igi label command at a specified location

procedure pp_label (fd, x, y, str)

int	fd
real	x, y
char	str[ARB]

begin
	call pp_move (fd, x, y)
        call fprintf (fd, "label '%s'\n")
            call pargstr (str)
end
