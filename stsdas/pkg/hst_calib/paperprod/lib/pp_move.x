# print out a simple igi move command

procedure pp_move (fd, x, y)

int	fd
real	x, y

begin
        call fprintf (fd, "move %g %g\n")
            call pargr (x)
            call pargr (y)
end
