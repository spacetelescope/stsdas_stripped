# print out a simple igi draw command

procedure pp_draw (fd, x, y)

int	fd
real	x, y

begin
        call fprintf (fd, "draw %g %g\n")
            call pargr (x)
            call pargr (y)
end
