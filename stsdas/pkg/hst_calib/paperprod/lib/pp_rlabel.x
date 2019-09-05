# print out a real number using an igi label command at a specified location
# consider adding the input parameter 'xlim' to calculate the offset
# as 8*(90./xlim)

procedure pp_rlabel (fd, x, y, num)

int	fd
real	x, y, num
real 	dx

begin
	dx = x
	call pp_move (fd, dx, y)
        call fprintf (fd, "label '%0.2f'\n")
            call pargr (num)

end
