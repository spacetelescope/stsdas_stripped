# start a new page

procedure pp_erase (fd)

int	fd

begin
        call fprintf (fd, "erase\n")
end
