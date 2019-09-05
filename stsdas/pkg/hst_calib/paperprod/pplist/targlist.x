include	"pplist.h"

# print paper product target list

procedure targlist (fd, ntarg, tltarg, ra, dec, desc, visit, propid, instru, 
			yoff)

int	fd
int	ntarg
char	tltarg[SZ_TARG, ARB]
double	ra[ARB], dec[ARB]
char	desc[SZ_DESC, ARB]
char	visit[ARB]
char	propid[ARB]
char	instru[ARB]
real	yoff

int	n

begin
	# print the target list
	do n = 1, ntarg {
	    if (n == 1)
		call targ_head (fd, yoff)
	    if (yoff > BOTTOM) {
		call pp_erase (fd)
		call list_banner (fd, visit, propid, instru, yoff)
		call targ_head (fd, yoff)
	    }

	    # fill in one line
	    call pp_move (fd, 0., yoff)
	    call fprintf (fd, "label '%-30s'\n")
		call pargstr (tltarg[1,n])
	    call pp_move (fd, 37., yoff)
	    call fprintf (fd, "label '%0.2H'\n")
		call pargd (ra[n])
	    call pp_move (fd, 50., yoff)
	    call fprintf (fd, "label '%0.1h'\n")
		call pargd (dec[n])
	    call pp_move (fd, 62., yoff)
	    call fprintf (fd, "label '%-35s'\n")
		call pargstr (desc[1,n])

	    # draw a line
	    call pp_move (fd, 0., yoff+0.75)
	    call fprintf (fd, "draw 100. %6.2f\n")
		call pargr (yoff+0.75)

	    yoff = yoff + 1.
	}
	call pp_move (fd, 0., yoff)
	call fprintf (fd, "draw 100. %6.2f\n")
	    call pargr (yoff)

	    yoff = yoff + 2.
end

# produce target list header

procedure targ_head (fd, yoff)

int	fd
real	yoff

begin
	# initialize and print title
	call fprintf (fd, "vpage 0.1 0.9 0.05 0.95\n")
	call fprintf (fd, "limits 1. 100. %6.2f 1.\n")
	    call pargr (BOTTOM)
	call pp_move (fd, 50., yoff)
	call fprintf (fd, "justify 2; expand 1.\n")
	call fprintf (fd, "label '%sfITarget List'; expand 0.75\n")
	    call pargstr ("\\")
	
	yoff = yoff + 2.

	# print column names
	call pp_label (fd, 15., yoff, "Target Name")
	call pp_label (fd, 42., yoff, "R.A. (J2000)")
	call pp_label (fd, 54., yoff, "Dec. (J2000)")
	call pp_label (fd, 75., yoff, "Description")

	# draw a double line
	call pp_move (fd, 0., yoff+1.)
	call fprintf (fd, "draw 100. %6.2f\n")
		call pargr (yoff+1.)
	call pp_move (fd, 0., yoff+1.25)
	call fprintf (fd, "draw 100. %6.2f\n")
		call pargr (yoff+1.25)

	call fprintf (fd, "justify 3\n")

	yoff = yoff + 1.5
end
