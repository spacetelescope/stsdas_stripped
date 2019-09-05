define	MAX_FACTORS	20

# FACTOR -- Display prime factors of a number.

procedure t_factor ()
#--
int	n, t, i, p[MAX_FACTORS]
int	clgeti()
bool	clgetb()

begin
	n = clgeti ("number")

	call prime_factor (n, t, p)

	if (t == 0) {
	    call error (0, "number too large for factoring routine")
	} else {
	    if (t > 1) {
		call printf ("%d has %d factors : ")
		    call pargi (n)
		    call pargi (t)
		do i=1,t-1 {
		    call printf ("%d ")
			call pargi (p[i])
		}
		call printf ("%d\n")
		    call pargi (p[t])
	    } else {				# added 8/6/93 by PEH
		call printf ("%d is prime\n")
		    call pargi (n)
	    }
	}

	if (clgetb("showwks")) {
	    call getwks (n, t, p)

	    call printf ("%d has %d Winograd factors : ")
		call pargi (n)
		call pargi (t)
	    do i=1,t-1 {
		call printf ("%d ")
		    call pargi (p[i])
	    }
	    call printf ("%d\n")
		call pargi (p[t])
	}
end
