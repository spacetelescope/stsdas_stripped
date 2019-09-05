# LISTPRIMES -- Display the first N prime numbers

procedure t_listprimes ()
#--
pointer	sp, p
int	nprimes, clgeti(), i

begin
	nprimes = clgeti ("nprimes")

	call smark (sp)
	call salloc (p, nprimes, TY_INT)

	call compute_primes (nprimes, Memi[p])

	do i=1,nprimes {
	    call printf ("%4d ")
		call pargi (Memi[p+i-1])
	    if (mod(i,15) == 0)
		call printf ("\n")
	}
	call printf ("\n")

	call sfree (sp)
end
