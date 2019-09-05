procedure getpst (namef, gsep, ntot, blza, inca)

%	character*64	namef
char	name[64]
real	gsep, ntot, blza, inca

pointer	pp
pointer	clopset()
real	clgpsetr()

begin

	# Unpack Fortran pset name string
	call f77upk (namef, name, 64)

	# open pset
	pp = clopset (name)

	# get parameter values
	gsep = clgpsetr (pp, "gsep")
	ntot = clgpsetr (pp, "ntot")
	blza = clgpsetr (pp, "blza")
	inca = clgpsetr (pp, "inca")

	# close pset
	call clcpset (pp)

end
