procedure prt_coords (alpha, delta)

double	alpha, delta		# Coordinates in radians

pointer	sp
pointer	declabel, decunits
pointer	ralabel,  raunits
int	ip

begin
	call smark  (sp)
	call salloc (ralabel,  SZ_LINE, TY_CHAR)
	call salloc (declabel, SZ_LINE, TY_CHAR)
	call salloc (raunits,  SZ_LINE, TY_CHAR)
	call salloc (decunits, SZ_LINE, TY_CHAR)

	call rad_hms (alpha, Memc[ralabel],  Memc[raunits], SZ_LINE)
	for (ip = 0;
	     Memc[ralabel+ip] != EOS && Memc[raunits+ip] != EOS;
	     ip = ip + 1)
	    # Merge the RA label and units strings
	    if (Memc[ralabel+ip] == ' ' && Memc[raunits+ip] != ' ')
		Memc[ralabel+ip] = Memc[raunits+ip] 
	    else if (Memc[raunits+ip] == ' ' && Memc[ralabel+ip] != ' ')
		Memc[raunits+ip] = Memc[ralabel+ip] 
		call rad_dms (delta, Memc[declabel], Memc[decunits], SZ_LINE)

	call rad_dms (delta, Memc[declabel],  Memc[decunits], SZ_LINE)
	for (ip = 0;
	     Memc[declabel+ip] != EOS && Memc[decunits+ip] != EOS;
	     ip = ip + 1)
	    # Merge the Dec label and units strings
	    if (Memc[declabel+ip] == ' ' && Memc[decunits+ip] != ' ')
		Memc[declabel+ip] = Memc[decunits+ip] 
	    else if (Memc[decunits+ip] == ' ' && Memc[declabel+ip] != ' ')
		Memc[decunits+ip] = Memc[declabel+ip] 

	call printf ("R.A. %s, Dec. %s\n")
	    call pargstr (Memc[ralabel])
	    call pargstr (Memc[declabel])

	call sfree (sp)
end
