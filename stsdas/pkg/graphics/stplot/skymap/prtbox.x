procedure prt_box (row, ra, dec, mag, class, name, namsiz, numvals, 
	ra1, dec1, ra2, dec2)

int	row[numvals]
double	ra[numvals], dec[numvals]
real	mag[numvals]
int	class[numvals]
char	name[namsiz,numvals]
int	namsiz
int	numvals
double	ra1, dec1, ra2, dec2

double	minra, mindec, maxra, maxdec
int	i

double	delta		# Coordinates in radians

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

	mindec = min (dec1, dec2)
	maxdec = max (dec1, dec2)
	minra  = min (ra1,  ra2)
	maxra  = max (ra1,  ra2)

	do i = 1, numvals {
	    if (ra[i]  < minra  || ra[i]  > maxra || 
		dec[i] < mindec || dec[i] > maxdec)
		next

	    # Format R.A.
	    call rad_hms (ra[i], Memc[ralabel], Memc[raunits], SZ_LINE)
	    for (ip = 0;
		 Memc[ralabel+ip] != EOS && Memc[raunits+ip] != EOS;
		 ip = ip + 1)
		# Merge the RA label and units strings
		if (Memc[ralabel+ip] == ' ' && Memc[raunits+ip] != ' ')
		    Memc[ralabel+ip] = Memc[raunits+ip] 
		else if (Memc[raunits+ip] == ' ' && Memc[ralabel+ip] != ' ')
		    Memc[raunits+ip] = Memc[ralabel+ip] 
		    call rad_dms (delta, Memc[declabel],
			Memc[decunits], SZ_LINE)

	    # Format Dec.
	    call rad_dms (dec[i], Memc[declabel], Memc[decunits], SZ_LINE)
	    for (ip = 0;
		 Memc[declabel+ip] != EOS && Memc[decunits+ip] != EOS;
		 ip = ip + 1)
		# Merge the Dec label and units strings
		if (Memc[declabel+ip] == ' ' && Memc[decunits+ip] != ' ')
		    Memc[declabel+ip] = Memc[decunits+ip] 
		else if (Memc[decunits+ip] == ' ' && Memc[declabel+ip] != ' ')
		    Memc[decunits+ip] = Memc[declabel+ip] 

	    call printf ("Row %5d\t%s\t%s\tMag. %.1f")
		call pargi   (row[i])
		call pargstr (Memc[ralabel])
		call pargstr (Memc[declabel])
		call pargr   (mag[i])

	    if (class[i] != 0) {
		# Object class
		call printf ("\t%d")
		    call pargi (class[i])
	    }

	    if (name[1,i] != EOS) {
		# Object name
		call printf ("\t%s")
		    call pargstr (name[1,i])
	    }

	    call printf ("\n")
    	}

	call sfree (sp)
end
