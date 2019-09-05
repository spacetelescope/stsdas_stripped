# date_str - Convert date string from dd/mm/yy to dd month_name yy

procedure date_str (date1, date2)

char	date1[ARB]	# date string with dd/mm/yy format
char	date2[ARB]	# date string with dd month yy format

char	mname[4]		

int	ip, yy, mm, dd, dum

int	next_num()

begin
	ip = 1

	dum = next_num (date1, ip, dd)
	dum = next_num (date1, ip, mm)
	dum = next_num (date1, ip, yy)

	switch (mm) {
	case 1:
	    call strcpy ("Jan", mname, 4)
	case 2:
	    call strcpy ("Feb", mname, 4)
	case 3:
	    call strcpy ("Mar", mname, 4)
	case 4:
	    call strcpy ("Apr", mname, 4)
	case 5:
	    call strcpy ("May", mname, 4)
	case 6:
	    call strcpy ("Jun", mname, 4)
	case 7:
	    call strcpy ("Jul", mname, 4)
	case 8:
	    call strcpy ("Aug", mname, 4)
	case 9:
	    call strcpy ("Sep", mname, 4)
	case 10:
	    call strcpy ("Oct", mname, 4)
	case 11:
	    call strcpy ("Nov", mname, 4)
	case 12:
	    call strcpy ("Dec", mname, 4)
	}

	call sprintf (date2, SZ_LINE, "%2d %s %2d")
	    call pargi (dd)
	    call pargstr (mname)
	    call pargi (yy)
end
