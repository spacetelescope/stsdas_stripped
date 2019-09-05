include	<math.h>

# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# STARUNITS -- Convert sky coordinates to standard units
#
# B.Simon	04-Jun-90	Original

procedure star_units (fi, colname, unitpar, coord, maxcoord)

pointer	fi		#  i: File descriptor
char	colname[ARB]	#  i: Column name
char	unitpar[ARB]	#  i: Default column units
double	coord[ARB]	# io: Sky coordinate vector
int	maxcoord	#  i: Length of vector
#--
double	uconst[3]
int	trip, index, i
pointer	sp, units

data	uconst / 1.0 , RADIAN , 15.0 /

string	ulist    "|degrees|radians|hours|"
string	badname  "Column name not found (%s)"
string	badunits "Illegal coordinate units (%s)"

int	strdic(), unit_file()

begin
	call smark (sp)
	call salloc (units, SZ_FNAME, TY_CHAR)

	if (unit_file (fi, colname, Memc[units], SZ_FNAME) == ERR)
	    call pixerror (badname, colname)
	call strlwr (Memc[units])

	do trip = 1, 2 {
	    index = strdic (Memc[units], Memc[units], SZ_FNAME, ulist)
	    if (index > 0)
		break
	    call clgstr (unitpar, Memc[units], SZ_FNAME)
	}

	if (index == 0) {
	    call pixerror (badunits, Memc[units])
	} else if (uconst[index] != 1.0) {
	    do i = 1, maxcoord
		coord[i] = coord[i] * uconst[index]
	}
	    
	call sfree (sp)
end
