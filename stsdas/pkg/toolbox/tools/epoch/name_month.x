include	"epoch.h"

# NAME_MONTH -- translate numerical month to abbreviated English month name
#
# Description:
# ------------
#----------------------------------------------------------------------------
procedure name_month (month, mname)

int	month			# input: Month (1-12)

char	mname[SZ_MONTH]		# output: Month name in three letters
#-----------------------------------------------------------------------------
begin

	switch (month) {
	case 1:
	    call strcpy ("Jan", mname, SZ_MONTH)
	case 2:
	    call strcpy ("Feb", mname, SZ_MONTH)
	case 3:
	    call strcpy ("Mar", mname, SZ_MONTH)
	case 4:
	    call strcpy ("Apr", mname, SZ_MONTH)
	case 5:
	    call strcpy ("May", mname, SZ_MONTH)
	case 6:
	    call strcpy ("Jun", mname, SZ_MONTH)
	case 7:
	    call strcpy ("Jul", mname, SZ_MONTH)
	case 8:
	    call strcpy ("Aug", mname, SZ_MONTH)
	case 9:
	    call strcpy ("Sep", mname, SZ_MONTH)
	case 10:
	    call strcpy ("Oct", mname, SZ_MONTH)
	case 11:
	    call strcpy ("Nov", mname, SZ_MONTH)
	case 12:
	    call strcpy ("Dec", mname, SZ_MONTH)
	default:
	    call error (1, "illegal month")
	}
end
