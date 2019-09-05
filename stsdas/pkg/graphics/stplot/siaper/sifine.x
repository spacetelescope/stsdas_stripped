include	<ctype.h>
include <tbset.h>
include "siaper.h"

# si_find_aperture - Search the SIAP_ID column for a matching name.
#
# Returns
#
# History
#    7Mar91 - Finished code begun by Andrew Cseko.
#             Jonathan D. Eisenhamer, STScI.
#---------------------------------------------------------------------------

int procedure si_find_aperture (siaf_tp, col, name, next_row)

pointer siaf_tp             # I:  aperture data table
pointer col                 # I:  pointer to SIAP_ID column - pattern to match
char  name[SIAP_ID_SIZE]    # I:  pattern to search for
int   next_row              # IO: Input the row to start searching at.
                            #     0 means to search all the rows (starting from
                            #     the last.
                            #     Output the row where a name is found.

# Declarations
int   i                         # Current row.
int   j                         # Generic.

char  siap_id[ SIAP_ID_SIZE ]   # The SIAP id.
char  old_name[ SIAP_ID_SIZE ]  # The original name.
char  pattern[ SIAP_ID_SIZE ]   # The pattern to look for.

data  old_name[1] / EOS /

# Function prototypes
int   strcmp(), tbpsta(), patmatch(), patmake()

begin

	if (0 != strcmp (old_name, name)) {
	    call strcpy (name, old_name, SIAP_ID_SIZE)
	    j = patmake (name, pattern, SIAP_ID_SIZE)
	}

	# Get the next row.
	if (next_row != 0) 
	    i = next_row - 1
	else
	    i=tbpsta (siaf_tp, TBL_NROWS) 

	# Search for the name through all the remaining rows.
	for (; i > 0 ; i=i-1) {
	    call tbegtt (siaf_tp, col, i, siap_id, SIAP_ID_SIZE)
	    call strlwr (siap_id)
	    call strtrm	(siap_id)

	    if (0 != patmatch (siap_id, pattern)) {
		next_row = i
		break 
	    }
	}

	# Return the row number.
	return i

end
#---------------------------------------------------------------------------
# End of si_find_aperture
#---------------------------------------------------------------------------
procedure strtrm (str)

char	str[ARB]		# IO: String to remove whitespace.

# Declarations
int	i, j, first, last

begin
	i = 1
	first = 0
	while (str[i] != EOS)
	    if (first == 0) {
		if (IS_WHITE(str[i]))
		    i = i + 1
		else {
		    first = i
		    i = i + 1
		}
	    } else {
		if (IS_WHITE(str[i]))
		    break
		else
		    i = i + 1
	    }
	last = i-1

	if (first == 0)
	    str[1] = EOS
	else {
	    i = 1
	    do j = first, last {
		str[i] = str[j]
		i = i + 1
	    }
	    str[i] = EOS
	}
end
#---------------------------------------------------------------------------
# End of strtrm
#---------------------------------------------------------------------------
