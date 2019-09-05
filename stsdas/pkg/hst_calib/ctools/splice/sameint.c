/* This file contains SameInt and SameString. */

# include <string.h>

# define  INT_WILDCARD      -1
# define  STRING_WILDCARD  "ANY"

/* When comparing a value (e.g. from the input header) with a value read
   from a table row in order to select the appropriate row, the values
   may either match exactly, or the table value may be a wildcard which
   matches anything.  The following routines return one if the values
   are the same (case sensitive for strings) or if the rowvalue is the
   wildcard; otherwise, zero is returned.

   Note that it is rowvalue, not value, that is compared with the
   wildcard, so when calling these functions, the value read from the
   table should be passed as the first argument, and the value with
   which it is to be compared should be passed as the second argument.

   Phil Hodge, 2000 Jan 31:
	Copied from lib/sameint.c and stiswild.h.
*/

int SameInt (int rowvalue, int value) {

	if (rowvalue == INT_WILDCARD)
	    return (1);
	else if (rowvalue == value)
	    return (1);
	else
	    return (0);
}

int SameString (char *rowvalue, char *value) {

	if (strcmp (rowvalue, STRING_WILDCARD) == 0)
	    return (1);
	else if (strcmp (rowvalue, value) == 0)
	    return (1);
	else
	    return (0);
}
