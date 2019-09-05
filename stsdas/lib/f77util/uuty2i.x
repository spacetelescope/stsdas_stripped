include	<iraf77.h>

define	TY_TEXT		-1	# Return negative number if character string

# UUTY2I -- Convert string datatype to integer.
#
# This routine translates datatypes specified as character strings to
# symbolic integers, expected by the F77/VOS interface.  There is NOT a
# one-to-one correspondence between input datatype strings and output
# integer datatypes.  This permits users to identify the same datatype
# with several different strings and have all of them translate.  For
# example, type boolean is returned if the user specifies a string that
# begins with "B" or "LOG".  This is convenient for the user, but the
# ambiguities are sort of a nuisance here.  Oh, well.
#
# C.D.Biemesderfer, STScI, 29-Apr-87  (Fortran original)
# C.D.Biemesderfer, STScI, 29-Jan-88  (SPP: installed in f77util)

procedure uuty2i (dtstr, intdt, istat)

				# i: Character string datatype descriptor
%      character *(*)   dtstr
int	intdt			# o: IRAF integer data type
int	istat			# o: Return status
#--
pointer	sp, ds

begin
	istat = ER_OK

	call smark (sp)
	call salloc (ds, SZ_LINE, TY_CHAR)

	call strcpy (dtstr, Memc[ds], SZ_LINE)
	call strlwr (Memc[ds])

	switch (Memc[ds]) {
	case 'b':
	    intdt = TY_BOOL

	case 'c':
	    switch (Memc[ds+1]) {
	    case 'h':
		intdt = TY_TEXT
	    case 'o':
		intdt = TY_COMPLEX
	    default:
		intdt = TY_TEXT
	    }

	case 'd':
	    intdt = TY_DOUBLE

	case 'f':			# "floating" ==> double
	    intdt = TY_DOUBLE

	case 'i':
	    intdt = TY_INT

	case 'l':
	    switch (Memc[ds+2]) {
	    case 'g':			# "logical"
		intdt = TY_BOOL
	    case 'n':			# "long"
		intdt = TY_LONG
	    default:
		intdt = TY_LONG
	    }

	case 'r':
	    intdt = TY_REAL

	case 's':
	    intdt = TY_SHORT

	case 'u':
	    switch (Memc[ds+1]) {
	    case 'b':
		intdt = TY_UBYTE
	    case 'i':
		intdt = TY_USHORT
	    default:
		intdt = TY_USHORT
	    }

	default:
	   istat = ER_IMBADTYPE
	}

	call sfree (sp)
end
