# UULOWC -- Convert to lower case
# This procedure converts an input Fortran-type character string to lower
# case and returns it as a Fortran-type string.  The input and output
# strings may be the same actual argument.
#
# P.E. Hodge, 12-Oct-87  Subroutine created

procedure uulowc (f77in, f77out)

%      character*(*) f77in
%      character*(*) f77out
#--
pointer sp			# stack pointer
pointer str			# local copy of string
int	f77len			# length of longer string:  f77in or f77out

begin
	call smark (sp)
%      f77len = max (len(f77in), len(f77out))
	call salloc (str, f77len, TY_CHAR)

	call f77upk (f77in, Memc[str], f77len)
	call strlwr (Memc[str])
	call f77pak (Memc[str], f77out, f77len)

	call sfree (sp)
end
