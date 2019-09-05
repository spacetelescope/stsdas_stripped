# UUSLEN -- Determine length of a string.
#
# C. D. Biemesderfer, STScI, 29 Jan 88

procedure uuslen (instr, lstr)

				# i: Input string
%      character *(*) instr
int	lstr			# o: Length of input string
#--
pointer	sp, st
int	strlen()

begin
	call smark (sp)
	call salloc (st, SZ_LINE, TY_CHAR)
	call f77upk (instr, Memc[st], SZ_LINE)
	lstr = strlen (Memc[st])
	call sfree (sp)
end
