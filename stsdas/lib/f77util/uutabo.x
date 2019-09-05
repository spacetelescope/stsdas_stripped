define	N_BLANK		8	# Number of blanks per tab

# UUTABO -- Replace tabs with blanks
#
# C. D. Biemesderfer, STScI, 29 Jan 88

procedure uutabo (instr, outstr)

				# i: Input string
%      character *(*) instr
				# o: Output string
%      character *(*) outstr
#--
begin
	call fvuuto (instr, N_BLANK, outstr)
end
