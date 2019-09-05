# INSTCODE -- Convert instrument name to code letter

int procedure instcode (instr)

char	instr[ARB]	# i: instrument
#--
int	index
pointer	sp, inst

string	code	"fmuvwxyz"			  	# Code letters
string	dict	"|fgs|multi|wfp2|hsp|wfpc|foc|fos|hrs"	# Instrument names

int	strdic()

begin
	call smark (sp)
	call salloc (inst, SZ_FNAME, TY_CHAR)

	call strcpy (instr, Memc[inst], SZ_FNAME)
	call strlwr (Memc[inst])

	index = strdic (Memc[inst], Memc[inst], SZ_FNAME, dict)
	if (index == 0)
	    call error (1, "Illegal instrument name")

	call sfree (sp)
	return (code[index])
end
