# GET_MSKVAR -- Get a variable from the mask, for vex_eval

procedure get_mskvar (stack, name)

pointer	stack		# u: Stack descriptor
char	name[ARB]	# i: Variable name
#--
include "imfill.com"

pointer	sp, errmsg, buffer

string	badname  "Second variable name in expression (%s)"

bool	strne()
pointer	stk_alloc()

begin
	# Allocate dynamic memory for strings

	call smark (sp)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	# Check variable name

	if (mask_var[1] == EOS) {
	    call strcpy (name, mask_var, SZ_VAR)

	} else if (strne (mask_var, name)) {
	    call sprintf (Memc[errmsg], SZ_LINE, badname)
	    call pargstr (name)
	    call error (1, Memc[errmsg])
	}

	# Allocate buffer on stack and copy mask line into it

	buffer = stk_alloc (stack, mask_len, TY_INT)
	call amovi (Memi[mask_line], Memi[buffer], mask_len)

	call sfree (sp)
end
 
