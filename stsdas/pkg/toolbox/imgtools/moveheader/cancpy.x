# CANCPY -- Return true if the value of this keyword can be copied (changed).

# Certain keywords in the user area describe the layout of the image file
# and cannot be changed without changing the layout.
#
# B.Simon	01-Jun-87	Original

bool procedure cancpy (key)

char	key[ARB]	# i: Keyword to be checked
#--
bool	can_change

int	stridx()
bool	strne()

begin
	can_change = true
	if (stridx(key[1],"GP") > 0) {
	    can_change = can_change && strne (key, "GROUPS")
	    can_change = can_change && strne (key, "GCOUNT")
	    can_change = can_change && strne (key, "PCOUNT")
	    can_change = can_change && strne (key, "PSIZE")
	}
	return(can_change)
end
