# replace one character in a string with another character.
# for example: replace all the single quotes with back-quotes

procedure pp_swapchar (str, char_old, char_new)

char	str[ARB]
int	char_old, char_new	# use int, so I can directly use '?' in the
				# calling program

int	i

int	strlen()

begin
	do i = 1, strlen(str) {
            if (str[i] == char_old) str[i] = char_new
	}
end
