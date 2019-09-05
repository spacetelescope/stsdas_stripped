procedure chtext (gp, line, text, script)

pointer	gp
int	line
char	text[ARB]
int	script

real	x, y

string	c_format "h=l;v=c;q=h"
string	u_format "h=l;v=b;q=h"
string	d_format "h=l;v=t;q=h"

begin
	x = 0.0
	y = real (line)

	if (script > 0)
	    # Superscript
	    call gtext (gp, x, y, text, u_format)
	else if (script > 0)
	    # Subscript
	    call gtext (gp, x, y, text, d_format)
	else
	    # Normal
	    call gtext (gp, x, y, text, c_format)
end
