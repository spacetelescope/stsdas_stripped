procedure dec2base (input, base)

int	input {prompt="Base 10 number to convert"}
int	base {min=2,max=36,prompt="Base to convert to"}
string	output {prompt="Output: String representation of the conversion"}
bool	verbose {yes,prompt="Print result to standard output?"}

begin
	# Declarations
	int	a, i, d, n, r
	string	out
	int	pbase

	# Get interactive parameters.
	r = input
	pbase = base

	# Initialize alphabet
	a = 'a'
	
	for (i=1; pbase**i <= r; i=i+1)
	    ;

	i=i-1
	output = ""
	while (i >= 0) {
	    d = pbase**i
	    n = r / d
	    r = r % d

	    if (n < 10)
		output = output//n
	    else {
		n = n+a-10
		printf ("%s%c\n", output, n) | scan (output)
	    }
	    i = i - 1
	}

	# That's all folks.
	if (verbose)
	    print (output)
end
