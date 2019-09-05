procedure base2dec (input, base)

string	input {prompt="Number to convert"}
int	base {min=2,max=36,prompt="Base of the input number"}
int	output {prompt="Output: Base 10 representation of input"}
bool	verbose {yes,prompt="Print result to standard output?"}

begin
	# Declarations
	int	a, A, z, Z, zero, nine
	int	c, i, l
	int	pbase
	string	pinput

	# Get interactive parameters.
	pinput = input
	pbase = base

	# Initialize alphabet parameters.
	a = 'a'
	A = 'A'
	z = 'z'
	Z = 'Z'
	zero = '0'
	nine = '9'
	
	l = strlen (pinput)
	output = 0
	for (i=1; i<=l; i=i+1) {
	    c = substr (pinput, i, i)
	    if (c >=zero && c<=nine)
		output = (pbase * output) + c - zero
	    else if (c >= A && c <= Z)
		output = (pbase * output) + c - A + 10
	    else if (c >= a && c <= z)
		output = (pbase * output) + c - a + 10
	    else
		printf ("base2dec: character %c cannot be converted.  Skipping...\n",
			c)
	}

	# That's all folks.
	if (verbose)
	    print (output)
end
