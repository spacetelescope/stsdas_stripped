#* HISTORY*
#* B.Simon	13-Aug-92	original

# REVERSE -- Reverse the values in a one dimensional array in place

procedure revrsd (a, n)

double	a[ARB]		# u: array to be reversed
int	n		# i: length of array
#--
double	temp
int	i, j

begin
	i = 1
	j = n

	while (i < j) {
	    temp = a[i]
	    a[i] = a[j]
	    a[j] = temp
	    i = i + 1
	    j = j - 1
	}

end

procedure revrsi (a, n)

int	a[ARB]		# u: array to be reversed
int	n		# i: length of array
#--
int	temp
int	i, j

begin
	i = 1
	j = n

	while (i < j) {
	    temp = a[i]
	    a[i] = a[j]
	    a[j] = temp
	    i = i + 1
	    j = j - 1
	}

end
