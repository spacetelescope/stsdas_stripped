double procedure cha_near (ex, arr, n)

#  CHA_NEAR -- Check proximity of array elements to each other.
#  Returns the element of the array arr(n) which is closest to an exact
#  value EX 

double	ex		# The Exact Value
double	arr[ARB]	# The Array of Rounded Values
int	n		# Dimension of Array ARR

int	j

begin
	for (j = 1;  j < n && (ex - arr[j]) > 0.0;  j = j + 1)
	    ;

	if (j > 1 && j < n)
	    if (abs (ex - arr[j-1]) < abs (ex - arr[j])) 
		j = j - 1

	return (arr[j])
end

