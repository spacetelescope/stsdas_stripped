# MERGE_ARR -- given two input real arrays with values in ascending order,
# create three output arrays, one  representing a merged list of the two
# input arrays, also in ascending order (duplicate values are retained).
# The other two arrays represent the location in the output array
# corresponding to the input value in the respective input arrays.
# Arrays are allocated by the calling routine.

procedure mergearr (arr1, arr2, n1, n2, arr_merge, index1, index2)

real	arr1[ARB]	# i: first array of monotonically increasing values
real	arr2[ARB]	# i: second array of ...
int	n1		# i: size of arr1
int	n2		# i: size of arr2
real	arr_merge[ARB]	# o: merged array
int	index1[ARB]	# o: mapping of arr1 values into arr_merge
int	index2[ARB]	# o: mapping of arr2 values into arr_merge

#--

int	n		# size of arr_merge
int 	i1, i2, i	# loop counter for arr1, arr2, and arr_merge
bool	done1, done2	# flags indicating end of array

# algorithm is straightforward. Move through both arrays choosing the smallest
# value to place in the output array, raising flag if end of input array is
# reached

begin
	done1 = false
	done2 = false
	n = n1 + n2
	i1 = 1
	i2 = 1

	do i = 1, n {
	    if (((arr1[i1] > arr2[i2]) || done1) && !done2) {
		arr_merge[i] = arr2[i2]
		index2[i2] = i
		if (i2 < n2)
		    i2 = i2 + 1 
		else 
		    done2 = true
	    } else {
		arr_merge[i] = arr1[i1]
		index1[i1] = i
		if (i1 < n1)
		    i1 = i1 + 1 
		else
		    done1 = true
	    }
	}
end


