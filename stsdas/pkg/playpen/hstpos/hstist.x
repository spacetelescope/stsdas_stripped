define	LOGPTR	32			# log2(maxpts) (4e9)

procedure hst_isort (b, a, npix)

real	a[ARB], b[ARB]		# input, output arrays
int	npix			# number of pixels

real	pivot, temp
int	i, j, k, p, lv[LOGPTR], uv[LOGPTR]
define	swap {temp=$1;$1=$2;$2=temp}

begin
        
	lv[1] = 1
	uv[1] = npix
	p = 1
        
	while (p > 0) {
	    if (lv[p] >= uv[p])			# only one elem in this subset
		p = p - 1			# pop stack
	    else {
                i = lv[p] - 1
                j = uv[p]
                
                # Select as the pivot the element at the center of the
                # array, to avoid quadratic behavior on an already sorted
                # array.
                
                k = (lv[p] + uv[p]) / 2
                swap (b[j], b[k])
                swap (a[j], a[k])
                pivot = b[j]			# pivot line
                
                while (i < j) {
                    for (i=i+1;  b[i] < pivot;  i=i+1)
                        ;
                    for (j=j-1;  j > i;  j=j-1)
                        if (b[j] <= pivot)
                            break
                    if (i < j) {
                        # out of order pair
                        swap (b[i], b[j])	# interchange elements
                        swap (a[i], a[j])
                    }
                }
                
                j = uv[p]			# move pivot to position i
                swap (b[i], b[j])		# interchange elements
                swap (a[i], a[j])
                
                if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
                    lv[p+1] = lv[p]
                    uv[p+1] = i - 1
                    lv[p] = i + 1
                } else {
                    lv[p+1] = i + 1
                    uv[p+1] = uv[p]
                    uv[p] = i - 1
                }
                
		p = p + 1			# push onto stack
	    }
	}
end

