# MNFILT -- Box smooth the array

procedure pof_mnfilt (data, npts, width)

double	data[ARB]
int	npts
int	width	

int	i, j, halfwidth, delta, nsum
double	sum
pointer	sp, temp

begin
 	call smark (sp)
	call salloc (temp, npts, TY_DOUBLE)

	width = (width / 2) * 2 + 1
	if (width > 1 && width < npts) {
	    halfwidth = width / 2

	    do i = 1, npts {
	        sum = 0.0d0
	        nsum = 0

	        if (i > halfwidth && i < (npts - halfwidth)) {
		    delta = halfwidth
	        } else {
	            if (i <= halfwidth)
		        delta = i / 2
	            else
		        delta = (npts - i + 1) / 2
	        }

	        do j = i - delta, i + delta {
		    sum = sum + data[j]
		    nsum = nsum + 1
	        }

	        Memd[temp + i - 1] = sum / nsum
	    }

	    call amovd (Memd[temp], data, npts)

	}

	call sfree (sp)

end
