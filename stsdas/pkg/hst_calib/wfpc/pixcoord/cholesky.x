# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
# CHOLESKY -- Solve normal equations by Cholesky decomposition
#
# B.Simon	17-Jul-90	Original

procedure cholesky (nterm, lhs, rhs, solution)

int	nterm		#  i: Number of terms in solution vector
double	lhs[ARB]	# io: Left hand side of normal equations (matrix)
double	rhs[ARB]	# io: Right hand side of normal equations (vector)
double	solution[ARB]	#  o: Solution vector
#--
double	sum
int	i, j, k

begin
	# LU decomposition of normal equations. LHS is replaced by solution.

	lhs[1] = sqrt (lhs[1])
	do j = 2, nterm {
	    lhs[j*(j-1)/2+1] = lhs[j*(j-1)/2+1] / lhs[1]
	}

	do i = 2, nterm {
	    sum = 0.0
	    do k = 1, i-1 {
		sum = sum + lhs[i*(i-1)/2+k] ** 2
	    }
	    lhs[i*(i-1)/2+i] = sqrt (lhs[i*(i-1)/2+i] - sum)

	    do j = i+1, nterm {
		sum = 0.0
		do k = 1, i-1 {
		    sum = sum + lhs[i*(i-1)/2+k] * lhs[j*(j-1)/2+k]
		}
		lhs[j*(j-1)/2+i] = (lhs[j*(j-1)/2+i] - sum) / lhs[i*(i-1)/2+i]
	    }
	}

	# Forward pass. RHS is overwritten

	rhs[1] = rhs[1] / lhs[1]
	do i = 2, nterm {
	    sum = 0.0
	    do k = 1, i-1 {
		sum = sum + lhs[i*(i-1)/2+k] * rhs[k]
	    }
	    rhs[i] = (rhs[i] - sum) / lhs[i*(i-1)/2+i]
	}

	# Back pass, which produces solution vector

	solution[nterm] = rhs[nterm] / lhs[nterm*(nterm-1)/2+nterm]
	do i = nterm-1, 1, -1 {
	    sum = 0.0
	    do k = i+1, nterm {
		sum = sum + lhs[k*(k-1)/2+i] * solution[k]
	    }
	    solution[i] = (rhs[i] - sum) / lhs[i*(i-1)/2+i]
	}

end
