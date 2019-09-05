# Copyright(c) 1994 Association of Universities for Research in Astronomy Inc.

# MEM.COM -- Include file for DMEM.X, MEM.X, storing some parameters and 
# variables for iteration in a common block.
 
define	E	1		# Array indexes of graddotgrad[4,4]
define	F	2	 	# E: chi-square	 F: total power
define	H	3		# H: entropy	 J: objective function
define	J	4	

real	chisq, chisq1, chisq2 	# Target chi-sq and its lower & upper limits
real	xchisq		 	# Current chi-squares
real	tp, xtp 		# Target and current total powers
real	alpha, beta		# Lagrange multipliers for chi-sq & total power
real	tol1sq			# Squared convergence tolerance for ME solution
real	damping			# Normalized damping factor 
real 	graddotgrad[4,4]	# Array containing dot products of gradients
real	gJ_on_gF		# |gradJ|/|gradF|
real 	test			# Parallelism test for ME solution 
real 	immax, immin		# Image maximum and minimum

common 	/me_com/ chisq, chisq1, chisq2, xchisq, tp, xtp, alpha, beta, tol1sq, 
	    damping, graddotgrad, gJ_on_gF, test, immax, immin 
