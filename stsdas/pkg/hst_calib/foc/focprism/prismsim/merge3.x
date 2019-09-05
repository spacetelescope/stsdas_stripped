# MERGE3 -- merge 3 different arrays of wavelengths and the corresponding
# arrays into one array
#
# Perry Greenfield, original 1-Nov-95

procedure merge3 (wave1, wave2, wave3, val1, val2, n1, n2, n3,
		wave, val, n, index)

real	wave1[ARB]	# i: first wavelength array
real	wave2[ARB]	# i: second ...
real	wave3[ARB]	# i: third ...
real	val1[ARB]	# i: values associated with wave1
real	val2[ARB]	# i: values associated with wave2
int	n1		# i: size of wave1
int	n2		# i: size of wave2
int	n3		# i: size of wave3
real	wave[ARB]	# o: combined wavelength array
real	val[ARB]	# o: combined interpolated values
int	n		# i: size of output arrays
int	index[ARB]	# o: index of wave3 into wave
#--

int 	tn1		# size of first set of combined arrays
int	lastj		# temp var used for linterp()
int	lastk		# temp var used for linterp()
int 	iw		# loop index 

pointer	twave		# temporary array for first merge
pointer index1		# index array for first merge, not needed later
pointer index2		# index array for first merge, not needed later
pointer tindex		# index array for second merge, not needed later
real	temp1
real	temp2

real	linterp()

begin
	lastj = 2
	lastk = 2

# allocate arrays

	tn1 = n1 + n2
	call malloc (twave, tn1, TY_REAL)
	call malloc (index1, n1, TY_INT)
	call malloc (index2, n2, TY_INT)
	call malloc (tindex, tn1, TY_INT)

# merge first two

	call mergearr (wave1, wave2, n1, n2, Memr[twave],
		Memi[index1], Memi[index2])

# merge result with last 

	call mergearr (Memr[twave], wave3, tn1, n3, wave,
		Memi[tindex], index)

# now interpolate val1 and val2 over merged wavelengths

	do iw = 1, n {
	    temp1 = linterp (wave[iw], wave1, val1, n1, lastj)
	    temp2 = linterp (wave[iw], wave2, val2, n2, lastk)
	    val[iw] = temp1 * temp2
	}
# done

	call mfree (twave, TY_REAL)
	call mfree (index1, TY_INT)
	call mfree (index2, TY_INT)
	call mfree (tindex, TY_INT)

end






