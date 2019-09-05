# FOURIER.CL -- Script to set up tasks in the IRAF fourier package

procedure fourier ()

string	mode="al"
#------------------------------------------------------------------------------
begin

package         fourier

task		autocorr,
		carith,
		crosscor,
		fconvolve,
		forward,
		inverse,
		powerspec,
		taperedge,
		topolar,
		frompolar,
		factor,
		listprimes,
		shift		= "fourier$x_fourier.e"

cl()
end
