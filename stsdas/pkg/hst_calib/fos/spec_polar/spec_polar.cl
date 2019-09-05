
# SPEC_POLAR.CL -- Spectropolarimetry tasks for the FOS
# Created: R.L. Williamson, 14-Jul-1993
# Modified: H.A. Bushouse,  13-Aug-1993; Added new tasks
# Modified: H.A. Bushouse,  24-Aug-1993; Changed 'specpol' to 'calpolar'
# Modified: H.A. Bushouse,  02-Dec-1993; Added new tasks
#

procedure spec_polar()
string	mode="al"

begin
	package spec_polar

	task	calpolar,
		comparesets,
		pcombine,
		plbias,
		polave,
		polbin,
		polcalc,
		polnorm,
		polplot = "spec_polar$x_spec_polar.e"

	cl()
end
