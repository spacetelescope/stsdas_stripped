# SLITLESS.CL -- slitless extraction and simulations software
# Created: M. Kuemmel; (ST-ECF), 18-May-2010

procedure slitless()
string	mode="al"

begin
	set axe     = "slitless$axe/"
	set axesim  = "slitless$axe/"

	package slitless
        
        task axe.pkg    = "axe$axe.cl"
	task axesim.pkg = "axesim$axesim.cl"

	cl()
end
