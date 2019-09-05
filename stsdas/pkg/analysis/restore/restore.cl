# RESTORE.CL -- Script to set up tasks in the RESTORE package

procedure restore()

string	mode="al"

begin
	package restore

	task	adaptive,
		hfilter,
		jansson,
		mem,
		sclean,
		wiener,
                lucy = "restore$x_restore.e"
	hidetask jansson

# Psets for wiener task
	task	filterpars	= "restore$filterpars.par"
	task	modelpars	= "restore$modelpars.par"
	task	noisepars	= "restore$noisepars.par"
	task	psfpars		= "restore$psfpars.par"
	task	lowpars		= "restore$lowpars.par"

	clbye
end
