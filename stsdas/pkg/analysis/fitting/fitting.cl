procedure fitting()
string	mode="al"

begin
	package fitting

	task prfit,
	     function,
	     gfit1d,
	     nfit1d,
	     ngaussfit,
	     n2gaussfit  = "fitting$x_fitting.e"

	task i2gaussfit  = "fitting$i2gaussfit.cl"

	task powerpars   = "fitting$powerpars.par"
	task bbodypars   = "fitting$bbodypars.par"
	task comppars    = "fitting$comppars.par"
	task twobbpars   = "fitting$twobbpars.par"
	task userpars    = "fitting$userpars.par"
	task gausspars   = "fitting$gausspars.par"
	task cgausspars  = "fitting$cgausspars.par"
	task tgausspars  = "fitting$tgausspars.par"
	task galprofpars = "fitting$galprofpars.par"
	task controlpars = "fitting$controlpars.par"
	task samplepars  = "fitting$samplepars.par"
	task errorpars   = "fitting$errorpars.par"

	cl()
end
