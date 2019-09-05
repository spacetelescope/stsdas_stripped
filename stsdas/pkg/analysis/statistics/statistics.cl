# STATISTICS.CL -- Script to set up tasks in the IRAF statistics package

procedure statistics()

string mode = 'a'

begin

	package statistics

	task bhkmethod,
	     buckleyjames,
	     coxhazard,
	     emmethod,
	     kmestimate,
	     kolmov,
	     schmittbin,
	     spearman,
	     twosampt		= "statistics$x_statistics.e"

	task $censor = "statistics$censor.cl"
	task $survival = "statistics$survival.cl"

	clbye

end
