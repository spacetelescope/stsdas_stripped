procedure hrs()
string	mode="al"

begin

	package hrs
	

	task 	dopoff,
                obsum,
		reflux,
		spiralmap,
		tacount,
		waveoff,
		zavgtemp,
		zwavefit,
		zwaveid = "hrs$x_hrs.e"

	task calhrs = "hrs$x_calhrs.e"

	task showspiral = "hrs$spiralmap/showspiral.cl"
	task zwavecal = "hrs$zwavecal.cl"

	# Psets.
	task findpars = "hrs$findpars.par"
	task fitpars = "hrs$fitpars.par"
	task linetabpar = "hrs$linetabpar.par"
	
	# Hide some tasks.
	hidetask findpars, fitpars, linetabpar
	hidetask zavgtemp, zwavefit, zwaveid
	
	cl()
end
