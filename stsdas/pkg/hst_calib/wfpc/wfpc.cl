#  wfpc.cl

procedure wfpc()
string	mode="al"

begin
	set calwfp = "wfpc$calwfp/"
	set calwp2 = "wfpc$calwp2/"
	set w_calib = "wfpc$w_calib/"

	package wfpc

	task	calwfp    = "calwfp$x_calwfp.e"
	task	calwp2    = "calwp2$x_calwp2.e"

	task	combine,
		bjdetect,
		crrej,
		engextr,
		invmetric,
		t_metric,
		mkdark,
		noisemodel,
		pixcoord,
		qmosaic,
		seam,
		uchcoord,
		uchscale,
		t_warmpix,
		wfixup,
		wmosaic,
		wstatistics = "wfpc$x_wfpc.e"

	task	checkwfpc = "wfpc$checkwfpc.cl"
	task	metric  = "wfpc$metric.cl"
	task	warmpix  = "wfpc$warmpix.cl"
	hidetask t_metric
	hidetask t_warmpix
	task	w_calib.pkg = "w_calib$w_calib.cl"
        pyexecute("wfpc$wfpc2destreak_iraf.py",tasknames="wdestreak") 



# Psets for combine & noisemodel:
	task	dqfpar = "wfpc$dqfpar.par"
	task	dqpar  = "wfpc$dqpar.par"
	task	dq     = "wfpc$dq.par"
	task	noisepar = "wfpc$noisepar.par"

	cl()

end
